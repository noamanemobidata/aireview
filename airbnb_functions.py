import random
import requests
import json
import datetime
import os
import functools
import random
import uuid
import time
import re



API_URL = os.getenv("AIRBNB_URL")
API_KEY = os.getenv("AIRBNB_KEY") 


class RandomRequest(object):

    @classmethod
    def get_random_user_agent(cls):
        with open(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'files/supported_ios_versions.txt')) as f:
            ios_versions = f.read().splitlines()
        with open(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'files/airbnb_versions.txt')) as f:
            airbnb_versions = f.read().splitlines()

        return "Airbnb/{} iPhone/{} Type/Phone".format(random.choice(airbnb_versions), random.choice(ios_versions))

    @classmethod
    def get_random_udid(cls):
        hex_digits = "0123456789abcdef"
        return ''.join(random.choice(hex_digits) for _ in range(40))

    @classmethod
    def get_random_uuid(cls):
        return str(uuid.uuid4()).upper()

class AuthError(Exception):
    """
    Authentication error
    """
    pass


class VerificationError(AuthError):
    """
    Authentication error
    """
    pass


class MissingParameterError(Exception):
    """
    Missing parameter error
    """
    pass


class MissingAccessTokenError(MissingParameterError):
    """
    Missing access token error
    """
    pass


def require_auth(function):
    """
    A decorator that wraps the passed in function and raises exception
    if access token is missing
    """
    @functools.wraps(function)
    def wrapper(self, *args, **kwargs):
        if not self.access_token():
            raise MissingAccessTokenError
        return function(self, *args, **kwargs)
    return wrapper


def randomizable(function):
    """
    A decorator which randomizes requests if needed
    """
    @functools.wraps(function)
    def wrapper(self, *args, **kwargs):
        if self.randomize:
            self.randomize_headers()
        return function(self, *args, **kwargs)
    return wrapper


class Api(object):
    """ Base API class
    """

    def __init__(self, username=None, password=None, access_token=None, api_key=API_KEY, session_cookie=None,
                 proxy=None, randomize=None):
        self._session = requests.Session()
        self._access_token = None
        self.user_agent = "Airbnb/19.18 AppVersion/19.18 iPhone/12.2 Type/Phone"
        self.udid = "9120210f8fb1ae837affff54a0a2f64da821d227"
        self.uuid = "C326397B-3A38-474B-973B-F022E6E4E6CC"
        self.randomize = randomize

        self._session.headers = {
            "accept": "application/json",
            "accept-encoding": "br, gzip, deflate",
            "content-type": "application/json",
            "x-airbnb-api-key": api_key,
            "user-agent": self.user_agent,
            "x-airbnb-screensize": "w=375.00;h=812.00",
            "x-airbnb-carrier-name": "T-Mobile",
            "x-airbnb-network-type": "wifi",
            "x-airbnb-currency": "USD",
            "x-airbnb-locale": "en",
            "x-airbnb-carrier-country": "us",
            "accept-language": "en-us",
            "airbnb-device-id": self.udid,
            "x-airbnb-advertising-id": self.uuid
        }

        if proxy:
            self._session.proxies = {
                "http": proxy,
                "https": proxy
            }

        if access_token:
            self._access_token = access_token

            if session_cookie and "_airbed_session_id=" in session_cookie:
                self._session.headers.update({
                    "Cookie": session_cookie
                })

            self._session.headers.update({
                "x-airbnb-oauth-token": self._access_token
            })

        elif username and password:
            login_payload = {"email": username,
                             "password": password,
                             "type": "email"}

            r = self._session.post(
                API_URL + "/logins", data=json.dumps(login_payload)
            )

            if r.status_code == 420:
                raise VerificationError
            elif r.status_code == 403:
                raise AuthError

            self._access_token = r.json()["login"]["id"]

            print("Your access token: {}".format(self._access_token))

            self._session.headers.update({
                "x-airbnb-oauth-token": self._access_token
            })
        else:
            # no auth
            pass

    def access_token(self):
        return self._access_token

    def set_user_agent(self, user_agent):
        self.user_agent = user_agent
        self._session.headers['user-agent'] = user_agent

    def set_udid(self, udid):
        self.udid = udid
        self._session.headers['airbnb-device-id'] = udid

    def set_uuid(self, uuid):
        self.uuid = uuid
        self._session.headers['x-airbnb-advertising-id'] = uuid

    def randomize_headers(self):
        self.set_user_agent(RandomRequest.get_random_user_agent())
        self.set_udid(RandomRequest.get_random_udid())
        self.set_uuid(RandomRequest.get_random_uuid())

    @require_auth
    def get_profile(self):
        """
        Get my own profile
        """
        r = self._session.get(API_URL + "/logins/me")
        r.raise_for_status()

        return r.json()


    @randomizable
    def get_reviews(self, listing_id, offset=0, limit=100):
        """
        Get reviews for a given listing
        """
        params = {
            '_order': 'language_country',
            'listing_id': str(listing_id),
            '_offset': str(offset),
            'role': 'all',
            '_limit': str(limit),
            '_format': 'for_mobile_client',
        }

        print(self._session.headers)

        r = self._session.get(API_URL + "/reviews", params=params)
        r.raise_for_status()

        return r.json()

def get_all_reviews(listing_id,limit=100):
  
    offset = 0
    all_reviews = []

    while True:
        api= Api(randomize=False)
        reviews = api.get_reviews(listing_id, offset=offset, limit=limit)
        all_reviews.extend(reviews["reviews"])
        offset += len(reviews["reviews"])
        time.sleep(1)
        if len(reviews["reviews"])<limit:
            break  
  
    
    return all_reviews
