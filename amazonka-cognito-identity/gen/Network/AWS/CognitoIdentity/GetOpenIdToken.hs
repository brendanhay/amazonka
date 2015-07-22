{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetOpenIdToken
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets an OpenID token, using a known Cognito ID. This known Cognito ID is
-- returned by GetId. You can optionally add additional logins for the
-- identity. Supplying multiple logins creates an implicit link.
--
-- The OpenId token is valid for 15 minutes.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetOpenIdToken.html>
module Network.AWS.CognitoIdentity.GetOpenIdToken
    (
    -- * Request
      GetOpenIdToken
    -- ** Request constructor
    , getOpenIdToken
    -- ** Request lenses
    , goitrqLogins
    , goitrqIdentityId

    -- * Response
    , GetOpenIdTokenResponse
    -- ** Response constructor
    , getOpenIdTokenResponse
    -- ** Response lenses
    , goitrsToken
    , goitrsIdentityId
    , goitrsStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the GetOpenIdToken action.
--
-- /See:/ 'getOpenIdToken' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitrqLogins'
--
-- * 'goitrqIdentityId'
data GetOpenIdToken = GetOpenIdToken'
    { _goitrqLogins     :: !(Maybe (Map Text Text))
    , _goitrqIdentityId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOpenIdToken' smart constructor.
getOpenIdToken :: Text -> GetOpenIdToken
getOpenIdToken pIdentityId =
    GetOpenIdToken'
    { _goitrqLogins = Nothing
    , _goitrqIdentityId = pIdentityId
    }

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. When using graph.facebook.com and www.amazon.com, supply the
-- access_token returned from the provider\'s authflow. For
-- accounts.google.com or any other OpenId Connect provider, always include
-- the id_token.
goitrqLogins :: Lens' GetOpenIdToken (HashMap Text Text)
goitrqLogins = lens _goitrqLogins (\ s a -> s{_goitrqLogins = a}) . _Default . _Map;

-- | A unique identifier in the format REGION:GUID.
goitrqIdentityId :: Lens' GetOpenIdToken Text
goitrqIdentityId = lens _goitrqIdentityId (\ s a -> s{_goitrqIdentityId = a});

instance AWSRequest GetOpenIdToken where
        type Sv GetOpenIdToken = CognitoIdentity
        type Rs GetOpenIdToken = GetOpenIdTokenResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetOpenIdTokenResponse' <$>
                   (x .?> "Token") <*> (x .?> "IdentityId") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetOpenIdToken where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.GetOpenIdToken" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOpenIdToken where
        toJSON GetOpenIdToken'{..}
          = object
              ["Logins" .= _goitrqLogins,
               "IdentityId" .= _goitrqIdentityId]

instance ToPath GetOpenIdToken where
        toPath = const "/"

instance ToQuery GetOpenIdToken where
        toQuery = const mempty

-- | Returned in response to a successful GetOpenIdToken request.
--
-- /See:/ 'getOpenIdTokenResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goitrsToken'
--
-- * 'goitrsIdentityId'
--
-- * 'goitrsStatus'
data GetOpenIdTokenResponse = GetOpenIdTokenResponse'
    { _goitrsToken      :: !(Maybe Text)
    , _goitrsIdentityId :: !(Maybe Text)
    , _goitrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOpenIdTokenResponse' smart constructor.
getOpenIdTokenResponse :: Int -> GetOpenIdTokenResponse
getOpenIdTokenResponse pStatus =
    GetOpenIdTokenResponse'
    { _goitrsToken = Nothing
    , _goitrsIdentityId = Nothing
    , _goitrsStatus = pStatus
    }

-- | An OpenID token, valid for 15 minutes.
goitrsToken :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrsToken = lens _goitrsToken (\ s a -> s{_goitrsToken = a});

-- | A unique identifier in the format REGION:GUID. Note that the IdentityId
-- returned may not match the one passed on input.
goitrsIdentityId :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrsIdentityId = lens _goitrsIdentityId (\ s a -> s{_goitrsIdentityId = a});

-- | FIXME: Undocumented member.
goitrsStatus :: Lens' GetOpenIdTokenResponse Int
goitrsStatus = lens _goitrsStatus (\ s a -> s{_goitrsStatus = a});
