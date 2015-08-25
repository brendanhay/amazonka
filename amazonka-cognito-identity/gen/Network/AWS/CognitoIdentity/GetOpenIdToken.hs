{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetOpenIdToken
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetOpenIdToken.html AWS API Reference> for GetOpenIdToken.
module Network.AWS.CognitoIdentity.GetOpenIdToken
    (
    -- * Creating a Request
      getOpenIdToken
    , GetOpenIdToken
    -- * Request Lenses
    , goitLogins
    , goitIdentityId

    -- * Destructuring the Response
    , getOpenIdTokenResponse
    , GetOpenIdTokenResponse
    -- * Response Lenses
    , goitrsToken
    , goitrsIdentityId
    , goitrsStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.CognitoIdentity.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the GetOpenIdToken action.
--
-- /See:/ 'getOpenIdToken' smart constructor.
data GetOpenIdToken = GetOpenIdToken'
    { _goitLogins     :: !(Maybe (Map Text Text))
    , _goitIdentityId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetOpenIdToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goitLogins'
--
-- * 'goitIdentityId'
getOpenIdToken
    :: Text -- ^ 'goitIdentityId'
    -> GetOpenIdToken
getOpenIdToken pIdentityId_ =
    GetOpenIdToken'
    { _goitLogins = Nothing
    , _goitIdentityId = pIdentityId_
    }

-- | A set of optional name-value pairs that map provider names to provider
-- tokens. When using graph.facebook.com and www.amazon.com, supply the
-- access_token returned from the provider\'s authflow. For
-- accounts.google.com or any other OpenId Connect provider, always include
-- the id_token.
goitLogins :: Lens' GetOpenIdToken (HashMap Text Text)
goitLogins = lens _goitLogins (\ s a -> s{_goitLogins = a}) . _Default . _Map;

-- | A unique identifier in the format REGION:GUID.
goitIdentityId :: Lens' GetOpenIdToken Text
goitIdentityId = lens _goitIdentityId (\ s a -> s{_goitIdentityId = a});

instance AWSRequest GetOpenIdToken where
        type Rs GetOpenIdToken = GetOpenIdTokenResponse
        request = postJSON cognitoIdentity
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
              (catMaybes
                 [("Logins" .=) <$> _goitLogins,
                  Just ("IdentityId" .= _goitIdentityId)])

instance ToPath GetOpenIdToken where
        toPath = const "/"

instance ToQuery GetOpenIdToken where
        toQuery = const mempty

-- | Returned in response to a successful GetOpenIdToken request.
--
-- /See:/ 'getOpenIdTokenResponse' smart constructor.
data GetOpenIdTokenResponse = GetOpenIdTokenResponse'
    { _goitrsToken      :: !(Maybe Text)
    , _goitrsIdentityId :: !(Maybe Text)
    , _goitrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetOpenIdTokenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goitrsToken'
--
-- * 'goitrsIdentityId'
--
-- * 'goitrsStatus'
getOpenIdTokenResponse
    :: Int -- ^ 'goitrsStatus'
    -> GetOpenIdTokenResponse
getOpenIdTokenResponse pStatus_ =
    GetOpenIdTokenResponse'
    { _goitrsToken = Nothing
    , _goitrsIdentityId = Nothing
    , _goitrsStatus = pStatus_
    }

-- | An OpenID token, valid for 15 minutes.
goitrsToken :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrsToken = lens _goitrsToken (\ s a -> s{_goitrsToken = a});

-- | A unique identifier in the format REGION:GUID. Note that the IdentityId
-- returned may not match the one passed on input.
goitrsIdentityId :: Lens' GetOpenIdTokenResponse (Maybe Text)
goitrsIdentityId = lens _goitrsIdentityId (\ s a -> s{_goitrsIdentityId = a});

-- | The response status code.
goitrsStatus :: Lens' GetOpenIdTokenResponse Int
goitrsStatus = lens _goitrsStatus (\ s a -> s{_goitrsStatus = a});
