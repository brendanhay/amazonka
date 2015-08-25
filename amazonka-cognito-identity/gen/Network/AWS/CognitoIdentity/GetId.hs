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
-- Module      : Network.AWS.CognitoIdentity.GetId
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates (or retrieves) a Cognito ID. Supplying multiple logins will
-- create an implicit linked account.
--
-- token+\";\"+tokenSecret.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- /See:/ <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetId.html AWS API Reference> for GetId.
module Network.AWS.CognitoIdentity.GetId
    (
    -- * Creating a Request
      getId
    , GetId
    -- * Request Lenses
    , giAccountId
    , giLogins
    , giIdentityPoolId

    -- * Destructuring the Response
    , getIdResponse
    , GetIdResponse
    -- * Response Lenses
    , girsIdentityId
    , girsStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.CognitoIdentity.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the GetId action.
--
-- /See:/ 'getId' smart constructor.
data GetId = GetId'
    { _giAccountId      :: !(Maybe Text)
    , _giLogins         :: !(Maybe (Map Text Text))
    , _giIdentityPoolId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giAccountId'
--
-- * 'giLogins'
--
-- * 'giIdentityPoolId'
getId
    :: Text -- ^ 'giIdentityPoolId'
    -> GetId
getId pIdentityPoolId_ =
    GetId'
    { _giAccountId = Nothing
    , _giLogins = Nothing
    , _giIdentityPoolId = pIdentityPoolId_
    }

-- | A standard AWS account ID (9+ digits).
giAccountId :: Lens' GetId (Maybe Text)
giAccountId = lens _giAccountId (\ s a -> s{_giAccountId = a});

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
--
-- The available provider names for 'Logins' are as follows:
--
-- -   Facebook: 'graph.facebook.com'
-- -   Google: 'accounts.google.com'
-- -   Amazon: 'www.amazon.com'
-- -   Twitter: 'www.twitter.com'
-- -   Digits: 'www.digits.com'
giLogins :: Lens' GetId (HashMap Text Text)
giLogins = lens _giLogins (\ s a -> s{_giLogins = a}) . _Default . _Map;

-- | An identity pool ID in the format REGION:GUID.
giIdentityPoolId :: Lens' GetId Text
giIdentityPoolId = lens _giIdentityPoolId (\ s a -> s{_giIdentityPoolId = a});

instance AWSRequest GetId where
        type Rs GetId = GetIdResponse
        request = postJSON cognitoIdentity
        response
          = receiveJSON
              (\ s h x ->
                 GetIdResponse' <$>
                   (x .?> "IdentityId") <*> (pure (fromEnum s)))

instance ToHeaders GetId where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.GetId" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetId where
        toJSON GetId'{..}
          = object
              (catMaybes
                 [("AccountId" .=) <$> _giAccountId,
                  ("Logins" .=) <$> _giLogins,
                  Just ("IdentityPoolId" .= _giIdentityPoolId)])

instance ToPath GetId where
        toPath = const "/"

instance ToQuery GetId where
        toQuery = const mempty

-- | Returned in response to a GetId request.
--
-- /See:/ 'getIdResponse' smart constructor.
data GetIdResponse = GetIdResponse'
    { _girsIdentityId :: !(Maybe Text)
    , _girsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetIdResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsIdentityId'
--
-- * 'girsStatus'
getIdResponse
    :: Int -- ^ 'girsStatus'
    -> GetIdResponse
getIdResponse pStatus_ =
    GetIdResponse'
    { _girsIdentityId = Nothing
    , _girsStatus = pStatus_
    }

-- | A unique identifier in the format REGION:GUID.
girsIdentityId :: Lens' GetIdResponse (Maybe Text)
girsIdentityId = lens _girsIdentityId (\ s a -> s{_girsIdentityId = a});

-- | The response status code.
girsStatus :: Lens' GetIdResponse Int
girsStatus = lens _girsStatus (\ s a -> s{_girsStatus = a});
