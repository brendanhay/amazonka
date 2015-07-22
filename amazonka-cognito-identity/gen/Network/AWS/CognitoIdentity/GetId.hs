{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetId
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Generates (or retrieves) a Cognito ID. Supplying multiple logins will
-- create an implicit linked account.
--
-- token+\";\"+tokenSecret.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetId.html>
module Network.AWS.CognitoIdentity.GetId
    (
    -- * Request
      GetId
    -- ** Request constructor
    , getId
    -- ** Request lenses
    , girqAccountId
    , girqLogins
    , girqIdentityPoolId

    -- * Response
    , GetIdResponse
    -- ** Response constructor
    , getIdResponse
    -- ** Response lenses
    , girsIdentityId
    , girsStatus
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the GetId action.
--
-- /See:/ 'getId' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girqAccountId'
--
-- * 'girqLogins'
--
-- * 'girqIdentityPoolId'
data GetId = GetId'
    { _girqAccountId      :: !(Maybe Text)
    , _girqLogins         :: !(Maybe (Map Text Text))
    , _girqIdentityPoolId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetId' smart constructor.
getId :: Text -> GetId
getId pIdentityPoolId_ =
    GetId'
    { _girqAccountId = Nothing
    , _girqLogins = Nothing
    , _girqIdentityPoolId = pIdentityPoolId_
    }

-- | A standard AWS account ID (9+ digits).
girqAccountId :: Lens' GetId (Maybe Text)
girqAccountId = lens _girqAccountId (\ s a -> s{_girqAccountId = a});

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
--
-- The available provider names for @Logins@ are as follows:
--
-- -   Facebook: @graph.facebook.com@
-- -   Google: @accounts.google.com@
-- -   Amazon: @www.amazon.com@
-- -   Twitter: @www.twitter.com@
-- -   Digits: @www.digits.com@
girqLogins :: Lens' GetId (HashMap Text Text)
girqLogins = lens _girqLogins (\ s a -> s{_girqLogins = a}) . _Default . _Map;

-- | An identity pool ID in the format REGION:GUID.
girqIdentityPoolId :: Lens' GetId Text
girqIdentityPoolId = lens _girqIdentityPoolId (\ s a -> s{_girqIdentityPoolId = a});

instance AWSRequest GetId where
        type Sv GetId = CognitoIdentity
        type Rs GetId = GetIdResponse
        request = postJSON
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
              ["AccountId" .= _girqAccountId,
               "Logins" .= _girqLogins,
               "IdentityPoolId" .= _girqIdentityPoolId]

instance ToPath GetId where
        toPath = const "/"

instance ToQuery GetId where
        toQuery = const mempty

-- | Returned in response to a GetId request.
--
-- /See:/ 'getIdResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girsIdentityId'
--
-- * 'girsStatus'
data GetIdResponse = GetIdResponse'
    { _girsIdentityId :: !(Maybe Text)
    , _girsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetIdResponse' smart constructor.
getIdResponse :: Int -> GetIdResponse
getIdResponse pStatus_ =
    GetIdResponse'
    { _girsIdentityId = Nothing
    , _girsStatus = pStatus_
    }

-- | A unique identifier in the format REGION:GUID.
girsIdentityId :: Lens' GetIdResponse (Maybe Text)
girsIdentityId = lens _girsIdentityId (\ s a -> s{_girsIdentityId = a});

-- | FIXME: Undocumented member.
girsStatus :: Lens' GetIdResponse Int
girsStatus = lens _girsStatus (\ s a -> s{_girsStatus = a});
