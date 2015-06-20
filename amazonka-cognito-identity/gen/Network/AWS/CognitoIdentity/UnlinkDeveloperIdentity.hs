{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Unlinks a @DeveloperUserIdentifier@ from an existing identity. Unlinked
-- developer users will be considered new identities next time they are
-- seen. If, for a given Cognito identity, you remove all federated
-- identities as well as the developer user identifier, the Cognito
-- identity becomes inaccessible.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_UnlinkDeveloperIdentity.html>
module Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
    (
    -- * Request
      UnlinkDeveloperIdentity
    -- ** Request constructor
    , unlinkDeveloperIdentity
    -- ** Request lenses
    , udiIdentityId
    , udiIdentityPoolId
    , udiDeveloperProviderName
    , udiDeveloperUserIdentifier

    -- * Response
    , UnlinkDeveloperIdentityResponse
    -- ** Response constructor
    , unlinkDeveloperIdentityResponse
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unlinkDeveloperIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udiIdentityId'
--
-- * 'udiIdentityPoolId'
--
-- * 'udiDeveloperProviderName'
--
-- * 'udiDeveloperUserIdentifier'
data UnlinkDeveloperIdentity = UnlinkDeveloperIdentity'{_udiIdentityId :: Text, _udiIdentityPoolId :: Text, _udiDeveloperProviderName :: Text, _udiDeveloperUserIdentifier :: Text} deriving (Eq, Read, Show)

-- | 'UnlinkDeveloperIdentity' smart constructor.
unlinkDeveloperIdentity :: Text -> Text -> Text -> Text -> UnlinkDeveloperIdentity
unlinkDeveloperIdentity pIdentityId pIdentityPoolId pDeveloperProviderName pDeveloperUserIdentifier = UnlinkDeveloperIdentity'{_udiIdentityId = pIdentityId, _udiIdentityPoolId = pIdentityPoolId, _udiDeveloperProviderName = pDeveloperProviderName, _udiDeveloperUserIdentifier = pDeveloperUserIdentifier};

-- | A unique identifier in the format REGION:GUID.
udiIdentityId :: Lens' UnlinkDeveloperIdentity Text
udiIdentityId = lens _udiIdentityId (\ s a -> s{_udiIdentityId = a});

-- | An identity pool ID in the format REGION:GUID.
udiIdentityPoolId :: Lens' UnlinkDeveloperIdentity Text
udiIdentityPoolId = lens _udiIdentityPoolId (\ s a -> s{_udiIdentityPoolId = a});

-- | The \"domain\" by which Cognito will refer to your users.
udiDeveloperProviderName :: Lens' UnlinkDeveloperIdentity Text
udiDeveloperProviderName = lens _udiDeveloperProviderName (\ s a -> s{_udiDeveloperProviderName = a});

-- | A unique ID used by your backend authentication process to identify a
-- user.
udiDeveloperUserIdentifier :: Lens' UnlinkDeveloperIdentity Text
udiDeveloperUserIdentifier = lens _udiDeveloperUserIdentifier (\ s a -> s{_udiDeveloperUserIdentifier = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest UnlinkDeveloperIdentity where
        type Sv UnlinkDeveloperIdentity = CognitoIdentity
        type Rs UnlinkDeveloperIdentity =
             UnlinkDeveloperIdentityResponse
        request = postJSON
        response
          = receiveNull UnlinkDeveloperIdentityResponse'

instance ToHeaders UnlinkDeveloperIdentity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.UnlinkDeveloperIdentity"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UnlinkDeveloperIdentity where
        toJSON UnlinkDeveloperIdentity'{..}
          = object
              ["IdentityId" .= _udiIdentityId,
               "IdentityPoolId" .= _udiIdentityPoolId,
               "DeveloperProviderName" .= _udiDeveloperProviderName,
               "DeveloperUserIdentifier" .=
                 _udiDeveloperUserIdentifier]

instance ToPath UnlinkDeveloperIdentity where
        toPath = const "/"

instance ToQuery UnlinkDeveloperIdentity where
        toQuery = const mempty

-- | /See:/ 'unlinkDeveloperIdentityResponse' smart constructor.
data UnlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse' deriving (Eq, Read, Show)

-- | 'UnlinkDeveloperIdentityResponse' smart constructor.
unlinkDeveloperIdentityResponse :: UnlinkDeveloperIdentityResponse
unlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse';
