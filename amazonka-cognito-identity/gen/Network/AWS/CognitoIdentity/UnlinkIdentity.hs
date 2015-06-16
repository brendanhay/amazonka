{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoIdentity.UnlinkIdentity
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

-- | Unlinks a federated identity from an existing account. Unlinked logins
-- will be considered new identities next time they are seen. Removing the
-- last linked login will make this identity inaccessible.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_UnlinkIdentity.html>
module Network.AWS.CognitoIdentity.UnlinkIdentity
    (
    -- * Request
      UnlinkIdentity
    -- ** Request constructor
    , unlinkIdentity
    -- ** Request lenses
    , uiIdentityId
    , uiLogins
    , uiLoginsToRemove

    -- * Response
    , UnlinkIdentityResponse
    -- ** Response constructor
    , unlinkIdentityResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CognitoIdentity.Types

-- | /See:/ 'unlinkIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uiIdentityId'
--
-- * 'uiLogins'
--
-- * 'uiLoginsToRemove'
data UnlinkIdentity = UnlinkIdentity'{_uiIdentityId :: Text, _uiLogins :: Map Text Text, _uiLoginsToRemove :: [Text]} deriving (Eq, Read, Show)

-- | 'UnlinkIdentity' smart constructor.
unlinkIdentity :: Text -> UnlinkIdentity
unlinkIdentity pIdentityId = UnlinkIdentity'{_uiIdentityId = pIdentityId, _uiLogins = mempty, _uiLoginsToRemove = mempty};

-- | A unique identifier in the format REGION:GUID.
uiIdentityId :: Lens' UnlinkIdentity Text
uiIdentityId = lens _uiIdentityId (\ s a -> s{_uiIdentityId = a});

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
uiLogins :: Lens' UnlinkIdentity (Map Text Text)
uiLogins = lens _uiLogins (\ s a -> s{_uiLogins = a}) . _Map;

-- | Provider names to unlink from this identity.
uiLoginsToRemove :: Lens' UnlinkIdentity [Text]
uiLoginsToRemove = lens _uiLoginsToRemove (\ s a -> s{_uiLoginsToRemove = a});

instance AWSRequest UnlinkIdentity where
        type Sv UnlinkIdentity = CognitoIdentity
        type Rs UnlinkIdentity = UnlinkIdentityResponse
        request = postJSON
        response = receiveNull UnlinkIdentityResponse'

instance ToHeaders UnlinkIdentity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.UnlinkIdentity" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UnlinkIdentity where
        toJSON UnlinkIdentity'{..}
          = object
              ["IdentityId" .= _uiIdentityId,
               "Logins" .= _uiLogins,
               "LoginsToRemove" .= _uiLoginsToRemove]

instance ToPath UnlinkIdentity where
        toPath = const "/"

instance ToQuery UnlinkIdentity where
        toQuery = const mempty

-- | /See:/ 'unlinkIdentityResponse' smart constructor.
data UnlinkIdentityResponse = UnlinkIdentityResponse' deriving (Eq, Read, Show)

-- | 'UnlinkIdentityResponse' smart constructor.
unlinkIdentityResponse :: UnlinkIdentityResponse
unlinkIdentityResponse = UnlinkIdentityResponse';
