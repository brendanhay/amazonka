{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.UnlinkIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a federated identity from an existing account. Unlinked logins
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
    , uirqIdentityId
    , uirqLogins
    , uirqLoginsToRemove

    -- * Response
    , UnlinkIdentityResponse
    -- ** Response constructor
    , unlinkIdentityResponse
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the UnlinkIdentity action.
--
-- /See:/ 'unlinkIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uirqIdentityId'
--
-- * 'uirqLogins'
--
-- * 'uirqLoginsToRemove'
data UnlinkIdentity = UnlinkIdentity'
    { _uirqIdentityId     :: !Text
    , _uirqLogins         :: !(Map Text Text)
    , _uirqLoginsToRemove :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnlinkIdentity' smart constructor.
unlinkIdentity :: Text -> UnlinkIdentity
unlinkIdentity pIdentityId =
    UnlinkIdentity'
    { _uirqIdentityId = pIdentityId
    , _uirqLogins = mempty
    , _uirqLoginsToRemove = mempty
    }

-- | A unique identifier in the format REGION:GUID.
uirqIdentityId :: Lens' UnlinkIdentity Text
uirqIdentityId = lens _uirqIdentityId (\ s a -> s{_uirqIdentityId = a});

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
uirqLogins :: Lens' UnlinkIdentity (HashMap Text Text)
uirqLogins = lens _uirqLogins (\ s a -> s{_uirqLogins = a}) . _Map;

-- | Provider names to unlink from this identity.
uirqLoginsToRemove :: Lens' UnlinkIdentity [Text]
uirqLoginsToRemove = lens _uirqLoginsToRemove (\ s a -> s{_uirqLoginsToRemove = a});

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
              ["IdentityId" .= _uirqIdentityId,
               "Logins" .= _uirqLogins,
               "LoginsToRemove" .= _uirqLoginsToRemove]

instance ToPath UnlinkIdentity where
        toPath = const "/"

instance ToQuery UnlinkIdentity where
        toQuery = const mempty

-- | /See:/ 'unlinkIdentityResponse' smart constructor.
data UnlinkIdentityResponse =
    UnlinkIdentityResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnlinkIdentityResponse' smart constructor.
unlinkIdentityResponse :: UnlinkIdentityResponse
unlinkIdentityResponse = UnlinkIdentityResponse'
