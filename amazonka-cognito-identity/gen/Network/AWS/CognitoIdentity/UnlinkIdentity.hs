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
-- Module      : Network.AWS.CognitoIdentity.UnlinkIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a federated identity from an existing account. Unlinked logins will be considered new identities next time they are seen. Removing the last linked login will make this identity inaccessible.
--
--
-- This is a public API. You do not need any credentials to call this API.
--
module Network.AWS.CognitoIdentity.UnlinkIdentity
    (
    -- * Creating a Request
      unlinkIdentity
    , UnlinkIdentity
    -- * Request Lenses
    , uiIdentityId
    , uiLogins
    , uiLoginsToRemove

    -- * Destructuring the Response
    , unlinkIdentityResponse
    , UnlinkIdentityResponse
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the UnlinkIdentity action.
--
--
--
-- /See:/ 'unlinkIdentity' smart constructor.
data UnlinkIdentity = UnlinkIdentity'
  { _uiIdentityId     :: !Text
  , _uiLogins         :: !(Map Text Text)
  , _uiLoginsToRemove :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnlinkIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiIdentityId' - A unique identifier in the format REGION:GUID.
--
-- * 'uiLogins' - A set of optional name-value pairs that map provider names to provider tokens.
--
-- * 'uiLoginsToRemove' - Provider names to unlink from this identity.
unlinkIdentity
    :: Text -- ^ 'uiIdentityId'
    -> UnlinkIdentity
unlinkIdentity pIdentityId_ =
  UnlinkIdentity'
    { _uiIdentityId = pIdentityId_
    , _uiLogins = mempty
    , _uiLoginsToRemove = mempty
    }


-- | A unique identifier in the format REGION:GUID.
uiIdentityId :: Lens' UnlinkIdentity Text
uiIdentityId = lens _uiIdentityId (\ s a -> s{_uiIdentityId = a})

-- | A set of optional name-value pairs that map provider names to provider tokens.
uiLogins :: Lens' UnlinkIdentity (HashMap Text Text)
uiLogins = lens _uiLogins (\ s a -> s{_uiLogins = a}) . _Map

-- | Provider names to unlink from this identity.
uiLoginsToRemove :: Lens' UnlinkIdentity [Text]
uiLoginsToRemove = lens _uiLoginsToRemove (\ s a -> s{_uiLoginsToRemove = a}) . _Coerce

instance AWSRequest UnlinkIdentity where
        type Rs UnlinkIdentity = UnlinkIdentityResponse
        request = postJSON cognitoIdentity
        response = receiveNull UnlinkIdentityResponse'

instance Hashable UnlinkIdentity where

instance NFData UnlinkIdentity where

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
              (catMaybes
                 [Just ("IdentityId" .= _uiIdentityId),
                  Just ("Logins" .= _uiLogins),
                  Just ("LoginsToRemove" .= _uiLoginsToRemove)])

instance ToPath UnlinkIdentity where
        toPath = const "/"

instance ToQuery UnlinkIdentity where
        toQuery = const mempty

-- | /See:/ 'unlinkIdentityResponse' smart constructor.
data UnlinkIdentityResponse =
  UnlinkIdentityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnlinkIdentityResponse' with the minimum fields required to make a request.
--
unlinkIdentityResponse
    :: UnlinkIdentityResponse
unlinkIdentityResponse = UnlinkIdentityResponse'


instance NFData UnlinkIdentityResponse where
