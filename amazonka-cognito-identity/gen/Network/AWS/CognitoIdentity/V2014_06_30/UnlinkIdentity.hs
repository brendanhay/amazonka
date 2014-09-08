{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.UnlinkIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Unlinks a federated identity from an existing account. Unlinked logins will
-- be considered new identities next time they are seen. Removing the last
-- linked login will make this identity inaccessible.
module Network.AWS.CognitoIdentity.V2014_06_30.UnlinkIdentity
    (
    -- * Request
      UnlinkIdentity
    -- ** Request constructor
    , mkUnlinkIdentity
    -- ** Request lenses
    , uiIdentityId
    , uiLogins
    , uiLoginsToRemove

    -- * Response
    , UnlinkIdentityResponse
    -- ** Response constructor
    , mkUnlinkIdentityResponse
    ) where

import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Input to the UnlinkIdentity action.
data UnlinkIdentity = UnlinkIdentity
    { _uiIdentityId :: Text
    , _uiLogins :: Map Text Text
    , _uiLoginsToRemove :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UnlinkIdentity' request.
mkUnlinkIdentity :: Text -- ^ 'uiIdentityId'
                 -> Map Text Text -- ^ 'uiLogins'
                 -> [Text] -- ^ 'uiLoginsToRemove'
                 -> UnlinkIdentity
mkUnlinkIdentity p1 p2 p3 = UnlinkIdentity
    { _uiIdentityId = p1
    , _uiLogins = p2
    , _uiLoginsToRemove = p3
    }

-- | A unique identifier in the format REGION:GUID.
uiIdentityId :: Lens' UnlinkIdentity Text
uiIdentityId = lens _uiIdentityId (\s a -> s { _uiIdentityId = a })

-- | A set of optional name/value pairs that map provider names to provider
-- tokens.
uiLogins :: Lens' UnlinkIdentity (Map Text Text)
uiLogins = lens _uiLogins (\s a -> s { _uiLogins = a })

-- | Provider names to unlink from this identity.
uiLoginsToRemove :: Lens' UnlinkIdentity [Text]
uiLoginsToRemove =
    lens _uiLoginsToRemove (\s a -> s { _uiLoginsToRemove = a })

instance ToPath UnlinkIdentity

instance ToQuery UnlinkIdentity

instance ToHeaders UnlinkIdentity

instance ToJSON UnlinkIdentity

data UnlinkIdentityResponse = UnlinkIdentityResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UnlinkIdentityResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUnlinkIdentityResponse :: UnlinkIdentityResponse
mkUnlinkIdentityResponse = UnlinkIdentityResponse

instance AWSRequest UnlinkIdentity where
    type Sv UnlinkIdentity = CognitoIdentity
    type Rs UnlinkIdentity = UnlinkIdentityResponse

    request = get
    response _ = nullaryResponse UnlinkIdentityResponse
