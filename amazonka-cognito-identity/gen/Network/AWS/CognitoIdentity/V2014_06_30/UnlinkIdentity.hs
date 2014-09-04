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
    , mkUnlinkIdentityInput
    -- ** Request lenses
    , uiiIdentityId
    , uiiLogins
    , uiiLoginsToRemove

    -- * Response
    , UnlinkIdentityResponse
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UnlinkIdentity' request.
mkUnlinkIdentityInput :: Text -- ^ 'uiiIdentityId'
                      -> Map Text Text -- ^ 'uiiLogins'
                      -> [Text] -- ^ 'uiiLoginsToRemove'
                      -> UnlinkIdentity
mkUnlinkIdentityInput p1 p2 p3 = UnlinkIdentity
    { _uiiIdentityId = p1
    , _uiiLogins = p2
    , _uiiLoginsToRemove = p3
    }
{-# INLINE mkUnlinkIdentityInput #-}

data UnlinkIdentity = UnlinkIdentity
    { _uiiIdentityId :: Text
      -- ^ A unique identifier in the format REGION:GUID.
    , _uiiLogins :: Map Text Text
      -- ^ A set of optional name/value pairs that map provider names to
      -- provider tokens.
    , _uiiLoginsToRemove :: [Text]
      -- ^ Provider names to unlink from this identity.
    } deriving (Show, Generic)

-- | A unique identifier in the format REGION:GUID.
uiiIdentityId :: Lens' UnlinkIdentity (Text)
uiiIdentityId = lens _uiiIdentityId (\s a -> s { _uiiIdentityId = a })
{-# INLINE uiiIdentityId #-}

-- | A set of optional name/value pairs that map provider names to provider
-- tokens.
uiiLogins :: Lens' UnlinkIdentity (Map Text Text)
uiiLogins = lens _uiiLogins (\s a -> s { _uiiLogins = a })
{-# INLINE uiiLogins #-}

-- | Provider names to unlink from this identity.
uiiLoginsToRemove :: Lens' UnlinkIdentity ([Text])
uiiLoginsToRemove = lens _uiiLoginsToRemove (\s a -> s { _uiiLoginsToRemove = a })
{-# INLINE uiiLoginsToRemove #-}

instance ToPath UnlinkIdentity

instance ToQuery UnlinkIdentity

instance ToHeaders UnlinkIdentity

instance ToJSON UnlinkIdentity

data UnlinkIdentityResponse = UnlinkIdentityResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UnlinkIdentity where
    type Sv UnlinkIdentity = CognitoIdentity
    type Rs UnlinkIdentity = UnlinkIdentityResponse

    request = get
    response _ = nullaryResponse UnlinkIdentityResponse
