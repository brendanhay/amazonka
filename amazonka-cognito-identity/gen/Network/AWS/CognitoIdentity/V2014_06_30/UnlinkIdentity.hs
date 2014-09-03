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
    , unlinkIdentity
    -- ** Request lenses
    , uiiIdentityId
    , uiiLoginsToRemove
    , uiiLogins

    -- * Response
    , UnlinkIdentityResponse
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UnlinkIdentity' request.
unlinkIdentity :: Text -- ^ 'uiiIdentityId'
               -> [Text] -- ^ 'uiiLoginsToRemove'
               -> Map Text Text -- ^ 'uiiLogins'
               -> UnlinkIdentity
unlinkIdentity p1 p2 p3 = UnlinkIdentity
    { _uiiIdentityId = p1
    , _uiiLoginsToRemove = p2
    , _uiiLogins = p3
    }

data UnlinkIdentity = UnlinkIdentity
    { _uiiIdentityId :: Text
      -- ^ A unique identifier in the format REGION:GUID.
    , _uiiLoginsToRemove :: [Text]
      -- ^ Provider names to unlink from this identity.
    , _uiiLogins :: Map Text Text
      -- ^ A set of optional name/value pairs that map provider names to
      -- provider tokens.
    } deriving (Show, Generic)

-- | A unique identifier in the format REGION:GUID.
uiiIdentityId
    :: Functor f
    => (Text
    -> f (Text))
    -> UnlinkIdentity
    -> f UnlinkIdentity
uiiIdentityId f x =
    (\y -> x { _uiiIdentityId = y })
       <$> f (_uiiIdentityId x)
{-# INLINE uiiIdentityId #-}

-- | Provider names to unlink from this identity.
uiiLoginsToRemove
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> UnlinkIdentity
    -> f UnlinkIdentity
uiiLoginsToRemove f x =
    (\y -> x { _uiiLoginsToRemove = y })
       <$> f (_uiiLoginsToRemove x)
{-# INLINE uiiLoginsToRemove #-}

-- | A set of optional name/value pairs that map provider names to provider
-- tokens.
uiiLogins
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> UnlinkIdentity
    -> f UnlinkIdentity
uiiLogins f x =
    (\y -> x { _uiiLogins = y })
       <$> f (_uiiLogins x)
{-# INLINE uiiLogins #-}

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
