{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.CognitoIdentity.V2014_06_30.UnlinkIdentity where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data UnlinkIdentity = UnlinkIdentity
    { _uiiIdentityId :: Text
      -- ^ A unique identifier in the format REGION:GUID.
    , _uiiLoginsToRemove :: [Text]
      -- ^ Provider names to unlink from this identity.
    , _uiiLogins :: Map Text Text
      -- ^ A set of optional name/value pairs that map provider names to
      -- provider tokens.
    } deriving (Show, Generic)

makeLenses ''UnlinkIdentity

instance ToPath UnlinkIdentity

instance ToQuery UnlinkIdentity

instance ToHeaders UnlinkIdentity

instance ToJSON UnlinkIdentity

data UnlinkIdentityResponse = UnlinkIdentityResponse
    deriving (Eq, Show, Generic)

makeLenses ''UnlinkIdentityResponse

instance AWSRequest UnlinkIdentity where
    type Sv UnlinkIdentity = CognitoIdentity
    type Rs UnlinkIdentity = UnlinkIdentityResponse

    request = get
    response _ = nullaryResponse UnlinkIdentityResponse
