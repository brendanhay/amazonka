{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.UpdateIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CognitoIdentity.V2014_06_30.UpdateIdentityPool where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateIdentityPool' request.
updateIdentityPool :: Text -- ^ '_irIdentityPoolId'
                   -> Text -- ^ '_irIdentityPoolName'
                   -> Bool -- ^ '_irAllowUnauthenticatedIdentities'
                   -> UpdateIdentityPool
updateIdentityPool p1 p2 p3 = UpdateIdentityPool
    { _irIdentityPoolId = p1
    , _irIdentityPoolName = p2
    , _irAllowUnauthenticatedIdentities = p3
    , _irSupportedLoginProviders = mempty
    }

data UpdateIdentityPool = UpdateIdentityPool
    { _irIdentityPoolId :: Text
    , _irIdentityPoolName :: Text
    , _irAllowUnauthenticatedIdentities :: Bool
    , _irSupportedLoginProviders :: HashMap Text Text
    } deriving (Generic)

makeLenses ''UpdateIdentityPool

instance ToPath UpdateIdentityPool

instance ToQuery UpdateIdentityPool

instance ToHeaders UpdateIdentityPool

instance ToJSON UpdateIdentityPool

data UpdateIdentityPoolResponse = UpdateIdentityPoolResponse
    { _itIdentityPoolId :: Text
    , _itIdentityPoolName :: Text
    , _itAllowUnauthenticatedIdentities :: Bool
    , _itSupportedLoginProviders :: HashMap Text Text
    } deriving (Generic)

makeLenses ''UpdateIdentityPoolResponse

instance FromJSON UpdateIdentityPoolResponse

instance AWSRequest UpdateIdentityPool where
    type Sv UpdateIdentityPool = CognitoIdentity
    type Rs UpdateIdentityPool = UpdateIdentityPoolResponse

    request = get
    response _ = jsonResponse
