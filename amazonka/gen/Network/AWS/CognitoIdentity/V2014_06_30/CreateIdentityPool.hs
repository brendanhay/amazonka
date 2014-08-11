{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.CreateIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CognitoIdentity.V2014_06_30.CreateIdentityPool where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateIdentityPool' request.
createIdentityPool :: Text -- ^ '_cipiIdentityPoolName'
                   -> Bool -- ^ '_cipiAllowUnauthenticatedIdentities'
                   -> CreateIdentityPool
createIdentityPool p1 p2 = CreateIdentityPool
    { _cipiIdentityPoolName = p1
    , _cipiAllowUnauthenticatedIdentities = p2
    , _cipiSupportedLoginProviders = mempty
    }

data CreateIdentityPool = CreateIdentityPool
    { _cipiIdentityPoolName :: Text
    , _cipiAllowUnauthenticatedIdentities :: Bool
    , _cipiSupportedLoginProviders :: HashMap Text Text
    } deriving (Show, Generic)

makeLenses ''CreateIdentityPool

instance ToPath CreateIdentityPool

instance ToQuery CreateIdentityPool

instance ToHeaders CreateIdentityPool

instance ToJSON CreateIdentityPool

data CreateIdentityPoolResponse = CreateIdentityPoolResponse
    { _iwIdentityPoolId :: Text
    , _iwIdentityPoolName :: Text
    , _iwAllowUnauthenticatedIdentities :: Bool
    , _iwSupportedLoginProviders :: HashMap Text Text
    } deriving (Show, Generic)

makeLenses ''CreateIdentityPoolResponse

instance FromJSON CreateIdentityPoolResponse

instance AWSRequest CreateIdentityPool where
    type Sv CreateIdentityPool = CognitoIdentity
    type Rs CreateIdentityPool = CreateIdentityPoolResponse

    request = get
    response _ = jsonResponse
