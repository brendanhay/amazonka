{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.ListIdentityPools
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CognitoIdentity.V2014_06_30.ListIdentityPools where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListIdentityPools' request.
listIdentityPools :: ListIdentityPools
listIdentityPools = ListIdentityPools
    { _lipiNextToken = Nothing
    , _lipiMaxResults = Nothing
    }

data ListIdentityPools = ListIdentityPools
    { _lipiNextToken :: Maybe Text
    , _lipiMaxResults :: Maybe Integer
    } deriving (Generic)

makeLenses ''ListIdentityPools

instance ToPath ListIdentityPools

instance ToQuery ListIdentityPools

instance ToHeaders ListIdentityPools

instance ToJSON ListIdentityPools

data ListIdentityPoolsResponse = ListIdentityPoolsResponse
    { _liprIdentityPools :: [IdentityPoolShortDescription]
    , _liprNextToken :: Maybe Text
    } deriving (Generic)

makeLenses ''ListIdentityPoolsResponse

instance FromJSON ListIdentityPoolsResponse

instance AWSRequest ListIdentityPools where
    type Sv ListIdentityPools = CognitoIdentity
    type Rs ListIdentityPools = ListIdentityPoolsResponse

    request = get
    response _ = jsonResponse
