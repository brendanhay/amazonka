{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.DescribeIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CognitoIdentity.V2014_06_30.DescribeIdentityPool where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude

data DescribeIdentityPool = DescribeIdentityPool
    { _dipiIdentityPoolId :: Text
    } deriving (Show, Generic)

makeLenses ''DescribeIdentityPool

instance ToPath DescribeIdentityPool

instance ToQuery DescribeIdentityPool

instance ToHeaders DescribeIdentityPool

instance ToJSON DescribeIdentityPool

data DescribeIdentityPoolResponse = DescribeIdentityPoolResponse
    { _ipIdentityPoolId :: Text
    , _ipIdentityPoolName :: Text
    , _ipAllowUnauthenticatedIdentities :: Bool
    , _ipSupportedLoginProviders :: HashMap Text Text
    } deriving (Show, Generic)

makeLenses ''DescribeIdentityPoolResponse

instance FromJSON DescribeIdentityPoolResponse

instance AWSRequest DescribeIdentityPool where
    type Sv DescribeIdentityPool = CognitoIdentity
    type Rs DescribeIdentityPool = DescribeIdentityPoolResponse

    request = get
    response _ = undefined
