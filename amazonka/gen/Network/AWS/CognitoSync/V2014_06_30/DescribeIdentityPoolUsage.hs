{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoSync.V2014_06_30.DescribeIdentityPoolUsage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets usage details (for example, data storage) about a particular identity
-- pool.
module Network.AWS.CognitoSync.V2014_06_30.DescribeIdentityPoolUsage where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.V2014_06_30.Types
import Network.AWS.Prelude

data DescribeIdentityPoolUsage = DescribeIdentityPoolUsage
    { _dipurIdentityPoolId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    } deriving (Generic)

makeLenses ''DescribeIdentityPoolUsage

instance ToPath DescribeIdentityPoolUsage where
    toPath DescribeIdentityPoolUsage{..} = mconcat
        [ "/identitypools/"
        , toBS _dipurIdentityPoolId
        ]

instance ToQuery DescribeIdentityPoolUsage

instance ToHeaders DescribeIdentityPoolUsage

instance ToJSON DescribeIdentityPoolUsage

data DescribeIdentityPoolUsageResponse = DescribeIdentityPoolUsageResponse
    { _dipusIdentityPoolUsage :: Maybe IdentityPoolUsage
      -- ^ Information about the usage of the identity pool.
    } deriving (Generic)

makeLenses ''DescribeIdentityPoolUsageResponse

instance FromJSON DescribeIdentityPoolUsageResponse

instance AWSRequest DescribeIdentityPoolUsage where
    type Sv DescribeIdentityPoolUsage = CognitoSync
    type Rs DescribeIdentityPoolUsage = DescribeIdentityPoolUsageResponse

    request = get
    response _ = jsonResponse
