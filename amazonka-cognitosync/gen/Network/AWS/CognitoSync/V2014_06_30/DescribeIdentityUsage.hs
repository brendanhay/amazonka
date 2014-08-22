{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.V2014_06_30.DescribeIdentityUsage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets usage information for an identity, including number of datasets and
-- data usage.
module Network.AWS.CognitoSync.V2014_06_30.DescribeIdentityUsage where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.V2014_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data DescribeIdentityUsage = DescribeIdentityUsage
    { _diurIdentityId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _diurIdentityPoolId :: Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    } deriving (Show, Generic)

makeLenses ''DescribeIdentityUsage

instance ToPath DescribeIdentityUsage where
    toPath DescribeIdentityUsage{..} = mconcat
        [ "/identitypools/"
        , toBS _diurIdentityPoolId
        , "/identities/"
        , toBS _diurIdentityId
        ]

instance ToQuery DescribeIdentityUsage

instance ToHeaders DescribeIdentityUsage

instance ToJSON DescribeIdentityUsage

data DescribeIdentityUsageResponse = DescribeIdentityUsageResponse
    { _diusIdentityUsage :: Maybe IdentityUsage
      -- ^ Usage information for the identity.
    } deriving (Show, Generic)

makeLenses ''DescribeIdentityUsageResponse

instance FromJSON DescribeIdentityUsageResponse

instance AWSRequest DescribeIdentityUsage where
    type Sv DescribeIdentityUsage = CognitoSync
    type Rs DescribeIdentityUsage = DescribeIdentityUsageResponse

    request = get
    response _ = jsonResponse
