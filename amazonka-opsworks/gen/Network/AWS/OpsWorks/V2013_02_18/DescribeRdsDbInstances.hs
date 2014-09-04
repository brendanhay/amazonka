{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeRdsDbInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes Amazon RDS instances.
module Network.AWS.OpsWorks.V2013_02_18.DescribeRdsDbInstances
    (
    -- * Request
      DescribeRdsDbInstances
    -- ** Request constructor
    , mkDescribeRdsDbInstancesRequest
    -- ** Request lenses
    , drdisStackId
    , drdisRdsDbInstanceArns

    -- * Response
    , DescribeRdsDbInstancesResponse
    -- ** Response lenses
    , drditRdsDbInstances
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeRdsDbInstances' request.
mkDescribeRdsDbInstancesRequest :: Text -- ^ 'drdisStackId'
                                -> DescribeRdsDbInstances
mkDescribeRdsDbInstancesRequest p1 = DescribeRdsDbInstances
    { _drdisStackId = p1
    , _drdisRdsDbInstanceArns = mempty
    }
{-# INLINE mkDescribeRdsDbInstancesRequest #-}

data DescribeRdsDbInstances = DescribeRdsDbInstances
    { _drdisStackId :: Text
      -- ^ The stack ID that the instances are registered with. The
      -- operation returns descriptions of all registered Amazon RDS
      -- instances.
    , _drdisRdsDbInstanceArns :: [Text]
      -- ^ An array containing the ARNs of the instances to be described.
    } deriving (Show, Generic)

-- | The stack ID that the instances are registered with. The operation returns
-- descriptions of all registered Amazon RDS instances.
drdisStackId :: Lens' DescribeRdsDbInstances (Text)
drdisStackId = lens _drdisStackId (\s a -> s { _drdisStackId = a })
{-# INLINE drdisStackId #-}

-- | An array containing the ARNs of the instances to be described.
drdisRdsDbInstanceArns :: Lens' DescribeRdsDbInstances ([Text])
drdisRdsDbInstanceArns = lens _drdisRdsDbInstanceArns (\s a -> s { _drdisRdsDbInstanceArns = a })
{-# INLINE drdisRdsDbInstanceArns #-}

instance ToPath DescribeRdsDbInstances

instance ToQuery DescribeRdsDbInstances

instance ToHeaders DescribeRdsDbInstances

instance ToJSON DescribeRdsDbInstances

newtype DescribeRdsDbInstancesResponse = DescribeRdsDbInstancesResponse
    { _drditRdsDbInstances :: [RdsDbInstance]
      -- ^ An a array of RdsDbInstance objects that describe the instances.
    } deriving (Show, Generic)

-- | An a array of RdsDbInstance objects that describe the instances.
drditRdsDbInstances :: Lens' DescribeRdsDbInstancesResponse ([RdsDbInstance])
drditRdsDbInstances = lens _drditRdsDbInstances (\s a -> s { _drditRdsDbInstances = a })
{-# INLINE drditRdsDbInstances #-}

instance FromJSON DescribeRdsDbInstancesResponse

instance AWSRequest DescribeRdsDbInstances where
    type Sv DescribeRdsDbInstances = OpsWorks
    type Rs DescribeRdsDbInstances = DescribeRdsDbInstancesResponse

    request = get
    response _ = jsonResponse
