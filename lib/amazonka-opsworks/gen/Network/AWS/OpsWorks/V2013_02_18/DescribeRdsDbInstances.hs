{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DescribeRdsDbInstances where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'DescribeRdsDbInstances' request.
describeRdsDbInstances :: Text -- ^ '_drdirStackId'
                       -> DescribeRdsDbInstances
describeRdsDbInstances p1 = DescribeRdsDbInstances
    { _drdirStackId = p1
    , _drdirRdsDbInstanceArns = mempty
    }

data DescribeRdsDbInstances = DescribeRdsDbInstances
    { _drdirStackId :: Text
      -- ^ The stack ID that the instances are registered with. The
      -- operation returns descriptions of all registered Amazon RDS
      -- instances.
    , _drdirRdsDbInstanceArns :: [Text]
      -- ^ An array containing the ARNs of the instances to be described.
    } deriving (Show, Generic)

makeLenses ''DescribeRdsDbInstances

instance ToPath DescribeRdsDbInstances

instance ToQuery DescribeRdsDbInstances

instance ToHeaders DescribeRdsDbInstances

instance ToJSON DescribeRdsDbInstances

data DescribeRdsDbInstancesResponse = DescribeRdsDbInstancesResponse
    { _drdisRdsDbInstances :: [RdsDbInstance]
      -- ^ An a array of RdsDbInstance objects that describe the instances.
    } deriving (Show, Generic)

makeLenses ''DescribeRdsDbInstancesResponse

instance FromJSON DescribeRdsDbInstancesResponse

instance AWSRequest DescribeRdsDbInstances where
    type Sv DescribeRdsDbInstances = OpsWorks
    type Rs DescribeRdsDbInstances = DescribeRdsDbInstancesResponse

    request = get
    response _ = jsonResponse
