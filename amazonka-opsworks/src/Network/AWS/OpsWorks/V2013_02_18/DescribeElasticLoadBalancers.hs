{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeElasticLoadBalancers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes a stack's Elastic Load Balancing instances. You must specify at
-- least one of the parameters. Required Permissions: To use this action, an
-- IAM user must have a Show, Deploy, or Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeElasticLoadBalancers where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'DescribeElasticLoadBalancers' request.
describeElasticLoadBalancers :: DescribeElasticLoadBalancers
describeElasticLoadBalancers = DescribeElasticLoadBalancers
    { _delbsStackId = Nothing
    , _delbsLayerIds = mempty
    }

data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers
    { _delbsStackId :: Maybe Text
      -- ^ A stack ID. The action describes the stack's Elastic Load
      -- Balancing instances.
    , _delbsLayerIds :: [Text]
      -- ^ A list of layer IDs. The action describes the Elastic Load
      -- Balancing instances for the specified layers.
    } deriving (Show, Generic)

makeLenses ''DescribeElasticLoadBalancers

instance ToPath DescribeElasticLoadBalancers

instance ToQuery DescribeElasticLoadBalancers

instance ToHeaders DescribeElasticLoadBalancers

instance ToJSON DescribeElasticLoadBalancers

data DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse
    { _delbtElasticLoadBalancers :: [ElasticLoadBalancer]
      -- ^ A list of ElasticLoadBalancer objects that describe the specified
      -- Elastic Load Balancing instances.
    } deriving (Show, Generic)

makeLenses ''DescribeElasticLoadBalancersResponse

instance FromJSON DescribeElasticLoadBalancersResponse

instance AWSRequest DescribeElasticLoadBalancers where
    type Sv DescribeElasticLoadBalancers = OpsWorks
    type Rs DescribeElasticLoadBalancers = DescribeElasticLoadBalancersResponse

    request = get
    response _ = jsonResponse
