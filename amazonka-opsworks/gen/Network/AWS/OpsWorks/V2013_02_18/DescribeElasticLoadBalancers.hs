{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DescribeElasticLoadBalancers
    (
    -- * Request
      DescribeElasticLoadBalancers
    -- ** Request constructor
    , mkDescribeElasticLoadBalancersRequest
    -- ** Request lenses
    , delbrStackId
    , delbrLayerIds

    -- * Response
    , DescribeElasticLoadBalancersResponse
    -- ** Response lenses
    , delbsElasticLoadBalancers
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeElasticLoadBalancers' request.
mkDescribeElasticLoadBalancersRequest :: DescribeElasticLoadBalancers
mkDescribeElasticLoadBalancersRequest = DescribeElasticLoadBalancers
    { _delbrStackId = Nothing
    , _delbrLayerIds = mempty
    }
{-# INLINE mkDescribeElasticLoadBalancersRequest #-}

data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers
    { _delbrStackId :: Maybe Text
      -- ^ A stack ID. The action describes the stack's Elastic Load
      -- Balancing instances.
    , _delbrLayerIds :: [Text]
      -- ^ A list of layer IDs. The action describes the Elastic Load
      -- Balancing instances for the specified layers.
    } deriving (Show, Generic)

-- | A stack ID. The action describes the stack's Elastic Load Balancing
-- instances.
delbrStackId :: Lens' DescribeElasticLoadBalancers (Maybe Text)
delbrStackId = lens _delbrStackId (\s a -> s { _delbrStackId = a })
{-# INLINE delbrStackId #-}

-- | A list of layer IDs. The action describes the Elastic Load Balancing
-- instances for the specified layers.
delbrLayerIds :: Lens' DescribeElasticLoadBalancers ([Text])
delbrLayerIds = lens _delbrLayerIds (\s a -> s { _delbrLayerIds = a })
{-# INLINE delbrLayerIds #-}

instance ToPath DescribeElasticLoadBalancers

instance ToQuery DescribeElasticLoadBalancers

instance ToHeaders DescribeElasticLoadBalancers

instance ToJSON DescribeElasticLoadBalancers

newtype DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse
    { _delbsElasticLoadBalancers :: [ElasticLoadBalancer]
      -- ^ A list of ElasticLoadBalancer objects that describe the specified
      -- Elastic Load Balancing instances.
    } deriving (Show, Generic)

-- | A list of ElasticLoadBalancer objects that describe the specified Elastic
-- Load Balancing instances.
delbsElasticLoadBalancers :: Lens' DescribeElasticLoadBalancersResponse ([ElasticLoadBalancer])
delbsElasticLoadBalancers = lens _delbsElasticLoadBalancers (\s a -> s { _delbsElasticLoadBalancers = a })
{-# INLINE delbsElasticLoadBalancers #-}

instance FromJSON DescribeElasticLoadBalancersResponse

instance AWSRequest DescribeElasticLoadBalancers where
    type Sv DescribeElasticLoadBalancers = OpsWorks
    type Rs DescribeElasticLoadBalancers = DescribeElasticLoadBalancersResponse

    request = get
    response _ = jsonResponse
