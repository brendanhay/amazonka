{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeElasticLoadBalancers
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
module Network.AWS.OpsWorks.DescribeElasticLoadBalancers
    (
    -- * Request
      DescribeElasticLoadBalancers
    -- ** Request constructor
    , describeElasticLoadBalancers
    -- ** Request lenses
    , delbStackId
    , delbLayerIds

    -- * Response
    , DescribeElasticLoadBalancersResponse
    -- ** Response constructor
    , describeElasticLoadBalancersResponse
    -- ** Response lenses
    , delbrElasticLoadBalancers
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers
    { _delbStackId :: Maybe Text
    , _delbLayerIds :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeElasticLoadBalancers' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @LayerIds ::@ @[Text]@
--
describeElasticLoadBalancers :: DescribeElasticLoadBalancers
describeElasticLoadBalancers = DescribeElasticLoadBalancers
    { _delbStackId = Nothing
    , _delbLayerIds = mempty
    }

-- | A stack ID. The action describes the stack's Elastic Load Balancing
-- instances.
delbStackId :: Lens' DescribeElasticLoadBalancers (Maybe Text)
delbStackId = lens _delbStackId (\s a -> s { _delbStackId = a })

-- | A list of layer IDs. The action describes the Elastic Load Balancing
-- instances for the specified layers.
delbLayerIds :: Lens' DescribeElasticLoadBalancers [Text]
delbLayerIds = lens _delbLayerIds (\s a -> s { _delbLayerIds = a })

instance ToPath DescribeElasticLoadBalancers

instance ToQuery DescribeElasticLoadBalancers

instance ToHeaders DescribeElasticLoadBalancers

instance ToJSON DescribeElasticLoadBalancers

-- | Contains the response to a DescribeElasticLoadBalancers request.
newtype DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse
    { _delbrElasticLoadBalancers :: [ElasticLoadBalancer]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeElasticLoadBalancersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ElasticLoadBalancers ::@ @[ElasticLoadBalancer]@
--
describeElasticLoadBalancersResponse :: DescribeElasticLoadBalancersResponse
describeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse
    { _delbrElasticLoadBalancers = mempty
    }

-- | A list of ElasticLoadBalancer objects that describe the specified Elastic
-- Load Balancing instances.
delbrElasticLoadBalancers :: Lens' DescribeElasticLoadBalancersResponse [ElasticLoadBalancer]
delbrElasticLoadBalancers =
    lens _delbrElasticLoadBalancers
         (\s a -> s { _delbrElasticLoadBalancers = a })

instance FromJSON DescribeElasticLoadBalancersResponse

instance AWSRequest DescribeElasticLoadBalancers where
    type Sv DescribeElasticLoadBalancers = OpsWorks
    type Rs DescribeElasticLoadBalancers = DescribeElasticLoadBalancersResponse

    request = get
    response _ = jsonResponse
