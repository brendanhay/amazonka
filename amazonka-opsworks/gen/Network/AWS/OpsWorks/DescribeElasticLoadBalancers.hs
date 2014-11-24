{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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

-- | Describes a stack's Elastic Load Balancing instances. Required Permissions:
-- To use this action, an IAM user must have a Show, Deploy, or Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html
-- Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeElasticLoadBalancers.html>
module Network.AWS.OpsWorks.DescribeElasticLoadBalancers
    (
    -- * Request
      DescribeElasticLoadBalancers
    -- ** Request constructor
    , describeElasticLoadBalancers
    -- ** Request lenses
    , delbLayerIds
    , delbStackId

    -- * Response
    , DescribeElasticLoadBalancersResponse
    -- ** Response constructor
    , describeElasticLoadBalancersResponse
    -- ** Response lenses
    , delbrElasticLoadBalancers
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers
    { _delbLayerIds :: List "InstanceIds" Text
    , _delbStackId  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeElasticLoadBalancers' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delbLayerIds' @::@ ['Text']
--
-- * 'delbStackId' @::@ 'Maybe' 'Text'
--
describeElasticLoadBalancers :: DescribeElasticLoadBalancers
describeElasticLoadBalancers = DescribeElasticLoadBalancers
    { _delbStackId  = Nothing
    , _delbLayerIds = mempty
    }

-- | A list of layer IDs. The action describes the Elastic Load Balancing
-- instances for the specified layers.
delbLayerIds :: Lens' DescribeElasticLoadBalancers [Text]
delbLayerIds = lens _delbLayerIds (\s a -> s { _delbLayerIds = a }) . _List

-- | A stack ID. The action describes the stack's Elastic Load Balancing
-- instances.
delbStackId :: Lens' DescribeElasticLoadBalancers (Maybe Text)
delbStackId = lens _delbStackId (\s a -> s { _delbStackId = a })

newtype DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse
    { _delbrElasticLoadBalancers :: List "ElasticLoadBalancers" ElasticLoadBalancer
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeElasticLoadBalancersResponse where
    type Item DescribeElasticLoadBalancersResponse = ElasticLoadBalancer

    fromList = DescribeElasticLoadBalancersResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _delbrElasticLoadBalancers

-- | 'DescribeElasticLoadBalancersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delbrElasticLoadBalancers' @::@ ['ElasticLoadBalancer']
--
describeElasticLoadBalancersResponse :: DescribeElasticLoadBalancersResponse
describeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse
    { _delbrElasticLoadBalancers = mempty
    }

-- | A list of @ElasticLoadBalancer@ objects that describe the specified
-- Elastic Load Balancing instances.
delbrElasticLoadBalancers :: Lens' DescribeElasticLoadBalancersResponse [ElasticLoadBalancer]
delbrElasticLoadBalancers =
    lens _delbrElasticLoadBalancers
        (\s a -> s { _delbrElasticLoadBalancers = a })
            . _List

instance ToPath DescribeElasticLoadBalancers where
    toPath = const "/"

instance ToQuery DescribeElasticLoadBalancers where
    toQuery = const mempty

instance ToHeaders DescribeElasticLoadBalancers

instance ToJSON DescribeElasticLoadBalancers where
    toJSON DescribeElasticLoadBalancers{..} = object
        [ "StackId"  .= _delbStackId
        , "LayerIds" .= _delbLayerIds
        ]

instance AWSRequest DescribeElasticLoadBalancers where
    type Sv DescribeElasticLoadBalancers = OpsWorks
    type Rs DescribeElasticLoadBalancers = DescribeElasticLoadBalancersResponse

    request  = post "DescribeElasticLoadBalancers"
    response = jsonResponse

instance FromJSON DescribeElasticLoadBalancersResponse where
    parseJSON = withObject "DescribeElasticLoadBalancersResponse" $ \o -> DescribeElasticLoadBalancersResponse
        <$> o .:  "ElasticLoadBalancers"
