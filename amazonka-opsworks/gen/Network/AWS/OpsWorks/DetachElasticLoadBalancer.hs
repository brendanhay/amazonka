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

-- Module      : Network.AWS.OpsWorks.DetachElasticLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Detaches a specified Elastic Load Balancing instance from its layer.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DetachElasticLoadBalancer.html>
module Network.AWS.OpsWorks.DetachElasticLoadBalancer
    (
    -- * Request
      DetachElasticLoadBalancer
    -- ** Request constructor
    , detachElasticLoadBalancer
    -- ** Request lenses
    , delbElasticLoadBalancerName
    , delbLayerId

    -- * Response
    , DetachElasticLoadBalancerResponse
    -- ** Response constructor
    , detachElasticLoadBalancerResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data DetachElasticLoadBalancer = DetachElasticLoadBalancer
    { _delbElasticLoadBalancerName :: Text
    , _delbLayerId                 :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DetachElasticLoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delbElasticLoadBalancerName' @::@ 'Text'
--
-- * 'delbLayerId' @::@ 'Text'
--
detachElasticLoadBalancer :: Text -- ^ 'delbElasticLoadBalancerName'
                          -> Text -- ^ 'delbLayerId'
                          -> DetachElasticLoadBalancer
detachElasticLoadBalancer p1 p2 = DetachElasticLoadBalancer
    { _delbElasticLoadBalancerName = p1
    , _delbLayerId                 = p2
    }

-- | The Elastic Load Balancing instance's name.
delbElasticLoadBalancerName :: Lens' DetachElasticLoadBalancer Text
delbElasticLoadBalancerName =
    lens _delbElasticLoadBalancerName
        (\s a -> s { _delbElasticLoadBalancerName = a })

-- | The ID of the layer that the Elastic Load Balancing instance is attached to.
delbLayerId :: Lens' DetachElasticLoadBalancer Text
delbLayerId = lens _delbLayerId (\s a -> s { _delbLayerId = a })

data DetachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DetachElasticLoadBalancerResponse' constructor.
detachElasticLoadBalancerResponse :: DetachElasticLoadBalancerResponse
detachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse

instance ToPath DetachElasticLoadBalancer where
    toPath = const "/"

instance ToQuery DetachElasticLoadBalancer where
    toQuery = const mempty

instance ToHeaders DetachElasticLoadBalancer

instance ToJSON DetachElasticLoadBalancer where
    toJSON DetachElasticLoadBalancer{..} = object
        [ "ElasticLoadBalancerName" .= _delbElasticLoadBalancerName
        , "LayerId"                 .= _delbLayerId
        ]

instance AWSRequest DetachElasticLoadBalancer where
    type Sv DetachElasticLoadBalancer = OpsWorks
    type Rs DetachElasticLoadBalancer = DetachElasticLoadBalancerResponse

    request  = post "DetachElasticLoadBalancer"
    response = nullResponse DetachElasticLoadBalancerResponse
