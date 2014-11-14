{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.OpsWorks.DetachElasticLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches a specified Elastic Load Balancing instance from its layer.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
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

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data DetachElasticLoadBalancer = DetachElasticLoadBalancer
    { _delbElasticLoadBalancerName :: Text
    , _delbLayerId                 :: Text
    } deriving (Eq, Ord, Show, Generic)

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

-- | The ID of the layer that the Elastic Load Balancing instance is attached
-- to.
delbLayerId :: Lens' DetachElasticLoadBalancer Text
delbLayerId = lens _delbLayerId (\s a -> s { _delbLayerId = a })

instance ToPath DetachElasticLoadBalancer where
    toPath = const "/"

instance ToQuery DetachElasticLoadBalancer where
    toQuery = const mempty

instance ToHeaders DetachElasticLoadBalancer

instance ToBody DetachElasticLoadBalancer where
    toBody = toBody . encode . _delbElasticLoadBalancerName

data DetachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DetachElasticLoadBalancerResponse' constructor.
detachElasticLoadBalancerResponse :: DetachElasticLoadBalancerResponse
detachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse

instance AWSRequest DetachElasticLoadBalancer where
    type Sv DetachElasticLoadBalancer = OpsWorks
    type Rs DetachElasticLoadBalancer = DetachElasticLoadBalancerResponse

    request  = post
    response = nullaryResponse DetachElasticLoadBalancerResponse
