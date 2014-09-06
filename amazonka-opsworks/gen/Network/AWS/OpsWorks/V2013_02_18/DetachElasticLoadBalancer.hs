{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DetachElasticLoadBalancer
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
module Network.AWS.OpsWorks.V2013_02_18.DetachElasticLoadBalancer
    (
    -- * Request
      DetachElasticLoadBalancer
    -- ** Request constructor
    , mkDetachElasticLoadBalancer
    -- ** Request lenses
    , delb1ElasticLoadBalancerName
    , delb1LayerId

    -- * Response
    , DetachElasticLoadBalancerResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DetachElasticLoadBalancer = DetachElasticLoadBalancer
    { _delb1ElasticLoadBalancerName :: Text
    , _delb1LayerId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachElasticLoadBalancer' request.
mkDetachElasticLoadBalancer :: Text -- ^ 'delb1ElasticLoadBalancerName'
                            -> Text -- ^ 'delb1LayerId'
                            -> DetachElasticLoadBalancer
mkDetachElasticLoadBalancer p1 p2 = DetachElasticLoadBalancer
    { _delb1ElasticLoadBalancerName = p1
    , _delb1LayerId = p2
    }
{-# INLINE mkDetachElasticLoadBalancer #-}

-- | The Elastic Load Balancing instance's name.
delb1ElasticLoadBalancerName :: Lens' DetachElasticLoadBalancer Text
delb1ElasticLoadBalancerName =
    lens _delb1ElasticLoadBalancerName
         (\s a -> s { _delb1ElasticLoadBalancerName = a })
{-# INLINE delb1ElasticLoadBalancerName #-}

-- | The ID of the layer that the Elastic Load Balancing instance is attached
-- to.
delb1LayerId :: Lens' DetachElasticLoadBalancer Text
delb1LayerId = lens _delb1LayerId (\s a -> s { _delb1LayerId = a })
{-# INLINE delb1LayerId #-}

instance ToPath DetachElasticLoadBalancer

instance ToQuery DetachElasticLoadBalancer

instance ToHeaders DetachElasticLoadBalancer

instance ToJSON DetachElasticLoadBalancer

data DetachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DetachElasticLoadBalancer where
    type Sv DetachElasticLoadBalancer = OpsWorks
    type Rs DetachElasticLoadBalancer = DetachElasticLoadBalancerResponse

    request = get
    response _ = nullaryResponse DetachElasticLoadBalancerResponse
