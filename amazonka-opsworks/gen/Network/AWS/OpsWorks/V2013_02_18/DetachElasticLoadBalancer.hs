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
    , detachElasticLoadBalancer
    -- ** Request lenses
    , delbtElasticLoadBalancerName
    , delbtLayerId

    -- * Response
    , DetachElasticLoadBalancerResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DetachElasticLoadBalancer' request.
detachElasticLoadBalancer :: Text -- ^ 'delbtElasticLoadBalancerName'
                          -> Text -- ^ 'delbtLayerId'
                          -> DetachElasticLoadBalancer
detachElasticLoadBalancer p1 p2 = DetachElasticLoadBalancer
    { _delbtElasticLoadBalancerName = p1
    , _delbtLayerId = p2
    }

data DetachElasticLoadBalancer = DetachElasticLoadBalancer
    { _delbtElasticLoadBalancerName :: Text
      -- ^ The Elastic Load Balancing instance's name.
    , _delbtLayerId :: Text
      -- ^ The ID of the layer that the Elastic Load Balancing instance is
      -- attached to.
    } deriving (Show, Generic)

-- | The Elastic Load Balancing instance's name.
delbtElasticLoadBalancerName
    :: Functor f
    => (Text
    -> f (Text))
    -> DetachElasticLoadBalancer
    -> f DetachElasticLoadBalancer
delbtElasticLoadBalancerName f x =
    (\y -> x { _delbtElasticLoadBalancerName = y })
       <$> f (_delbtElasticLoadBalancerName x)
{-# INLINE delbtElasticLoadBalancerName #-}

-- | The ID of the layer that the Elastic Load Balancing instance is attached
-- to.
delbtLayerId
    :: Functor f
    => (Text
    -> f (Text))
    -> DetachElasticLoadBalancer
    -> f DetachElasticLoadBalancer
delbtLayerId f x =
    (\y -> x { _delbtLayerId = y })
       <$> f (_delbtLayerId x)
{-# INLINE delbtLayerId #-}

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
