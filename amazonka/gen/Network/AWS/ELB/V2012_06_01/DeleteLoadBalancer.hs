{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DeleteLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified load balancer. If attempting to recreate the load
-- balancer, you must reconfigure all the settings. The DNS name associated
-- with a deleted load balancer will no longer be usable. Once deleted, the
-- name and associated DNS record of the load balancer no longer exist and
-- traffic sent to any of its IP addresses will no longer be delivered to
-- back-end instances. To successfully call this API, you must provide the
-- same account credentials as were used to create the load balancer. By
-- design, if the load balancer does not exist or has already been deleted, a
-- call to DeleteLoadBalancer action still succeeds.
module Network.AWS.ELB.V2012_06_01.DeleteLoadBalancer where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

data DeleteLoadBalancer = DeleteLoadBalancer
    { _dapjLoadBalancerName :: Text
      -- ^ The name associated with the load balancer.
    } deriving (Show, Generic)

makeLenses ''DeleteLoadBalancer

instance ToQuery DeleteLoadBalancer where
    toQuery = genericToQuery def

data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteLoadBalancerResponse

instance AWSRequest DeleteLoadBalancer where
    type Sv DeleteLoadBalancer = ELB
    type Rs DeleteLoadBalancer = DeleteLoadBalancerResponse

    request = post "DeleteLoadBalancer"
    response _ _ = return (Right DeleteLoadBalancerResponse)
