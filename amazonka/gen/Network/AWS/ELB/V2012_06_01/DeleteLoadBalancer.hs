{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ELB.V2012_06_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteLoadBalancer = DeleteLoadBalancer
    { _dapjLoadBalancerName :: Text
      -- ^ The name associated with the load balancer.
    } deriving (Generic)

instance ToQuery DeleteLoadBalancer where
    toQuery = genericToQuery def

instance AWSRequest DeleteLoadBalancer where
    type Sv DeleteLoadBalancer = ELB
    type Rs DeleteLoadBalancer = DeleteLoadBalancerResponse

    request = post "DeleteLoadBalancer"
    response _ _ = return (Right DeleteLoadBalancerResponse)

data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse
    deriving (Eq, Show, Generic)
