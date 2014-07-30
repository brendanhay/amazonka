{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a policy from the load balancer. The specified policy must not be
-- enabled for any listeners.
module Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerPolicy where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy
    { _dlbpjLoadBalancerName :: Text
      -- ^ The mnemonic name associated with the load balancer.
    , _dlbpjPolicyName :: Text
      -- ^ The mnemonic name for the policy being deleted.
    } deriving (Generic)

instance ToQuery DeleteLoadBalancerPolicy where
    toQuery = genericToQuery def

instance AWSRequest DeleteLoadBalancerPolicy where
    type Sv DeleteLoadBalancerPolicy = ELB
    type Rs DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicyResponse

    request = post "DeleteLoadBalancerPolicy"
    response _ _ = return (Right DeleteLoadBalancerPolicyResponse)

data DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse
    deriving (Eq, Show, Generic)
