{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerPolicy
    (
    -- * Request
      DeleteLoadBalancerPolicy
    -- ** Request constructor
    , mkDeleteLoadBalancerPolicyInput
    -- ** Request lenses
    , dlbpiLoadBalancerName
    , dlbpiPolicyName

    -- * Response
    , DeleteLoadBalancerPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLoadBalancerPolicy' request.
mkDeleteLoadBalancerPolicyInput :: Text -- ^ 'dlbpiLoadBalancerName'
                                -> Text -- ^ 'dlbpiPolicyName'
                                -> DeleteLoadBalancerPolicy
mkDeleteLoadBalancerPolicyInput p1 p2 = DeleteLoadBalancerPolicy
    { _dlbpiLoadBalancerName = p1
    , _dlbpiPolicyName = p2
    }
{-# INLINE mkDeleteLoadBalancerPolicyInput #-}

data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy
    { _dlbpiLoadBalancerName :: Text
      -- ^ The mnemonic name associated with the load balancer.
    , _dlbpiPolicyName :: Text
      -- ^ The mnemonic name for the policy being deleted.
    } deriving (Show, Generic)

-- | The mnemonic name associated with the load balancer.
dlbpiLoadBalancerName :: Lens' DeleteLoadBalancerPolicy (Text)
dlbpiLoadBalancerName = lens _dlbpiLoadBalancerName (\s a -> s { _dlbpiLoadBalancerName = a })
{-# INLINE dlbpiLoadBalancerName #-}

-- | The mnemonic name for the policy being deleted.
dlbpiPolicyName :: Lens' DeleteLoadBalancerPolicy (Text)
dlbpiPolicyName = lens _dlbpiPolicyName (\s a -> s { _dlbpiPolicyName = a })
{-# INLINE dlbpiPolicyName #-}

instance ToQuery DeleteLoadBalancerPolicy where
    toQuery = genericQuery def

    deriving (Eq, Show, Generic)

instance AWSRequest DeleteLoadBalancerPolicy where
    type Sv DeleteLoadBalancerPolicy = ELB
    type Rs DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicyResponse

    request = post "DeleteLoadBalancerPolicy"
    response _ = nullaryResponse DeleteLoadBalancerPolicyResponse
