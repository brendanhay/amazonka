{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB
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
module Network.AWS.ELB
    (
    -- * Request
      DeleteLoadBalancerPolicy
    -- ** Request constructor
    , mkDeleteLoadBalancerPolicy
    -- ** Request lenses
    , dlbpLoadBalancerName
    , dlbpPolicyName

    -- * Response
    , DeleteLoadBalancerPolicyResponse
    -- ** Response constructor
    , mkDeleteLoadBalancerPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the DeleteLoadBalancerPolicy action.
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy
    { _dlbpLoadBalancerName :: !Text
    , _dlbpPolicyName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLoadBalancerPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
mkDeleteLoadBalancerPolicy :: Text -- ^ 'dlbpLoadBalancerName'
                           -> Text -- ^ 'dlbpPolicyName'
                           -> DeleteLoadBalancerPolicy
mkDeleteLoadBalancerPolicy p1 p2 = DeleteLoadBalancerPolicy
    { _dlbpLoadBalancerName = p1
    , _dlbpPolicyName = p2
    }

-- | The mnemonic name associated with the load balancer.
dlbpLoadBalancerName :: Lens' DeleteLoadBalancerPolicy Text
dlbpLoadBalancerName =
    lens _dlbpLoadBalancerName (\s a -> s { _dlbpLoadBalancerName = a })

-- | The mnemonic name for the policy being deleted.
dlbpPolicyName :: Lens' DeleteLoadBalancerPolicy Text
dlbpPolicyName = lens _dlbpPolicyName (\s a -> s { _dlbpPolicyName = a })

instance ToQuery DeleteLoadBalancerPolicy where
    toQuery = genericQuery def

-- | The output for the DeleteLoadBalancerPolicy action.
data DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLoadBalancerPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteLoadBalancerPolicyResponse :: DeleteLoadBalancerPolicyResponse
mkDeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse

instance AWSRequest DeleteLoadBalancerPolicy where
    type Sv DeleteLoadBalancerPolicy = ELB
    type Rs DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicyResponse

    request = post "DeleteLoadBalancerPolicy"
    response _ = nullaryResponse DeleteLoadBalancerPolicyResponse
