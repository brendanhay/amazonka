{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ELB.DeleteLoadBalancerPolicy
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
module Network.AWS.ELB.DeleteLoadBalancerPolicy
    (
    -- * Request
      DeleteLoadBalancerPolicyInput
    -- ** Request constructor
    , deleteLoadBalancerPolicy
    -- ** Request lenses
    , dlbpi1LoadBalancerName
    , dlbpi1PolicyName

    -- * Response
    , DeleteLoadBalancerPolicyResponse
    -- ** Response constructor
    , deleteLoadBalancerPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data DeleteLoadBalancerPolicyInput = DeleteLoadBalancerPolicyInput
    { _dlbpi1LoadBalancerName :: Text
    , _dlbpi1PolicyName       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLoadBalancerPolicyInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlbpi1LoadBalancerName' @::@ 'Text'
--
-- * 'dlbpi1PolicyName' @::@ 'Text'
--
deleteLoadBalancerPolicy :: Text -- ^ 'dlbpi1LoadBalancerName'
                         -> Text -- ^ 'dlbpi1PolicyName'
                         -> DeleteLoadBalancerPolicyInput
deleteLoadBalancerPolicy p1 p2 = DeleteLoadBalancerPolicyInput
    { _dlbpi1LoadBalancerName = p1
    , _dlbpi1PolicyName       = p2
    }

-- | The mnemonic name associated with the load balancer.
dlbpi1LoadBalancerName :: Lens' DeleteLoadBalancerPolicyInput Text
dlbpi1LoadBalancerName =
    lens _dlbpi1LoadBalancerName (\s a -> s { _dlbpi1LoadBalancerName = a })

-- | The mnemonic name for the policy being deleted.
dlbpi1PolicyName :: Lens' DeleteLoadBalancerPolicyInput Text
dlbpi1PolicyName = lens _dlbpi1PolicyName (\s a -> s { _dlbpi1PolicyName = a })

instance ToPath DeleteLoadBalancerPolicyInput where
    toPath = const "/"

instance ToQuery DeleteLoadBalancerPolicyInput

data DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse

-- | 'DeleteLoadBalancerPolicyResponse' constructor.
deleteLoadBalancerPolicyResponse :: DeleteLoadBalancerPolicyResponse
deleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse

instance AWSRequest DeleteLoadBalancerPolicyInput where
    type Sv DeleteLoadBalancerPolicyInput = ELB
    type Rs DeleteLoadBalancerPolicyInput = DeleteLoadBalancerPolicyResponse

    request  = post "DeleteLoadBalancerPolicy"
    response = const (nullaryResponse DeleteLoadBalancerPolicyResponse)
