{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates, updates, or disables a policy with a listener on the load
-- balancer. You can associate multiple policies with a listener.
module Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
    (
    -- * Request
      SetLoadBalancerPoliciesOfListener
    -- ** Request constructor
    , setLoadBalancerPoliciesOfListener
    -- ** Request lenses
    , slbpolLoadBalancerName
    , slbpolLoadBalancerPort
    , slbpolPolicyNames

    -- * Response
    , SetLoadBalancerPoliciesOfListenerResponse
    -- ** Response constructor
    , setLoadBalancerPoliciesOfListenerResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener
    { _slbpolLoadBalancerName :: Text
    , _slbpolLoadBalancerPort :: Int
    , _slbpolPolicyNames      :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetLoadBalancerPoliciesOfListener' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbpolLoadBalancerName' @::@ 'Text'
--
-- * 'slbpolLoadBalancerPort' @::@ 'Int'
--
-- * 'slbpolPolicyNames' @::@ ['Text']
--
setLoadBalancerPoliciesOfListener :: Text -- ^ 'slbpolLoadBalancerName'
                                  -> Int -- ^ 'slbpolLoadBalancerPort'
                                  -> SetLoadBalancerPoliciesOfListener
setLoadBalancerPoliciesOfListener p1 p2 = SetLoadBalancerPoliciesOfListener
    { _slbpolLoadBalancerName = p1
    , _slbpolLoadBalancerPort = p2
    , _slbpolPolicyNames      = mempty
    }

-- | The name of the load balancer.
slbpolLoadBalancerName :: Lens' SetLoadBalancerPoliciesOfListener Text
slbpolLoadBalancerName =
    lens _slbpolLoadBalancerName (\s a -> s { _slbpolLoadBalancerName = a })

-- | The external port of the load balancer to associate the policy.
slbpolLoadBalancerPort :: Lens' SetLoadBalancerPoliciesOfListener Int
slbpolLoadBalancerPort =
    lens _slbpolLoadBalancerPort (\s a -> s { _slbpolLoadBalancerPort = a })

-- | List of policies to be associated with the listener. If the list is
-- empty, the current policy is removed from the listener.
slbpolPolicyNames :: Lens' SetLoadBalancerPoliciesOfListener [Text]
slbpolPolicyNames =
    lens _slbpolPolicyNames (\s a -> s { _slbpolPolicyNames = a })

data SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetLoadBalancerPoliciesOfListenerResponse' constructor.
setLoadBalancerPoliciesOfListenerResponse :: SetLoadBalancerPoliciesOfListenerResponse
setLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse

instance AWSRequest SetLoadBalancerPoliciesOfListener where
    type Sv SetLoadBalancerPoliciesOfListener = ELB
    type Rs SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListenerResponse

    request  = post "SetLoadBalancerPoliciesOfListener"
    response = nullResponse SetLoadBalancerPoliciesOfListenerResponse

instance ToPath SetLoadBalancerPoliciesOfListener where
    toPath = const "/"

instance ToHeaders SetLoadBalancerPoliciesOfListener

instance ToQuery SetLoadBalancerPoliciesOfListener
