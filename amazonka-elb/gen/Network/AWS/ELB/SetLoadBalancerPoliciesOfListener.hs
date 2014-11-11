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
      SetLoadBalancerPoliciesOfListenerInput
    -- ** Request constructor
    , setLoadBalancerPoliciesOfListenerInput
    -- ** Request lenses
    , slbpoliLoadBalancerName
    , slbpoliLoadBalancerPort
    , slbpoliPolicyNames

    -- * Response
    , SetLoadBalancerPoliciesOfListenerResponse
    -- ** Response constructor
    , setLoadBalancerPoliciesOfListenerResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data SetLoadBalancerPoliciesOfListenerInput = SetLoadBalancerPoliciesOfListenerInput
    { _slbpoliLoadBalancerName :: Text
    , _slbpoliLoadBalancerPort :: Int
    , _slbpoliPolicyNames      :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetLoadBalancerPoliciesOfListenerInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbpoliLoadBalancerName' @::@ 'Text'
--
-- * 'slbpoliLoadBalancerPort' @::@ 'Int'
--
-- * 'slbpoliPolicyNames' @::@ ['Text']
--
setLoadBalancerPoliciesOfListenerInput :: Text -- ^ 'slbpoliLoadBalancerName'
                                       -> Int -- ^ 'slbpoliLoadBalancerPort'
                                       -> SetLoadBalancerPoliciesOfListenerInput
setLoadBalancerPoliciesOfListenerInput p1 p2 = SetLoadBalancerPoliciesOfListenerInput
    { _slbpoliLoadBalancerName = p1
    , _slbpoliLoadBalancerPort = p2
    , _slbpoliPolicyNames      = mempty
    }

-- | The name of the load balancer.
slbpoliLoadBalancerName :: Lens' SetLoadBalancerPoliciesOfListenerInput Text
slbpoliLoadBalancerName =
    lens _slbpoliLoadBalancerName (\s a -> s { _slbpoliLoadBalancerName = a })

-- | The external port of the load balancer to associate the policy.
slbpoliLoadBalancerPort :: Lens' SetLoadBalancerPoliciesOfListenerInput Int
slbpoliLoadBalancerPort =
    lens _slbpoliLoadBalancerPort (\s a -> s { _slbpoliLoadBalancerPort = a })

-- | List of policies to be associated with the listener. If the list is
-- empty, the current policy is removed from the listener.
slbpoliPolicyNames :: Lens' SetLoadBalancerPoliciesOfListenerInput [Text]
slbpoliPolicyNames =
    lens _slbpoliPolicyNames (\s a -> s { _slbpoliPolicyNames = a })
instance ToQuery SetLoadBalancerPoliciesOfListenerInput

instance ToPath SetLoadBalancerPoliciesOfListenerInput where
    toPath = const "/"

data SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetLoadBalancerPoliciesOfListenerResponse' constructor.
setLoadBalancerPoliciesOfListenerResponse :: SetLoadBalancerPoliciesOfListenerResponse
setLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse
instance FromXML SetLoadBalancerPoliciesOfListenerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SetLoadBalancerPoliciesOfListenerResponse"

instance AWSRequest SetLoadBalancerPoliciesOfListenerInput where
    type Sv SetLoadBalancerPoliciesOfListenerInput = ELB
    type Rs SetLoadBalancerPoliciesOfListenerInput = SetLoadBalancerPoliciesOfListenerResponse

    request  = post "SetLoadBalancerPoliciesOfListener"
    response = nullaryResponse SetLoadBalancerPoliciesOfListenerResponse
