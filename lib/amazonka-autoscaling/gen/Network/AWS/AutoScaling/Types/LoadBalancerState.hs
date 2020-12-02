{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LoadBalancerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LoadBalancerState where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the state of a Classic Load Balancer.
--
--
-- If you specify a load balancer when creating the Auto Scaling group, the state of the load balancer is @InService@ .
--
-- If you attach a load balancer to an existing Auto Scaling group, the initial state is @Adding@ . The state transitions to @Added@ after all instances in the group are registered with the load balancer. If Elastic Load Balancing health checks are enabled for the load balancer, the state transitions to @InService@ after at least one instance in the group passes the health check. If EC2 health checks are enabled instead, the load balancer remains in the @Added@ state.
--
--
-- /See:/ 'loadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { _lbsState ::
      !(Maybe Text),
    _lbsLoadBalancerName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbsState' - One of the following load balancer states:     * @Adding@ - The instances in the group are being registered with the load balancer.     * @Added@ - All instances in the group are registered with the load balancer.     * @InService@ - At least one instance in the group passed an ELB health check.     * @Removing@ - The instances in the group are being deregistered from the load balancer. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.     * @Removed@ - All instances in the group are deregistered from the load balancer.
--
-- * 'lbsLoadBalancerName' - The name of the load balancer.
loadBalancerState ::
  LoadBalancerState
loadBalancerState =
  LoadBalancerState'
    { _lbsState = Nothing,
      _lbsLoadBalancerName = Nothing
    }

-- | One of the following load balancer states:     * @Adding@ - The instances in the group are being registered with the load balancer.     * @Added@ - All instances in the group are registered with the load balancer.     * @InService@ - At least one instance in the group passed an ELB health check.     * @Removing@ - The instances in the group are being deregistered from the load balancer. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.     * @Removed@ - All instances in the group are deregistered from the load balancer.
lbsState :: Lens' LoadBalancerState (Maybe Text)
lbsState = lens _lbsState (\s a -> s {_lbsState = a})

-- | The name of the load balancer.
lbsLoadBalancerName :: Lens' LoadBalancerState (Maybe Text)
lbsLoadBalancerName = lens _lbsLoadBalancerName (\s a -> s {_lbsLoadBalancerName = a})

instance FromXML LoadBalancerState where
  parseXML x =
    LoadBalancerState'
      <$> (x .@? "State") <*> (x .@? "LoadBalancerName")

instance Hashable LoadBalancerState

instance NFData LoadBalancerState
