{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the state of a target group.
--
--
-- If you attach a target group to an existing Auto Scaling group, the initial state is @Adding@ . The state transitions to @Added@ after all Auto Scaling instances are registered with the target group. If Elastic Load Balancing health checks are enabled, the state transitions to @InService@ after at least one Auto Scaling instance passes the health check. If EC2 health checks are enabled instead, the target group remains in the @Added@ state.
--
--
-- /See:/ 'loadBalancerTargetGroupState' smart constructor.
data LoadBalancerTargetGroupState = LoadBalancerTargetGroupState'
  { _lbtgsState ::
      !(Maybe Text),
    _lbtgsLoadBalancerTargetGroupARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerTargetGroupState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbtgsState' - The state of the target group.     * @Adding@ - The Auto Scaling instances are being registered with the target group.     * @Added@ - All Auto Scaling instances are registered with the target group.     * @InService@ - At least one Auto Scaling instance passed an ELB health check.     * @Removing@ - The Auto Scaling instances are being deregistered from the target group. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.     * @Removed@ - All Auto Scaling instances are deregistered from the target group.
--
-- * 'lbtgsLoadBalancerTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
loadBalancerTargetGroupState ::
  LoadBalancerTargetGroupState
loadBalancerTargetGroupState =
  LoadBalancerTargetGroupState'
    { _lbtgsState = Nothing,
      _lbtgsLoadBalancerTargetGroupARN = Nothing
    }

-- | The state of the target group.     * @Adding@ - The Auto Scaling instances are being registered with the target group.     * @Added@ - All Auto Scaling instances are registered with the target group.     * @InService@ - At least one Auto Scaling instance passed an ELB health check.     * @Removing@ - The Auto Scaling instances are being deregistered from the target group. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.     * @Removed@ - All Auto Scaling instances are deregistered from the target group.
lbtgsState :: Lens' LoadBalancerTargetGroupState (Maybe Text)
lbtgsState = lens _lbtgsState (\s a -> s {_lbtgsState = a})

-- | The Amazon Resource Name (ARN) of the target group.
lbtgsLoadBalancerTargetGroupARN :: Lens' LoadBalancerTargetGroupState (Maybe Text)
lbtgsLoadBalancerTargetGroupARN = lens _lbtgsLoadBalancerTargetGroupARN (\s a -> s {_lbtgsLoadBalancerTargetGroupARN = a})

instance FromXML LoadBalancerTargetGroupState where
  parseXML x =
    LoadBalancerTargetGroupState'
      <$> (x .@? "State") <*> (x .@? "LoadBalancerTargetGroupARN")

instance Hashable LoadBalancerTargetGroupState

instance NFData LoadBalancerTargetGroupState
