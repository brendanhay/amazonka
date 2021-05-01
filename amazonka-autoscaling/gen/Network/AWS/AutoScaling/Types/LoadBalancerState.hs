{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LoadBalancerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LoadBalancerState where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the state of a Classic Load Balancer.
--
-- If you specify a load balancer when creating the Auto Scaling group, the
-- state of the load balancer is @InService@.
--
-- If you attach a load balancer to an existing Auto Scaling group, the
-- initial state is @Adding@. The state transitions to @Added@ after all
-- instances in the group are registered with the load balancer. If Elastic
-- Load Balancing health checks are enabled for the load balancer, the
-- state transitions to @InService@ after at least one instance in the
-- group passes the health check. If EC2 health checks are enabled instead,
-- the load balancer remains in the @Added@ state.
--
-- /See:/ 'newLoadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { -- | One of the following load balancer states:
    --
    -- -   @Adding@ - The instances in the group are being registered with the
    --     load balancer.
    --
    -- -   @Added@ - All instances in the group are registered with the load
    --     balancer.
    --
    -- -   @InService@ - At least one instance in the group passed an ELB
    --     health check.
    --
    -- -   @Removing@ - The instances in the group are being deregistered from
    --     the load balancer. If connection draining is enabled, Elastic Load
    --     Balancing waits for in-flight requests to complete before
    --     deregistering the instances.
    --
    -- -   @Removed@ - All instances in the group are deregistered from the
    --     load balancer.
    state :: Prelude.Maybe Prelude.Text,
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'loadBalancerState_state' - One of the following load balancer states:
--
-- -   @Adding@ - The instances in the group are being registered with the
--     load balancer.
--
-- -   @Added@ - All instances in the group are registered with the load
--     balancer.
--
-- -   @InService@ - At least one instance in the group passed an ELB
--     health check.
--
-- -   @Removing@ - The instances in the group are being deregistered from
--     the load balancer. If connection draining is enabled, Elastic Load
--     Balancing waits for in-flight requests to complete before
--     deregistering the instances.
--
-- -   @Removed@ - All instances in the group are deregistered from the
--     load balancer.
--
-- 'loadBalancerName', 'loadBalancerState_loadBalancerName' - The name of the load balancer.
newLoadBalancerState ::
  LoadBalancerState
newLoadBalancerState =
  LoadBalancerState'
    { state = Prelude.Nothing,
      loadBalancerName = Prelude.Nothing
    }

-- | One of the following load balancer states:
--
-- -   @Adding@ - The instances in the group are being registered with the
--     load balancer.
--
-- -   @Added@ - All instances in the group are registered with the load
--     balancer.
--
-- -   @InService@ - At least one instance in the group passed an ELB
--     health check.
--
-- -   @Removing@ - The instances in the group are being deregistered from
--     the load balancer. If connection draining is enabled, Elastic Load
--     Balancing waits for in-flight requests to complete before
--     deregistering the instances.
--
-- -   @Removed@ - All instances in the group are deregistered from the
--     load balancer.
loadBalancerState_state :: Lens.Lens' LoadBalancerState (Prelude.Maybe Prelude.Text)
loadBalancerState_state = Lens.lens (\LoadBalancerState' {state} -> state) (\s@LoadBalancerState' {} a -> s {state = a} :: LoadBalancerState)

-- | The name of the load balancer.
loadBalancerState_loadBalancerName :: Lens.Lens' LoadBalancerState (Prelude.Maybe Prelude.Text)
loadBalancerState_loadBalancerName = Lens.lens (\LoadBalancerState' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancerState' {} a -> s {loadBalancerName = a} :: LoadBalancerState)

instance Prelude.FromXML LoadBalancerState where
  parseXML x =
    LoadBalancerState'
      Prelude.<$> (x Prelude..@? "State")
      Prelude.<*> (x Prelude..@? "LoadBalancerName")

instance Prelude.Hashable LoadBalancerState

instance Prelude.NFData LoadBalancerState
