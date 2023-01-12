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
-- Module      : Amazonka.AutoScaling.Types.LoadBalancerState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LoadBalancerState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of a Classic Load Balancer.
--
-- /See:/ 'newLoadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | One of the following load balancer states:
    --
    -- -   @Adding@ - The Auto Scaling instances are being registered with the
    --     load balancer.
    --
    -- -   @Added@ - All Auto Scaling instances are registered with the load
    --     balancer.
    --
    -- -   @InService@ - At least one Auto Scaling instance passed an @ELB@
    --     health check.
    --
    -- -   @Removing@ - The Auto Scaling instances are being deregistered from
    --     the load balancer. If connection draining is enabled, Elastic Load
    --     Balancing waits for in-flight requests to complete before
    --     deregistering the instances.
    --
    -- -   @Removed@ - All Auto Scaling instances are deregistered from the
    --     load balancer.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'loadBalancerState_loadBalancerName' - The name of the load balancer.
--
-- 'state', 'loadBalancerState_state' - One of the following load balancer states:
--
-- -   @Adding@ - The Auto Scaling instances are being registered with the
--     load balancer.
--
-- -   @Added@ - All Auto Scaling instances are registered with the load
--     balancer.
--
-- -   @InService@ - At least one Auto Scaling instance passed an @ELB@
--     health check.
--
-- -   @Removing@ - The Auto Scaling instances are being deregistered from
--     the load balancer. If connection draining is enabled, Elastic Load
--     Balancing waits for in-flight requests to complete before
--     deregistering the instances.
--
-- -   @Removed@ - All Auto Scaling instances are deregistered from the
--     load balancer.
newLoadBalancerState ::
  LoadBalancerState
newLoadBalancerState =
  LoadBalancerState'
    { loadBalancerName =
        Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The name of the load balancer.
loadBalancerState_loadBalancerName :: Lens.Lens' LoadBalancerState (Prelude.Maybe Prelude.Text)
loadBalancerState_loadBalancerName = Lens.lens (\LoadBalancerState' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancerState' {} a -> s {loadBalancerName = a} :: LoadBalancerState)

-- | One of the following load balancer states:
--
-- -   @Adding@ - The Auto Scaling instances are being registered with the
--     load balancer.
--
-- -   @Added@ - All Auto Scaling instances are registered with the load
--     balancer.
--
-- -   @InService@ - At least one Auto Scaling instance passed an @ELB@
--     health check.
--
-- -   @Removing@ - The Auto Scaling instances are being deregistered from
--     the load balancer. If connection draining is enabled, Elastic Load
--     Balancing waits for in-flight requests to complete before
--     deregistering the instances.
--
-- -   @Removed@ - All Auto Scaling instances are deregistered from the
--     load balancer.
loadBalancerState_state :: Lens.Lens' LoadBalancerState (Prelude.Maybe Prelude.Text)
loadBalancerState_state = Lens.lens (\LoadBalancerState' {state} -> state) (\s@LoadBalancerState' {} a -> s {state = a} :: LoadBalancerState)

instance Data.FromXML LoadBalancerState where
  parseXML x =
    LoadBalancerState'
      Prelude.<$> (x Data..@? "LoadBalancerName")
      Prelude.<*> (x Data..@? "State")

instance Prelude.Hashable LoadBalancerState where
  hashWithSalt _salt LoadBalancerState' {..} =
    _salt `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` state

instance Prelude.NFData LoadBalancerState where
  rnf LoadBalancerState' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf state
