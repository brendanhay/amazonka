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
-- Module      : Amazonka.AutoScaling.Types.LoadBalancerTargetGroupState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LoadBalancerTargetGroupState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of a target group.
--
-- /See:/ 'newLoadBalancerTargetGroupState' smart constructor.
data LoadBalancerTargetGroupState = LoadBalancerTargetGroupState'
  { -- | The state of the target group.
    --
    -- -   @Adding@ - The Auto Scaling instances are being registered with the
    --     target group.
    --
    -- -   @Added@ - All Auto Scaling instances are registered with the target
    --     group.
    --
    -- -   @InService@ - At least one Auto Scaling instance passed an @ELB@
    --     health check.
    --
    -- -   @Removing@ - The Auto Scaling instances are being deregistered from
    --     the target group. If connection draining is enabled, Elastic Load
    --     Balancing waits for in-flight requests to complete before
    --     deregistering the instances.
    --
    -- -   @Removed@ - All Auto Scaling instances are deregistered from the
    --     target group.
    state :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the target group.
    loadBalancerTargetGroupARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerTargetGroupState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'loadBalancerTargetGroupState_state' - The state of the target group.
--
-- -   @Adding@ - The Auto Scaling instances are being registered with the
--     target group.
--
-- -   @Added@ - All Auto Scaling instances are registered with the target
--     group.
--
-- -   @InService@ - At least one Auto Scaling instance passed an @ELB@
--     health check.
--
-- -   @Removing@ - The Auto Scaling instances are being deregistered from
--     the target group. If connection draining is enabled, Elastic Load
--     Balancing waits for in-flight requests to complete before
--     deregistering the instances.
--
-- -   @Removed@ - All Auto Scaling instances are deregistered from the
--     target group.
--
-- 'loadBalancerTargetGroupARN', 'loadBalancerTargetGroupState_loadBalancerTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
newLoadBalancerTargetGroupState ::
  LoadBalancerTargetGroupState
newLoadBalancerTargetGroupState =
  LoadBalancerTargetGroupState'
    { state =
        Prelude.Nothing,
      loadBalancerTargetGroupARN = Prelude.Nothing
    }

-- | The state of the target group.
--
-- -   @Adding@ - The Auto Scaling instances are being registered with the
--     target group.
--
-- -   @Added@ - All Auto Scaling instances are registered with the target
--     group.
--
-- -   @InService@ - At least one Auto Scaling instance passed an @ELB@
--     health check.
--
-- -   @Removing@ - The Auto Scaling instances are being deregistered from
--     the target group. If connection draining is enabled, Elastic Load
--     Balancing waits for in-flight requests to complete before
--     deregistering the instances.
--
-- -   @Removed@ - All Auto Scaling instances are deregistered from the
--     target group.
loadBalancerTargetGroupState_state :: Lens.Lens' LoadBalancerTargetGroupState (Prelude.Maybe Prelude.Text)
loadBalancerTargetGroupState_state = Lens.lens (\LoadBalancerTargetGroupState' {state} -> state) (\s@LoadBalancerTargetGroupState' {} a -> s {state = a} :: LoadBalancerTargetGroupState)

-- | The Amazon Resource Name (ARN) of the target group.
loadBalancerTargetGroupState_loadBalancerTargetGroupARN :: Lens.Lens' LoadBalancerTargetGroupState (Prelude.Maybe Prelude.Text)
loadBalancerTargetGroupState_loadBalancerTargetGroupARN = Lens.lens (\LoadBalancerTargetGroupState' {loadBalancerTargetGroupARN} -> loadBalancerTargetGroupARN) (\s@LoadBalancerTargetGroupState' {} a -> s {loadBalancerTargetGroupARN = a} :: LoadBalancerTargetGroupState)

instance Core.FromXML LoadBalancerTargetGroupState where
  parseXML x =
    LoadBalancerTargetGroupState'
      Prelude.<$> (x Core..@? "State")
      Prelude.<*> (x Core..@? "LoadBalancerTargetGroupARN")

instance
  Prelude.Hashable
    LoadBalancerTargetGroupState
  where
  hashWithSalt _salt LoadBalancerTargetGroupState' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` loadBalancerTargetGroupARN

instance Prelude.NFData LoadBalancerTargetGroupState where
  rnf LoadBalancerTargetGroupState' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf loadBalancerTargetGroupARN
