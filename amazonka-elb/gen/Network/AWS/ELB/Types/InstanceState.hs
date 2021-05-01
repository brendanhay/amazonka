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
-- Module      : Network.AWS.ELB.Types.InstanceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.InstanceState where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the state of an EC2 instance.
--
-- /See:/ 'newInstanceState' smart constructor.
data InstanceState = InstanceState'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Information about the cause of @OutOfService@ instances. Specifically,
    -- whether the cause is Elastic Load Balancing or the instance.
    --
    -- Valid values: @ELB@ | @Instance@ | @N\/A@
    reasonCode :: Prelude.Maybe Prelude.Text,
    -- | The current state of the instance.
    --
    -- Valid values: @InService@ | @OutOfService@ | @Unknown@
    state :: Prelude.Maybe Prelude.Text,
    -- | A description of the instance state. This string can contain one or more
    -- of the following messages.
    --
    -- -   @N\/A@
    --
    -- -   @A transient error occurred. Please try again later.@
    --
    -- -   @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@
    --
    -- -   @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@
    --
    -- -   @Instance registration is still in progress.@
    --
    -- -   @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@
    --
    -- -   @Instance is not currently registered with the LoadBalancer.@
    --
    -- -   @Instance deregistration currently in progress.@
    --
    -- -   @Disable Availability Zone is currently in progress.@
    --
    -- -   @Instance is in pending state.@
    --
    -- -   @Instance is in stopped state.@
    --
    -- -   @Instance is in terminated state.@
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceState_instanceId' - The ID of the instance.
--
-- 'reasonCode', 'instanceState_reasonCode' - Information about the cause of @OutOfService@ instances. Specifically,
-- whether the cause is Elastic Load Balancing or the instance.
--
-- Valid values: @ELB@ | @Instance@ | @N\/A@
--
-- 'state', 'instanceState_state' - The current state of the instance.
--
-- Valid values: @InService@ | @OutOfService@ | @Unknown@
--
-- 'description', 'instanceState_description' - A description of the instance state. This string can contain one or more
-- of the following messages.
--
-- -   @N\/A@
--
-- -   @A transient error occurred. Please try again later.@
--
-- -   @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@
--
-- -   @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@
--
-- -   @Instance registration is still in progress.@
--
-- -   @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@
--
-- -   @Instance is not currently registered with the LoadBalancer.@
--
-- -   @Instance deregistration currently in progress.@
--
-- -   @Disable Availability Zone is currently in progress.@
--
-- -   @Instance is in pending state.@
--
-- -   @Instance is in stopped state.@
--
-- -   @Instance is in terminated state.@
newInstanceState ::
  InstanceState
newInstanceState =
  InstanceState'
    { instanceId = Prelude.Nothing,
      reasonCode = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ID of the instance.
instanceState_instanceId :: Lens.Lens' InstanceState (Prelude.Maybe Prelude.Text)
instanceState_instanceId = Lens.lens (\InstanceState' {instanceId} -> instanceId) (\s@InstanceState' {} a -> s {instanceId = a} :: InstanceState)

-- | Information about the cause of @OutOfService@ instances. Specifically,
-- whether the cause is Elastic Load Balancing or the instance.
--
-- Valid values: @ELB@ | @Instance@ | @N\/A@
instanceState_reasonCode :: Lens.Lens' InstanceState (Prelude.Maybe Prelude.Text)
instanceState_reasonCode = Lens.lens (\InstanceState' {reasonCode} -> reasonCode) (\s@InstanceState' {} a -> s {reasonCode = a} :: InstanceState)

-- | The current state of the instance.
--
-- Valid values: @InService@ | @OutOfService@ | @Unknown@
instanceState_state :: Lens.Lens' InstanceState (Prelude.Maybe Prelude.Text)
instanceState_state = Lens.lens (\InstanceState' {state} -> state) (\s@InstanceState' {} a -> s {state = a} :: InstanceState)

-- | A description of the instance state. This string can contain one or more
-- of the following messages.
--
-- -   @N\/A@
--
-- -   @A transient error occurred. Please try again later.@
--
-- -   @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@
--
-- -   @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@
--
-- -   @Instance registration is still in progress.@
--
-- -   @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@
--
-- -   @Instance is not currently registered with the LoadBalancer.@
--
-- -   @Instance deregistration currently in progress.@
--
-- -   @Disable Availability Zone is currently in progress.@
--
-- -   @Instance is in pending state.@
--
-- -   @Instance is in stopped state.@
--
-- -   @Instance is in terminated state.@
instanceState_description :: Lens.Lens' InstanceState (Prelude.Maybe Prelude.Text)
instanceState_description = Lens.lens (\InstanceState' {description} -> description) (\s@InstanceState' {} a -> s {description = a} :: InstanceState)

instance Prelude.FromXML InstanceState where
  parseXML x =
    InstanceState'
      Prelude.<$> (x Prelude..@? "InstanceId")
      Prelude.<*> (x Prelude..@? "ReasonCode")
      Prelude.<*> (x Prelude..@? "State")
      Prelude.<*> (x Prelude..@? "Description")

instance Prelude.Hashable InstanceState

instance Prelude.NFData InstanceState
