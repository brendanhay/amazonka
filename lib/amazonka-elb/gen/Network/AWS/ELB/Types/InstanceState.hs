{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.InstanceState
  ( InstanceState (..),

    -- * Smart constructor
    mkInstanceState,

    -- * Lenses
    isDescription,
    isInstanceId,
    isReasonCode,
    isState,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.Description as Types
import qualified Network.AWS.ELB.Types.InstanceId as Types
import qualified Network.AWS.ELB.Types.ReasonCode as Types
import qualified Network.AWS.ELB.Types.State as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the state of an EC2 instance.
--
-- /See:/ 'mkInstanceState' smart constructor.
data InstanceState = InstanceState'
  { -- | A description of the instance state. This string can contain one or more of the following messages.
    --
    --
    --     * @N/A@
    --
    --
    --     * @A transient error occurred. Please try again later.@
    --
    --
    --     * @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@
    --
    --
    --     * @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@
    --
    --
    --     * @Instance registration is still in progress.@
    --
    --
    --     * @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@
    --
    --
    --     * @Instance is not currently registered with the LoadBalancer.@
    --
    --
    --     * @Instance deregistration currently in progress.@
    --
    --
    --     * @Disable Availability Zone is currently in progress.@
    --
    --
    --     * @Instance is in pending state.@
    --
    --
    --     * @Instance is in stopped state.@
    --
    --
    --     * @Instance is in terminated state.@
    description :: Core.Maybe Types.Description,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | Information about the cause of @OutOfService@ instances. Specifically, whether the cause is Elastic Load Balancing or the instance.
    --
    -- Valid values: @ELB@ | @Instance@ | @N/A@
    reasonCode :: Core.Maybe Types.ReasonCode,
    -- | The current state of the instance.
    --
    -- Valid values: @InService@ | @OutOfService@ | @Unknown@
    state :: Core.Maybe Types.State
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceState' value with any optional fields omitted.
mkInstanceState ::
  InstanceState
mkInstanceState =
  InstanceState'
    { description = Core.Nothing,
      instanceId = Core.Nothing,
      reasonCode = Core.Nothing,
      state = Core.Nothing
    }

-- | A description of the instance state. This string can contain one or more of the following messages.
--
--
--     * @N/A@
--
--
--     * @A transient error occurred. Please try again later.@
--
--
--     * @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@
--
--
--     * @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@
--
--
--     * @Instance registration is still in progress.@
--
--
--     * @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@
--
--
--     * @Instance is not currently registered with the LoadBalancer.@
--
--
--     * @Instance deregistration currently in progress.@
--
--
--     * @Disable Availability Zone is currently in progress.@
--
--
--     * @Instance is in pending state.@
--
--
--     * @Instance is in stopped state.@
--
--
--     * @Instance is in terminated state.@
--
--
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDescription :: Lens.Lens' InstanceState (Core.Maybe Types.Description)
isDescription = Lens.field @"description"
{-# DEPRECATED isDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInstanceId :: Lens.Lens' InstanceState (Core.Maybe Types.InstanceId)
isInstanceId = Lens.field @"instanceId"
{-# DEPRECATED isInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Information about the cause of @OutOfService@ instances. Specifically, whether the cause is Elastic Load Balancing or the instance.
--
-- Valid values: @ELB@ | @Instance@ | @N/A@
--
-- /Note:/ Consider using 'reasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isReasonCode :: Lens.Lens' InstanceState (Core.Maybe Types.ReasonCode)
isReasonCode = Lens.field @"reasonCode"
{-# DEPRECATED isReasonCode "Use generic-lens or generic-optics with 'reasonCode' instead." #-}

-- | The current state of the instance.
--
-- Valid values: @InService@ | @OutOfService@ | @Unknown@
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isState :: Lens.Lens' InstanceState (Core.Maybe Types.State)
isState = Lens.field @"state"
{-# DEPRECATED isState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromXML InstanceState where
  parseXML x =
    InstanceState'
      Core.<$> (x Core..@? "Description")
      Core.<*> (x Core..@? "InstanceId")
      Core.<*> (x Core..@? "ReasonCode")
      Core.<*> (x Core..@? "State")
