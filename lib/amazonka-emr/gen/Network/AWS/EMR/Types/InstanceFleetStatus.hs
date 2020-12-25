{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetStatus
  ( InstanceFleetStatus (..),

    -- * Smart constructor
    mkInstanceFleetStatus,

    -- * Lenses
    ifsState,
    ifsStateChangeReason,
    ifsTimeline,
  )
where

import qualified Network.AWS.EMR.Types.InstanceFleetState as Types
import qualified Network.AWS.EMR.Types.InstanceFleetStateChangeReason as Types
import qualified Network.AWS.EMR.Types.InstanceFleetTimeline as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of the instance fleet.
--
-- /See:/ 'mkInstanceFleetStatus' smart constructor.
data InstanceFleetStatus = InstanceFleetStatus'
  { -- | A code representing the instance fleet status.
    --
    --
    --     * @PROVISIONING@ —The instance fleet is provisioning EC2 resources and is not yet ready to run jobs.
    --
    --
    --     * @BOOTSTRAPPING@ —EC2 instances and other resources have been provisioned and the bootstrap actions specified for the instances are underway.
    --
    --
    --     * @RUNNING@ —EC2 instances and other resources are running. They are either executing jobs or waiting to execute jobs.
    --
    --
    --     * @RESIZING@ —A resize operation is underway. EC2 instances are either being added or removed.
    --
    --
    --     * @SUSPENDED@ —A resize operation could not complete. Existing EC2 instances are running, but instances can't be added or removed.
    --
    --
    --     * @TERMINATING@ —The instance fleet is terminating EC2 instances.
    --
    --
    --     * @TERMINATED@ —The instance fleet is no longer active, and all EC2 instances have been terminated.
    state :: Core.Maybe Types.InstanceFleetState,
    -- | Provides status change reason details for the instance fleet.
    stateChangeReason :: Core.Maybe Types.InstanceFleetStateChangeReason,
    -- | Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
    timeline :: Core.Maybe Types.InstanceFleetTimeline
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InstanceFleetStatus' value with any optional fields omitted.
mkInstanceFleetStatus ::
  InstanceFleetStatus
mkInstanceFleetStatus =
  InstanceFleetStatus'
    { state = Core.Nothing,
      stateChangeReason = Core.Nothing,
      timeline = Core.Nothing
    }

-- | A code representing the instance fleet status.
--
--
--     * @PROVISIONING@ —The instance fleet is provisioning EC2 resources and is not yet ready to run jobs.
--
--
--     * @BOOTSTRAPPING@ —EC2 instances and other resources have been provisioned and the bootstrap actions specified for the instances are underway.
--
--
--     * @RUNNING@ —EC2 instances and other resources are running. They are either executing jobs or waiting to execute jobs.
--
--
--     * @RESIZING@ —A resize operation is underway. EC2 instances are either being added or removed.
--
--
--     * @SUSPENDED@ —A resize operation could not complete. Existing EC2 instances are running, but instances can't be added or removed.
--
--
--     * @TERMINATING@ —The instance fleet is terminating EC2 instances.
--
--
--     * @TERMINATED@ —The instance fleet is no longer active, and all EC2 instances have been terminated.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifsState :: Lens.Lens' InstanceFleetStatus (Core.Maybe Types.InstanceFleetState)
ifsState = Lens.field @"state"
{-# DEPRECATED ifsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Provides status change reason details for the instance fleet.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifsStateChangeReason :: Lens.Lens' InstanceFleetStatus (Core.Maybe Types.InstanceFleetStateChangeReason)
ifsStateChangeReason = Lens.field @"stateChangeReason"
{-# DEPRECATED ifsStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

-- | Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifsTimeline :: Lens.Lens' InstanceFleetStatus (Core.Maybe Types.InstanceFleetTimeline)
ifsTimeline = Lens.field @"timeline"
{-# DEPRECATED ifsTimeline "Use generic-lens or generic-optics with 'timeline' instead." #-}

instance Core.FromJSON InstanceFleetStatus where
  parseJSON =
    Core.withObject "InstanceFleetStatus" Core.$
      \x ->
        InstanceFleetStatus'
          Core.<$> (x Core..:? "State")
          Core.<*> (x Core..:? "StateChangeReason")
          Core.<*> (x Core..:? "Timeline")
