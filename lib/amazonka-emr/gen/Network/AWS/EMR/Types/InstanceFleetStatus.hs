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

import Network.AWS.EMR.Types.InstanceFleetState
import Network.AWS.EMR.Types.InstanceFleetStateChangeReason
import Network.AWS.EMR.Types.InstanceFleetTimeline
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
    state :: Lude.Maybe InstanceFleetState,
    -- | Provides status change reason details for the instance fleet.
    stateChangeReason :: Lude.Maybe InstanceFleetStateChangeReason,
    -- | Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
    timeline :: Lude.Maybe InstanceFleetTimeline
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceFleetStatus' with the minimum fields required to make a request.
--
-- * 'state' - A code representing the instance fleet status.
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
-- * 'stateChangeReason' - Provides status change reason details for the instance fleet.
-- * 'timeline' - Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
mkInstanceFleetStatus ::
  InstanceFleetStatus
mkInstanceFleetStatus =
  InstanceFleetStatus'
    { state = Lude.Nothing,
      stateChangeReason = Lude.Nothing,
      timeline = Lude.Nothing
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
ifsState :: Lens.Lens' InstanceFleetStatus (Lude.Maybe InstanceFleetState)
ifsState = Lens.lens (state :: InstanceFleetStatus -> Lude.Maybe InstanceFleetState) (\s a -> s {state = a} :: InstanceFleetStatus)
{-# DEPRECATED ifsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Provides status change reason details for the instance fleet.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifsStateChangeReason :: Lens.Lens' InstanceFleetStatus (Lude.Maybe InstanceFleetStateChangeReason)
ifsStateChangeReason = Lens.lens (stateChangeReason :: InstanceFleetStatus -> Lude.Maybe InstanceFleetStateChangeReason) (\s a -> s {stateChangeReason = a} :: InstanceFleetStatus)
{-# DEPRECATED ifsStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

-- | Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.
--
-- /Note:/ Consider using 'timeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifsTimeline :: Lens.Lens' InstanceFleetStatus (Lude.Maybe InstanceFleetTimeline)
ifsTimeline = Lens.lens (timeline :: InstanceFleetStatus -> Lude.Maybe InstanceFleetTimeline) (\s a -> s {timeline = a} :: InstanceFleetStatus)
{-# DEPRECATED ifsTimeline "Use generic-lens or generic-optics with 'timeline' instead." #-}

instance Lude.FromJSON InstanceFleetStatus where
  parseJSON =
    Lude.withObject
      "InstanceFleetStatus"
      ( \x ->
          InstanceFleetStatus'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StateChangeReason")
            Lude.<*> (x Lude..:? "Timeline")
      )
