{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem
  ( DisableFastSnapshotRestoreSuccessItem (..)
  -- * Smart constructor
  , mkDisableFastSnapshotRestoreSuccessItem
  -- * Lenses
  , dAvailabilityZone
  , dDisabledTime
  , dDisablingTime
  , dEnabledTime
  , dEnablingTime
  , dOptimizingTime
  , dOwnerAlias
  , dOwnerId
  , dSnapshotId
  , dState
  , dStateTransitionReason
  ) where

import qualified Network.AWS.EC2.Types.FastSnapshotRestoreStateCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes fast snapshot restores that were successfully disabled.
--
-- /See:/ 'mkDisableFastSnapshotRestoreSuccessItem' smart constructor.
data DisableFastSnapshotRestoreSuccessItem = DisableFastSnapshotRestoreSuccessItem'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone.
  , disabledTime :: Core.Maybe Core.UTCTime
    -- ^ The time at which fast snapshot restores entered the @disabled@ state.
  , disablingTime :: Core.Maybe Core.UTCTime
    -- ^ The time at which fast snapshot restores entered the @disabling@ state.
  , enabledTime :: Core.Maybe Core.UTCTime
    -- ^ The time at which fast snapshot restores entered the @enabled@ state.
  , enablingTime :: Core.Maybe Core.UTCTime
    -- ^ The time at which fast snapshot restores entered the @enabling@ state.
  , optimizingTime :: Core.Maybe Core.UTCTime
    -- ^ The time at which fast snapshot restores entered the @optimizing@ state.
  , ownerAlias :: Core.Maybe Core.Text
    -- ^ The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that enabled fast snapshot restores on the snapshot.
  , snapshotId :: Core.Maybe Core.Text
    -- ^ The ID of the snapshot.
  , state :: Core.Maybe Types.FastSnapshotRestoreStateCode
    -- ^ The state of fast snapshot restores for the snapshot.
  , stateTransitionReason :: Core.Maybe Core.Text
    -- ^ The reason for the state transition. The possible values are as follows:
--
--
--     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .
--
--
--     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DisableFastSnapshotRestoreSuccessItem' value with any optional fields omitted.
mkDisableFastSnapshotRestoreSuccessItem
    :: DisableFastSnapshotRestoreSuccessItem
mkDisableFastSnapshotRestoreSuccessItem
  = DisableFastSnapshotRestoreSuccessItem'{availabilityZone =
                                             Core.Nothing,
                                           disabledTime = Core.Nothing,
                                           disablingTime = Core.Nothing, enabledTime = Core.Nothing,
                                           enablingTime = Core.Nothing,
                                           optimizingTime = Core.Nothing, ownerAlias = Core.Nothing,
                                           ownerId = Core.Nothing, snapshotId = Core.Nothing,
                                           state = Core.Nothing,
                                           stateTransitionReason = Core.Nothing}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAvailabilityZone :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
dAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE dAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The time at which fast snapshot restores entered the @disabled@ state.
--
-- /Note:/ Consider using 'disabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDisabledTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
dDisabledTime = Lens.field @"disabledTime"
{-# INLINEABLE dDisabledTime #-}
{-# DEPRECATED disabledTime "Use generic-lens or generic-optics with 'disabledTime' instead"  #-}

-- | The time at which fast snapshot restores entered the @disabling@ state.
--
-- /Note:/ Consider using 'disablingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDisablingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
dDisablingTime = Lens.field @"disablingTime"
{-# INLINEABLE dDisablingTime #-}
{-# DEPRECATED disablingTime "Use generic-lens or generic-optics with 'disablingTime' instead"  #-}

-- | The time at which fast snapshot restores entered the @enabled@ state.
--
-- /Note:/ Consider using 'enabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEnabledTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
dEnabledTime = Lens.field @"enabledTime"
{-# INLINEABLE dEnabledTime #-}
{-# DEPRECATED enabledTime "Use generic-lens or generic-optics with 'enabledTime' instead"  #-}

-- | The time at which fast snapshot restores entered the @enabling@ state.
--
-- /Note:/ Consider using 'enablingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEnablingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
dEnablingTime = Lens.field @"enablingTime"
{-# INLINEABLE dEnablingTime #-}
{-# DEPRECATED enablingTime "Use generic-lens or generic-optics with 'enablingTime' instead"  #-}

-- | The time at which fast snapshot restores entered the @optimizing@ state.
--
-- /Note:/ Consider using 'optimizingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOptimizingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
dOptimizingTime = Lens.field @"optimizingTime"
{-# INLINEABLE dOptimizingTime #-}
{-# DEPRECATED optimizingTime "Use generic-lens or generic-optics with 'optimizingTime' instead"  #-}

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOwnerAlias :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
dOwnerAlias = Lens.field @"ownerAlias"
{-# INLINEABLE dOwnerAlias #-}
{-# DEPRECATED ownerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead"  #-}

-- | The ID of the AWS account that enabled fast snapshot restores on the snapshot.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOwnerId :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
dOwnerId = Lens.field @"ownerId"
{-# INLINEABLE dOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotId :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
dSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE dSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The state of fast snapshot restores for the snapshot.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dState :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Types.FastSnapshotRestoreStateCode)
dState = Lens.field @"state"
{-# INLINEABLE dState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason for the state transition. The possible values are as follows:
--
--
--     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .
--
--
--     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
--
--
--
-- /Note:/ Consider using 'stateTransitionReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStateTransitionReason :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
dStateTransitionReason = Lens.field @"stateTransitionReason"
{-# INLINEABLE dStateTransitionReason #-}
{-# DEPRECATED stateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead"  #-}

instance Core.FromXML DisableFastSnapshotRestoreSuccessItem where
        parseXML x
          = DisableFastSnapshotRestoreSuccessItem' Core.<$>
              (x Core..@? "availabilityZone") Core.<*> x Core..@? "disabledTime"
                Core.<*> x Core..@? "disablingTime"
                Core.<*> x Core..@? "enabledTime"
                Core.<*> x Core..@? "enablingTime"
                Core.<*> x Core..@? "optimizingTime"
                Core.<*> x Core..@? "ownerAlias"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "snapshotId"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "stateTransitionReason"
