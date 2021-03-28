{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem
  ( EnableFastSnapshotRestoreSuccessItem (..)
  -- * Smart constructor
  , mkEnableFastSnapshotRestoreSuccessItem
  -- * Lenses
  , efsrsiAvailabilityZone
  , efsrsiDisabledTime
  , efsrsiDisablingTime
  , efsrsiEnabledTime
  , efsrsiEnablingTime
  , efsrsiOptimizingTime
  , efsrsiOwnerAlias
  , efsrsiOwnerId
  , efsrsiSnapshotId
  , efsrsiState
  , efsrsiStateTransitionReason
  ) where

import qualified Network.AWS.EC2.Types.FastSnapshotRestoreStateCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes fast snapshot restores that were successfully enabled.
--
-- /See:/ 'mkEnableFastSnapshotRestoreSuccessItem' smart constructor.
data EnableFastSnapshotRestoreSuccessItem = EnableFastSnapshotRestoreSuccessItem'
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
    -- ^ The state of fast snapshot restores.
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

-- | Creates a 'EnableFastSnapshotRestoreSuccessItem' value with any optional fields omitted.
mkEnableFastSnapshotRestoreSuccessItem
    :: EnableFastSnapshotRestoreSuccessItem
mkEnableFastSnapshotRestoreSuccessItem
  = EnableFastSnapshotRestoreSuccessItem'{availabilityZone =
                                            Core.Nothing,
                                          disabledTime = Core.Nothing, disablingTime = Core.Nothing,
                                          enabledTime = Core.Nothing, enablingTime = Core.Nothing,
                                          optimizingTime = Core.Nothing, ownerAlias = Core.Nothing,
                                          ownerId = Core.Nothing, snapshotId = Core.Nothing,
                                          state = Core.Nothing,
                                          stateTransitionReason = Core.Nothing}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiAvailabilityZone :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
efsrsiAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE efsrsiAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The time at which fast snapshot restores entered the @disabled@ state.
--
-- /Note:/ Consider using 'disabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiDisabledTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
efsrsiDisabledTime = Lens.field @"disabledTime"
{-# INLINEABLE efsrsiDisabledTime #-}
{-# DEPRECATED disabledTime "Use generic-lens or generic-optics with 'disabledTime' instead"  #-}

-- | The time at which fast snapshot restores entered the @disabling@ state.
--
-- /Note:/ Consider using 'disablingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiDisablingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
efsrsiDisablingTime = Lens.field @"disablingTime"
{-# INLINEABLE efsrsiDisablingTime #-}
{-# DEPRECATED disablingTime "Use generic-lens or generic-optics with 'disablingTime' instead"  #-}

-- | The time at which fast snapshot restores entered the @enabled@ state.
--
-- /Note:/ Consider using 'enabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiEnabledTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
efsrsiEnabledTime = Lens.field @"enabledTime"
{-# INLINEABLE efsrsiEnabledTime #-}
{-# DEPRECATED enabledTime "Use generic-lens or generic-optics with 'enabledTime' instead"  #-}

-- | The time at which fast snapshot restores entered the @enabling@ state.
--
-- /Note:/ Consider using 'enablingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiEnablingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
efsrsiEnablingTime = Lens.field @"enablingTime"
{-# INLINEABLE efsrsiEnablingTime #-}
{-# DEPRECATED enablingTime "Use generic-lens or generic-optics with 'enablingTime' instead"  #-}

-- | The time at which fast snapshot restores entered the @optimizing@ state.
--
-- /Note:/ Consider using 'optimizingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiOptimizingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.UTCTime)
efsrsiOptimizingTime = Lens.field @"optimizingTime"
{-# INLINEABLE efsrsiOptimizingTime #-}
{-# DEPRECATED optimizingTime "Use generic-lens or generic-optics with 'optimizingTime' instead"  #-}

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiOwnerAlias :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
efsrsiOwnerAlias = Lens.field @"ownerAlias"
{-# INLINEABLE efsrsiOwnerAlias #-}
{-# DEPRECATED ownerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead"  #-}

-- | The ID of the AWS account that enabled fast snapshot restores on the snapshot.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiOwnerId :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
efsrsiOwnerId = Lens.field @"ownerId"
{-# INLINEABLE efsrsiOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiSnapshotId :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
efsrsiSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE efsrsiSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The state of fast snapshot restores.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiState :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Types.FastSnapshotRestoreStateCode)
efsrsiState = Lens.field @"state"
{-# INLINEABLE efsrsiState #-}
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
efsrsiStateTransitionReason :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Core.Maybe Core.Text)
efsrsiStateTransitionReason = Lens.field @"stateTransitionReason"
{-# INLINEABLE efsrsiStateTransitionReason #-}
{-# DEPRECATED stateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead"  #-}

instance Core.FromXML EnableFastSnapshotRestoreSuccessItem where
        parseXML x
          = EnableFastSnapshotRestoreSuccessItem' Core.<$>
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
