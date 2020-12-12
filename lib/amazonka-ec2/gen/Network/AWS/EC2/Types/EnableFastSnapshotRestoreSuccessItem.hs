{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreSuccessItem
  ( EnableFastSnapshotRestoreSuccessItem (..),

    -- * Smart constructor
    mkEnableFastSnapshotRestoreSuccessItem,

    -- * Lenses
    efsrsiDisablingTime,
    efsrsiState,
    efsrsiOwnerAlias,
    efsrsiDisabledTime,
    efsrsiEnabledTime,
    efsrsiOptimizingTime,
    efsrsiOwnerId,
    efsrsiStateTransitionReason,
    efsrsiAvailabilityZone,
    efsrsiSnapshotId,
    efsrsiEnablingTime,
  )
where

import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes fast snapshot restores that were successfully enabled.
--
-- /See:/ 'mkEnableFastSnapshotRestoreSuccessItem' smart constructor.
data EnableFastSnapshotRestoreSuccessItem = EnableFastSnapshotRestoreSuccessItem'
  { disablingTime ::
      Lude.Maybe
        Lude.DateTime,
    state ::
      Lude.Maybe
        FastSnapshotRestoreStateCode,
    ownerAlias ::
      Lude.Maybe
        Lude.Text,
    disabledTime ::
      Lude.Maybe
        Lude.DateTime,
    enabledTime ::
      Lude.Maybe
        Lude.DateTime,
    optimizingTime ::
      Lude.Maybe
        Lude.DateTime,
    ownerId ::
      Lude.Maybe
        Lude.Text,
    stateTransitionReason ::
      Lude.Maybe
        Lude.Text,
    availabilityZone ::
      Lude.Maybe
        Lude.Text,
    snapshotId ::
      Lude.Maybe
        Lude.Text,
    enablingTime ::
      Lude.Maybe
        Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableFastSnapshotRestoreSuccessItem' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone.
-- * 'disabledTime' - The time at which fast snapshot restores entered the @disabled@ state.
-- * 'disablingTime' - The time at which fast snapshot restores entered the @disabling@ state.
-- * 'enabledTime' - The time at which fast snapshot restores entered the @enabled@ state.
-- * 'enablingTime' - The time at which fast snapshot restores entered the @enabling@ state.
-- * 'optimizingTime' - The time at which fast snapshot restores entered the @optimizing@ state.
-- * 'ownerAlias' - The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
-- * 'ownerId' - The ID of the AWS account that enabled fast snapshot restores on the snapshot.
-- * 'snapshotId' - The ID of the snapshot.
-- * 'state' - The state of fast snapshot restores.
-- * 'stateTransitionReason' - The reason for the state transition. The possible values are as follows:
--
--
--     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .
--
--
--     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
mkEnableFastSnapshotRestoreSuccessItem ::
  EnableFastSnapshotRestoreSuccessItem
mkEnableFastSnapshotRestoreSuccessItem =
  EnableFastSnapshotRestoreSuccessItem'
    { disablingTime =
        Lude.Nothing,
      state = Lude.Nothing,
      ownerAlias = Lude.Nothing,
      disabledTime = Lude.Nothing,
      enabledTime = Lude.Nothing,
      optimizingTime = Lude.Nothing,
      ownerId = Lude.Nothing,
      stateTransitionReason = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      snapshotId = Lude.Nothing,
      enablingTime = Lude.Nothing
    }

-- | The time at which fast snapshot restores entered the @disabling@ state.
--
-- /Note:/ Consider using 'disablingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiDisablingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
efsrsiDisablingTime = Lens.lens (disablingTime :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {disablingTime = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiDisablingTime "Use generic-lens or generic-optics with 'disablingTime' instead." #-}

-- | The state of fast snapshot restores.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiState :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe FastSnapshotRestoreStateCode)
efsrsiState = Lens.lens (state :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe FastSnapshotRestoreStateCode) (\s a -> s {state = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiOwnerAlias :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
efsrsiOwnerAlias = Lens.lens (ownerAlias :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {ownerAlias = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

-- | The time at which fast snapshot restores entered the @disabled@ state.
--
-- /Note:/ Consider using 'disabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiDisabledTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
efsrsiDisabledTime = Lens.lens (disabledTime :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {disabledTime = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiDisabledTime "Use generic-lens or generic-optics with 'disabledTime' instead." #-}

-- | The time at which fast snapshot restores entered the @enabled@ state.
--
-- /Note:/ Consider using 'enabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiEnabledTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
efsrsiEnabledTime = Lens.lens (enabledTime :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {enabledTime = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiEnabledTime "Use generic-lens or generic-optics with 'enabledTime' instead." #-}

-- | The time at which fast snapshot restores entered the @optimizing@ state.
--
-- /Note:/ Consider using 'optimizingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiOptimizingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
efsrsiOptimizingTime = Lens.lens (optimizingTime :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {optimizingTime = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiOptimizingTime "Use generic-lens or generic-optics with 'optimizingTime' instead." #-}

-- | The ID of the AWS account that enabled fast snapshot restores on the snapshot.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiOwnerId :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
efsrsiOwnerId = Lens.lens (ownerId :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

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
efsrsiStateTransitionReason :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
efsrsiStateTransitionReason = Lens.lens (stateTransitionReason :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {stateTransitionReason = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiStateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiAvailabilityZone :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
efsrsiAvailabilityZone = Lens.lens (availabilityZone :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiSnapshotId :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
efsrsiSnapshotId = Lens.lens (snapshotId :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The time at which fast snapshot restores entered the @enabling@ state.
--
-- /Note:/ Consider using 'enablingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrsiEnablingTime :: Lens.Lens' EnableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
efsrsiEnablingTime = Lens.lens (enablingTime :: EnableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {enablingTime = a} :: EnableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED efsrsiEnablingTime "Use generic-lens or generic-optics with 'enablingTime' instead." #-}

instance Lude.FromXML EnableFastSnapshotRestoreSuccessItem where
  parseXML x =
    EnableFastSnapshotRestoreSuccessItem'
      Lude.<$> (x Lude..@? "disablingTime")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "ownerAlias")
      Lude.<*> (x Lude..@? "disabledTime")
      Lude.<*> (x Lude..@? "enabledTime")
      Lude.<*> (x Lude..@? "optimizingTime")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "stateTransitionReason")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "snapshotId")
      Lude.<*> (x Lude..@? "enablingTime")
