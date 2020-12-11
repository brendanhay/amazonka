-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DescribeFastSnapshotRestoreSuccessItem
  ( DescribeFastSnapshotRestoreSuccessItem (..),

    -- * Smart constructor
    mkDescribeFastSnapshotRestoreSuccessItem,

    -- * Lenses
    dfsrsiDisablingTime,
    dfsrsiState,
    dfsrsiOwnerAlias,
    dfsrsiDisabledTime,
    dfsrsiEnabledTime,
    dfsrsiOptimizingTime,
    dfsrsiOwnerId,
    dfsrsiStateTransitionReason,
    dfsrsiAvailabilityZone,
    dfsrsiSnapshotId,
    dfsrsiEnablingTime,
  )
where

import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes fast snapshot restores for a snapshot.
--
-- /See:/ 'mkDescribeFastSnapshotRestoreSuccessItem' smart constructor.
data DescribeFastSnapshotRestoreSuccessItem = DescribeFastSnapshotRestoreSuccessItem'
  { disablingTime ::
      Lude.Maybe
        Lude.ISO8601,
    state ::
      Lude.Maybe
        FastSnapshotRestoreStateCode,
    ownerAlias ::
      Lude.Maybe
        Lude.Text,
    disabledTime ::
      Lude.Maybe
        Lude.ISO8601,
    enabledTime ::
      Lude.Maybe
        Lude.ISO8601,
    optimizingTime ::
      Lude.Maybe
        Lude.ISO8601,
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
        Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFastSnapshotRestoreSuccessItem' with the minimum fields required to make a request.
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
mkDescribeFastSnapshotRestoreSuccessItem ::
  DescribeFastSnapshotRestoreSuccessItem
mkDescribeFastSnapshotRestoreSuccessItem =
  DescribeFastSnapshotRestoreSuccessItem'
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
dfsrsiDisablingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.ISO8601)
dfsrsiDisablingTime = Lens.lens (disablingTime :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.ISO8601) (\s a -> s {disablingTime = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiDisablingTime "Use generic-lens or generic-optics with 'disablingTime' instead." #-}

-- | The state of fast snapshot restores.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsiState :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe FastSnapshotRestoreStateCode)
dfsrsiState = Lens.lens (state :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe FastSnapshotRestoreStateCode) (\s a -> s {state = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsiOwnerAlias :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dfsrsiOwnerAlias = Lens.lens (ownerAlias :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {ownerAlias = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

-- | The time at which fast snapshot restores entered the @disabled@ state.
--
-- /Note:/ Consider using 'disabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsiDisabledTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.ISO8601)
dfsrsiDisabledTime = Lens.lens (disabledTime :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.ISO8601) (\s a -> s {disabledTime = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiDisabledTime "Use generic-lens or generic-optics with 'disabledTime' instead." #-}

-- | The time at which fast snapshot restores entered the @enabled@ state.
--
-- /Note:/ Consider using 'enabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsiEnabledTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.ISO8601)
dfsrsiEnabledTime = Lens.lens (enabledTime :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.ISO8601) (\s a -> s {enabledTime = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiEnabledTime "Use generic-lens or generic-optics with 'enabledTime' instead." #-}

-- | The time at which fast snapshot restores entered the @optimizing@ state.
--
-- /Note:/ Consider using 'optimizingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsiOptimizingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.ISO8601)
dfsrsiOptimizingTime = Lens.lens (optimizingTime :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.ISO8601) (\s a -> s {optimizingTime = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiOptimizingTime "Use generic-lens or generic-optics with 'optimizingTime' instead." #-}

-- | The ID of the AWS account that enabled fast snapshot restores on the snapshot.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsiOwnerId :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dfsrsiOwnerId = Lens.lens (ownerId :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

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
dfsrsiStateTransitionReason :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dfsrsiStateTransitionReason = Lens.lens (stateTransitionReason :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {stateTransitionReason = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiStateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsiAvailabilityZone :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dfsrsiAvailabilityZone = Lens.lens (availabilityZone :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsiSnapshotId :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dfsrsiSnapshotId = Lens.lens (snapshotId :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The time at which fast snapshot restores entered the @enabling@ state.
--
-- /Note:/ Consider using 'enablingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsiEnablingTime :: Lens.Lens' DescribeFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.ISO8601)
dfsrsiEnablingTime = Lens.lens (enablingTime :: DescribeFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.ISO8601) (\s a -> s {enablingTime = a} :: DescribeFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dfsrsiEnablingTime "Use generic-lens or generic-optics with 'enablingTime' instead." #-}

instance Lude.FromXML DescribeFastSnapshotRestoreSuccessItem where
  parseXML x =
    DescribeFastSnapshotRestoreSuccessItem'
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
