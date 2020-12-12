{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreSuccessItem
  ( DisableFastSnapshotRestoreSuccessItem (..),

    -- * Smart constructor
    mkDisableFastSnapshotRestoreSuccessItem,

    -- * Lenses
    dDisablingTime,
    dState,
    dOwnerAlias,
    dDisabledTime,
    dEnabledTime,
    dOptimizingTime,
    dOwnerId,
    dStateTransitionReason,
    dAvailabilityZone,
    dSnapshotId,
    dEnablingTime,
  )
where

import Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes fast snapshot restores that were successfully disabled.
--
-- /See:/ 'mkDisableFastSnapshotRestoreSuccessItem' smart constructor.
data DisableFastSnapshotRestoreSuccessItem = DisableFastSnapshotRestoreSuccessItem'
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

-- | Creates a value of 'DisableFastSnapshotRestoreSuccessItem' with the minimum fields required to make a request.
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
-- * 'state' - The state of fast snapshot restores for the snapshot.
-- * 'stateTransitionReason' - The reason for the state transition. The possible values are as follows:
--
--
--     * @Client.UserInitiated@ - The state successfully transitioned to @enabling@ or @disabling@ .
--
--
--     * @Client.UserInitiated - Lifecycle state transition@ - The state successfully transitioned to @optimizing@ , @enabled@ , or @disabled@ .
mkDisableFastSnapshotRestoreSuccessItem ::
  DisableFastSnapshotRestoreSuccessItem
mkDisableFastSnapshotRestoreSuccessItem =
  DisableFastSnapshotRestoreSuccessItem'
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
dDisablingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
dDisablingTime = Lens.lens (disablingTime :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {disablingTime = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dDisablingTime "Use generic-lens or generic-optics with 'disablingTime' instead." #-}

-- | The state of fast snapshot restores for the snapshot.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dState :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe FastSnapshotRestoreStateCode)
dState = Lens.lens (state :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe FastSnapshotRestoreStateCode) (\s a -> s {state = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The AWS owner alias that enabled fast snapshot restores on the snapshot. This is intended for future use.
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOwnerAlias :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dOwnerAlias = Lens.lens (ownerAlias :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {ownerAlias = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

-- | The time at which fast snapshot restores entered the @disabled@ state.
--
-- /Note:/ Consider using 'disabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDisabledTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
dDisabledTime = Lens.lens (disabledTime :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {disabledTime = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dDisabledTime "Use generic-lens or generic-optics with 'disabledTime' instead." #-}

-- | The time at which fast snapshot restores entered the @enabled@ state.
--
-- /Note:/ Consider using 'enabledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEnabledTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
dEnabledTime = Lens.lens (enabledTime :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {enabledTime = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dEnabledTime "Use generic-lens or generic-optics with 'enabledTime' instead." #-}

-- | The time at which fast snapshot restores entered the @optimizing@ state.
--
-- /Note:/ Consider using 'optimizingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOptimizingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
dOptimizingTime = Lens.lens (optimizingTime :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {optimizingTime = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dOptimizingTime "Use generic-lens or generic-optics with 'optimizingTime' instead." #-}

-- | The ID of the AWS account that enabled fast snapshot restores on the snapshot.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOwnerId :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dOwnerId = Lens.lens (ownerId :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

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
dStateTransitionReason :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dStateTransitionReason = Lens.lens (stateTransitionReason :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {stateTransitionReason = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dStateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAvailabilityZone :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dAvailabilityZone = Lens.lens (availabilityZone :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotId :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.Text)
dSnapshotId = Lens.lens (snapshotId :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The time at which fast snapshot restores entered the @enabling@ state.
--
-- /Note:/ Consider using 'enablingTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEnablingTime :: Lens.Lens' DisableFastSnapshotRestoreSuccessItem (Lude.Maybe Lude.DateTime)
dEnablingTime = Lens.lens (enablingTime :: DisableFastSnapshotRestoreSuccessItem -> Lude.Maybe Lude.DateTime) (\s a -> s {enablingTime = a} :: DisableFastSnapshotRestoreSuccessItem)
{-# DEPRECATED dEnablingTime "Use generic-lens or generic-optics with 'enablingTime' instead." #-}

instance Lude.FromXML DisableFastSnapshotRestoreSuccessItem where
  parseXML x =
    DisableFastSnapshotRestoreSuccessItem'
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
