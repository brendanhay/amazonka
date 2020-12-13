{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
  ( DisableFastSnapshotRestoreStateErrorItem (..),

    -- * Smart constructor
    mkDisableFastSnapshotRestoreStateErrorItem,

    -- * Lenses
    dfsrseiError,
    dfsrseiAvailabilityZone,
  )
where

import Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an error that occurred when disabling fast snapshot restores.
--
-- /See:/ 'mkDisableFastSnapshotRestoreStateErrorItem' smart constructor.
data DisableFastSnapshotRestoreStateErrorItem = DisableFastSnapshotRestoreStateErrorItem'
  { -- | The error.
    error :: Lude.Maybe DisableFastSnapshotRestoreStateError,
    -- | The Availability Zone.
    availabilityZone :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableFastSnapshotRestoreStateErrorItem' with the minimum fields required to make a request.
--
-- * 'error' - The error.
-- * 'availabilityZone' - The Availability Zone.
mkDisableFastSnapshotRestoreStateErrorItem ::
  DisableFastSnapshotRestoreStateErrorItem
mkDisableFastSnapshotRestoreStateErrorItem =
  DisableFastSnapshotRestoreStateErrorItem'
    { error = Lude.Nothing,
      availabilityZone = Lude.Nothing
    }

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrseiError :: Lens.Lens' DisableFastSnapshotRestoreStateErrorItem (Lude.Maybe DisableFastSnapshotRestoreStateError)
dfsrseiError = Lens.lens (error :: DisableFastSnapshotRestoreStateErrorItem -> Lude.Maybe DisableFastSnapshotRestoreStateError) (\s a -> s {error = a} :: DisableFastSnapshotRestoreStateErrorItem)
{-# DEPRECATED dfsrseiError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrseiAvailabilityZone :: Lens.Lens' DisableFastSnapshotRestoreStateErrorItem (Lude.Maybe Lude.Text)
dfsrseiAvailabilityZone = Lens.lens (availabilityZone :: DisableFastSnapshotRestoreStateErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: DisableFastSnapshotRestoreStateErrorItem)
{-# DEPRECATED dfsrseiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

instance Lude.FromXML DisableFastSnapshotRestoreStateErrorItem where
  parseXML x =
    DisableFastSnapshotRestoreStateErrorItem'
      Lude.<$> (x Lude..@? "error") Lude.<*> (x Lude..@? "availabilityZone")
