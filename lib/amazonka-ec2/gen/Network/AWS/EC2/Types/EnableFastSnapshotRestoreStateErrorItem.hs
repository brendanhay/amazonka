{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
  ( EnableFastSnapshotRestoreStateErrorItem (..),

    -- * Smart constructor
    mkEnableFastSnapshotRestoreStateErrorItem,

    -- * Lenses
    efsrseiError,
    efsrseiAvailabilityZone,
  )
where

import Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an error that occurred when enabling fast snapshot restores.
--
-- /See:/ 'mkEnableFastSnapshotRestoreStateErrorItem' smart constructor.
data EnableFastSnapshotRestoreStateErrorItem = EnableFastSnapshotRestoreStateErrorItem'
  { error ::
      Lude.Maybe
        EnableFastSnapshotRestoreStateError,
    availabilityZone ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableFastSnapshotRestoreStateErrorItem' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone.
-- * 'error' - The error.
mkEnableFastSnapshotRestoreStateErrorItem ::
  EnableFastSnapshotRestoreStateErrorItem
mkEnableFastSnapshotRestoreStateErrorItem =
  EnableFastSnapshotRestoreStateErrorItem'
    { error = Lude.Nothing,
      availabilityZone = Lude.Nothing
    }

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrseiError :: Lens.Lens' EnableFastSnapshotRestoreStateErrorItem (Lude.Maybe EnableFastSnapshotRestoreStateError)
efsrseiError = Lens.lens (error :: EnableFastSnapshotRestoreStateErrorItem -> Lude.Maybe EnableFastSnapshotRestoreStateError) (\s a -> s {error = a} :: EnableFastSnapshotRestoreStateErrorItem)
{-# DEPRECATED efsrseiError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsrseiAvailabilityZone :: Lens.Lens' EnableFastSnapshotRestoreStateErrorItem (Lude.Maybe Lude.Text)
efsrseiAvailabilityZone = Lens.lens (availabilityZone :: EnableFastSnapshotRestoreStateErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: EnableFastSnapshotRestoreStateErrorItem)
{-# DEPRECATED efsrseiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

instance Lude.FromXML EnableFastSnapshotRestoreStateErrorItem where
  parseXML x =
    EnableFastSnapshotRestoreStateErrorItem'
      Lude.<$> (x Lude..@? "error") Lude.<*> (x Lude..@? "availabilityZone")
