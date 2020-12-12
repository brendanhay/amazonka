{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreErrorItem
  ( EnableFastSnapshotRestoreErrorItem (..),

    -- * Smart constructor
    mkEnableFastSnapshotRestoreErrorItem,

    -- * Lenses
    efsreiFastSnapshotRestoreStateErrors,
    efsreiSnapshotId,
  )
where

import Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the errors that occurred when enabling fast snapshot restores.
--
-- /See:/ 'mkEnableFastSnapshotRestoreErrorItem' smart constructor.
data EnableFastSnapshotRestoreErrorItem = EnableFastSnapshotRestoreErrorItem'
  { fastSnapshotRestoreStateErrors ::
      Lude.Maybe
        [EnableFastSnapshotRestoreStateErrorItem],
    snapshotId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableFastSnapshotRestoreErrorItem' with the minimum fields required to make a request.
--
-- * 'fastSnapshotRestoreStateErrors' - The errors.
-- * 'snapshotId' - The ID of the snapshot.
mkEnableFastSnapshotRestoreErrorItem ::
  EnableFastSnapshotRestoreErrorItem
mkEnableFastSnapshotRestoreErrorItem =
  EnableFastSnapshotRestoreErrorItem'
    { fastSnapshotRestoreStateErrors =
        Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | The errors.
--
-- /Note:/ Consider using 'fastSnapshotRestoreStateErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsreiFastSnapshotRestoreStateErrors :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Lude.Maybe [EnableFastSnapshotRestoreStateErrorItem])
efsreiFastSnapshotRestoreStateErrors = Lens.lens (fastSnapshotRestoreStateErrors :: EnableFastSnapshotRestoreErrorItem -> Lude.Maybe [EnableFastSnapshotRestoreStateErrorItem]) (\s a -> s {fastSnapshotRestoreStateErrors = a} :: EnableFastSnapshotRestoreErrorItem)
{-# DEPRECATED efsreiFastSnapshotRestoreStateErrors "Use generic-lens or generic-optics with 'fastSnapshotRestoreStateErrors' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efsreiSnapshotId :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Lude.Maybe Lude.Text)
efsreiSnapshotId = Lens.lens (snapshotId :: EnableFastSnapshotRestoreErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: EnableFastSnapshotRestoreErrorItem)
{-# DEPRECATED efsreiSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML EnableFastSnapshotRestoreErrorItem where
  parseXML x =
    EnableFastSnapshotRestoreErrorItem'
      Lude.<$> ( x Lude..@? "fastSnapshotRestoreStateErrorSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "snapshotId")
