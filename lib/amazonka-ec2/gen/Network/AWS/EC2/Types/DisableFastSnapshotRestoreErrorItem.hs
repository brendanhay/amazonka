{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem
  ( DisableFastSnapshotRestoreErrorItem (..),

    -- * Smart constructor
    mkDisableFastSnapshotRestoreErrorItem,

    -- * Lenses
    dfsreiFastSnapshotRestoreStateErrors,
    dfsreiSnapshotId,
  )
where

import Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the errors that occurred when disabling fast snapshot restores.
--
-- /See:/ 'mkDisableFastSnapshotRestoreErrorItem' smart constructor.
data DisableFastSnapshotRestoreErrorItem = DisableFastSnapshotRestoreErrorItem'
  { -- | The errors.
    fastSnapshotRestoreStateErrors :: Lude.Maybe [DisableFastSnapshotRestoreStateErrorItem],
    -- | The ID of the snapshot.
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableFastSnapshotRestoreErrorItem' with the minimum fields required to make a request.
--
-- * 'fastSnapshotRestoreStateErrors' - The errors.
-- * 'snapshotId' - The ID of the snapshot.
mkDisableFastSnapshotRestoreErrorItem ::
  DisableFastSnapshotRestoreErrorItem
mkDisableFastSnapshotRestoreErrorItem =
  DisableFastSnapshotRestoreErrorItem'
    { fastSnapshotRestoreStateErrors =
        Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | The errors.
--
-- /Note:/ Consider using 'fastSnapshotRestoreStateErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsreiFastSnapshotRestoreStateErrors :: Lens.Lens' DisableFastSnapshotRestoreErrorItem (Lude.Maybe [DisableFastSnapshotRestoreStateErrorItem])
dfsreiFastSnapshotRestoreStateErrors = Lens.lens (fastSnapshotRestoreStateErrors :: DisableFastSnapshotRestoreErrorItem -> Lude.Maybe [DisableFastSnapshotRestoreStateErrorItem]) (\s a -> s {fastSnapshotRestoreStateErrors = a} :: DisableFastSnapshotRestoreErrorItem)
{-# DEPRECATED dfsreiFastSnapshotRestoreStateErrors "Use generic-lens or generic-optics with 'fastSnapshotRestoreStateErrors' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsreiSnapshotId :: Lens.Lens' DisableFastSnapshotRestoreErrorItem (Lude.Maybe Lude.Text)
dfsreiSnapshotId = Lens.lens (snapshotId :: DisableFastSnapshotRestoreErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: DisableFastSnapshotRestoreErrorItem)
{-# DEPRECATED dfsreiSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML DisableFastSnapshotRestoreErrorItem where
  parseXML x =
    DisableFastSnapshotRestoreErrorItem'
      Lude.<$> ( x Lude..@? "fastSnapshotRestoreStateErrorSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "snapshotId")
