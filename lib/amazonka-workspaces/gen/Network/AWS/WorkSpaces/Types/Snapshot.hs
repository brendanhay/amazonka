{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Snapshot
  ( Snapshot (..),

    -- * Smart constructor
    mkSnapshot,

    -- * Lenses
    sSnapshotTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
newtype Snapshot = Snapshot'
  { -- | The time when the snapshot was created.
    snapshotTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- * 'snapshotTime' - The time when the snapshot was created.
mkSnapshot ::
  Snapshot
mkSnapshot = Snapshot' {snapshotTime = Lude.Nothing}

-- | The time when the snapshot was created.
--
-- /Note:/ Consider using 'snapshotTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotTime :: Lens.Lens' Snapshot (Lude.Maybe Lude.Timestamp)
sSnapshotTime = Lens.lens (snapshotTime :: Snapshot -> Lude.Maybe Lude.Timestamp) (\s a -> s {snapshotTime = a} :: Snapshot)
{-# DEPRECATED sSnapshotTime "Use generic-lens or generic-optics with 'snapshotTime' instead." #-}

instance Lude.FromJSON Snapshot where
  parseJSON =
    Lude.withObject
      "Snapshot"
      (\x -> Snapshot' Lude.<$> (x Lude..:? "SnapshotTime"))
