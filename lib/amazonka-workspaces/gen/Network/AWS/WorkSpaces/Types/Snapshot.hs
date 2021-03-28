{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Snapshot
  ( Snapshot (..)
  -- * Smart constructor
  , mkSnapshot
  -- * Lenses
  , sSnapshotTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a snapshot.
--
-- /See:/ 'mkSnapshot' smart constructor.
newtype Snapshot = Snapshot'
  { snapshotTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the snapshot was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'Snapshot' value with any optional fields omitted.
mkSnapshot
    :: Snapshot
mkSnapshot = Snapshot'{snapshotTime = Core.Nothing}

-- | The time when the snapshot was created.
--
-- /Note:/ Consider using 'snapshotTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSnapshotTime :: Lens.Lens' Snapshot (Core.Maybe Core.NominalDiffTime)
sSnapshotTime = Lens.field @"snapshotTime"
{-# INLINEABLE sSnapshotTime #-}
{-# DEPRECATED snapshotTime "Use generic-lens or generic-optics with 'snapshotTime' instead"  #-}

instance Core.FromJSON Snapshot where
        parseJSON
          = Core.withObject "Snapshot" Core.$
              \ x -> Snapshot' Core.<$> (x Core..:? "SnapshotTime")
