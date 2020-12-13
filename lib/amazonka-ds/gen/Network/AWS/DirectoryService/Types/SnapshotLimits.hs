{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SnapshotLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SnapshotLimits
  ( SnapshotLimits (..),

    -- * Smart constructor
    mkSnapshotLimits,

    -- * Lenses
    slManualSnapshotsLimitReached,
    slManualSnapshotsCurrentCount,
    slManualSnapshotsLimit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains manual snapshot limit information for a directory.
--
-- /See:/ 'mkSnapshotLimits' smart constructor.
data SnapshotLimits = SnapshotLimits'
  { -- | Indicates if the manual snapshot limit has been reached.
    manualSnapshotsLimitReached :: Lude.Maybe Lude.Bool,
    -- | The current number of manual snapshots of the directory.
    manualSnapshotsCurrentCount :: Lude.Maybe Lude.Natural,
    -- | The maximum number of manual snapshots allowed.
    manualSnapshotsLimit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotLimits' with the minimum fields required to make a request.
--
-- * 'manualSnapshotsLimitReached' - Indicates if the manual snapshot limit has been reached.
-- * 'manualSnapshotsCurrentCount' - The current number of manual snapshots of the directory.
-- * 'manualSnapshotsLimit' - The maximum number of manual snapshots allowed.
mkSnapshotLimits ::
  SnapshotLimits
mkSnapshotLimits =
  SnapshotLimits'
    { manualSnapshotsLimitReached = Lude.Nothing,
      manualSnapshotsCurrentCount = Lude.Nothing,
      manualSnapshotsLimit = Lude.Nothing
    }

-- | Indicates if the manual snapshot limit has been reached.
--
-- /Note:/ Consider using 'manualSnapshotsLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slManualSnapshotsLimitReached :: Lens.Lens' SnapshotLimits (Lude.Maybe Lude.Bool)
slManualSnapshotsLimitReached = Lens.lens (manualSnapshotsLimitReached :: SnapshotLimits -> Lude.Maybe Lude.Bool) (\s a -> s {manualSnapshotsLimitReached = a} :: SnapshotLimits)
{-# DEPRECATED slManualSnapshotsLimitReached "Use generic-lens or generic-optics with 'manualSnapshotsLimitReached' instead." #-}

-- | The current number of manual snapshots of the directory.
--
-- /Note:/ Consider using 'manualSnapshotsCurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slManualSnapshotsCurrentCount :: Lens.Lens' SnapshotLimits (Lude.Maybe Lude.Natural)
slManualSnapshotsCurrentCount = Lens.lens (manualSnapshotsCurrentCount :: SnapshotLimits -> Lude.Maybe Lude.Natural) (\s a -> s {manualSnapshotsCurrentCount = a} :: SnapshotLimits)
{-# DEPRECATED slManualSnapshotsCurrentCount "Use generic-lens or generic-optics with 'manualSnapshotsCurrentCount' instead." #-}

-- | The maximum number of manual snapshots allowed.
--
-- /Note:/ Consider using 'manualSnapshotsLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slManualSnapshotsLimit :: Lens.Lens' SnapshotLimits (Lude.Maybe Lude.Natural)
slManualSnapshotsLimit = Lens.lens (manualSnapshotsLimit :: SnapshotLimits -> Lude.Maybe Lude.Natural) (\s a -> s {manualSnapshotsLimit = a} :: SnapshotLimits)
{-# DEPRECATED slManualSnapshotsLimit "Use generic-lens or generic-optics with 'manualSnapshotsLimit' instead." #-}

instance Lude.FromJSON SnapshotLimits where
  parseJSON =
    Lude.withObject
      "SnapshotLimits"
      ( \x ->
          SnapshotLimits'
            Lude.<$> (x Lude..:? "ManualSnapshotsLimitReached")
            Lude.<*> (x Lude..:? "ManualSnapshotsCurrentCount")
            Lude.<*> (x Lude..:? "ManualSnapshotsLimit")
      )
