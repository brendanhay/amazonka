{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ArchiveGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ArchiveGroupSettings
  ( ArchiveGroupSettings (..),

    -- * Smart constructor
    mkArchiveGroupSettings,

    -- * Lenses
    agsDestination,
    agsRolloverInterval,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.OutputLocationRef as Types
import qualified Network.AWS.Prelude as Core

-- | Archive Group Settings
--
-- /See:/ 'mkArchiveGroupSettings' smart constructor.
data ArchiveGroupSettings = ArchiveGroupSettings'
  { -- | A directory and base filename where archive files should be written.
    destination :: Types.OutputLocationRef,
    -- | Number of seconds to write to archive file before closing and starting a new one.
    rolloverInterval :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArchiveGroupSettings' value with any optional fields omitted.
mkArchiveGroupSettings ::
  -- | 'destination'
  Types.OutputLocationRef ->
  ArchiveGroupSettings
mkArchiveGroupSettings destination =
  ArchiveGroupSettings'
    { destination,
      rolloverInterval = Core.Nothing
    }

-- | A directory and base filename where archive files should be written.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agsDestination :: Lens.Lens' ArchiveGroupSettings Types.OutputLocationRef
agsDestination = Lens.field @"destination"
{-# DEPRECATED agsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Number of seconds to write to archive file before closing and starting a new one.
--
-- /Note:/ Consider using 'rolloverInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agsRolloverInterval :: Lens.Lens' ArchiveGroupSettings (Core.Maybe Core.Natural)
agsRolloverInterval = Lens.field @"rolloverInterval"
{-# DEPRECATED agsRolloverInterval "Use generic-lens or generic-optics with 'rolloverInterval' instead." #-}

instance Core.FromJSON ArchiveGroupSettings where
  toJSON ArchiveGroupSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("destination" Core..= destination),
            ("rolloverInterval" Core..=) Core.<$> rolloverInterval
          ]
      )

instance Core.FromJSON ArchiveGroupSettings where
  parseJSON =
    Core.withObject "ArchiveGroupSettings" Core.$
      \x ->
        ArchiveGroupSettings'
          Core.<$> (x Core..: "destination") Core.<*> (x Core..:? "rolloverInterval")
