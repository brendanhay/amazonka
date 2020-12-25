{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailSettings
  ( AvailSettings (..),

    -- * Smart constructor
    mkAvailSettings,

    -- * Lenses
    asScte35SpliceInsert,
    asScte35TimeSignalApos,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Scte35SpliceInsert as Types
import qualified Network.AWS.MediaLive.Types.Scte35TimeSignalApos as Types
import qualified Network.AWS.Prelude as Core

-- | Avail Settings
--
-- /See:/ 'mkAvailSettings' smart constructor.
data AvailSettings = AvailSettings'
  { scte35SpliceInsert :: Core.Maybe Types.Scte35SpliceInsert,
    scte35TimeSignalApos :: Core.Maybe Types.Scte35TimeSignalApos
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AvailSettings' value with any optional fields omitted.
mkAvailSettings ::
  AvailSettings
mkAvailSettings =
  AvailSettings'
    { scte35SpliceInsert = Core.Nothing,
      scte35TimeSignalApos = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte35SpliceInsert' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asScte35SpliceInsert :: Lens.Lens' AvailSettings (Core.Maybe Types.Scte35SpliceInsert)
asScte35SpliceInsert = Lens.field @"scte35SpliceInsert"
{-# DEPRECATED asScte35SpliceInsert "Use generic-lens or generic-optics with 'scte35SpliceInsert' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte35TimeSignalApos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asScte35TimeSignalApos :: Lens.Lens' AvailSettings (Core.Maybe Types.Scte35TimeSignalApos)
asScte35TimeSignalApos = Lens.field @"scte35TimeSignalApos"
{-# DEPRECATED asScte35TimeSignalApos "Use generic-lens or generic-optics with 'scte35TimeSignalApos' instead." #-}

instance Core.FromJSON AvailSettings where
  toJSON AvailSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("scte35SpliceInsert" Core..=) Core.<$> scte35SpliceInsert,
            ("scte35TimeSignalApos" Core..=) Core.<$> scte35TimeSignalApos
          ]
      )

instance Core.FromJSON AvailSettings where
  parseJSON =
    Core.withObject "AvailSettings" Core.$
      \x ->
        AvailSettings'
          Core.<$> (x Core..:? "scte35SpliceInsert")
          Core.<*> (x Core..:? "scte35TimeSignalApos")
