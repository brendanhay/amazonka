{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeBurnin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeBurnin
  ( TimecodeBurnin (..),

    -- * Smart constructor
    mkTimecodeBurnin,

    -- * Lenses
    tbFontSize,
    tbPosition,
    tbPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.TimecodeBurninPosition as Types
import qualified Network.AWS.Prelude as Core

-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
--
-- /See:/ 'mkTimecodeBurnin' smart constructor.
data TimecodeBurnin = TimecodeBurnin'
  { -- | Use Font Size (FontSize) to set the font size of any burned-in timecode. Valid values are 10, 16, 32, 48.
    fontSize :: Core.Maybe Core.Natural,
    -- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
    position :: Core.Maybe Types.TimecodeBurninPosition,
    -- | Use Prefix (Prefix) to place ASCII characters before any burned-in timecode. For example, a prefix of "EZ-" will result in the timecode "EZ-00:00:00:00". Provide either the characters themselves or the ASCII code equivalents. The supported range of characters is 0x20 through 0x7e. This includes letters, numbers, and all special characters represented on a standard English keyboard.
    prefix :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimecodeBurnin' value with any optional fields omitted.
mkTimecodeBurnin ::
  TimecodeBurnin
mkTimecodeBurnin =
  TimecodeBurnin'
    { fontSize = Core.Nothing,
      position = Core.Nothing,
      prefix = Core.Nothing
    }

-- | Use Font Size (FontSize) to set the font size of any burned-in timecode. Valid values are 10, 16, 32, 48.
--
-- /Note:/ Consider using 'fontSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbFontSize :: Lens.Lens' TimecodeBurnin (Core.Maybe Core.Natural)
tbFontSize = Lens.field @"fontSize"
{-# DEPRECATED tbFontSize "Use generic-lens or generic-optics with 'fontSize' instead." #-}

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbPosition :: Lens.Lens' TimecodeBurnin (Core.Maybe Types.TimecodeBurninPosition)
tbPosition = Lens.field @"position"
{-# DEPRECATED tbPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | Use Prefix (Prefix) to place ASCII characters before any burned-in timecode. For example, a prefix of "EZ-" will result in the timecode "EZ-00:00:00:00". Provide either the characters themselves or the ASCII code equivalents. The supported range of characters is 0x20 through 0x7e. This includes letters, numbers, and all special characters represented on a standard English keyboard.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbPrefix :: Lens.Lens' TimecodeBurnin (Core.Maybe Core.Text)
tbPrefix = Lens.field @"prefix"
{-# DEPRECATED tbPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

instance Core.FromJSON TimecodeBurnin where
  toJSON TimecodeBurnin {..} =
    Core.object
      ( Core.catMaybes
          [ ("fontSize" Core..=) Core.<$> fontSize,
            ("position" Core..=) Core.<$> position,
            ("prefix" Core..=) Core.<$> prefix
          ]
      )

instance Core.FromJSON TimecodeBurnin where
  parseJSON =
    Core.withObject "TimecodeBurnin" Core.$
      \x ->
        TimecodeBurnin'
          Core.<$> (x Core..:? "fontSize")
          Core.<*> (x Core..:? "position")
          Core.<*> (x Core..:? "prefix")
