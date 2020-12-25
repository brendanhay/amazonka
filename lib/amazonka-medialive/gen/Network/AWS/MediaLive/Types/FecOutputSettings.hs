{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FecOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FecOutputSettings
  ( FecOutputSettings (..),

    -- * Smart constructor
    mkFecOutputSettings,

    -- * Lenses
    fosColumnDepth,
    fosIncludeFec,
    fosRowLength,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.FecOutputIncludeFec as Types
import qualified Network.AWS.Prelude as Core

-- | Fec Output Settings
--
-- /See:/ 'mkFecOutputSettings' smart constructor.
data FecOutputSettings = FecOutputSettings'
  { -- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.  The number of transport stream packets per column error correction packet. Must be between 4 and 20, inclusive.
    columnDepth :: Core.Maybe Core.Natural,
    -- | Enables column only or column and row based FEC
    includeFec :: Core.Maybe Types.FecOutputIncludeFec,
    -- | Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.  Must be between 1 and 20, inclusive. If only Column FEC is used, then larger values increase robustness.  If Row FEC is used, then this is the number of transport stream packets per row error correction packet, and the value must be between 4 and 20, inclusive, if includeFec is columnAndRow. If includeFec is column, this value must be 1 to 20, inclusive.
    rowLength :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FecOutputSettings' value with any optional fields omitted.
mkFecOutputSettings ::
  FecOutputSettings
mkFecOutputSettings =
  FecOutputSettings'
    { columnDepth = Core.Nothing,
      includeFec = Core.Nothing,
      rowLength = Core.Nothing
    }

-- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.  The number of transport stream packets per column error correction packet. Must be between 4 and 20, inclusive.
--
-- /Note:/ Consider using 'columnDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fosColumnDepth :: Lens.Lens' FecOutputSettings (Core.Maybe Core.Natural)
fosColumnDepth = Lens.field @"columnDepth"
{-# DEPRECATED fosColumnDepth "Use generic-lens or generic-optics with 'columnDepth' instead." #-}

-- | Enables column only or column and row based FEC
--
-- /Note:/ Consider using 'includeFec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fosIncludeFec :: Lens.Lens' FecOutputSettings (Core.Maybe Types.FecOutputIncludeFec)
fosIncludeFec = Lens.field @"includeFec"
{-# DEPRECATED fosIncludeFec "Use generic-lens or generic-optics with 'includeFec' instead." #-}

-- | Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.  Must be between 1 and 20, inclusive. If only Column FEC is used, then larger values increase robustness.  If Row FEC is used, then this is the number of transport stream packets per row error correction packet, and the value must be between 4 and 20, inclusive, if includeFec is columnAndRow. If includeFec is column, this value must be 1 to 20, inclusive.
--
-- /Note:/ Consider using 'rowLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fosRowLength :: Lens.Lens' FecOutputSettings (Core.Maybe Core.Natural)
fosRowLength = Lens.field @"rowLength"
{-# DEPRECATED fosRowLength "Use generic-lens or generic-optics with 'rowLength' instead." #-}

instance Core.FromJSON FecOutputSettings where
  toJSON FecOutputSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("columnDepth" Core..=) Core.<$> columnDepth,
            ("includeFec" Core..=) Core.<$> includeFec,
            ("rowLength" Core..=) Core.<$> rowLength
          ]
      )

instance Core.FromJSON FecOutputSettings where
  parseJSON =
    Core.withObject "FecOutputSettings" Core.$
      \x ->
        FecOutputSettings'
          Core.<$> (x Core..:? "columnDepth")
          Core.<*> (x Core..:? "includeFec")
          Core.<*> (x Core..:? "rowLength")
