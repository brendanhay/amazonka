{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AncillarySourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AncillarySourceSettings
  ( AncillarySourceSettings (..),

    -- * Smart constructor
    mkAncillarySourceSettings,

    -- * Lenses
    assConvert608To708,
    assSourceAncillaryChannelNumber,
    assTerminateCaptions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AncillaryConvert608To708 as Types
import qualified Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for ancillary captions source.
--
-- /See:/ 'mkAncillarySourceSettings' smart constructor.
data AncillarySourceSettings = AncillarySourceSettings'
  { -- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
    convert608To708 :: Core.Maybe Types.AncillaryConvert608To708,
    -- | Specifies the 608 channel number in the ancillary data track from which to extract captions. Unused for passthrough.
    sourceAncillaryChannelNumber :: Core.Maybe Core.Natural,
    -- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
    terminateCaptions :: Core.Maybe Types.AncillaryTerminateCaptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AncillarySourceSettings' value with any optional fields omitted.
mkAncillarySourceSettings ::
  AncillarySourceSettings
mkAncillarySourceSettings =
  AncillarySourceSettings'
    { convert608To708 = Core.Nothing,
      sourceAncillaryChannelNumber = Core.Nothing,
      terminateCaptions = Core.Nothing
    }

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assConvert608To708 :: Lens.Lens' AncillarySourceSettings (Core.Maybe Types.AncillaryConvert608To708)
assConvert608To708 = Lens.field @"convert608To708"
{-# DEPRECATED assConvert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead." #-}

-- | Specifies the 608 channel number in the ancillary data track from which to extract captions. Unused for passthrough.
--
-- /Note:/ Consider using 'sourceAncillaryChannelNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assSourceAncillaryChannelNumber :: Lens.Lens' AncillarySourceSettings (Core.Maybe Core.Natural)
assSourceAncillaryChannelNumber = Lens.field @"sourceAncillaryChannelNumber"
{-# DEPRECATED assSourceAncillaryChannelNumber "Use generic-lens or generic-optics with 'sourceAncillaryChannelNumber' instead." #-}

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
--
-- /Note:/ Consider using 'terminateCaptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assTerminateCaptions :: Lens.Lens' AncillarySourceSettings (Core.Maybe Types.AncillaryTerminateCaptions)
assTerminateCaptions = Lens.field @"terminateCaptions"
{-# DEPRECATED assTerminateCaptions "Use generic-lens or generic-optics with 'terminateCaptions' instead." #-}

instance Core.FromJSON AncillarySourceSettings where
  toJSON AncillarySourceSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("convert608To708" Core..=) Core.<$> convert608To708,
            ("sourceAncillaryChannelNumber" Core..=)
              Core.<$> sourceAncillaryChannelNumber,
            ("terminateCaptions" Core..=) Core.<$> terminateCaptions
          ]
      )

instance Core.FromJSON AncillarySourceSettings where
  parseJSON =
    Core.withObject "AncillarySourceSettings" Core.$
      \x ->
        AncillarySourceSettings'
          Core.<$> (x Core..:? "convert608To708")
          Core.<*> (x Core..:? "sourceAncillaryChannelNumber")
          Core.<*> (x Core..:? "terminateCaptions")
