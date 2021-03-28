{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
  ( EmbeddedSourceSettings (..)
  -- * Smart constructor
  , mkEmbeddedSourceSettings
  -- * Lenses
  , essConvert608To708
  , essSource608ChannelNumber
  , essSource608TrackNumber
  , essTerminateCaptions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.EmbeddedConvert608To708 as Types
import qualified Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for embedded captions Source
--
-- /See:/ 'mkEmbeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { convert608To708 :: Core.Maybe Types.EmbeddedConvert608To708
    -- ^ Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
  , source608ChannelNumber :: Core.Maybe Core.Natural
    -- ^ Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
  , source608TrackNumber :: Core.Maybe Core.Natural
    -- ^ Specifies the video track index used for extracting captions. The system only supports one input video track, so this should always be set to '1'.
  , terminateCaptions :: Core.Maybe Types.EmbeddedTerminateCaptions
    -- ^ By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmbeddedSourceSettings' value with any optional fields omitted.
mkEmbeddedSourceSettings
    :: EmbeddedSourceSettings
mkEmbeddedSourceSettings
  = EmbeddedSourceSettings'{convert608To708 = Core.Nothing,
                            source608ChannelNumber = Core.Nothing,
                            source608TrackNumber = Core.Nothing,
                            terminateCaptions = Core.Nothing}

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essConvert608To708 :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Types.EmbeddedConvert608To708)
essConvert608To708 = Lens.field @"convert608To708"
{-# INLINEABLE essConvert608To708 #-}
{-# DEPRECATED convert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead"  #-}

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
--
-- /Note:/ Consider using 'source608ChannelNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essSource608ChannelNumber :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Core.Natural)
essSource608ChannelNumber = Lens.field @"source608ChannelNumber"
{-# INLINEABLE essSource608ChannelNumber #-}
{-# DEPRECATED source608ChannelNumber "Use generic-lens or generic-optics with 'source608ChannelNumber' instead"  #-}

-- | Specifies the video track index used for extracting captions. The system only supports one input video track, so this should always be set to '1'.
--
-- /Note:/ Consider using 'source608TrackNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essSource608TrackNumber :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Core.Natural)
essSource608TrackNumber = Lens.field @"source608TrackNumber"
{-# INLINEABLE essSource608TrackNumber #-}
{-# DEPRECATED source608TrackNumber "Use generic-lens or generic-optics with 'source608TrackNumber' instead"  #-}

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
--
-- /Note:/ Consider using 'terminateCaptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essTerminateCaptions :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Types.EmbeddedTerminateCaptions)
essTerminateCaptions = Lens.field @"terminateCaptions"
{-# INLINEABLE essTerminateCaptions #-}
{-# DEPRECATED terminateCaptions "Use generic-lens or generic-optics with 'terminateCaptions' instead"  #-}

instance Core.FromJSON EmbeddedSourceSettings where
        toJSON EmbeddedSourceSettings{..}
          = Core.object
              (Core.catMaybes
                 [("convert608To708" Core..=) Core.<$> convert608To708,
                  ("source608ChannelNumber" Core..=) Core.<$> source608ChannelNumber,
                  ("source608TrackNumber" Core..=) Core.<$> source608TrackNumber,
                  ("terminateCaptions" Core..=) Core.<$> terminateCaptions])

instance Core.FromJSON EmbeddedSourceSettings where
        parseJSON
          = Core.withObject "EmbeddedSourceSettings" Core.$
              \ x ->
                EmbeddedSourceSettings' Core.<$>
                  (x Core..:? "convert608To708") Core.<*>
                    x Core..:? "source608ChannelNumber"
                    Core.<*> x Core..:? "source608TrackNumber"
                    Core.<*> x Core..:? "terminateCaptions"
