{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.EmbeddedSourceSettings
  ( EmbeddedSourceSettings (..)
  -- * Smart constructor
  , mkEmbeddedSourceSettings
  -- * Lenses
  , essConvert608To708
  , essScte20Detection
  , essSource608ChannelNumber
  , essSource608TrackNumber
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.EmbeddedConvert608To708 as Types
import qualified Network.AWS.MediaLive.Types.EmbeddedScte20Detection as Types
import qualified Network.AWS.Prelude as Core

-- | Embedded Source Settings
--
-- /See:/ 'mkEmbeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { convert608To708 :: Core.Maybe Types.EmbeddedConvert608To708
    -- ^ If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
  , scte20Detection :: Core.Maybe Types.EmbeddedScte20Detection
    -- ^ Set to "auto" to handle streams with intermittent and/or non-aligned SCTE-20 and Embedded captions.
  , source608ChannelNumber :: Core.Maybe Core.Natural
    -- ^ Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
  , source608TrackNumber :: Core.Maybe Core.Natural
    -- ^ This field is unused and deprecated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmbeddedSourceSettings' value with any optional fields omitted.
mkEmbeddedSourceSettings
    :: EmbeddedSourceSettings
mkEmbeddedSourceSettings
  = EmbeddedSourceSettings'{convert608To708 = Core.Nothing,
                            scte20Detection = Core.Nothing,
                            source608ChannelNumber = Core.Nothing,
                            source608TrackNumber = Core.Nothing}

-- | If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essConvert608To708 :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Types.EmbeddedConvert608To708)
essConvert608To708 = Lens.field @"convert608To708"
{-# INLINEABLE essConvert608To708 #-}
{-# DEPRECATED convert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead"  #-}

-- | Set to "auto" to handle streams with intermittent and/or non-aligned SCTE-20 and Embedded captions.
--
-- /Note:/ Consider using 'scte20Detection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essScte20Detection :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Types.EmbeddedScte20Detection)
essScte20Detection = Lens.field @"scte20Detection"
{-# INLINEABLE essScte20Detection #-}
{-# DEPRECATED scte20Detection "Use generic-lens or generic-optics with 'scte20Detection' instead"  #-}

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
--
-- /Note:/ Consider using 'source608ChannelNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essSource608ChannelNumber :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Core.Natural)
essSource608ChannelNumber = Lens.field @"source608ChannelNumber"
{-# INLINEABLE essSource608ChannelNumber #-}
{-# DEPRECATED source608ChannelNumber "Use generic-lens or generic-optics with 'source608ChannelNumber' instead"  #-}

-- | This field is unused and deprecated.
--
-- /Note:/ Consider using 'source608TrackNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essSource608TrackNumber :: Lens.Lens' EmbeddedSourceSettings (Core.Maybe Core.Natural)
essSource608TrackNumber = Lens.field @"source608TrackNumber"
{-# INLINEABLE essSource608TrackNumber #-}
{-# DEPRECATED source608TrackNumber "Use generic-lens or generic-optics with 'source608TrackNumber' instead"  #-}

instance Core.FromJSON EmbeddedSourceSettings where
        toJSON EmbeddedSourceSettings{..}
          = Core.object
              (Core.catMaybes
                 [("convert608To708" Core..=) Core.<$> convert608To708,
                  ("scte20Detection" Core..=) Core.<$> scte20Detection,
                  ("source608ChannelNumber" Core..=) Core.<$> source608ChannelNumber,
                  ("source608TrackNumber" Core..=) Core.<$> source608TrackNumber])

instance Core.FromJSON EmbeddedSourceSettings where
        parseJSON
          = Core.withObject "EmbeddedSourceSettings" Core.$
              \ x ->
                EmbeddedSourceSettings' Core.<$>
                  (x Core..:? "convert608To708") Core.<*>
                    x Core..:? "scte20Detection"
                    Core.<*> x Core..:? "source608ChannelNumber"
                    Core.<*> x Core..:? "source608TrackNumber"
