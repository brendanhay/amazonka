{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
  ( EmbeddedSourceSettings (..),

    -- * Smart constructor
    mkEmbeddedSourceSettings,

    -- * Lenses
    essConvert608To708,
    essTerminateCaptions,
    essSource608TrackNumber,
    essSource608ChannelNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.EmbeddedConvert608To708
import Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions
import qualified Network.AWS.Prelude as Lude

-- | Settings for embedded captions Source
--
-- /See:/ 'mkEmbeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { -- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
    convert608To708 :: Lude.Maybe EmbeddedConvert608To708,
    -- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
    terminateCaptions :: Lude.Maybe EmbeddedTerminateCaptions,
    -- | Specifies the video track index used for extracting captions. The system only supports one input video track, so this should always be set to '1'.
    source608TrackNumber :: Lude.Maybe Lude.Natural,
    -- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
    source608ChannelNumber :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmbeddedSourceSettings' with the minimum fields required to make a request.
--
-- * 'convert608To708' - Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
-- * 'terminateCaptions' - By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
-- * 'source608TrackNumber' - Specifies the video track index used for extracting captions. The system only supports one input video track, so this should always be set to '1'.
-- * 'source608ChannelNumber' - Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
mkEmbeddedSourceSettings ::
  EmbeddedSourceSettings
mkEmbeddedSourceSettings =
  EmbeddedSourceSettings'
    { convert608To708 = Lude.Nothing,
      terminateCaptions = Lude.Nothing,
      source608TrackNumber = Lude.Nothing,
      source608ChannelNumber = Lude.Nothing
    }

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essConvert608To708 :: Lens.Lens' EmbeddedSourceSettings (Lude.Maybe EmbeddedConvert608To708)
essConvert608To708 = Lens.lens (convert608To708 :: EmbeddedSourceSettings -> Lude.Maybe EmbeddedConvert608To708) (\s a -> s {convert608To708 = a} :: EmbeddedSourceSettings)
{-# DEPRECATED essConvert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead." #-}

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
--
-- /Note:/ Consider using 'terminateCaptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essTerminateCaptions :: Lens.Lens' EmbeddedSourceSettings (Lude.Maybe EmbeddedTerminateCaptions)
essTerminateCaptions = Lens.lens (terminateCaptions :: EmbeddedSourceSettings -> Lude.Maybe EmbeddedTerminateCaptions) (\s a -> s {terminateCaptions = a} :: EmbeddedSourceSettings)
{-# DEPRECATED essTerminateCaptions "Use generic-lens or generic-optics with 'terminateCaptions' instead." #-}

-- | Specifies the video track index used for extracting captions. The system only supports one input video track, so this should always be set to '1'.
--
-- /Note:/ Consider using 'source608TrackNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essSource608TrackNumber :: Lens.Lens' EmbeddedSourceSettings (Lude.Maybe Lude.Natural)
essSource608TrackNumber = Lens.lens (source608TrackNumber :: EmbeddedSourceSettings -> Lude.Maybe Lude.Natural) (\s a -> s {source608TrackNumber = a} :: EmbeddedSourceSettings)
{-# DEPRECATED essSource608TrackNumber "Use generic-lens or generic-optics with 'source608TrackNumber' instead." #-}

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
--
-- /Note:/ Consider using 'source608ChannelNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essSource608ChannelNumber :: Lens.Lens' EmbeddedSourceSettings (Lude.Maybe Lude.Natural)
essSource608ChannelNumber = Lens.lens (source608ChannelNumber :: EmbeddedSourceSettings -> Lude.Maybe Lude.Natural) (\s a -> s {source608ChannelNumber = a} :: EmbeddedSourceSettings)
{-# DEPRECATED essSource608ChannelNumber "Use generic-lens or generic-optics with 'source608ChannelNumber' instead." #-}

instance Lude.FromJSON EmbeddedSourceSettings where
  parseJSON =
    Lude.withObject
      "EmbeddedSourceSettings"
      ( \x ->
          EmbeddedSourceSettings'
            Lude.<$> (x Lude..:? "convert608To708")
            Lude.<*> (x Lude..:? "terminateCaptions")
            Lude.<*> (x Lude..:? "source608TrackNumber")
            Lude.<*> (x Lude..:? "source608ChannelNumber")
      )

instance Lude.ToJSON EmbeddedSourceSettings where
  toJSON EmbeddedSourceSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("convert608To708" Lude..=) Lude.<$> convert608To708,
            ("terminateCaptions" Lude..=) Lude.<$> terminateCaptions,
            ("source608TrackNumber" Lude..=) Lude.<$> source608TrackNumber,
            ("source608ChannelNumber" Lude..=)
              Lude.<$> source608ChannelNumber
          ]
      )
