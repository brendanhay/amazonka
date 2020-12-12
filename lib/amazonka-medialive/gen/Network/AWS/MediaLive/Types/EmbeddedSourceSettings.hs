{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedSourceSettings
  ( EmbeddedSourceSettings (..),

    -- * Smart constructor
    mkEmbeddedSourceSettings,

    -- * Lenses
    essConvert608To708,
    essScte20Detection,
    essSource608TrackNumber,
    essSource608ChannelNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.EmbeddedConvert608To708
import Network.AWS.MediaLive.Types.EmbeddedScte20Detection
import qualified Network.AWS.Prelude as Lude

-- | Embedded Source Settings
--
-- /See:/ 'mkEmbeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { convert608To708 ::
      Lude.Maybe EmbeddedConvert608To708,
    scte20Detection ::
      Lude.Maybe EmbeddedScte20Detection,
    source608TrackNumber ::
      Lude.Maybe Lude.Natural,
    source608ChannelNumber ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmbeddedSourceSettings' with the minimum fields required to make a request.
--
-- * 'convert608To708' - If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
-- * 'scte20Detection' - Set to "auto" to handle streams with intermittent and/or non-aligned SCTE-20 and Embedded captions.
-- * 'source608ChannelNumber' - Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
-- * 'source608TrackNumber' - This field is unused and deprecated.
mkEmbeddedSourceSettings ::
  EmbeddedSourceSettings
mkEmbeddedSourceSettings =
  EmbeddedSourceSettings'
    { convert608To708 = Lude.Nothing,
      scte20Detection = Lude.Nothing,
      source608TrackNumber = Lude.Nothing,
      source608ChannelNumber = Lude.Nothing
    }

-- | If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essConvert608To708 :: Lens.Lens' EmbeddedSourceSettings (Lude.Maybe EmbeddedConvert608To708)
essConvert608To708 = Lens.lens (convert608To708 :: EmbeddedSourceSettings -> Lude.Maybe EmbeddedConvert608To708) (\s a -> s {convert608To708 = a} :: EmbeddedSourceSettings)
{-# DEPRECATED essConvert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead." #-}

-- | Set to "auto" to handle streams with intermittent and/or non-aligned SCTE-20 and Embedded captions.
--
-- /Note:/ Consider using 'scte20Detection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
essScte20Detection :: Lens.Lens' EmbeddedSourceSettings (Lude.Maybe EmbeddedScte20Detection)
essScte20Detection = Lens.lens (scte20Detection :: EmbeddedSourceSettings -> Lude.Maybe EmbeddedScte20Detection) (\s a -> s {scte20Detection = a} :: EmbeddedSourceSettings)
{-# DEPRECATED essScte20Detection "Use generic-lens or generic-optics with 'scte20Detection' instead." #-}

-- | This field is unused and deprecated.
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
            Lude.<*> (x Lude..:? "scte20Detection")
            Lude.<*> (x Lude..:? "source608TrackNumber")
            Lude.<*> (x Lude..:? "source608ChannelNumber")
      )

instance Lude.ToJSON EmbeddedSourceSettings where
  toJSON EmbeddedSourceSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("convert608To708" Lude..=) Lude.<$> convert608To708,
            ("scte20Detection" Lude..=) Lude.<$> scte20Detection,
            ("source608TrackNumber" Lude..=) Lude.<$> source608TrackNumber,
            ("source608ChannelNumber" Lude..=)
              Lude.<$> source608ChannelNumber
          ]
      )
