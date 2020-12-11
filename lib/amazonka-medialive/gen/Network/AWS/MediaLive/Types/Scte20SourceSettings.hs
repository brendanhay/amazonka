-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte20SourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20SourceSettings
  ( Scte20SourceSettings (..),

    -- * Smart constructor
    mkScte20SourceSettings,

    -- * Lenses
    sssConvert608To708,
    sssSource608ChannelNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte20Convert608To708
import qualified Network.AWS.Prelude as Lude

-- | Scte20 Source Settings
--
-- /See:/ 'mkScte20SourceSettings' smart constructor.
data Scte20SourceSettings = Scte20SourceSettings'
  { convert608To708 ::
      Lude.Maybe Scte20Convert608To708,
    source608ChannelNumber :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte20SourceSettings' with the minimum fields required to make a request.
--
-- * 'convert608To708' - If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
-- * 'source608ChannelNumber' - Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
mkScte20SourceSettings ::
  Scte20SourceSettings
mkScte20SourceSettings =
  Scte20SourceSettings'
    { convert608To708 = Lude.Nothing,
      source608ChannelNumber = Lude.Nothing
    }

-- | If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssConvert608To708 :: Lens.Lens' Scte20SourceSettings (Lude.Maybe Scte20Convert608To708)
sssConvert608To708 = Lens.lens (convert608To708 :: Scte20SourceSettings -> Lude.Maybe Scte20Convert608To708) (\s a -> s {convert608To708 = a} :: Scte20SourceSettings)
{-# DEPRECATED sssConvert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead." #-}

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
--
-- /Note:/ Consider using 'source608ChannelNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSource608ChannelNumber :: Lens.Lens' Scte20SourceSettings (Lude.Maybe Lude.Natural)
sssSource608ChannelNumber = Lens.lens (source608ChannelNumber :: Scte20SourceSettings -> Lude.Maybe Lude.Natural) (\s a -> s {source608ChannelNumber = a} :: Scte20SourceSettings)
{-# DEPRECATED sssSource608ChannelNumber "Use generic-lens or generic-optics with 'source608ChannelNumber' instead." #-}

instance Lude.FromJSON Scte20SourceSettings where
  parseJSON =
    Lude.withObject
      "Scte20SourceSettings"
      ( \x ->
          Scte20SourceSettings'
            Lude.<$> (x Lude..:? "convert608To708")
            Lude.<*> (x Lude..:? "source608ChannelNumber")
      )

instance Lude.ToJSON Scte20SourceSettings where
  toJSON Scte20SourceSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("convert608To708" Lude..=) Lude.<$> convert608To708,
            ("source608ChannelNumber" Lude..=)
              Lude.<$> source608ChannelNumber
          ]
      )
