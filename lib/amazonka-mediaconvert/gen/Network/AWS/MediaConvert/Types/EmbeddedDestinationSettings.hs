{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
  ( EmbeddedDestinationSettings (..),

    -- * Smart constructor
    mkEmbeddedDestinationSettings,

    -- * Lenses
    edsDestination608ChannelNumber,
    edsDestination708ServiceNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings specific to embedded/ancillary caption outputs, including 608/708 Channel destination number.
--
-- /See:/ 'mkEmbeddedDestinationSettings' smart constructor.
data EmbeddedDestinationSettings = EmbeddedDestinationSettings'
  { -- | Ignore this setting unless your input captions are SCC format and your output captions are embedded in the video stream. Specify a CC number for each captions channel in this output. If you have two channels, choose CC numbers that aren't in the same field. For example, choose 1 and 3. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
    destination608ChannelNumber :: Lude.Maybe Lude.Natural,
    -- | Ignore this setting unless your input captions are SCC format and you want both 608 and 708 captions embedded in your output stream. Optionally, specify the 708 service number for each output captions channel. Choose a different number for each channel. To use this setting, also set Force 608 to 708 upconvert (Convert608To708) to Upconvert (UPCONVERT) in your input captions selector settings. If you choose to upconvert but don't specify a 708 service number, MediaConvert uses the number that you specify for CC channel number (destination608ChannelNumber) for the 708 service number. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
    destination708ServiceNumber :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmbeddedDestinationSettings' with the minimum fields required to make a request.
--
-- * 'destination608ChannelNumber' - Ignore this setting unless your input captions are SCC format and your output captions are embedded in the video stream. Specify a CC number for each captions channel in this output. If you have two channels, choose CC numbers that aren't in the same field. For example, choose 1 and 3. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
-- * 'destination708ServiceNumber' - Ignore this setting unless your input captions are SCC format and you want both 608 and 708 captions embedded in your output stream. Optionally, specify the 708 service number for each output captions channel. Choose a different number for each channel. To use this setting, also set Force 608 to 708 upconvert (Convert608To708) to Upconvert (UPCONVERT) in your input captions selector settings. If you choose to upconvert but don't specify a 708 service number, MediaConvert uses the number that you specify for CC channel number (destination608ChannelNumber) for the 708 service number. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
mkEmbeddedDestinationSettings ::
  EmbeddedDestinationSettings
mkEmbeddedDestinationSettings =
  EmbeddedDestinationSettings'
    { destination608ChannelNumber =
        Lude.Nothing,
      destination708ServiceNumber = Lude.Nothing
    }

-- | Ignore this setting unless your input captions are SCC format and your output captions are embedded in the video stream. Specify a CC number for each captions channel in this output. If you have two channels, choose CC numbers that aren't in the same field. For example, choose 1 and 3. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
--
-- /Note:/ Consider using 'destination608ChannelNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsDestination608ChannelNumber :: Lens.Lens' EmbeddedDestinationSettings (Lude.Maybe Lude.Natural)
edsDestination608ChannelNumber = Lens.lens (destination608ChannelNumber :: EmbeddedDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {destination608ChannelNumber = a} :: EmbeddedDestinationSettings)
{-# DEPRECATED edsDestination608ChannelNumber "Use generic-lens or generic-optics with 'destination608ChannelNumber' instead." #-}

-- | Ignore this setting unless your input captions are SCC format and you want both 608 and 708 captions embedded in your output stream. Optionally, specify the 708 service number for each output captions channel. Choose a different number for each channel. To use this setting, also set Force 608 to 708 upconvert (Convert608To708) to Upconvert (UPCONVERT) in your input captions selector settings. If you choose to upconvert but don't specify a 708 service number, MediaConvert uses the number that you specify for CC channel number (destination608ChannelNumber) for the 708 service number. For more information, see https://docs.aws.amazon.com/console/mediaconvert/dual-scc-to-embedded.
--
-- /Note:/ Consider using 'destination708ServiceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edsDestination708ServiceNumber :: Lens.Lens' EmbeddedDestinationSettings (Lude.Maybe Lude.Natural)
edsDestination708ServiceNumber = Lens.lens (destination708ServiceNumber :: EmbeddedDestinationSettings -> Lude.Maybe Lude.Natural) (\s a -> s {destination708ServiceNumber = a} :: EmbeddedDestinationSettings)
{-# DEPRECATED edsDestination708ServiceNumber "Use generic-lens or generic-optics with 'destination708ServiceNumber' instead." #-}

instance Lude.FromJSON EmbeddedDestinationSettings where
  parseJSON =
    Lude.withObject
      "EmbeddedDestinationSettings"
      ( \x ->
          EmbeddedDestinationSettings'
            Lude.<$> (x Lude..:? "destination608ChannelNumber")
            Lude.<*> (x Lude..:? "destination708ServiceNumber")
      )

instance Lude.ToJSON EmbeddedDestinationSettings where
  toJSON EmbeddedDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("destination608ChannelNumber" Lude..=)
              Lude.<$> destination608ChannelNumber,
            ("destination708ServiceNumber" Lude..=)
              Lude.<$> destination708ServiceNumber
          ]
      )
