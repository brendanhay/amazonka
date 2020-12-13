{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3Settings
  ( Ac3Settings (..),

    -- * Smart constructor
    mkAc3Settings,

    -- * Lenses
    aLfeFilter,
    aMetadataControl,
    aBitstreamMode,
    aCodingMode,
    aSampleRate,
    aDynamicRangeCompressionProfile,
    aBitrate,
    aDialnorm,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Ac3BitstreamMode
import Network.AWS.MediaConvert.Types.Ac3CodingMode
import Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
import Network.AWS.MediaConvert.Types.Ac3LfeFilter
import Network.AWS.MediaConvert.Types.Ac3MetadataControl
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
--
-- /See:/ 'mkAc3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
    lfeFilter :: Lude.Maybe Ac3LfeFilter,
    -- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
    metadataControl :: Lude.Maybe Ac3MetadataControl,
    -- | Specify the bitstream mode for the AC-3 stream that the encoder emits. For more information about the AC3 bitstream mode, see ATSC A/52-2012 (Annex E).
    bitstreamMode :: Lude.Maybe Ac3BitstreamMode,
    -- | Dolby Digital coding mode. Determines number of channels.
    codingMode :: Lude.Maybe Ac3CodingMode,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Lude.Maybe Lude.Natural,
    -- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
    dynamicRangeCompressionProfile :: Lude.Maybe Ac3DynamicRangeCompressionProfile,
    -- | Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
    bitrate :: Lude.Maybe Lude.Natural,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
    dialnorm :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Ac3Settings' with the minimum fields required to make a request.
--
-- * 'lfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
-- * 'metadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
-- * 'bitstreamMode' - Specify the bitstream mode for the AC-3 stream that the encoder emits. For more information about the AC3 bitstream mode, see ATSC A/52-2012 (Annex E).
-- * 'codingMode' - Dolby Digital coding mode. Determines number of channels.
-- * 'sampleRate' - This value is always 48000. It represents the sample rate in Hz.
-- * 'dynamicRangeCompressionProfile' - If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
-- * 'bitrate' - Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
-- * 'dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
mkAc3Settings ::
  Ac3Settings
mkAc3Settings =
  Ac3Settings'
    { lfeFilter = Lude.Nothing,
      metadataControl = Lude.Nothing,
      bitstreamMode = Lude.Nothing,
      codingMode = Lude.Nothing,
      sampleRate = Lude.Nothing,
      dynamicRangeCompressionProfile = Lude.Nothing,
      bitrate = Lude.Nothing,
      dialnorm = Lude.Nothing
    }

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
--
-- /Note:/ Consider using 'lfeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLfeFilter :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3LfeFilter)
aLfeFilter = Lens.lens (lfeFilter :: Ac3Settings -> Lude.Maybe Ac3LfeFilter) (\s a -> s {lfeFilter = a} :: Ac3Settings)
{-# DEPRECATED aLfeFilter "Use generic-lens or generic-optics with 'lfeFilter' instead." #-}

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- /Note:/ Consider using 'metadataControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMetadataControl :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3MetadataControl)
aMetadataControl = Lens.lens (metadataControl :: Ac3Settings -> Lude.Maybe Ac3MetadataControl) (\s a -> s {metadataControl = a} :: Ac3Settings)
{-# DEPRECATED aMetadataControl "Use generic-lens or generic-optics with 'metadataControl' instead." #-}

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits. For more information about the AC3 bitstream mode, see ATSC A/52-2012 (Annex E).
--
-- /Note:/ Consider using 'bitstreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBitstreamMode :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3BitstreamMode)
aBitstreamMode = Lens.lens (bitstreamMode :: Ac3Settings -> Lude.Maybe Ac3BitstreamMode) (\s a -> s {bitstreamMode = a} :: Ac3Settings)
{-# DEPRECATED aBitstreamMode "Use generic-lens or generic-optics with 'bitstreamMode' instead." #-}

-- | Dolby Digital coding mode. Determines number of channels.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCodingMode :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3CodingMode)
aCodingMode = Lens.lens (codingMode :: Ac3Settings -> Lude.Maybe Ac3CodingMode) (\s a -> s {codingMode = a} :: Ac3Settings)
{-# DEPRECATED aCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | This value is always 48000. It represents the sample rate in Hz.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSampleRate :: Lens.Lens' Ac3Settings (Lude.Maybe Lude.Natural)
aSampleRate = Lens.lens (sampleRate :: Ac3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: Ac3Settings)
{-# DEPRECATED aSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
--
-- /Note:/ Consider using 'dynamicRangeCompressionProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDynamicRangeCompressionProfile :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3DynamicRangeCompressionProfile)
aDynamicRangeCompressionProfile = Lens.lens (dynamicRangeCompressionProfile :: Ac3Settings -> Lude.Maybe Ac3DynamicRangeCompressionProfile) (\s a -> s {dynamicRangeCompressionProfile = a} :: Ac3Settings)
{-# DEPRECATED aDynamicRangeCompressionProfile "Use generic-lens or generic-optics with 'dynamicRangeCompressionProfile' instead." #-}

-- | Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aBitrate :: Lens.Lens' Ac3Settings (Lude.Maybe Lude.Natural)
aBitrate = Lens.lens (bitrate :: Ac3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: Ac3Settings)
{-# DEPRECATED aBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
--
-- /Note:/ Consider using 'dialnorm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDialnorm :: Lens.Lens' Ac3Settings (Lude.Maybe Lude.Natural)
aDialnorm = Lens.lens (dialnorm :: Ac3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {dialnorm = a} :: Ac3Settings)
{-# DEPRECATED aDialnorm "Use generic-lens or generic-optics with 'dialnorm' instead." #-}

instance Lude.FromJSON Ac3Settings where
  parseJSON =
    Lude.withObject
      "Ac3Settings"
      ( \x ->
          Ac3Settings'
            Lude.<$> (x Lude..:? "lfeFilter")
            Lude.<*> (x Lude..:? "metadataControl")
            Lude.<*> (x Lude..:? "bitstreamMode")
            Lude.<*> (x Lude..:? "codingMode")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "dynamicRangeCompressionProfile")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "dialnorm")
      )

instance Lude.ToJSON Ac3Settings where
  toJSON Ac3Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("lfeFilter" Lude..=) Lude.<$> lfeFilter,
            ("metadataControl" Lude..=) Lude.<$> metadataControl,
            ("bitstreamMode" Lude..=) Lude.<$> bitstreamMode,
            ("codingMode" Lude..=) Lude.<$> codingMode,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("dynamicRangeCompressionProfile" Lude..=)
              Lude.<$> dynamicRangeCompressionProfile,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("dialnorm" Lude..=) Lude.<$> dialnorm
          ]
      )
