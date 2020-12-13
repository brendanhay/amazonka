{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3Settings
  ( Ac3Settings (..),

    -- * Smart constructor
    mkAc3Settings,

    -- * Lenses
    asLfeFilter,
    asMetadataControl,
    asBitstreamMode,
    asCodingMode,
    asBitrate,
    asDialnorm,
    asDrcProfile,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Ac3BitstreamMode
import Network.AWS.MediaLive.Types.Ac3CodingMode
import Network.AWS.MediaLive.Types.Ac3DrcProfile
import Network.AWS.MediaLive.Types.Ac3LfeFilter
import Network.AWS.MediaLive.Types.Ac3MetadataControl
import qualified Network.AWS.Prelude as Lude

-- | Ac3 Settings
--
-- /See:/ 'mkAc3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { -- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid in codingMode32Lfe mode.
    lfeFilter :: Lude.Maybe Ac3LfeFilter,
    -- | When set to "followInput", encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
    metadataControl :: Lude.Maybe Ac3MetadataControl,
    -- | Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
    bitstreamMode :: Lude.Maybe Ac3BitstreamMode,
    -- | Dolby Digital coding mode. Determines number of channels.
    codingMode :: Lude.Maybe Ac3CodingMode,
    -- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
    bitrate :: Lude.Maybe Lude.Double,
    -- | Sets the dialnorm for the output. If excluded and input audio is Dolby Digital, dialnorm will be passed through.
    dialnorm :: Lude.Maybe Lude.Natural,
    -- | If set to filmStandard, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
    drcProfile :: Lude.Maybe Ac3DrcProfile
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Ac3Settings' with the minimum fields required to make a request.
--
-- * 'lfeFilter' - When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid in codingMode32Lfe mode.
-- * 'metadataControl' - When set to "followInput", encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
-- * 'bitstreamMode' - Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
-- * 'codingMode' - Dolby Digital coding mode. Determines number of channels.
-- * 'bitrate' - Average bitrate in bits/second. Valid bitrates depend on the coding mode.
-- * 'dialnorm' - Sets the dialnorm for the output. If excluded and input audio is Dolby Digital, dialnorm will be passed through.
-- * 'drcProfile' - If set to filmStandard, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
mkAc3Settings ::
  Ac3Settings
mkAc3Settings =
  Ac3Settings'
    { lfeFilter = Lude.Nothing,
      metadataControl = Lude.Nothing,
      bitstreamMode = Lude.Nothing,
      codingMode = Lude.Nothing,
      bitrate = Lude.Nothing,
      dialnorm = Lude.Nothing,
      drcProfile = Lude.Nothing
    }

-- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid in codingMode32Lfe mode.
--
-- /Note:/ Consider using 'lfeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLfeFilter :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3LfeFilter)
asLfeFilter = Lens.lens (lfeFilter :: Ac3Settings -> Lude.Maybe Ac3LfeFilter) (\s a -> s {lfeFilter = a} :: Ac3Settings)
{-# DEPRECATED asLfeFilter "Use generic-lens or generic-optics with 'lfeFilter' instead." #-}

-- | When set to "followInput", encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- /Note:/ Consider using 'metadataControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMetadataControl :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3MetadataControl)
asMetadataControl = Lens.lens (metadataControl :: Ac3Settings -> Lude.Maybe Ac3MetadataControl) (\s a -> s {metadataControl = a} :: Ac3Settings)
{-# DEPRECATED asMetadataControl "Use generic-lens or generic-optics with 'metadataControl' instead." #-}

-- | Specifies the bitstream mode (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
--
-- /Note:/ Consider using 'bitstreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asBitstreamMode :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3BitstreamMode)
asBitstreamMode = Lens.lens (bitstreamMode :: Ac3Settings -> Lude.Maybe Ac3BitstreamMode) (\s a -> s {bitstreamMode = a} :: Ac3Settings)
{-# DEPRECATED asBitstreamMode "Use generic-lens or generic-optics with 'bitstreamMode' instead." #-}

-- | Dolby Digital coding mode. Determines number of channels.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCodingMode :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3CodingMode)
asCodingMode = Lens.lens (codingMode :: Ac3Settings -> Lude.Maybe Ac3CodingMode) (\s a -> s {codingMode = a} :: Ac3Settings)
{-# DEPRECATED asCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asBitrate :: Lens.Lens' Ac3Settings (Lude.Maybe Lude.Double)
asBitrate = Lens.lens (bitrate :: Ac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {bitrate = a} :: Ac3Settings)
{-# DEPRECATED asBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | Sets the dialnorm for the output. If excluded and input audio is Dolby Digital, dialnorm will be passed through.
--
-- /Note:/ Consider using 'dialnorm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDialnorm :: Lens.Lens' Ac3Settings (Lude.Maybe Lude.Natural)
asDialnorm = Lens.lens (dialnorm :: Ac3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {dialnorm = a} :: Ac3Settings)
{-# DEPRECATED asDialnorm "Use generic-lens or generic-optics with 'dialnorm' instead." #-}

-- | If set to filmStandard, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
--
-- /Note:/ Consider using 'drcProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDrcProfile :: Lens.Lens' Ac3Settings (Lude.Maybe Ac3DrcProfile)
asDrcProfile = Lens.lens (drcProfile :: Ac3Settings -> Lude.Maybe Ac3DrcProfile) (\s a -> s {drcProfile = a} :: Ac3Settings)
{-# DEPRECATED asDrcProfile "Use generic-lens or generic-optics with 'drcProfile' instead." #-}

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
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "dialnorm")
            Lude.<*> (x Lude..:? "drcProfile")
      )

instance Lude.ToJSON Ac3Settings where
  toJSON Ac3Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("lfeFilter" Lude..=) Lude.<$> lfeFilter,
            ("metadataControl" Lude..=) Lude.<$> metadataControl,
            ("bitstreamMode" Lude..=) Lude.<$> bitstreamMode,
            ("codingMode" Lude..=) Lude.<$> codingMode,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("dialnorm" Lude..=) Lude.<$> dialnorm,
            ("drcProfile" Lude..=) Lude.<$> drcProfile
          ]
      )
