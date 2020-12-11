-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3Settings
  ( Eac3Settings (..),

    -- * Smart constructor
    mkEac3Settings,

    -- * Lenses
    esStereoDownmix,
    esLoRoCenterMixLevel,
    esLtRtCenterMixLevel,
    esLfeFilter,
    esLtRtSurroundMixLevel,
    esMetadataControl,
    esLoRoSurroundMixLevel,
    esSurroundMode,
    esAttenuationControl,
    esPassthroughControl,
    esBitstreamMode,
    esLfeControl,
    esCodingMode,
    esDrcLine,
    esDrcRf,
    esDcFilter,
    esBitrate,
    esPhaseControl,
    esSurroundExMode,
    esDialnorm,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Eac3AttenuationControl
import Network.AWS.MediaLive.Types.Eac3BitstreamMode
import Network.AWS.MediaLive.Types.Eac3CodingMode
import Network.AWS.MediaLive.Types.Eac3DcFilter
import Network.AWS.MediaLive.Types.Eac3DrcLine
import Network.AWS.MediaLive.Types.Eac3DrcRf
import Network.AWS.MediaLive.Types.Eac3LfeControl
import Network.AWS.MediaLive.Types.Eac3LfeFilter
import Network.AWS.MediaLive.Types.Eac3MetadataControl
import Network.AWS.MediaLive.Types.Eac3PassthroughControl
import Network.AWS.MediaLive.Types.Eac3PhaseControl
import Network.AWS.MediaLive.Types.Eac3StereoDownmix
import Network.AWS.MediaLive.Types.Eac3SurroundExMode
import Network.AWS.MediaLive.Types.Eac3SurroundMode
import qualified Network.AWS.Prelude as Lude

-- | Eac3 Settings
--
-- /See:/ 'mkEac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'
  { stereoDownmix ::
      Lude.Maybe Eac3StereoDownmix,
    loRoCenterMixLevel :: Lude.Maybe Lude.Double,
    ltRtCenterMixLevel :: Lude.Maybe Lude.Double,
    lfeFilter :: Lude.Maybe Eac3LfeFilter,
    ltRtSurroundMixLevel :: Lude.Maybe Lude.Double,
    metadataControl :: Lude.Maybe Eac3MetadataControl,
    loRoSurroundMixLevel :: Lude.Maybe Lude.Double,
    surroundMode :: Lude.Maybe Eac3SurroundMode,
    attenuationControl :: Lude.Maybe Eac3AttenuationControl,
    passthroughControl :: Lude.Maybe Eac3PassthroughControl,
    bitstreamMode :: Lude.Maybe Eac3BitstreamMode,
    lfeControl :: Lude.Maybe Eac3LfeControl,
    codingMode :: Lude.Maybe Eac3CodingMode,
    drcLine :: Lude.Maybe Eac3DrcLine,
    drcRf :: Lude.Maybe Eac3DrcRf,
    dcFilter :: Lude.Maybe Eac3DcFilter,
    bitrate :: Lude.Maybe Lude.Double,
    phaseControl :: Lude.Maybe Eac3PhaseControl,
    surroundExMode :: Lude.Maybe Eac3SurroundExMode,
    dialnorm :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Eac3Settings' with the minimum fields required to make a request.
--
-- * 'attenuationControl' - When set to attenuate3Db, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
-- * 'bitrate' - Average bitrate in bits/second. Valid bitrates depend on the coding mode.
-- * 'bitstreamMode' - Specifies the bitstream mode (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.
-- * 'codingMode' - Dolby Digital Plus coding mode. Determines number of channels.
-- * 'dcFilter' - When set to enabled, activates a DC highpass filter for all input channels.
-- * 'dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
-- * 'drcLine' - Sets the Dolby dynamic range compression profile.
-- * 'drcRf' - Sets the profile for heavy Dolby dynamic range compression, ensures that the instantaneous signal peaks do not exceed specified levels.
-- * 'lfeControl' - When encoding 3/2 audio, setting to lfe enables the LFE channel
-- * 'lfeFilter' - When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with codingMode32 coding mode.
-- * 'loRoCenterMixLevel' - Left only/Right only center mix level. Only used for 3/2 coding mode.
-- * 'loRoSurroundMixLevel' - Left only/Right only surround mix level. Only used for 3/2 coding mode.
-- * 'ltRtCenterMixLevel' - Left total/Right total center mix level. Only used for 3/2 coding mode.
-- * 'ltRtSurroundMixLevel' - Left total/Right total surround mix level. Only used for 3/2 coding mode.
-- * 'metadataControl' - When set to followInput, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
-- * 'passthroughControl' - When set to whenPossible, input DD+ audio will be passed through if it is present on the input. This detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
-- * 'phaseControl' - When set to shift90Degrees, applies a 90-degree phase shift to the surround channels. Only used for 3/2 coding mode.
-- * 'stereoDownmix' - Stereo downmix preference. Only used for 3/2 coding mode.
-- * 'surroundExMode' - When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
-- * 'surroundMode' - When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
mkEac3Settings ::
  Eac3Settings
mkEac3Settings =
  Eac3Settings'
    { stereoDownmix = Lude.Nothing,
      loRoCenterMixLevel = Lude.Nothing,
      ltRtCenterMixLevel = Lude.Nothing,
      lfeFilter = Lude.Nothing,
      ltRtSurroundMixLevel = Lude.Nothing,
      metadataControl = Lude.Nothing,
      loRoSurroundMixLevel = Lude.Nothing,
      surroundMode = Lude.Nothing,
      attenuationControl = Lude.Nothing,
      passthroughControl = Lude.Nothing,
      bitstreamMode = Lude.Nothing,
      lfeControl = Lude.Nothing,
      codingMode = Lude.Nothing,
      drcLine = Lude.Nothing,
      drcRf = Lude.Nothing,
      dcFilter = Lude.Nothing,
      bitrate = Lude.Nothing,
      phaseControl = Lude.Nothing,
      surroundExMode = Lude.Nothing,
      dialnorm = Lude.Nothing
    }

-- | Stereo downmix preference. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'stereoDownmix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStereoDownmix :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3StereoDownmix)
esStereoDownmix = Lens.lens (stereoDownmix :: Eac3Settings -> Lude.Maybe Eac3StereoDownmix) (\s a -> s {stereoDownmix = a} :: Eac3Settings)
{-# DEPRECATED esStereoDownmix "Use generic-lens or generic-optics with 'stereoDownmix' instead." #-}

-- | Left only/Right only center mix level. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'loRoCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLoRoCenterMixLevel :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Double)
esLoRoCenterMixLevel = Lens.lens (loRoCenterMixLevel :: Eac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {loRoCenterMixLevel = a} :: Eac3Settings)
{-# DEPRECATED esLoRoCenterMixLevel "Use generic-lens or generic-optics with 'loRoCenterMixLevel' instead." #-}

-- | Left total/Right total center mix level. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'ltRtCenterMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLtRtCenterMixLevel :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Double)
esLtRtCenterMixLevel = Lens.lens (ltRtCenterMixLevel :: Eac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {ltRtCenterMixLevel = a} :: Eac3Settings)
{-# DEPRECATED esLtRtCenterMixLevel "Use generic-lens or generic-optics with 'ltRtCenterMixLevel' instead." #-}

-- | When set to enabled, applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with codingMode32 coding mode.
--
-- /Note:/ Consider using 'lfeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLfeFilter :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3LfeFilter)
esLfeFilter = Lens.lens (lfeFilter :: Eac3Settings -> Lude.Maybe Eac3LfeFilter) (\s a -> s {lfeFilter = a} :: Eac3Settings)
{-# DEPRECATED esLfeFilter "Use generic-lens or generic-optics with 'lfeFilter' instead." #-}

-- | Left total/Right total surround mix level. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'ltRtSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLtRtSurroundMixLevel :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Double)
esLtRtSurroundMixLevel = Lens.lens (ltRtSurroundMixLevel :: Eac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {ltRtSurroundMixLevel = a} :: Eac3Settings)
{-# DEPRECATED esLtRtSurroundMixLevel "Use generic-lens or generic-optics with 'ltRtSurroundMixLevel' instead." #-}

-- | When set to followInput, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- /Note:/ Consider using 'metadataControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esMetadataControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3MetadataControl)
esMetadataControl = Lens.lens (metadataControl :: Eac3Settings -> Lude.Maybe Eac3MetadataControl) (\s a -> s {metadataControl = a} :: Eac3Settings)
{-# DEPRECATED esMetadataControl "Use generic-lens or generic-optics with 'metadataControl' instead." #-}

-- | Left only/Right only surround mix level. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'loRoSurroundMixLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLoRoSurroundMixLevel :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Double)
esLoRoSurroundMixLevel = Lens.lens (loRoSurroundMixLevel :: Eac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {loRoSurroundMixLevel = a} :: Eac3Settings)
{-# DEPRECATED esLoRoSurroundMixLevel "Use generic-lens or generic-optics with 'loRoSurroundMixLevel' instead." #-}

-- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
--
-- /Note:/ Consider using 'surroundMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSurroundMode :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3SurroundMode)
esSurroundMode = Lens.lens (surroundMode :: Eac3Settings -> Lude.Maybe Eac3SurroundMode) (\s a -> s {surroundMode = a} :: Eac3Settings)
{-# DEPRECATED esSurroundMode "Use generic-lens or generic-optics with 'surroundMode' instead." #-}

-- | When set to attenuate3Db, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'attenuationControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esAttenuationControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3AttenuationControl)
esAttenuationControl = Lens.lens (attenuationControl :: Eac3Settings -> Lude.Maybe Eac3AttenuationControl) (\s a -> s {attenuationControl = a} :: Eac3Settings)
{-# DEPRECATED esAttenuationControl "Use generic-lens or generic-optics with 'attenuationControl' instead." #-}

-- | When set to whenPossible, input DD+ audio will be passed through if it is present on the input. This detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
--
-- /Note:/ Consider using 'passthroughControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPassthroughControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3PassthroughControl)
esPassthroughControl = Lens.lens (passthroughControl :: Eac3Settings -> Lude.Maybe Eac3PassthroughControl) (\s a -> s {passthroughControl = a} :: Eac3Settings)
{-# DEPRECATED esPassthroughControl "Use generic-lens or generic-optics with 'passthroughControl' instead." #-}

-- | Specifies the bitstream mode (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.
--
-- /Note:/ Consider using 'bitstreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esBitstreamMode :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3BitstreamMode)
esBitstreamMode = Lens.lens (bitstreamMode :: Eac3Settings -> Lude.Maybe Eac3BitstreamMode) (\s a -> s {bitstreamMode = a} :: Eac3Settings)
{-# DEPRECATED esBitstreamMode "Use generic-lens or generic-optics with 'bitstreamMode' instead." #-}

-- | When encoding 3/2 audio, setting to lfe enables the LFE channel
--
-- /Note:/ Consider using 'lfeControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esLfeControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3LfeControl)
esLfeControl = Lens.lens (lfeControl :: Eac3Settings -> Lude.Maybe Eac3LfeControl) (\s a -> s {lfeControl = a} :: Eac3Settings)
{-# DEPRECATED esLfeControl "Use generic-lens or generic-optics with 'lfeControl' instead." #-}

-- | Dolby Digital Plus coding mode. Determines number of channels.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCodingMode :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3CodingMode)
esCodingMode = Lens.lens (codingMode :: Eac3Settings -> Lude.Maybe Eac3CodingMode) (\s a -> s {codingMode = a} :: Eac3Settings)
{-# DEPRECATED esCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | Sets the Dolby dynamic range compression profile.
--
-- /Note:/ Consider using 'drcLine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDrcLine :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3DrcLine)
esDrcLine = Lens.lens (drcLine :: Eac3Settings -> Lude.Maybe Eac3DrcLine) (\s a -> s {drcLine = a} :: Eac3Settings)
{-# DEPRECATED esDrcLine "Use generic-lens or generic-optics with 'drcLine' instead." #-}

-- | Sets the profile for heavy Dolby dynamic range compression, ensures that the instantaneous signal peaks do not exceed specified levels.
--
-- /Note:/ Consider using 'drcRf' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDrcRf :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3DrcRf)
esDrcRf = Lens.lens (drcRf :: Eac3Settings -> Lude.Maybe Eac3DrcRf) (\s a -> s {drcRf = a} :: Eac3Settings)
{-# DEPRECATED esDrcRf "Use generic-lens or generic-optics with 'drcRf' instead." #-}

-- | When set to enabled, activates a DC highpass filter for all input channels.
--
-- /Note:/ Consider using 'dcFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDcFilter :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3DcFilter)
esDcFilter = Lens.lens (dcFilter :: Eac3Settings -> Lude.Maybe Eac3DcFilter) (\s a -> s {dcFilter = a} :: Eac3Settings)
{-# DEPRECATED esDcFilter "Use generic-lens or generic-optics with 'dcFilter' instead." #-}

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esBitrate :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Double)
esBitrate = Lens.lens (bitrate :: Eac3Settings -> Lude.Maybe Lude.Double) (\s a -> s {bitrate = a} :: Eac3Settings)
{-# DEPRECATED esBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | When set to shift90Degrees, applies a 90-degree phase shift to the surround channels. Only used for 3/2 coding mode.
--
-- /Note:/ Consider using 'phaseControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esPhaseControl :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3PhaseControl)
esPhaseControl = Lens.lens (phaseControl :: Eac3Settings -> Lude.Maybe Eac3PhaseControl) (\s a -> s {phaseControl = a} :: Eac3Settings)
{-# DEPRECATED esPhaseControl "Use generic-lens or generic-optics with 'phaseControl' instead." #-}

-- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
--
-- /Note:/ Consider using 'surroundExMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSurroundExMode :: Lens.Lens' Eac3Settings (Lude.Maybe Eac3SurroundExMode)
esSurroundExMode = Lens.lens (surroundExMode :: Eac3Settings -> Lude.Maybe Eac3SurroundExMode) (\s a -> s {surroundExMode = a} :: Eac3Settings)
{-# DEPRECATED esSurroundExMode "Use generic-lens or generic-optics with 'surroundExMode' instead." #-}

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
--
-- /Note:/ Consider using 'dialnorm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDialnorm :: Lens.Lens' Eac3Settings (Lude.Maybe Lude.Natural)
esDialnorm = Lens.lens (dialnorm :: Eac3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {dialnorm = a} :: Eac3Settings)
{-# DEPRECATED esDialnorm "Use generic-lens or generic-optics with 'dialnorm' instead." #-}

instance Lude.FromJSON Eac3Settings where
  parseJSON =
    Lude.withObject
      "Eac3Settings"
      ( \x ->
          Eac3Settings'
            Lude.<$> (x Lude..:? "stereoDownmix")
            Lude.<*> (x Lude..:? "loRoCenterMixLevel")
            Lude.<*> (x Lude..:? "ltRtCenterMixLevel")
            Lude.<*> (x Lude..:? "lfeFilter")
            Lude.<*> (x Lude..:? "ltRtSurroundMixLevel")
            Lude.<*> (x Lude..:? "metadataControl")
            Lude.<*> (x Lude..:? "loRoSurroundMixLevel")
            Lude.<*> (x Lude..:? "surroundMode")
            Lude.<*> (x Lude..:? "attenuationControl")
            Lude.<*> (x Lude..:? "passthroughControl")
            Lude.<*> (x Lude..:? "bitstreamMode")
            Lude.<*> (x Lude..:? "lfeControl")
            Lude.<*> (x Lude..:? "codingMode")
            Lude.<*> (x Lude..:? "drcLine")
            Lude.<*> (x Lude..:? "drcRf")
            Lude.<*> (x Lude..:? "dcFilter")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "phaseControl")
            Lude.<*> (x Lude..:? "surroundExMode")
            Lude.<*> (x Lude..:? "dialnorm")
      )

instance Lude.ToJSON Eac3Settings where
  toJSON Eac3Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stereoDownmix" Lude..=) Lude.<$> stereoDownmix,
            ("loRoCenterMixLevel" Lude..=) Lude.<$> loRoCenterMixLevel,
            ("ltRtCenterMixLevel" Lude..=) Lude.<$> ltRtCenterMixLevel,
            ("lfeFilter" Lude..=) Lude.<$> lfeFilter,
            ("ltRtSurroundMixLevel" Lude..=) Lude.<$> ltRtSurroundMixLevel,
            ("metadataControl" Lude..=) Lude.<$> metadataControl,
            ("loRoSurroundMixLevel" Lude..=) Lude.<$> loRoSurroundMixLevel,
            ("surroundMode" Lude..=) Lude.<$> surroundMode,
            ("attenuationControl" Lude..=) Lude.<$> attenuationControl,
            ("passthroughControl" Lude..=) Lude.<$> passthroughControl,
            ("bitstreamMode" Lude..=) Lude.<$> bitstreamMode,
            ("lfeControl" Lude..=) Lude.<$> lfeControl,
            ("codingMode" Lude..=) Lude.<$> codingMode,
            ("drcLine" Lude..=) Lude.<$> drcLine,
            ("drcRf" Lude..=) Lude.<$> drcRf,
            ("dcFilter" Lude..=) Lude.<$> dcFilter,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("phaseControl" Lude..=) Lude.<$> phaseControl,
            ("surroundExMode" Lude..=) Lude.<$> surroundExMode,
            ("dialnorm" Lude..=) Lude.<$> dialnorm
          ]
      )
