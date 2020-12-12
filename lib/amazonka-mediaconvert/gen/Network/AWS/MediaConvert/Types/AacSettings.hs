{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacSettings
  ( AacSettings (..),

    -- * Smart constructor
    mkAacSettings,

    -- * Lenses
    assAudioDescriptionBroadcasterMix,
    assRawFormat,
    assCodingMode,
    assRateControlMode,
    assSampleRate,
    assSpecification,
    assCodecProfile,
    assBitrate,
    assVbrQuality,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
import Network.AWS.MediaConvert.Types.AacCodecProfile
import Network.AWS.MediaConvert.Types.AacCodingMode
import Network.AWS.MediaConvert.Types.AacRateControlMode
import Network.AWS.MediaConvert.Types.AacRawFormat
import Network.AWS.MediaConvert.Types.AacSpecification
import Network.AWS.MediaConvert.Types.AacVbrQuality
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
--
-- /See:/ 'mkAacSettings' smart constructor.
data AacSettings = AacSettings'
  { audioDescriptionBroadcasterMix ::
      Lude.Maybe AacAudioDescriptionBroadcasterMix,
    rawFormat :: Lude.Maybe AacRawFormat,
    codingMode :: Lude.Maybe AacCodingMode,
    rateControlMode :: Lude.Maybe AacRateControlMode,
    sampleRate :: Lude.Maybe Lude.Natural,
    specification :: Lude.Maybe AacSpecification,
    codecProfile :: Lude.Maybe AacCodecProfile,
    bitrate :: Lude.Maybe Lude.Natural,
    vbrQuality :: Lude.Maybe AacVbrQuality
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AacSettings' with the minimum fields required to make a request.
--
-- * 'audioDescriptionBroadcasterMix' - Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
-- * 'bitrate' - Specify the average bitrate in bits per second. The set of valid values for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000, 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000, 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is also constrained by the values that you choose for Profile (codecProfile), Bitrate control mode (codingMode), and Sample rate (sampleRate). Default values depend on Bitrate control mode and Profile.
-- * 'codecProfile' - AAC Profile.
-- * 'codingMode' - Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
-- * 'rateControlMode' - Rate Control Mode.
-- * 'rawFormat' - Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
-- * 'sampleRate' - Sample rate in Hz. Valid values depend on rate control mode and profile.
-- * 'specification' - Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
-- * 'vbrQuality' - VBR Quality Level - Only used if rate_control_mode is VBR.
mkAacSettings ::
  AacSettings
mkAacSettings =
  AacSettings'
    { audioDescriptionBroadcasterMix = Lude.Nothing,
      rawFormat = Lude.Nothing,
      codingMode = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      sampleRate = Lude.Nothing,
      specification = Lude.Nothing,
      codecProfile = Lude.Nothing,
      bitrate = Lude.Nothing,
      vbrQuality = Lude.Nothing
    }

-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
--
-- /Note:/ Consider using 'audioDescriptionBroadcasterMix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAudioDescriptionBroadcasterMix :: Lens.Lens' AacSettings (Lude.Maybe AacAudioDescriptionBroadcasterMix)
assAudioDescriptionBroadcasterMix = Lens.lens (audioDescriptionBroadcasterMix :: AacSettings -> Lude.Maybe AacAudioDescriptionBroadcasterMix) (\s a -> s {audioDescriptionBroadcasterMix = a} :: AacSettings)
{-# DEPRECATED assAudioDescriptionBroadcasterMix "Use generic-lens or generic-optics with 'audioDescriptionBroadcasterMix' instead." #-}

-- | Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
--
-- /Note:/ Consider using 'rawFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assRawFormat :: Lens.Lens' AacSettings (Lude.Maybe AacRawFormat)
assRawFormat = Lens.lens (rawFormat :: AacSettings -> Lude.Maybe AacRawFormat) (\s a -> s {rawFormat = a} :: AacSettings)
{-# DEPRECATED assRawFormat "Use generic-lens or generic-optics with 'rawFormat' instead." #-}

-- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
--
-- /Note:/ Consider using 'codingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assCodingMode :: Lens.Lens' AacSettings (Lude.Maybe AacCodingMode)
assCodingMode = Lens.lens (codingMode :: AacSettings -> Lude.Maybe AacCodingMode) (\s a -> s {codingMode = a} :: AacSettings)
{-# DEPRECATED assCodingMode "Use generic-lens or generic-optics with 'codingMode' instead." #-}

-- | Rate Control Mode.
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assRateControlMode :: Lens.Lens' AacSettings (Lude.Maybe AacRateControlMode)
assRateControlMode = Lens.lens (rateControlMode :: AacSettings -> Lude.Maybe AacRateControlMode) (\s a -> s {rateControlMode = a} :: AacSettings)
{-# DEPRECATED assRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assSampleRate :: Lens.Lens' AacSettings (Lude.Maybe Lude.Natural)
assSampleRate = Lens.lens (sampleRate :: AacSettings -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: AacSettings)
{-# DEPRECATED assSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
--
-- /Note:/ Consider using 'specification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assSpecification :: Lens.Lens' AacSettings (Lude.Maybe AacSpecification)
assSpecification = Lens.lens (specification :: AacSettings -> Lude.Maybe AacSpecification) (\s a -> s {specification = a} :: AacSettings)
{-# DEPRECATED assSpecification "Use generic-lens or generic-optics with 'specification' instead." #-}

-- | AAC Profile.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assCodecProfile :: Lens.Lens' AacSettings (Lude.Maybe AacCodecProfile)
assCodecProfile = Lens.lens (codecProfile :: AacSettings -> Lude.Maybe AacCodecProfile) (\s a -> s {codecProfile = a} :: AacSettings)
{-# DEPRECATED assCodecProfile "Use generic-lens or generic-optics with 'codecProfile' instead." #-}

-- | Specify the average bitrate in bits per second. The set of valid values for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000, 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000, 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is also constrained by the values that you choose for Profile (codecProfile), Bitrate control mode (codingMode), and Sample rate (sampleRate). Default values depend on Bitrate control mode and Profile.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assBitrate :: Lens.Lens' AacSettings (Lude.Maybe Lude.Natural)
assBitrate = Lens.lens (bitrate :: AacSettings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: AacSettings)
{-# DEPRECATED assBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | VBR Quality Level - Only used if rate_control_mode is VBR.
--
-- /Note:/ Consider using 'vbrQuality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assVbrQuality :: Lens.Lens' AacSettings (Lude.Maybe AacVbrQuality)
assVbrQuality = Lens.lens (vbrQuality :: AacSettings -> Lude.Maybe AacVbrQuality) (\s a -> s {vbrQuality = a} :: AacSettings)
{-# DEPRECATED assVbrQuality "Use generic-lens or generic-optics with 'vbrQuality' instead." #-}

instance Lude.FromJSON AacSettings where
  parseJSON =
    Lude.withObject
      "AacSettings"
      ( \x ->
          AacSettings'
            Lude.<$> (x Lude..:? "audioDescriptionBroadcasterMix")
            Lude.<*> (x Lude..:? "rawFormat")
            Lude.<*> (x Lude..:? "codingMode")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "sampleRate")
            Lude.<*> (x Lude..:? "specification")
            Lude.<*> (x Lude..:? "codecProfile")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "vbrQuality")
      )

instance Lude.ToJSON AacSettings where
  toJSON AacSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("audioDescriptionBroadcasterMix" Lude..=)
              Lude.<$> audioDescriptionBroadcasterMix,
            ("rawFormat" Lude..=) Lude.<$> rawFormat,
            ("codingMode" Lude..=) Lude.<$> codingMode,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("sampleRate" Lude..=) Lude.<$> sampleRate,
            ("specification" Lude..=) Lude.<$> specification,
            ("codecProfile" Lude..=) Lude.<$> codecProfile,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("vbrQuality" Lude..=) Lude.<$> vbrQuality
          ]
      )
