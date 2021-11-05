{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.AacSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AacSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
import Amazonka.MediaConvert.Types.AacCodecProfile
import Amazonka.MediaConvert.Types.AacCodingMode
import Amazonka.MediaConvert.Types.AacRateControlMode
import Amazonka.MediaConvert.Types.AacRawFormat
import Amazonka.MediaConvert.Types.AacSpecification
import Amazonka.MediaConvert.Types.AacVbrQuality
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AAC. The service accepts one of two mutually exclusive
-- groups of AAC settings--VBR and CBR. To select one of these modes, set
-- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
-- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
-- quality (vbrQuality). In CBR mode, you use the setting Bitrate
-- (bitrate). Defaults and valid values depend on the rate control mode.
--
-- /See:/ 'newAacSettings' smart constructor.
data AacSettings = AacSettings'
  { -- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio
    -- + audio description (AD) as a stereo pair. The value for AudioType will
    -- be set to 3, which signals to downstream systems that this stream
    -- contains \"broadcaster mixed AD\". Note that the input received by the
    -- encoder must contain pre-mixed audio; the encoder does not perform the
    -- mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any
    -- values you provide in AudioType and FollowInputAudioType. Choose NORMAL
    -- when the input does not contain pre-mixed audio + audio description
    -- (AD). In this case, the encoder will use any values you provide for
    -- AudioType and FollowInputAudioType.
    audioDescriptionBroadcasterMix :: Prelude.Maybe AacAudioDescriptionBroadcasterMix,
    -- | Enables LATM\/LOAS AAC output. Note that if you use LATM\/LOAS AAC in an
    -- output, you must choose \"No container\" for the output container.
    rawFormat :: Prelude.Maybe AacRawFormat,
    -- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid
    -- values depend on rate control mode and profile. \"1.0 - Audio
    -- Description (Receiver Mix)\" setting receives a stereo description plus
    -- control track and emits a mono AAC encode of the description track, with
    -- control data emitted in the PES header as per ETSI TS 101 154 Annex E.
    codingMode :: Prelude.Maybe AacCodingMode,
    -- | Rate Control Mode.
    rateControlMode :: Prelude.Maybe AacRateControlMode,
    -- | Sample rate in Hz. Valid values depend on rate control mode and profile.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport
    -- Stream containers.
    specification :: Prelude.Maybe AacSpecification,
    -- | AAC Profile.
    codecProfile :: Prelude.Maybe AacCodecProfile,
    -- | Specify the average bitrate in bits per second. The set of valid values
    -- for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000,
    -- 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000,
    -- 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000,
    -- 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is
    -- also constrained by the values that you choose for Profile
    -- (codecProfile), Bitrate control mode (codingMode), and Sample rate
    -- (sampleRate). Default values depend on Bitrate control mode and Profile.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | VBR Quality Level - Only used if rate_control_mode is VBR.
    vbrQuality :: Prelude.Maybe AacVbrQuality
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AacSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioDescriptionBroadcasterMix', 'aacSettings_audioDescriptionBroadcasterMix' - Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio
-- + audio description (AD) as a stereo pair. The value for AudioType will
-- be set to 3, which signals to downstream systems that this stream
-- contains \"broadcaster mixed AD\". Note that the input received by the
-- encoder must contain pre-mixed audio; the encoder does not perform the
-- mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any
-- values you provide in AudioType and FollowInputAudioType. Choose NORMAL
-- when the input does not contain pre-mixed audio + audio description
-- (AD). In this case, the encoder will use any values you provide for
-- AudioType and FollowInputAudioType.
--
-- 'rawFormat', 'aacSettings_rawFormat' - Enables LATM\/LOAS AAC output. Note that if you use LATM\/LOAS AAC in an
-- output, you must choose \"No container\" for the output container.
--
-- 'codingMode', 'aacSettings_codingMode' - Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid
-- values depend on rate control mode and profile. \"1.0 - Audio
-- Description (Receiver Mix)\" setting receives a stereo description plus
-- control track and emits a mono AAC encode of the description track, with
-- control data emitted in the PES header as per ETSI TS 101 154 Annex E.
--
-- 'rateControlMode', 'aacSettings_rateControlMode' - Rate Control Mode.
--
-- 'sampleRate', 'aacSettings_sampleRate' - Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- 'specification', 'aacSettings_specification' - Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport
-- Stream containers.
--
-- 'codecProfile', 'aacSettings_codecProfile' - AAC Profile.
--
-- 'bitrate', 'aacSettings_bitrate' - Specify the average bitrate in bits per second. The set of valid values
-- for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000,
-- 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000,
-- 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000,
-- 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is
-- also constrained by the values that you choose for Profile
-- (codecProfile), Bitrate control mode (codingMode), and Sample rate
-- (sampleRate). Default values depend on Bitrate control mode and Profile.
--
-- 'vbrQuality', 'aacSettings_vbrQuality' - VBR Quality Level - Only used if rate_control_mode is VBR.
newAacSettings ::
  AacSettings
newAacSettings =
  AacSettings'
    { audioDescriptionBroadcasterMix =
        Prelude.Nothing,
      rawFormat = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      specification = Prelude.Nothing,
      codecProfile = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      vbrQuality = Prelude.Nothing
    }

-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio
-- + audio description (AD) as a stereo pair. The value for AudioType will
-- be set to 3, which signals to downstream systems that this stream
-- contains \"broadcaster mixed AD\". Note that the input received by the
-- encoder must contain pre-mixed audio; the encoder does not perform the
-- mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any
-- values you provide in AudioType and FollowInputAudioType. Choose NORMAL
-- when the input does not contain pre-mixed audio + audio description
-- (AD). In this case, the encoder will use any values you provide for
-- AudioType and FollowInputAudioType.
aacSettings_audioDescriptionBroadcasterMix :: Lens.Lens' AacSettings (Prelude.Maybe AacAudioDescriptionBroadcasterMix)
aacSettings_audioDescriptionBroadcasterMix = Lens.lens (\AacSettings' {audioDescriptionBroadcasterMix} -> audioDescriptionBroadcasterMix) (\s@AacSettings' {} a -> s {audioDescriptionBroadcasterMix = a} :: AacSettings)

-- | Enables LATM\/LOAS AAC output. Note that if you use LATM\/LOAS AAC in an
-- output, you must choose \"No container\" for the output container.
aacSettings_rawFormat :: Lens.Lens' AacSettings (Prelude.Maybe AacRawFormat)
aacSettings_rawFormat = Lens.lens (\AacSettings' {rawFormat} -> rawFormat) (\s@AacSettings' {} a -> s {rawFormat = a} :: AacSettings)

-- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid
-- values depend on rate control mode and profile. \"1.0 - Audio
-- Description (Receiver Mix)\" setting receives a stereo description plus
-- control track and emits a mono AAC encode of the description track, with
-- control data emitted in the PES header as per ETSI TS 101 154 Annex E.
aacSettings_codingMode :: Lens.Lens' AacSettings (Prelude.Maybe AacCodingMode)
aacSettings_codingMode = Lens.lens (\AacSettings' {codingMode} -> codingMode) (\s@AacSettings' {} a -> s {codingMode = a} :: AacSettings)

-- | Rate Control Mode.
aacSettings_rateControlMode :: Lens.Lens' AacSettings (Prelude.Maybe AacRateControlMode)
aacSettings_rateControlMode = Lens.lens (\AacSettings' {rateControlMode} -> rateControlMode) (\s@AacSettings' {} a -> s {rateControlMode = a} :: AacSettings)

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
aacSettings_sampleRate :: Lens.Lens' AacSettings (Prelude.Maybe Prelude.Natural)
aacSettings_sampleRate = Lens.lens (\AacSettings' {sampleRate} -> sampleRate) (\s@AacSettings' {} a -> s {sampleRate = a} :: AacSettings)

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport
-- Stream containers.
aacSettings_specification :: Lens.Lens' AacSettings (Prelude.Maybe AacSpecification)
aacSettings_specification = Lens.lens (\AacSettings' {specification} -> specification) (\s@AacSettings' {} a -> s {specification = a} :: AacSettings)

-- | AAC Profile.
aacSettings_codecProfile :: Lens.Lens' AacSettings (Prelude.Maybe AacCodecProfile)
aacSettings_codecProfile = Lens.lens (\AacSettings' {codecProfile} -> codecProfile) (\s@AacSettings' {} a -> s {codecProfile = a} :: AacSettings)

-- | Specify the average bitrate in bits per second. The set of valid values
-- for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000,
-- 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000,
-- 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000,
-- 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is
-- also constrained by the values that you choose for Profile
-- (codecProfile), Bitrate control mode (codingMode), and Sample rate
-- (sampleRate). Default values depend on Bitrate control mode and Profile.
aacSettings_bitrate :: Lens.Lens' AacSettings (Prelude.Maybe Prelude.Natural)
aacSettings_bitrate = Lens.lens (\AacSettings' {bitrate} -> bitrate) (\s@AacSettings' {} a -> s {bitrate = a} :: AacSettings)

-- | VBR Quality Level - Only used if rate_control_mode is VBR.
aacSettings_vbrQuality :: Lens.Lens' AacSettings (Prelude.Maybe AacVbrQuality)
aacSettings_vbrQuality = Lens.lens (\AacSettings' {vbrQuality} -> vbrQuality) (\s@AacSettings' {} a -> s {vbrQuality = a} :: AacSettings)

instance Core.FromJSON AacSettings where
  parseJSON =
    Core.withObject
      "AacSettings"
      ( \x ->
          AacSettings'
            Prelude.<$> (x Core..:? "audioDescriptionBroadcasterMix")
            Prelude.<*> (x Core..:? "rawFormat")
            Prelude.<*> (x Core..:? "codingMode")
            Prelude.<*> (x Core..:? "rateControlMode")
            Prelude.<*> (x Core..:? "sampleRate")
            Prelude.<*> (x Core..:? "specification")
            Prelude.<*> (x Core..:? "codecProfile")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "vbrQuality")
      )

instance Prelude.Hashable AacSettings

instance Prelude.NFData AacSettings

instance Core.ToJSON AacSettings where
  toJSON AacSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("audioDescriptionBroadcasterMix" Core..=)
              Prelude.<$> audioDescriptionBroadcasterMix,
            ("rawFormat" Core..=) Prelude.<$> rawFormat,
            ("codingMode" Core..=) Prelude.<$> codingMode,
            ("rateControlMode" Core..=)
              Prelude.<$> rateControlMode,
            ("sampleRate" Core..=) Prelude.<$> sampleRate,
            ("specification" Core..=) Prelude.<$> specification,
            ("codecProfile" Core..=) Prelude.<$> codecProfile,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("vbrQuality" Core..=) Prelude.<$> vbrQuality
          ]
      )
