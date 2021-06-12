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
-- Module      : Network.AWS.MediaLive.Types.AacSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AacCodingMode
import Network.AWS.MediaLive.Types.AacInputType
import Network.AWS.MediaLive.Types.AacProfile
import Network.AWS.MediaLive.Types.AacRateControlMode
import Network.AWS.MediaLive.Types.AacRawFormat
import Network.AWS.MediaLive.Types.AacSpec
import Network.AWS.MediaLive.Types.AacVbrQuality

-- | Aac Settings
--
-- /See:/ 'newAacSettings' smart constructor.
data AacSettings = AacSettings'
  { -- | Rate Control Mode.
    rateControlMode :: Core.Maybe AacRateControlMode,
    -- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control
    -- mode and profile. The adReceiverMix setting receives a stereo
    -- description plus control track and emits a mono AAC encode of the
    -- description track, with control data emitted in the PES header as per
    -- ETSI TS 101 154 Annex E.
    codingMode :: Core.Maybe AacCodingMode,
    -- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2
    -- Transport Stream containers.
    spec :: Core.Maybe AacSpec,
    -- | Sets LATM \/ LOAS AAC output for raw containers.
    rawFormat :: Core.Maybe AacRawFormat,
    -- | Sample rate in Hz. Valid values depend on rate control mode and profile.
    sampleRate :: Core.Maybe Core.Double,
    -- | Set to \"broadcasterMixedAd\" when input contains pre-mixed main audio +
    -- AD (narration) as a stereo pair. The Audio Type field (audioType) will
    -- be set to 3, which signals to downstream systems that this stream
    -- contains \"broadcaster mixed AD\". Note that the input received by the
    -- encoder must contain pre-mixed audio; the encoder does not perform the
    -- mixing. The values in audioTypeControl and audioType (in
    -- AudioDescription) are ignored when set to broadcasterMixedAd. Leave set
    -- to \"normal\" when input does not contain pre-mixed audio + AD.
    inputType :: Core.Maybe AacInputType,
    -- | AAC Profile.
    profile :: Core.Maybe AacProfile,
    -- | VBR Quality Level - Only used if rateControlMode is VBR.
    vbrQuality :: Core.Maybe AacVbrQuality,
    -- | Average bitrate in bits\/second. Valid values depend on rate control
    -- mode and profile.
    bitrate :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AacSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rateControlMode', 'aacSettings_rateControlMode' - Rate Control Mode.
--
-- 'codingMode', 'aacSettings_codingMode' - Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control
-- mode and profile. The adReceiverMix setting receives a stereo
-- description plus control track and emits a mono AAC encode of the
-- description track, with control data emitted in the PES header as per
-- ETSI TS 101 154 Annex E.
--
-- 'spec', 'aacSettings_spec' - Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2
-- Transport Stream containers.
--
-- 'rawFormat', 'aacSettings_rawFormat' - Sets LATM \/ LOAS AAC output for raw containers.
--
-- 'sampleRate', 'aacSettings_sampleRate' - Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- 'inputType', 'aacSettings_inputType' - Set to \"broadcasterMixedAd\" when input contains pre-mixed main audio +
-- AD (narration) as a stereo pair. The Audio Type field (audioType) will
-- be set to 3, which signals to downstream systems that this stream
-- contains \"broadcaster mixed AD\". Note that the input received by the
-- encoder must contain pre-mixed audio; the encoder does not perform the
-- mixing. The values in audioTypeControl and audioType (in
-- AudioDescription) are ignored when set to broadcasterMixedAd. Leave set
-- to \"normal\" when input does not contain pre-mixed audio + AD.
--
-- 'profile', 'aacSettings_profile' - AAC Profile.
--
-- 'vbrQuality', 'aacSettings_vbrQuality' - VBR Quality Level - Only used if rateControlMode is VBR.
--
-- 'bitrate', 'aacSettings_bitrate' - Average bitrate in bits\/second. Valid values depend on rate control
-- mode and profile.
newAacSettings ::
  AacSettings
newAacSettings =
  AacSettings'
    { rateControlMode = Core.Nothing,
      codingMode = Core.Nothing,
      spec = Core.Nothing,
      rawFormat = Core.Nothing,
      sampleRate = Core.Nothing,
      inputType = Core.Nothing,
      profile = Core.Nothing,
      vbrQuality = Core.Nothing,
      bitrate = Core.Nothing
    }

-- | Rate Control Mode.
aacSettings_rateControlMode :: Lens.Lens' AacSettings (Core.Maybe AacRateControlMode)
aacSettings_rateControlMode = Lens.lens (\AacSettings' {rateControlMode} -> rateControlMode) (\s@AacSettings' {} a -> s {rateControlMode = a} :: AacSettings)

-- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control
-- mode and profile. The adReceiverMix setting receives a stereo
-- description plus control track and emits a mono AAC encode of the
-- description track, with control data emitted in the PES header as per
-- ETSI TS 101 154 Annex E.
aacSettings_codingMode :: Lens.Lens' AacSettings (Core.Maybe AacCodingMode)
aacSettings_codingMode = Lens.lens (\AacSettings' {codingMode} -> codingMode) (\s@AacSettings' {} a -> s {codingMode = a} :: AacSettings)

-- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2
-- Transport Stream containers.
aacSettings_spec :: Lens.Lens' AacSettings (Core.Maybe AacSpec)
aacSettings_spec = Lens.lens (\AacSettings' {spec} -> spec) (\s@AacSettings' {} a -> s {spec = a} :: AacSettings)

-- | Sets LATM \/ LOAS AAC output for raw containers.
aacSettings_rawFormat :: Lens.Lens' AacSettings (Core.Maybe AacRawFormat)
aacSettings_rawFormat = Lens.lens (\AacSettings' {rawFormat} -> rawFormat) (\s@AacSettings' {} a -> s {rawFormat = a} :: AacSettings)

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
aacSettings_sampleRate :: Lens.Lens' AacSettings (Core.Maybe Core.Double)
aacSettings_sampleRate = Lens.lens (\AacSettings' {sampleRate} -> sampleRate) (\s@AacSettings' {} a -> s {sampleRate = a} :: AacSettings)

-- | Set to \"broadcasterMixedAd\" when input contains pre-mixed main audio +
-- AD (narration) as a stereo pair. The Audio Type field (audioType) will
-- be set to 3, which signals to downstream systems that this stream
-- contains \"broadcaster mixed AD\". Note that the input received by the
-- encoder must contain pre-mixed audio; the encoder does not perform the
-- mixing. The values in audioTypeControl and audioType (in
-- AudioDescription) are ignored when set to broadcasterMixedAd. Leave set
-- to \"normal\" when input does not contain pre-mixed audio + AD.
aacSettings_inputType :: Lens.Lens' AacSettings (Core.Maybe AacInputType)
aacSettings_inputType = Lens.lens (\AacSettings' {inputType} -> inputType) (\s@AacSettings' {} a -> s {inputType = a} :: AacSettings)

-- | AAC Profile.
aacSettings_profile :: Lens.Lens' AacSettings (Core.Maybe AacProfile)
aacSettings_profile = Lens.lens (\AacSettings' {profile} -> profile) (\s@AacSettings' {} a -> s {profile = a} :: AacSettings)

-- | VBR Quality Level - Only used if rateControlMode is VBR.
aacSettings_vbrQuality :: Lens.Lens' AacSettings (Core.Maybe AacVbrQuality)
aacSettings_vbrQuality = Lens.lens (\AacSettings' {vbrQuality} -> vbrQuality) (\s@AacSettings' {} a -> s {vbrQuality = a} :: AacSettings)

-- | Average bitrate in bits\/second. Valid values depend on rate control
-- mode and profile.
aacSettings_bitrate :: Lens.Lens' AacSettings (Core.Maybe Core.Double)
aacSettings_bitrate = Lens.lens (\AacSettings' {bitrate} -> bitrate) (\s@AacSettings' {} a -> s {bitrate = a} :: AacSettings)

instance Core.FromJSON AacSettings where
  parseJSON =
    Core.withObject
      "AacSettings"
      ( \x ->
          AacSettings'
            Core.<$> (x Core..:? "rateControlMode")
            Core.<*> (x Core..:? "codingMode")
            Core.<*> (x Core..:? "spec")
            Core.<*> (x Core..:? "rawFormat")
            Core.<*> (x Core..:? "sampleRate")
            Core.<*> (x Core..:? "inputType")
            Core.<*> (x Core..:? "profile")
            Core.<*> (x Core..:? "vbrQuality")
            Core.<*> (x Core..:? "bitrate")
      )

instance Core.Hashable AacSettings

instance Core.NFData AacSettings

instance Core.ToJSON AacSettings where
  toJSON AacSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("rateControlMode" Core..=)
              Core.<$> rateControlMode,
            ("codingMode" Core..=) Core.<$> codingMode,
            ("spec" Core..=) Core.<$> spec,
            ("rawFormat" Core..=) Core.<$> rawFormat,
            ("sampleRate" Core..=) Core.<$> sampleRate,
            ("inputType" Core..=) Core.<$> inputType,
            ("profile" Core..=) Core.<$> profile,
            ("vbrQuality" Core..=) Core.<$> vbrQuality,
            ("bitrate" Core..=) Core.<$> bitrate
          ]
      )
