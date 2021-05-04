{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AacCodingMode
import Network.AWS.MediaLive.Types.AacInputType
import Network.AWS.MediaLive.Types.AacProfile
import Network.AWS.MediaLive.Types.AacRateControlMode
import Network.AWS.MediaLive.Types.AacRawFormat
import Network.AWS.MediaLive.Types.AacSpec
import Network.AWS.MediaLive.Types.AacVbrQuality
import qualified Network.AWS.Prelude as Prelude

-- | Aac Settings
--
-- /See:/ 'newAacSettings' smart constructor.
data AacSettings = AacSettings'
  { -- | Rate Control Mode.
    rateControlMode :: Prelude.Maybe AacRateControlMode,
    -- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control
    -- mode and profile. The adReceiverMix setting receives a stereo
    -- description plus control track and emits a mono AAC encode of the
    -- description track, with control data emitted in the PES header as per
    -- ETSI TS 101 154 Annex E.
    codingMode :: Prelude.Maybe AacCodingMode,
    -- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2
    -- Transport Stream containers.
    spec :: Prelude.Maybe AacSpec,
    -- | Sets LATM \/ LOAS AAC output for raw containers.
    rawFormat :: Prelude.Maybe AacRawFormat,
    -- | Sample rate in Hz. Valid values depend on rate control mode and profile.
    sampleRate :: Prelude.Maybe Prelude.Double,
    -- | Set to \"broadcasterMixedAd\" when input contains pre-mixed main audio +
    -- AD (narration) as a stereo pair. The Audio Type field (audioType) will
    -- be set to 3, which signals to downstream systems that this stream
    -- contains \"broadcaster mixed AD\". Note that the input received by the
    -- encoder must contain pre-mixed audio; the encoder does not perform the
    -- mixing. The values in audioTypeControl and audioType (in
    -- AudioDescription) are ignored when set to broadcasterMixedAd. Leave set
    -- to \"normal\" when input does not contain pre-mixed audio + AD.
    inputType :: Prelude.Maybe AacInputType,
    -- | AAC Profile.
    profile :: Prelude.Maybe AacProfile,
    -- | VBR Quality Level - Only used if rateControlMode is VBR.
    vbrQuality :: Prelude.Maybe AacVbrQuality,
    -- | Average bitrate in bits\/second. Valid values depend on rate control
    -- mode and profile.
    bitrate :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { rateControlMode = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      spec = Prelude.Nothing,
      rawFormat = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      inputType = Prelude.Nothing,
      profile = Prelude.Nothing,
      vbrQuality = Prelude.Nothing,
      bitrate = Prelude.Nothing
    }

-- | Rate Control Mode.
aacSettings_rateControlMode :: Lens.Lens' AacSettings (Prelude.Maybe AacRateControlMode)
aacSettings_rateControlMode = Lens.lens (\AacSettings' {rateControlMode} -> rateControlMode) (\s@AacSettings' {} a -> s {rateControlMode = a} :: AacSettings)

-- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control
-- mode and profile. The adReceiverMix setting receives a stereo
-- description plus control track and emits a mono AAC encode of the
-- description track, with control data emitted in the PES header as per
-- ETSI TS 101 154 Annex E.
aacSettings_codingMode :: Lens.Lens' AacSettings (Prelude.Maybe AacCodingMode)
aacSettings_codingMode = Lens.lens (\AacSettings' {codingMode} -> codingMode) (\s@AacSettings' {} a -> s {codingMode = a} :: AacSettings)

-- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2
-- Transport Stream containers.
aacSettings_spec :: Lens.Lens' AacSettings (Prelude.Maybe AacSpec)
aacSettings_spec = Lens.lens (\AacSettings' {spec} -> spec) (\s@AacSettings' {} a -> s {spec = a} :: AacSettings)

-- | Sets LATM \/ LOAS AAC output for raw containers.
aacSettings_rawFormat :: Lens.Lens' AacSettings (Prelude.Maybe AacRawFormat)
aacSettings_rawFormat = Lens.lens (\AacSettings' {rawFormat} -> rawFormat) (\s@AacSettings' {} a -> s {rawFormat = a} :: AacSettings)

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
aacSettings_sampleRate :: Lens.Lens' AacSettings (Prelude.Maybe Prelude.Double)
aacSettings_sampleRate = Lens.lens (\AacSettings' {sampleRate} -> sampleRate) (\s@AacSettings' {} a -> s {sampleRate = a} :: AacSettings)

-- | Set to \"broadcasterMixedAd\" when input contains pre-mixed main audio +
-- AD (narration) as a stereo pair. The Audio Type field (audioType) will
-- be set to 3, which signals to downstream systems that this stream
-- contains \"broadcaster mixed AD\". Note that the input received by the
-- encoder must contain pre-mixed audio; the encoder does not perform the
-- mixing. The values in audioTypeControl and audioType (in
-- AudioDescription) are ignored when set to broadcasterMixedAd. Leave set
-- to \"normal\" when input does not contain pre-mixed audio + AD.
aacSettings_inputType :: Lens.Lens' AacSettings (Prelude.Maybe AacInputType)
aacSettings_inputType = Lens.lens (\AacSettings' {inputType} -> inputType) (\s@AacSettings' {} a -> s {inputType = a} :: AacSettings)

-- | AAC Profile.
aacSettings_profile :: Lens.Lens' AacSettings (Prelude.Maybe AacProfile)
aacSettings_profile = Lens.lens (\AacSettings' {profile} -> profile) (\s@AacSettings' {} a -> s {profile = a} :: AacSettings)

-- | VBR Quality Level - Only used if rateControlMode is VBR.
aacSettings_vbrQuality :: Lens.Lens' AacSettings (Prelude.Maybe AacVbrQuality)
aacSettings_vbrQuality = Lens.lens (\AacSettings' {vbrQuality} -> vbrQuality) (\s@AacSettings' {} a -> s {vbrQuality = a} :: AacSettings)

-- | Average bitrate in bits\/second. Valid values depend on rate control
-- mode and profile.
aacSettings_bitrate :: Lens.Lens' AacSettings (Prelude.Maybe Prelude.Double)
aacSettings_bitrate = Lens.lens (\AacSettings' {bitrate} -> bitrate) (\s@AacSettings' {} a -> s {bitrate = a} :: AacSettings)

instance Prelude.FromJSON AacSettings where
  parseJSON =
    Prelude.withObject
      "AacSettings"
      ( \x ->
          AacSettings'
            Prelude.<$> (x Prelude..:? "rateControlMode")
            Prelude.<*> (x Prelude..:? "codingMode")
            Prelude.<*> (x Prelude..:? "spec")
            Prelude.<*> (x Prelude..:? "rawFormat")
            Prelude.<*> (x Prelude..:? "sampleRate")
            Prelude.<*> (x Prelude..:? "inputType")
            Prelude.<*> (x Prelude..:? "profile")
            Prelude.<*> (x Prelude..:? "vbrQuality")
            Prelude.<*> (x Prelude..:? "bitrate")
      )

instance Prelude.Hashable AacSettings

instance Prelude.NFData AacSettings

instance Prelude.ToJSON AacSettings where
  toJSON AacSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("rateControlMode" Prelude..=)
              Prelude.<$> rateControlMode,
            ("codingMode" Prelude..=) Prelude.<$> codingMode,
            ("spec" Prelude..=) Prelude.<$> spec,
            ("rawFormat" Prelude..=) Prelude.<$> rawFormat,
            ("sampleRate" Prelude..=) Prelude.<$> sampleRate,
            ("inputType" Prelude..=) Prelude.<$> inputType,
            ("profile" Prelude..=) Prelude.<$> profile,
            ("vbrQuality" Prelude..=) Prelude.<$> vbrQuality,
            ("bitrate" Prelude..=) Prelude.<$> bitrate
          ]
      )
