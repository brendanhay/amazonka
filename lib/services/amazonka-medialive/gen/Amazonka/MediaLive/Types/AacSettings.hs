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
-- Module      : Amazonka.MediaLive.Types.AacSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AacSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AacCodingMode
import Amazonka.MediaLive.Types.AacInputType
import Amazonka.MediaLive.Types.AacProfile
import Amazonka.MediaLive.Types.AacRateControlMode
import Amazonka.MediaLive.Types.AacRawFormat
import Amazonka.MediaLive.Types.AacSpec
import Amazonka.MediaLive.Types.AacVbrQuality
import qualified Amazonka.Prelude as Prelude

-- | Aac Settings
--
-- /See:/ 'newAacSettings' smart constructor.
data AacSettings = AacSettings'
  { -- | Average bitrate in bits\/second. Valid values depend on rate control
    -- mode and profile.
    bitrate :: Prelude.Maybe Prelude.Double,
    -- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control
    -- mode and profile. The adReceiverMix setting receives a stereo
    -- description plus control track and emits a mono AAC encode of the
    -- description track, with control data emitted in the PES header as per
    -- ETSI TS 101 154 Annex E.
    codingMode :: Prelude.Maybe AacCodingMode,
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
    -- | Rate Control Mode.
    rateControlMode :: Prelude.Maybe AacRateControlMode,
    -- | Sets LATM \/ LOAS AAC output for raw containers.
    rawFormat :: Prelude.Maybe AacRawFormat,
    -- | Sample rate in Hz. Valid values depend on rate control mode and profile.
    sampleRate :: Prelude.Maybe Prelude.Double,
    -- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2
    -- Transport Stream containers.
    spec :: Prelude.Maybe AacSpec,
    -- | VBR Quality Level - Only used if rateControlMode is VBR.
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
-- 'bitrate', 'aacSettings_bitrate' - Average bitrate in bits\/second. Valid values depend on rate control
-- mode and profile.
--
-- 'codingMode', 'aacSettings_codingMode' - Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control
-- mode and profile. The adReceiverMix setting receives a stereo
-- description plus control track and emits a mono AAC encode of the
-- description track, with control data emitted in the PES header as per
-- ETSI TS 101 154 Annex E.
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
-- 'rateControlMode', 'aacSettings_rateControlMode' - Rate Control Mode.
--
-- 'rawFormat', 'aacSettings_rawFormat' - Sets LATM \/ LOAS AAC output for raw containers.
--
-- 'sampleRate', 'aacSettings_sampleRate' - Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- 'spec', 'aacSettings_spec' - Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2
-- Transport Stream containers.
--
-- 'vbrQuality', 'aacSettings_vbrQuality' - VBR Quality Level - Only used if rateControlMode is VBR.
newAacSettings ::
  AacSettings
newAacSettings =
  AacSettings'
    { bitrate = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      inputType = Prelude.Nothing,
      profile = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      rawFormat = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      spec = Prelude.Nothing,
      vbrQuality = Prelude.Nothing
    }

-- | Average bitrate in bits\/second. Valid values depend on rate control
-- mode and profile.
aacSettings_bitrate :: Lens.Lens' AacSettings (Prelude.Maybe Prelude.Double)
aacSettings_bitrate = Lens.lens (\AacSettings' {bitrate} -> bitrate) (\s@AacSettings' {} a -> s {bitrate = a} :: AacSettings)

-- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control
-- mode and profile. The adReceiverMix setting receives a stereo
-- description plus control track and emits a mono AAC encode of the
-- description track, with control data emitted in the PES header as per
-- ETSI TS 101 154 Annex E.
aacSettings_codingMode :: Lens.Lens' AacSettings (Prelude.Maybe AacCodingMode)
aacSettings_codingMode = Lens.lens (\AacSettings' {codingMode} -> codingMode) (\s@AacSettings' {} a -> s {codingMode = a} :: AacSettings)

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

-- | Rate Control Mode.
aacSettings_rateControlMode :: Lens.Lens' AacSettings (Prelude.Maybe AacRateControlMode)
aacSettings_rateControlMode = Lens.lens (\AacSettings' {rateControlMode} -> rateControlMode) (\s@AacSettings' {} a -> s {rateControlMode = a} :: AacSettings)

-- | Sets LATM \/ LOAS AAC output for raw containers.
aacSettings_rawFormat :: Lens.Lens' AacSettings (Prelude.Maybe AacRawFormat)
aacSettings_rawFormat = Lens.lens (\AacSettings' {rawFormat} -> rawFormat) (\s@AacSettings' {} a -> s {rawFormat = a} :: AacSettings)

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
aacSettings_sampleRate :: Lens.Lens' AacSettings (Prelude.Maybe Prelude.Double)
aacSettings_sampleRate = Lens.lens (\AacSettings' {sampleRate} -> sampleRate) (\s@AacSettings' {} a -> s {sampleRate = a} :: AacSettings)

-- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2
-- Transport Stream containers.
aacSettings_spec :: Lens.Lens' AacSettings (Prelude.Maybe AacSpec)
aacSettings_spec = Lens.lens (\AacSettings' {spec} -> spec) (\s@AacSettings' {} a -> s {spec = a} :: AacSettings)

-- | VBR Quality Level - Only used if rateControlMode is VBR.
aacSettings_vbrQuality :: Lens.Lens' AacSettings (Prelude.Maybe AacVbrQuality)
aacSettings_vbrQuality = Lens.lens (\AacSettings' {vbrQuality} -> vbrQuality) (\s@AacSettings' {} a -> s {vbrQuality = a} :: AacSettings)

instance Data.FromJSON AacSettings where
  parseJSON =
    Data.withObject
      "AacSettings"
      ( \x ->
          AacSettings'
            Prelude.<$> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "codingMode")
            Prelude.<*> (x Data..:? "inputType")
            Prelude.<*> (x Data..:? "profile")
            Prelude.<*> (x Data..:? "rateControlMode")
            Prelude.<*> (x Data..:? "rawFormat")
            Prelude.<*> (x Data..:? "sampleRate")
            Prelude.<*> (x Data..:? "spec")
            Prelude.<*> (x Data..:? "vbrQuality")
      )

instance Prelude.Hashable AacSettings where
  hashWithSalt _salt AacSettings' {..} =
    _salt `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` codingMode
      `Prelude.hashWithSalt` inputType
      `Prelude.hashWithSalt` profile
      `Prelude.hashWithSalt` rateControlMode
      `Prelude.hashWithSalt` rawFormat
      `Prelude.hashWithSalt` sampleRate
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` vbrQuality

instance Prelude.NFData AacSettings where
  rnf AacSettings' {..} =
    Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf codingMode
      `Prelude.seq` Prelude.rnf inputType
      `Prelude.seq` Prelude.rnf profile
      `Prelude.seq` Prelude.rnf rateControlMode
      `Prelude.seq` Prelude.rnf rawFormat
      `Prelude.seq` Prelude.rnf sampleRate
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf vbrQuality

instance Data.ToJSON AacSettings where
  toJSON AacSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitrate" Data..=) Prelude.<$> bitrate,
            ("codingMode" Data..=) Prelude.<$> codingMode,
            ("inputType" Data..=) Prelude.<$> inputType,
            ("profile" Data..=) Prelude.<$> profile,
            ("rateControlMode" Data..=)
              Prelude.<$> rateControlMode,
            ("rawFormat" Data..=) Prelude.<$> rawFormat,
            ("sampleRate" Data..=) Prelude.<$> sampleRate,
            ("spec" Data..=) Prelude.<$> spec,
            ("vbrQuality" Data..=) Prelude.<$> vbrQuality
          ]
      )
