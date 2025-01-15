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
-- Module      : Amazonka.MediaConvert.Types.Xavc4kProfileSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Xavc4kProfileSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Xavc4kProfileBitrateClass
import Amazonka.MediaConvert.Types.Xavc4kProfileCodecProfile
import Amazonka.MediaConvert.Types.Xavc4kProfileQualityTuningLevel
import Amazonka.MediaConvert.Types.XavcFlickerAdaptiveQuantization
import Amazonka.MediaConvert.Types.XavcGopBReference
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value XAVC_4K.
--
-- /See:/ 'newXavc4kProfileSettings' smart constructor.
data Xavc4kProfileSettings = Xavc4kProfileSettings'
  { -- | Specify the XAVC 4k (Long GOP) Bitrate Class to set the bitrate of your
    -- output. Outputs of the same class have similar image quality over the
    -- operating points that are valid for that class.
    bitrateClass :: Prelude.Maybe Xavc4kProfileBitrateClass,
    -- | Specify the codec profile for this output. Choose High, 8-bit, 4:2:0
    -- (HIGH) or High, 10-bit, 4:2:2 (HIGH_422). These profiles are specified
    -- in ITU-T H.264.
    codecProfile :: Prelude.Maybe Xavc4kProfileCodecProfile,
    -- | The best way to set up adaptive quantization is to keep the default
    -- value, Auto (AUTO), for the setting Adaptive quantization
    -- (XavcAdaptiveQuantization). When you do so, MediaConvert automatically
    -- applies the best types of quantization for your video content. Include
    -- this setting in your JSON job specification only when you choose to
    -- change the default value for Adaptive quantization. Enable this setting
    -- to have the encoder reduce I-frame pop. I-frame pop appears as a visual
    -- flicker that can arise when the encoder saves bits by copying some
    -- macroblocks many times from frame to frame, and then refreshes them at
    -- the I-frame. When you enable this setting, the encoder updates these
    -- macroblocks slightly more often to smooth out the flicker. This setting
    -- is disabled by default. Related setting: In addition to enabling this
    -- setting, you must also set Adaptive quantization (adaptiveQuantization)
    -- to a value other than Off (OFF) or Auto (AUTO). Use Adaptive
    -- quantization to adjust the degree of smoothing that Flicker adaptive
    -- quantization provides.
    flickerAdaptiveQuantization :: Prelude.Maybe XavcFlickerAdaptiveQuantization,
    -- | Specify whether the encoder uses B-frames as reference frames for other
    -- pictures in the same GOP. Choose Allow (ENABLED) to allow the encoder to
    -- use B-frames as reference frames. Choose Don\'t allow (DISABLED) to
    -- prevent the encoder from using B-frames as reference frames.
    gopBReference :: Prelude.Maybe XavcGopBReference,
    -- | Frequency of closed GOPs. In streaming applications, it is recommended
    -- that this be set to 1 so a decoder joining mid-stream will receive an
    -- IDR frame as quickly as possible. Setting this value to 0 will break
    -- output segmenting.
    gopClosedCadence :: Prelude.Maybe Prelude.Natural,
    -- | Specify the size of the buffer that MediaConvert uses in the HRD buffer
    -- model for this output. Specify this value in bits; for example, enter
    -- five megabits as 5000000. When you don\'t set this value, or you set it
    -- to zero, MediaConvert calculates the default by doubling the bitrate of
    -- this output point.
    hrdBufferSize :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
    -- you want to trade off encoding speed for output video quality. The
    -- default behavior is faster, lower quality, single-pass encoding.
    qualityTuningLevel :: Prelude.Maybe Xavc4kProfileQualityTuningLevel,
    -- | Number of slices per picture. Must be less than or equal to the number
    -- of macroblock rows for progressive pictures, and less than or equal to
    -- half the number of macroblock rows for interlaced pictures.
    slices :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Xavc4kProfileSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitrateClass', 'xavc4kProfileSettings_bitrateClass' - Specify the XAVC 4k (Long GOP) Bitrate Class to set the bitrate of your
-- output. Outputs of the same class have similar image quality over the
-- operating points that are valid for that class.
--
-- 'codecProfile', 'xavc4kProfileSettings_codecProfile' - Specify the codec profile for this output. Choose High, 8-bit, 4:2:0
-- (HIGH) or High, 10-bit, 4:2:2 (HIGH_422). These profiles are specified
-- in ITU-T H.264.
--
-- 'flickerAdaptiveQuantization', 'xavc4kProfileSettings_flickerAdaptiveQuantization' - The best way to set up adaptive quantization is to keep the default
-- value, Auto (AUTO), for the setting Adaptive quantization
-- (XavcAdaptiveQuantization). When you do so, MediaConvert automatically
-- applies the best types of quantization for your video content. Include
-- this setting in your JSON job specification only when you choose to
-- change the default value for Adaptive quantization. Enable this setting
-- to have the encoder reduce I-frame pop. I-frame pop appears as a visual
-- flicker that can arise when the encoder saves bits by copying some
-- macroblocks many times from frame to frame, and then refreshes them at
-- the I-frame. When you enable this setting, the encoder updates these
-- macroblocks slightly more often to smooth out the flicker. This setting
-- is disabled by default. Related setting: In addition to enabling this
-- setting, you must also set Adaptive quantization (adaptiveQuantization)
-- to a value other than Off (OFF) or Auto (AUTO). Use Adaptive
-- quantization to adjust the degree of smoothing that Flicker adaptive
-- quantization provides.
--
-- 'gopBReference', 'xavc4kProfileSettings_gopBReference' - Specify whether the encoder uses B-frames as reference frames for other
-- pictures in the same GOP. Choose Allow (ENABLED) to allow the encoder to
-- use B-frames as reference frames. Choose Don\'t allow (DISABLED) to
-- prevent the encoder from using B-frames as reference frames.
--
-- 'gopClosedCadence', 'xavc4kProfileSettings_gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
--
-- 'hrdBufferSize', 'xavc4kProfileSettings_hrdBufferSize' - Specify the size of the buffer that MediaConvert uses in the HRD buffer
-- model for this output. Specify this value in bits; for example, enter
-- five megabits as 5000000. When you don\'t set this value, or you set it
-- to zero, MediaConvert calculates the default by doubling the bitrate of
-- this output point.
--
-- 'qualityTuningLevel', 'xavc4kProfileSettings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
--
-- 'slices', 'xavc4kProfileSettings_slices' - Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
newXavc4kProfileSettings ::
  Xavc4kProfileSettings
newXavc4kProfileSettings =
  Xavc4kProfileSettings'
    { bitrateClass =
        Prelude.Nothing,
      codecProfile = Prelude.Nothing,
      flickerAdaptiveQuantization = Prelude.Nothing,
      gopBReference = Prelude.Nothing,
      gopClosedCadence = Prelude.Nothing,
      hrdBufferSize = Prelude.Nothing,
      qualityTuningLevel = Prelude.Nothing,
      slices = Prelude.Nothing
    }

-- | Specify the XAVC 4k (Long GOP) Bitrate Class to set the bitrate of your
-- output. Outputs of the same class have similar image quality over the
-- operating points that are valid for that class.
xavc4kProfileSettings_bitrateClass :: Lens.Lens' Xavc4kProfileSettings (Prelude.Maybe Xavc4kProfileBitrateClass)
xavc4kProfileSettings_bitrateClass = Lens.lens (\Xavc4kProfileSettings' {bitrateClass} -> bitrateClass) (\s@Xavc4kProfileSettings' {} a -> s {bitrateClass = a} :: Xavc4kProfileSettings)

-- | Specify the codec profile for this output. Choose High, 8-bit, 4:2:0
-- (HIGH) or High, 10-bit, 4:2:2 (HIGH_422). These profiles are specified
-- in ITU-T H.264.
xavc4kProfileSettings_codecProfile :: Lens.Lens' Xavc4kProfileSettings (Prelude.Maybe Xavc4kProfileCodecProfile)
xavc4kProfileSettings_codecProfile = Lens.lens (\Xavc4kProfileSettings' {codecProfile} -> codecProfile) (\s@Xavc4kProfileSettings' {} a -> s {codecProfile = a} :: Xavc4kProfileSettings)

-- | The best way to set up adaptive quantization is to keep the default
-- value, Auto (AUTO), for the setting Adaptive quantization
-- (XavcAdaptiveQuantization). When you do so, MediaConvert automatically
-- applies the best types of quantization for your video content. Include
-- this setting in your JSON job specification only when you choose to
-- change the default value for Adaptive quantization. Enable this setting
-- to have the encoder reduce I-frame pop. I-frame pop appears as a visual
-- flicker that can arise when the encoder saves bits by copying some
-- macroblocks many times from frame to frame, and then refreshes them at
-- the I-frame. When you enable this setting, the encoder updates these
-- macroblocks slightly more often to smooth out the flicker. This setting
-- is disabled by default. Related setting: In addition to enabling this
-- setting, you must also set Adaptive quantization (adaptiveQuantization)
-- to a value other than Off (OFF) or Auto (AUTO). Use Adaptive
-- quantization to adjust the degree of smoothing that Flicker adaptive
-- quantization provides.
xavc4kProfileSettings_flickerAdaptiveQuantization :: Lens.Lens' Xavc4kProfileSettings (Prelude.Maybe XavcFlickerAdaptiveQuantization)
xavc4kProfileSettings_flickerAdaptiveQuantization = Lens.lens (\Xavc4kProfileSettings' {flickerAdaptiveQuantization} -> flickerAdaptiveQuantization) (\s@Xavc4kProfileSettings' {} a -> s {flickerAdaptiveQuantization = a} :: Xavc4kProfileSettings)

-- | Specify whether the encoder uses B-frames as reference frames for other
-- pictures in the same GOP. Choose Allow (ENABLED) to allow the encoder to
-- use B-frames as reference frames. Choose Don\'t allow (DISABLED) to
-- prevent the encoder from using B-frames as reference frames.
xavc4kProfileSettings_gopBReference :: Lens.Lens' Xavc4kProfileSettings (Prelude.Maybe XavcGopBReference)
xavc4kProfileSettings_gopBReference = Lens.lens (\Xavc4kProfileSettings' {gopBReference} -> gopBReference) (\s@Xavc4kProfileSettings' {} a -> s {gopBReference = a} :: Xavc4kProfileSettings)

-- | Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
xavc4kProfileSettings_gopClosedCadence :: Lens.Lens' Xavc4kProfileSettings (Prelude.Maybe Prelude.Natural)
xavc4kProfileSettings_gopClosedCadence = Lens.lens (\Xavc4kProfileSettings' {gopClosedCadence} -> gopClosedCadence) (\s@Xavc4kProfileSettings' {} a -> s {gopClosedCadence = a} :: Xavc4kProfileSettings)

-- | Specify the size of the buffer that MediaConvert uses in the HRD buffer
-- model for this output. Specify this value in bits; for example, enter
-- five megabits as 5000000. When you don\'t set this value, or you set it
-- to zero, MediaConvert calculates the default by doubling the bitrate of
-- this output point.
xavc4kProfileSettings_hrdBufferSize :: Lens.Lens' Xavc4kProfileSettings (Prelude.Maybe Prelude.Natural)
xavc4kProfileSettings_hrdBufferSize = Lens.lens (\Xavc4kProfileSettings' {hrdBufferSize} -> hrdBufferSize) (\s@Xavc4kProfileSettings' {} a -> s {hrdBufferSize = a} :: Xavc4kProfileSettings)

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
xavc4kProfileSettings_qualityTuningLevel :: Lens.Lens' Xavc4kProfileSettings (Prelude.Maybe Xavc4kProfileQualityTuningLevel)
xavc4kProfileSettings_qualityTuningLevel = Lens.lens (\Xavc4kProfileSettings' {qualityTuningLevel} -> qualityTuningLevel) (\s@Xavc4kProfileSettings' {} a -> s {qualityTuningLevel = a} :: Xavc4kProfileSettings)

-- | Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
xavc4kProfileSettings_slices :: Lens.Lens' Xavc4kProfileSettings (Prelude.Maybe Prelude.Natural)
xavc4kProfileSettings_slices = Lens.lens (\Xavc4kProfileSettings' {slices} -> slices) (\s@Xavc4kProfileSettings' {} a -> s {slices = a} :: Xavc4kProfileSettings)

instance Data.FromJSON Xavc4kProfileSettings where
  parseJSON =
    Data.withObject
      "Xavc4kProfileSettings"
      ( \x ->
          Xavc4kProfileSettings'
            Prelude.<$> (x Data..:? "bitrateClass")
            Prelude.<*> (x Data..:? "codecProfile")
            Prelude.<*> (x Data..:? "flickerAdaptiveQuantization")
            Prelude.<*> (x Data..:? "gopBReference")
            Prelude.<*> (x Data..:? "gopClosedCadence")
            Prelude.<*> (x Data..:? "hrdBufferSize")
            Prelude.<*> (x Data..:? "qualityTuningLevel")
            Prelude.<*> (x Data..:? "slices")
      )

instance Prelude.Hashable Xavc4kProfileSettings where
  hashWithSalt _salt Xavc4kProfileSettings' {..} =
    _salt
      `Prelude.hashWithSalt` bitrateClass
      `Prelude.hashWithSalt` codecProfile
      `Prelude.hashWithSalt` flickerAdaptiveQuantization
      `Prelude.hashWithSalt` gopBReference
      `Prelude.hashWithSalt` gopClosedCadence
      `Prelude.hashWithSalt` hrdBufferSize
      `Prelude.hashWithSalt` qualityTuningLevel
      `Prelude.hashWithSalt` slices

instance Prelude.NFData Xavc4kProfileSettings where
  rnf Xavc4kProfileSettings' {..} =
    Prelude.rnf bitrateClass `Prelude.seq`
      Prelude.rnf codecProfile `Prelude.seq`
        Prelude.rnf flickerAdaptiveQuantization `Prelude.seq`
          Prelude.rnf gopBReference `Prelude.seq`
            Prelude.rnf gopClosedCadence `Prelude.seq`
              Prelude.rnf hrdBufferSize `Prelude.seq`
                Prelude.rnf qualityTuningLevel `Prelude.seq`
                  Prelude.rnf slices

instance Data.ToJSON Xavc4kProfileSettings where
  toJSON Xavc4kProfileSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitrateClass" Data..=) Prelude.<$> bitrateClass,
            ("codecProfile" Data..=) Prelude.<$> codecProfile,
            ("flickerAdaptiveQuantization" Data..=)
              Prelude.<$> flickerAdaptiveQuantization,
            ("gopBReference" Data..=) Prelude.<$> gopBReference,
            ("gopClosedCadence" Data..=)
              Prelude.<$> gopClosedCadence,
            ("hrdBufferSize" Data..=) Prelude.<$> hrdBufferSize,
            ("qualityTuningLevel" Data..=)
              Prelude.<$> qualityTuningLevel,
            ("slices" Data..=) Prelude.<$> slices
          ]
      )
