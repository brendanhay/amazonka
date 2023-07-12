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
-- Module      : Amazonka.MediaConvert.Types.XavcHdProfileSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcHdProfileSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.XavcFlickerAdaptiveQuantization
import Amazonka.MediaConvert.Types.XavcGopBReference
import Amazonka.MediaConvert.Types.XavcHdProfileBitrateClass
import Amazonka.MediaConvert.Types.XavcHdProfileQualityTuningLevel
import Amazonka.MediaConvert.Types.XavcHdProfileTelecine
import Amazonka.MediaConvert.Types.XavcInterlaceMode
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value XAVC_HD.
--
-- /See:/ 'newXavcHdProfileSettings' smart constructor.
data XavcHdProfileSettings = XavcHdProfileSettings'
  { -- | Specify the XAVC HD (Long GOP) Bitrate Class to set the bitrate of your
    -- output. Outputs of the same class have similar image quality over the
    -- operating points that are valid for that class.
    bitrateClass :: Prelude.Maybe XavcHdProfileBitrateClass,
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
    -- | Choose the scan line type for the output. Keep the default value,
    -- Progressive (PROGRESSIVE) to create a progressive output, regardless of
    -- the scan type of your input. Use Top field first (TOP_FIELD) or Bottom
    -- field first (BOTTOM_FIELD) to create an output that\'s interlaced with
    -- the same field polarity throughout. Use Follow, default top
    -- (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to
    -- produce outputs with the same field polarity as the source. For jobs
    -- that have multiple inputs, the output field polarity might change over
    -- the course of the output. Follow behavior depends on the input scan
    -- type. If the source is interlaced, the output will be interlaced with
    -- the same polarity as the source. If the source is progressive, the
    -- output will be interlaced with top field bottom field first, depending
    -- on which of the Follow options you choose.
    interlaceMode :: Prelude.Maybe XavcInterlaceMode,
    -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
    -- you want to trade off encoding speed for output video quality. The
    -- default behavior is faster, lower quality, single-pass encoding.
    qualityTuningLevel :: Prelude.Maybe XavcHdProfileQualityTuningLevel,
    -- | Number of slices per picture. Must be less than or equal to the number
    -- of macroblock rows for progressive pictures, and less than or equal to
    -- half the number of macroblock rows for interlaced pictures.
    slices :: Prelude.Maybe Prelude.Natural,
    -- | Ignore this setting unless you set Frame rate (framerateNumerator
    -- divided by framerateDenominator) to 29.970. If your input framerate is
    -- 23.976, choose Hard (HARD). Otherwise, keep the default value None
    -- (NONE). For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-telecine-and-inverse-telecine.html.
    telecine :: Prelude.Maybe XavcHdProfileTelecine
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'XavcHdProfileSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitrateClass', 'xavcHdProfileSettings_bitrateClass' - Specify the XAVC HD (Long GOP) Bitrate Class to set the bitrate of your
-- output. Outputs of the same class have similar image quality over the
-- operating points that are valid for that class.
--
-- 'flickerAdaptiveQuantization', 'xavcHdProfileSettings_flickerAdaptiveQuantization' - The best way to set up adaptive quantization is to keep the default
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
-- 'gopBReference', 'xavcHdProfileSettings_gopBReference' - Specify whether the encoder uses B-frames as reference frames for other
-- pictures in the same GOP. Choose Allow (ENABLED) to allow the encoder to
-- use B-frames as reference frames. Choose Don\'t allow (DISABLED) to
-- prevent the encoder from using B-frames as reference frames.
--
-- 'gopClosedCadence', 'xavcHdProfileSettings_gopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
--
-- 'hrdBufferSize', 'xavcHdProfileSettings_hrdBufferSize' - Specify the size of the buffer that MediaConvert uses in the HRD buffer
-- model for this output. Specify this value in bits; for example, enter
-- five megabits as 5000000. When you don\'t set this value, or you set it
-- to zero, MediaConvert calculates the default by doubling the bitrate of
-- this output point.
--
-- 'interlaceMode', 'xavcHdProfileSettings_interlaceMode' - Choose the scan line type for the output. Keep the default value,
-- Progressive (PROGRESSIVE) to create a progressive output, regardless of
-- the scan type of your input. Use Top field first (TOP_FIELD) or Bottom
-- field first (BOTTOM_FIELD) to create an output that\'s interlaced with
-- the same field polarity throughout. Use Follow, default top
-- (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to
-- produce outputs with the same field polarity as the source. For jobs
-- that have multiple inputs, the output field polarity might change over
-- the course of the output. Follow behavior depends on the input scan
-- type. If the source is interlaced, the output will be interlaced with
-- the same polarity as the source. If the source is progressive, the
-- output will be interlaced with top field bottom field first, depending
-- on which of the Follow options you choose.
--
-- 'qualityTuningLevel', 'xavcHdProfileSettings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
--
-- 'slices', 'xavcHdProfileSettings_slices' - Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
--
-- 'telecine', 'xavcHdProfileSettings_telecine' - Ignore this setting unless you set Frame rate (framerateNumerator
-- divided by framerateDenominator) to 29.970. If your input framerate is
-- 23.976, choose Hard (HARD). Otherwise, keep the default value None
-- (NONE). For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-telecine-and-inverse-telecine.html.
newXavcHdProfileSettings ::
  XavcHdProfileSettings
newXavcHdProfileSettings =
  XavcHdProfileSettings'
    { bitrateClass =
        Prelude.Nothing,
      flickerAdaptiveQuantization = Prelude.Nothing,
      gopBReference = Prelude.Nothing,
      gopClosedCadence = Prelude.Nothing,
      hrdBufferSize = Prelude.Nothing,
      interlaceMode = Prelude.Nothing,
      qualityTuningLevel = Prelude.Nothing,
      slices = Prelude.Nothing,
      telecine = Prelude.Nothing
    }

-- | Specify the XAVC HD (Long GOP) Bitrate Class to set the bitrate of your
-- output. Outputs of the same class have similar image quality over the
-- operating points that are valid for that class.
xavcHdProfileSettings_bitrateClass :: Lens.Lens' XavcHdProfileSettings (Prelude.Maybe XavcHdProfileBitrateClass)
xavcHdProfileSettings_bitrateClass = Lens.lens (\XavcHdProfileSettings' {bitrateClass} -> bitrateClass) (\s@XavcHdProfileSettings' {} a -> s {bitrateClass = a} :: XavcHdProfileSettings)

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
xavcHdProfileSettings_flickerAdaptiveQuantization :: Lens.Lens' XavcHdProfileSettings (Prelude.Maybe XavcFlickerAdaptiveQuantization)
xavcHdProfileSettings_flickerAdaptiveQuantization = Lens.lens (\XavcHdProfileSettings' {flickerAdaptiveQuantization} -> flickerAdaptiveQuantization) (\s@XavcHdProfileSettings' {} a -> s {flickerAdaptiveQuantization = a} :: XavcHdProfileSettings)

-- | Specify whether the encoder uses B-frames as reference frames for other
-- pictures in the same GOP. Choose Allow (ENABLED) to allow the encoder to
-- use B-frames as reference frames. Choose Don\'t allow (DISABLED) to
-- prevent the encoder from using B-frames as reference frames.
xavcHdProfileSettings_gopBReference :: Lens.Lens' XavcHdProfileSettings (Prelude.Maybe XavcGopBReference)
xavcHdProfileSettings_gopBReference = Lens.lens (\XavcHdProfileSettings' {gopBReference} -> gopBReference) (\s@XavcHdProfileSettings' {} a -> s {gopBReference = a} :: XavcHdProfileSettings)

-- | Frequency of closed GOPs. In streaming applications, it is recommended
-- that this be set to 1 so a decoder joining mid-stream will receive an
-- IDR frame as quickly as possible. Setting this value to 0 will break
-- output segmenting.
xavcHdProfileSettings_gopClosedCadence :: Lens.Lens' XavcHdProfileSettings (Prelude.Maybe Prelude.Natural)
xavcHdProfileSettings_gopClosedCadence = Lens.lens (\XavcHdProfileSettings' {gopClosedCadence} -> gopClosedCadence) (\s@XavcHdProfileSettings' {} a -> s {gopClosedCadence = a} :: XavcHdProfileSettings)

-- | Specify the size of the buffer that MediaConvert uses in the HRD buffer
-- model for this output. Specify this value in bits; for example, enter
-- five megabits as 5000000. When you don\'t set this value, or you set it
-- to zero, MediaConvert calculates the default by doubling the bitrate of
-- this output point.
xavcHdProfileSettings_hrdBufferSize :: Lens.Lens' XavcHdProfileSettings (Prelude.Maybe Prelude.Natural)
xavcHdProfileSettings_hrdBufferSize = Lens.lens (\XavcHdProfileSettings' {hrdBufferSize} -> hrdBufferSize) (\s@XavcHdProfileSettings' {} a -> s {hrdBufferSize = a} :: XavcHdProfileSettings)

-- | Choose the scan line type for the output. Keep the default value,
-- Progressive (PROGRESSIVE) to create a progressive output, regardless of
-- the scan type of your input. Use Top field first (TOP_FIELD) or Bottom
-- field first (BOTTOM_FIELD) to create an output that\'s interlaced with
-- the same field polarity throughout. Use Follow, default top
-- (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to
-- produce outputs with the same field polarity as the source. For jobs
-- that have multiple inputs, the output field polarity might change over
-- the course of the output. Follow behavior depends on the input scan
-- type. If the source is interlaced, the output will be interlaced with
-- the same polarity as the source. If the source is progressive, the
-- output will be interlaced with top field bottom field first, depending
-- on which of the Follow options you choose.
xavcHdProfileSettings_interlaceMode :: Lens.Lens' XavcHdProfileSettings (Prelude.Maybe XavcInterlaceMode)
xavcHdProfileSettings_interlaceMode = Lens.lens (\XavcHdProfileSettings' {interlaceMode} -> interlaceMode) (\s@XavcHdProfileSettings' {} a -> s {interlaceMode = a} :: XavcHdProfileSettings)

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
xavcHdProfileSettings_qualityTuningLevel :: Lens.Lens' XavcHdProfileSettings (Prelude.Maybe XavcHdProfileQualityTuningLevel)
xavcHdProfileSettings_qualityTuningLevel = Lens.lens (\XavcHdProfileSettings' {qualityTuningLevel} -> qualityTuningLevel) (\s@XavcHdProfileSettings' {} a -> s {qualityTuningLevel = a} :: XavcHdProfileSettings)

-- | Number of slices per picture. Must be less than or equal to the number
-- of macroblock rows for progressive pictures, and less than or equal to
-- half the number of macroblock rows for interlaced pictures.
xavcHdProfileSettings_slices :: Lens.Lens' XavcHdProfileSettings (Prelude.Maybe Prelude.Natural)
xavcHdProfileSettings_slices = Lens.lens (\XavcHdProfileSettings' {slices} -> slices) (\s@XavcHdProfileSettings' {} a -> s {slices = a} :: XavcHdProfileSettings)

-- | Ignore this setting unless you set Frame rate (framerateNumerator
-- divided by framerateDenominator) to 29.970. If your input framerate is
-- 23.976, choose Hard (HARD). Otherwise, keep the default value None
-- (NONE). For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-telecine-and-inverse-telecine.html.
xavcHdProfileSettings_telecine :: Lens.Lens' XavcHdProfileSettings (Prelude.Maybe XavcHdProfileTelecine)
xavcHdProfileSettings_telecine = Lens.lens (\XavcHdProfileSettings' {telecine} -> telecine) (\s@XavcHdProfileSettings' {} a -> s {telecine = a} :: XavcHdProfileSettings)

instance Data.FromJSON XavcHdProfileSettings where
  parseJSON =
    Data.withObject
      "XavcHdProfileSettings"
      ( \x ->
          XavcHdProfileSettings'
            Prelude.<$> (x Data..:? "bitrateClass")
            Prelude.<*> (x Data..:? "flickerAdaptiveQuantization")
            Prelude.<*> (x Data..:? "gopBReference")
            Prelude.<*> (x Data..:? "gopClosedCadence")
            Prelude.<*> (x Data..:? "hrdBufferSize")
            Prelude.<*> (x Data..:? "interlaceMode")
            Prelude.<*> (x Data..:? "qualityTuningLevel")
            Prelude.<*> (x Data..:? "slices")
            Prelude.<*> (x Data..:? "telecine")
      )

instance Prelude.Hashable XavcHdProfileSettings where
  hashWithSalt _salt XavcHdProfileSettings' {..} =
    _salt
      `Prelude.hashWithSalt` bitrateClass
      `Prelude.hashWithSalt` flickerAdaptiveQuantization
      `Prelude.hashWithSalt` gopBReference
      `Prelude.hashWithSalt` gopClosedCadence
      `Prelude.hashWithSalt` hrdBufferSize
      `Prelude.hashWithSalt` interlaceMode
      `Prelude.hashWithSalt` qualityTuningLevel
      `Prelude.hashWithSalt` slices
      `Prelude.hashWithSalt` telecine

instance Prelude.NFData XavcHdProfileSettings where
  rnf XavcHdProfileSettings' {..} =
    Prelude.rnf bitrateClass
      `Prelude.seq` Prelude.rnf flickerAdaptiveQuantization
      `Prelude.seq` Prelude.rnf gopBReference
      `Prelude.seq` Prelude.rnf gopClosedCadence
      `Prelude.seq` Prelude.rnf hrdBufferSize
      `Prelude.seq` Prelude.rnf interlaceMode
      `Prelude.seq` Prelude.rnf qualityTuningLevel
      `Prelude.seq` Prelude.rnf slices
      `Prelude.seq` Prelude.rnf telecine

instance Data.ToJSON XavcHdProfileSettings where
  toJSON XavcHdProfileSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitrateClass" Data..=) Prelude.<$> bitrateClass,
            ("flickerAdaptiveQuantization" Data..=)
              Prelude.<$> flickerAdaptiveQuantization,
            ("gopBReference" Data..=) Prelude.<$> gopBReference,
            ("gopClosedCadence" Data..=)
              Prelude.<$> gopClosedCadence,
            ("hrdBufferSize" Data..=) Prelude.<$> hrdBufferSize,
            ("interlaceMode" Data..=) Prelude.<$> interlaceMode,
            ("qualityTuningLevel" Data..=)
              Prelude.<$> qualityTuningLevel,
            ("slices" Data..=) Prelude.<$> slices,
            ("telecine" Data..=) Prelude.<$> telecine
          ]
      )
