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
-- Module      : Amazonka.MediaConvert.Types.AvcIntraSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AvcIntraSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AvcIntraClass
import Amazonka.MediaConvert.Types.AvcIntraFramerateControl
import Amazonka.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.AvcIntraInterlaceMode
import Amazonka.MediaConvert.Types.AvcIntraScanTypeConversionMode
import Amazonka.MediaConvert.Types.AvcIntraSlowPal
import Amazonka.MediaConvert.Types.AvcIntraTelecine
import Amazonka.MediaConvert.Types.AvcIntraUhdSettings
import qualified Amazonka.Prelude as Prelude

-- | Required when you choose AVC-Intra for your output video codec. For more
-- information about the AVC-Intra settings, see the relevant
-- specification. For detailed information about SD and HD in AVC-Intra,
-- see https:\/\/ieeexplore.ieee.org\/document\/7290936. For information
-- about 4K\/2K in AVC-Intra, see
-- https:\/\/pro-av.panasonic.net\/en\/avc-ultra\/AVC-ULTRAoverview.pdf.
--
-- /See:/ 'newAvcIntraSettings' smart constructor.
data AvcIntraSettings = AvcIntraSettings'
  { -- | When you do frame rate conversion from 23.976 frames per second (fps) to
    -- 29.97 fps, and your output scan type is interlaced, you can optionally
    -- enable hard telecine (HARD) to create a smoother picture. When you keep
    -- the default value, None (NONE), MediaConvert does a standard frame rate
    -- conversion to 29.97 without doing anything with the field polarity to
    -- create a smoother picture.
    telecine :: Prelude.Maybe AvcIntraTelecine,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | Use this setting for interlaced outputs, when your output frame rate is
    -- half of your input frame rate. In this situation, choose Optimized
    -- interlacing (INTERLACED_OPTIMIZE) to create a better quality interlaced
    -- output. In this case, each progressive frame from the input corresponds
    -- to an interlaced field in the output. Keep the default value, Basic
    -- interlacing (INTERLACED), for all other output frame rates. With basic
    -- interlacing, MediaConvert performs any frame rate conversion first and
    -- then interlaces the frames. When you choose Optimized interlacing and
    -- you set your output frame rate to a value that isn\'t suitable for
    -- optimized interlacing, MediaConvert automatically falls back to basic
    -- interlacing. Required settings: To use optimized interlacing, you must
    -- set Telecine (telecine) to None (NONE) or Soft (SOFT). You can\'t use
    -- optimized interlacing for hard telecine outputs. You must also set
    -- Interlace mode (interlaceMode) to a value other than Progressive
    -- (PROGRESSIVE).
    scanTypeConversionMode :: Prelude.Maybe AvcIntraScanTypeConversionMode,
    -- | If you are using the console, use the Framerate setting to specify the
    -- frame rate for this output. If you want to keep the same frame rate as
    -- the input video, choose Follow source. If you want to do frame rate
    -- conversion, choose a frame rate from the dropdown list or choose Custom.
    -- The framerates shown in the dropdown list are decimal approximations of
    -- fractions. If you choose Custom, specify your frame rate as a fraction.
    -- If you are creating your transcoding job specification as a JSON file
    -- without the console, use FramerateControl to specify which value the
    -- service uses for the frame rate for this output. Choose
    -- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
    -- from the input. Choose SPECIFIED if you want the service to use the
    -- frame rate you specify in the settings FramerateNumerator and
    -- FramerateDenominator.
    framerateControl :: Prelude.Maybe AvcIntraFramerateControl,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Optional when you set AVC-Intra class (avcIntraClass) to Class 4K\/2K
    -- (CLASS_4K_2K). When you set AVC-Intra class to a different value, this
    -- object isn\'t allowed.
    avcIntraUhdSettings :: Prelude.Maybe AvcIntraUhdSettings,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
    -- per second (fps). Enable slow PAL to create a 25 fps output. When you
    -- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
    -- resamples your audio to keep it synchronized with the video. Note that
    -- enabling this setting will slightly reduce the duration of your video.
    -- Required settings: You must also set Framerate to 25. In your JSON job
    -- specification, set (framerateControl) to (SPECIFIED),
    -- (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Prelude.Maybe AvcIntraSlowPal,
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
    interlaceMode :: Prelude.Maybe AvcIntraInterlaceMode,
    -- | Choose the method that you want MediaConvert to use when increasing or
    -- decreasing the frame rate. We recommend using drop duplicate
    -- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
    -- 30 fps. For numerically complex conversions, you can use interpolate
    -- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
    -- might introduce undesirable video artifacts. For complex frame rate
    -- conversions, especially if your source video has already been converted
    -- from its original cadence, use FrameFormer (FRAMEFORMER) to do
    -- motion-compensated interpolation. FrameFormer chooses the best
    -- conversion method frame by frame. Note that using FrameFormer increases
    -- the transcoding time and incurs a significant add-on cost.
    framerateConversionAlgorithm :: Prelude.Maybe AvcIntraFramerateConversionAlgorithm,
    -- | Specify the AVC-Intra class of your output. The AVC-Intra class
    -- selection determines the output video bit rate depending on the frame
    -- rate of the output. Outputs with higher class values have higher
    -- bitrates and improved image quality. Note that for Class 4K\/2K,
    -- MediaConvert supports only 4:2:2 chroma subsampling.
    avcIntraClass :: Prelude.Maybe AvcIntraClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvcIntraSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'telecine', 'avcIntraSettings_telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard telecine (HARD) to create a smoother picture. When you keep
-- the default value, None (NONE), MediaConvert does a standard frame rate
-- conversion to 29.97 without doing anything with the field polarity to
-- create a smoother picture.
--
-- 'framerateDenominator', 'avcIntraSettings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'scanTypeConversionMode', 'avcIntraSettings_scanTypeConversionMode' - Use this setting for interlaced outputs, when your output frame rate is
-- half of your input frame rate. In this situation, choose Optimized
-- interlacing (INTERLACED_OPTIMIZE) to create a better quality interlaced
-- output. In this case, each progressive frame from the input corresponds
-- to an interlaced field in the output. Keep the default value, Basic
-- interlacing (INTERLACED), for all other output frame rates. With basic
-- interlacing, MediaConvert performs any frame rate conversion first and
-- then interlaces the frames. When you choose Optimized interlacing and
-- you set your output frame rate to a value that isn\'t suitable for
-- optimized interlacing, MediaConvert automatically falls back to basic
-- interlacing. Required settings: To use optimized interlacing, you must
-- set Telecine (telecine) to None (NONE) or Soft (SOFT). You can\'t use
-- optimized interlacing for hard telecine outputs. You must also set
-- Interlace mode (interlaceMode) to a value other than Progressive
-- (PROGRESSIVE).
--
-- 'framerateControl', 'avcIntraSettings_framerateControl' - If you are using the console, use the Framerate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list or choose Custom.
-- The framerates shown in the dropdown list are decimal approximations of
-- fractions. If you choose Custom, specify your frame rate as a fraction.
-- If you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate you specify in the settings FramerateNumerator and
-- FramerateDenominator.
--
-- 'framerateNumerator', 'avcIntraSettings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'avcIntraUhdSettings', 'avcIntraSettings_avcIntraUhdSettings' - Optional when you set AVC-Intra class (avcIntraClass) to Class 4K\/2K
-- (CLASS_4K_2K). When you set AVC-Intra class to a different value, this
-- object isn\'t allowed.
--
-- 'slowPal', 'avcIntraSettings_slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- 'interlaceMode', 'avcIntraSettings_interlaceMode' - Choose the scan line type for the output. Keep the default value,
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
-- 'framerateConversionAlgorithm', 'avcIntraSettings_framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or
-- decreasing the frame rate. We recommend using drop duplicate
-- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
-- 30 fps. For numerically complex conversions, you can use interpolate
-- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
-- might introduce undesirable video artifacts. For complex frame rate
-- conversions, especially if your source video has already been converted
-- from its original cadence, use FrameFormer (FRAMEFORMER) to do
-- motion-compensated interpolation. FrameFormer chooses the best
-- conversion method frame by frame. Note that using FrameFormer increases
-- the transcoding time and incurs a significant add-on cost.
--
-- 'avcIntraClass', 'avcIntraSettings_avcIntraClass' - Specify the AVC-Intra class of your output. The AVC-Intra class
-- selection determines the output video bit rate depending on the frame
-- rate of the output. Outputs with higher class values have higher
-- bitrates and improved image quality. Note that for Class 4K\/2K,
-- MediaConvert supports only 4:2:2 chroma subsampling.
newAvcIntraSettings ::
  AvcIntraSettings
newAvcIntraSettings =
  AvcIntraSettings'
    { telecine = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      scanTypeConversionMode = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      avcIntraUhdSettings = Prelude.Nothing,
      slowPal = Prelude.Nothing,
      interlaceMode = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      avcIntraClass = Prelude.Nothing
    }

-- | When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard telecine (HARD) to create a smoother picture. When you keep
-- the default value, None (NONE), MediaConvert does a standard frame rate
-- conversion to 29.97 without doing anything with the field polarity to
-- create a smoother picture.
avcIntraSettings_telecine :: Lens.Lens' AvcIntraSettings (Prelude.Maybe AvcIntraTelecine)
avcIntraSettings_telecine = Lens.lens (\AvcIntraSettings' {telecine} -> telecine) (\s@AvcIntraSettings' {} a -> s {telecine = a} :: AvcIntraSettings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
avcIntraSettings_framerateDenominator :: Lens.Lens' AvcIntraSettings (Prelude.Maybe Prelude.Natural)
avcIntraSettings_framerateDenominator = Lens.lens (\AvcIntraSettings' {framerateDenominator} -> framerateDenominator) (\s@AvcIntraSettings' {} a -> s {framerateDenominator = a} :: AvcIntraSettings)

-- | Use this setting for interlaced outputs, when your output frame rate is
-- half of your input frame rate. In this situation, choose Optimized
-- interlacing (INTERLACED_OPTIMIZE) to create a better quality interlaced
-- output. In this case, each progressive frame from the input corresponds
-- to an interlaced field in the output. Keep the default value, Basic
-- interlacing (INTERLACED), for all other output frame rates. With basic
-- interlacing, MediaConvert performs any frame rate conversion first and
-- then interlaces the frames. When you choose Optimized interlacing and
-- you set your output frame rate to a value that isn\'t suitable for
-- optimized interlacing, MediaConvert automatically falls back to basic
-- interlacing. Required settings: To use optimized interlacing, you must
-- set Telecine (telecine) to None (NONE) or Soft (SOFT). You can\'t use
-- optimized interlacing for hard telecine outputs. You must also set
-- Interlace mode (interlaceMode) to a value other than Progressive
-- (PROGRESSIVE).
avcIntraSettings_scanTypeConversionMode :: Lens.Lens' AvcIntraSettings (Prelude.Maybe AvcIntraScanTypeConversionMode)
avcIntraSettings_scanTypeConversionMode = Lens.lens (\AvcIntraSettings' {scanTypeConversionMode} -> scanTypeConversionMode) (\s@AvcIntraSettings' {} a -> s {scanTypeConversionMode = a} :: AvcIntraSettings)

-- | If you are using the console, use the Framerate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list or choose Custom.
-- The framerates shown in the dropdown list are decimal approximations of
-- fractions. If you choose Custom, specify your frame rate as a fraction.
-- If you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate you specify in the settings FramerateNumerator and
-- FramerateDenominator.
avcIntraSettings_framerateControl :: Lens.Lens' AvcIntraSettings (Prelude.Maybe AvcIntraFramerateControl)
avcIntraSettings_framerateControl = Lens.lens (\AvcIntraSettings' {framerateControl} -> framerateControl) (\s@AvcIntraSettings' {} a -> s {framerateControl = a} :: AvcIntraSettings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
avcIntraSettings_framerateNumerator :: Lens.Lens' AvcIntraSettings (Prelude.Maybe Prelude.Natural)
avcIntraSettings_framerateNumerator = Lens.lens (\AvcIntraSettings' {framerateNumerator} -> framerateNumerator) (\s@AvcIntraSettings' {} a -> s {framerateNumerator = a} :: AvcIntraSettings)

-- | Optional when you set AVC-Intra class (avcIntraClass) to Class 4K\/2K
-- (CLASS_4K_2K). When you set AVC-Intra class to a different value, this
-- object isn\'t allowed.
avcIntraSettings_avcIntraUhdSettings :: Lens.Lens' AvcIntraSettings (Prelude.Maybe AvcIntraUhdSettings)
avcIntraSettings_avcIntraUhdSettings = Lens.lens (\AvcIntraSettings' {avcIntraUhdSettings} -> avcIntraUhdSettings) (\s@AvcIntraSettings' {} a -> s {avcIntraUhdSettings = a} :: AvcIntraSettings)

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
avcIntraSettings_slowPal :: Lens.Lens' AvcIntraSettings (Prelude.Maybe AvcIntraSlowPal)
avcIntraSettings_slowPal = Lens.lens (\AvcIntraSettings' {slowPal} -> slowPal) (\s@AvcIntraSettings' {} a -> s {slowPal = a} :: AvcIntraSettings)

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
avcIntraSettings_interlaceMode :: Lens.Lens' AvcIntraSettings (Prelude.Maybe AvcIntraInterlaceMode)
avcIntraSettings_interlaceMode = Lens.lens (\AvcIntraSettings' {interlaceMode} -> interlaceMode) (\s@AvcIntraSettings' {} a -> s {interlaceMode = a} :: AvcIntraSettings)

-- | Choose the method that you want MediaConvert to use when increasing or
-- decreasing the frame rate. We recommend using drop duplicate
-- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
-- 30 fps. For numerically complex conversions, you can use interpolate
-- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
-- might introduce undesirable video artifacts. For complex frame rate
-- conversions, especially if your source video has already been converted
-- from its original cadence, use FrameFormer (FRAMEFORMER) to do
-- motion-compensated interpolation. FrameFormer chooses the best
-- conversion method frame by frame. Note that using FrameFormer increases
-- the transcoding time and incurs a significant add-on cost.
avcIntraSettings_framerateConversionAlgorithm :: Lens.Lens' AvcIntraSettings (Prelude.Maybe AvcIntraFramerateConversionAlgorithm)
avcIntraSettings_framerateConversionAlgorithm = Lens.lens (\AvcIntraSettings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@AvcIntraSettings' {} a -> s {framerateConversionAlgorithm = a} :: AvcIntraSettings)

-- | Specify the AVC-Intra class of your output. The AVC-Intra class
-- selection determines the output video bit rate depending on the frame
-- rate of the output. Outputs with higher class values have higher
-- bitrates and improved image quality. Note that for Class 4K\/2K,
-- MediaConvert supports only 4:2:2 chroma subsampling.
avcIntraSettings_avcIntraClass :: Lens.Lens' AvcIntraSettings (Prelude.Maybe AvcIntraClass)
avcIntraSettings_avcIntraClass = Lens.lens (\AvcIntraSettings' {avcIntraClass} -> avcIntraClass) (\s@AvcIntraSettings' {} a -> s {avcIntraClass = a} :: AvcIntraSettings)

instance Data.FromJSON AvcIntraSettings where
  parseJSON =
    Data.withObject
      "AvcIntraSettings"
      ( \x ->
          AvcIntraSettings'
            Prelude.<$> (x Data..:? "telecine")
            Prelude.<*> (x Data..:? "framerateDenominator")
            Prelude.<*> (x Data..:? "scanTypeConversionMode")
            Prelude.<*> (x Data..:? "framerateControl")
            Prelude.<*> (x Data..:? "framerateNumerator")
            Prelude.<*> (x Data..:? "avcIntraUhdSettings")
            Prelude.<*> (x Data..:? "slowPal")
            Prelude.<*> (x Data..:? "interlaceMode")
            Prelude.<*> (x Data..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Data..:? "avcIntraClass")
      )

instance Prelude.Hashable AvcIntraSettings where
  hashWithSalt _salt AvcIntraSettings' {..} =
    _salt `Prelude.hashWithSalt` telecine
      `Prelude.hashWithSalt` framerateDenominator
      `Prelude.hashWithSalt` scanTypeConversionMode
      `Prelude.hashWithSalt` framerateControl
      `Prelude.hashWithSalt` framerateNumerator
      `Prelude.hashWithSalt` avcIntraUhdSettings
      `Prelude.hashWithSalt` slowPal
      `Prelude.hashWithSalt` interlaceMode
      `Prelude.hashWithSalt` framerateConversionAlgorithm
      `Prelude.hashWithSalt` avcIntraClass

instance Prelude.NFData AvcIntraSettings where
  rnf AvcIntraSettings' {..} =
    Prelude.rnf telecine
      `Prelude.seq` Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf scanTypeConversionMode
      `Prelude.seq` Prelude.rnf framerateControl
      `Prelude.seq` Prelude.rnf framerateNumerator
      `Prelude.seq` Prelude.rnf avcIntraUhdSettings
      `Prelude.seq` Prelude.rnf slowPal
      `Prelude.seq` Prelude.rnf interlaceMode
      `Prelude.seq` Prelude.rnf framerateConversionAlgorithm
      `Prelude.seq` Prelude.rnf avcIntraClass

instance Data.ToJSON AvcIntraSettings where
  toJSON AvcIntraSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("telecine" Data..=) Prelude.<$> telecine,
            ("framerateDenominator" Data..=)
              Prelude.<$> framerateDenominator,
            ("scanTypeConversionMode" Data..=)
              Prelude.<$> scanTypeConversionMode,
            ("framerateControl" Data..=)
              Prelude.<$> framerateControl,
            ("framerateNumerator" Data..=)
              Prelude.<$> framerateNumerator,
            ("avcIntraUhdSettings" Data..=)
              Prelude.<$> avcIntraUhdSettings,
            ("slowPal" Data..=) Prelude.<$> slowPal,
            ("interlaceMode" Data..=) Prelude.<$> interlaceMode,
            ("framerateConversionAlgorithm" Data..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("avcIntraClass" Data..=) Prelude.<$> avcIntraClass
          ]
      )
