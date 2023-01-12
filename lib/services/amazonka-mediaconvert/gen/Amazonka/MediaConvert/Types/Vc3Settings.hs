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
-- Module      : Amazonka.MediaConvert.Types.Vc3Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Vc3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Vc3Class
import Amazonka.MediaConvert.Types.Vc3FramerateControl
import Amazonka.MediaConvert.Types.Vc3FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.Vc3InterlaceMode
import Amazonka.MediaConvert.Types.Vc3ScanTypeConversionMode
import Amazonka.MediaConvert.Types.Vc3SlowPal
import Amazonka.MediaConvert.Types.Vc3Telecine
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VC3
--
-- /See:/ 'newVc3Settings' smart constructor.
data Vc3Settings = Vc3Settings'
  { -- | If you are using the console, use the Framerate setting to specify the
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
    framerateControl :: Prelude.Maybe Vc3FramerateControl,
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
    framerateConversionAlgorithm :: Prelude.Maybe Vc3FramerateConversionAlgorithm,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Choose the scan line type for this output. If you don\'t
    -- specify a value, MediaConvert will create a progressive output.
    interlaceMode :: Prelude.Maybe Vc3InterlaceMode,
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
    scanTypeConversionMode :: Prelude.Maybe Vc3ScanTypeConversionMode,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
    -- per second (fps). Enable slow PAL to create a 25 fps output by
    -- relabeling the video frames and resampling your audio. Note that
    -- enabling this setting will slightly reduce the duration of your video.
    -- Related settings: You must also set Framerate to 25. In your JSON job
    -- specification, set (framerateControl) to (SPECIFIED),
    -- (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Prelude.Maybe Vc3SlowPal,
    -- | When you do frame rate conversion from 23.976 frames per second (fps) to
    -- 29.97 fps, and your output scan type is interlaced, you can optionally
    -- enable hard telecine (HARD) to create a smoother picture. When you keep
    -- the default value, None (NONE), MediaConvert does a standard frame rate
    -- conversion to 29.97 without doing anything with the field polarity to
    -- create a smoother picture.
    telecine :: Prelude.Maybe Vc3Telecine,
    -- | Specify the VC3 class to choose the quality characteristics for this
    -- output. VC3 class, together with the settings Framerate
    -- (framerateNumerator and framerateDenominator) and Resolution (height and
    -- width), determine your output bitrate. For example, say that your video
    -- resolution is 1920x1080 and your framerate is 29.97. Then Class 145
    -- (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps
    -- and Class 220 (CLASS_220) gives you and output with a bitrate of
    -- approximately 220 Mbps. VC3 class also specifies the color bit depth of
    -- your output.
    vc3Class :: Prelude.Maybe Vc3Class
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Vc3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framerateControl', 'vc3Settings_framerateControl' - If you are using the console, use the Framerate setting to specify the
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
-- 'framerateConversionAlgorithm', 'vc3Settings_framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or
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
-- 'framerateDenominator', 'vc3Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'framerateNumerator', 'vc3Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'interlaceMode', 'vc3Settings_interlaceMode' - Optional. Choose the scan line type for this output. If you don\'t
-- specify a value, MediaConvert will create a progressive output.
--
-- 'scanTypeConversionMode', 'vc3Settings_scanTypeConversionMode' - Use this setting for interlaced outputs, when your output frame rate is
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
-- 'slowPal', 'vc3Settings_slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output by
-- relabeling the video frames and resampling your audio. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Related settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- 'telecine', 'vc3Settings_telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard telecine (HARD) to create a smoother picture. When you keep
-- the default value, None (NONE), MediaConvert does a standard frame rate
-- conversion to 29.97 without doing anything with the field polarity to
-- create a smoother picture.
--
-- 'vc3Class', 'vc3Settings_vc3Class' - Specify the VC3 class to choose the quality characteristics for this
-- output. VC3 class, together with the settings Framerate
-- (framerateNumerator and framerateDenominator) and Resolution (height and
-- width), determine your output bitrate. For example, say that your video
-- resolution is 1920x1080 and your framerate is 29.97. Then Class 145
-- (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps
-- and Class 220 (CLASS_220) gives you and output with a bitrate of
-- approximately 220 Mbps. VC3 class also specifies the color bit depth of
-- your output.
newVc3Settings ::
  Vc3Settings
newVc3Settings =
  Vc3Settings'
    { framerateControl = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      interlaceMode = Prelude.Nothing,
      scanTypeConversionMode = Prelude.Nothing,
      slowPal = Prelude.Nothing,
      telecine = Prelude.Nothing,
      vc3Class = Prelude.Nothing
    }

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
vc3Settings_framerateControl :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3FramerateControl)
vc3Settings_framerateControl = Lens.lens (\Vc3Settings' {framerateControl} -> framerateControl) (\s@Vc3Settings' {} a -> s {framerateControl = a} :: Vc3Settings)

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
vc3Settings_framerateConversionAlgorithm :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3FramerateConversionAlgorithm)
vc3Settings_framerateConversionAlgorithm = Lens.lens (\Vc3Settings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@Vc3Settings' {} a -> s {framerateConversionAlgorithm = a} :: Vc3Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
vc3Settings_framerateDenominator :: Lens.Lens' Vc3Settings (Prelude.Maybe Prelude.Natural)
vc3Settings_framerateDenominator = Lens.lens (\Vc3Settings' {framerateDenominator} -> framerateDenominator) (\s@Vc3Settings' {} a -> s {framerateDenominator = a} :: Vc3Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
vc3Settings_framerateNumerator :: Lens.Lens' Vc3Settings (Prelude.Maybe Prelude.Natural)
vc3Settings_framerateNumerator = Lens.lens (\Vc3Settings' {framerateNumerator} -> framerateNumerator) (\s@Vc3Settings' {} a -> s {framerateNumerator = a} :: Vc3Settings)

-- | Optional. Choose the scan line type for this output. If you don\'t
-- specify a value, MediaConvert will create a progressive output.
vc3Settings_interlaceMode :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3InterlaceMode)
vc3Settings_interlaceMode = Lens.lens (\Vc3Settings' {interlaceMode} -> interlaceMode) (\s@Vc3Settings' {} a -> s {interlaceMode = a} :: Vc3Settings)

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
vc3Settings_scanTypeConversionMode :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3ScanTypeConversionMode)
vc3Settings_scanTypeConversionMode = Lens.lens (\Vc3Settings' {scanTypeConversionMode} -> scanTypeConversionMode) (\s@Vc3Settings' {} a -> s {scanTypeConversionMode = a} :: Vc3Settings)

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output by
-- relabeling the video frames and resampling your audio. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Related settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
vc3Settings_slowPal :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3SlowPal)
vc3Settings_slowPal = Lens.lens (\Vc3Settings' {slowPal} -> slowPal) (\s@Vc3Settings' {} a -> s {slowPal = a} :: Vc3Settings)

-- | When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard telecine (HARD) to create a smoother picture. When you keep
-- the default value, None (NONE), MediaConvert does a standard frame rate
-- conversion to 29.97 without doing anything with the field polarity to
-- create a smoother picture.
vc3Settings_telecine :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3Telecine)
vc3Settings_telecine = Lens.lens (\Vc3Settings' {telecine} -> telecine) (\s@Vc3Settings' {} a -> s {telecine = a} :: Vc3Settings)

-- | Specify the VC3 class to choose the quality characteristics for this
-- output. VC3 class, together with the settings Framerate
-- (framerateNumerator and framerateDenominator) and Resolution (height and
-- width), determine your output bitrate. For example, say that your video
-- resolution is 1920x1080 and your framerate is 29.97. Then Class 145
-- (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps
-- and Class 220 (CLASS_220) gives you and output with a bitrate of
-- approximately 220 Mbps. VC3 class also specifies the color bit depth of
-- your output.
vc3Settings_vc3Class :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3Class)
vc3Settings_vc3Class = Lens.lens (\Vc3Settings' {vc3Class} -> vc3Class) (\s@Vc3Settings' {} a -> s {vc3Class = a} :: Vc3Settings)

instance Data.FromJSON Vc3Settings where
  parseJSON =
    Data.withObject
      "Vc3Settings"
      ( \x ->
          Vc3Settings'
            Prelude.<$> (x Data..:? "framerateControl")
            Prelude.<*> (x Data..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Data..:? "framerateDenominator")
            Prelude.<*> (x Data..:? "framerateNumerator")
            Prelude.<*> (x Data..:? "interlaceMode")
            Prelude.<*> (x Data..:? "scanTypeConversionMode")
            Prelude.<*> (x Data..:? "slowPal")
            Prelude.<*> (x Data..:? "telecine")
            Prelude.<*> (x Data..:? "vc3Class")
      )

instance Prelude.Hashable Vc3Settings where
  hashWithSalt _salt Vc3Settings' {..} =
    _salt `Prelude.hashWithSalt` framerateControl
      `Prelude.hashWithSalt` framerateConversionAlgorithm
      `Prelude.hashWithSalt` framerateDenominator
      `Prelude.hashWithSalt` framerateNumerator
      `Prelude.hashWithSalt` interlaceMode
      `Prelude.hashWithSalt` scanTypeConversionMode
      `Prelude.hashWithSalt` slowPal
      `Prelude.hashWithSalt` telecine
      `Prelude.hashWithSalt` vc3Class

instance Prelude.NFData Vc3Settings where
  rnf Vc3Settings' {..} =
    Prelude.rnf framerateControl
      `Prelude.seq` Prelude.rnf framerateConversionAlgorithm
      `Prelude.seq` Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf framerateNumerator
      `Prelude.seq` Prelude.rnf interlaceMode
      `Prelude.seq` Prelude.rnf scanTypeConversionMode
      `Prelude.seq` Prelude.rnf slowPal
      `Prelude.seq` Prelude.rnf telecine
      `Prelude.seq` Prelude.rnf vc3Class

instance Data.ToJSON Vc3Settings where
  toJSON Vc3Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("framerateControl" Data..=)
              Prelude.<$> framerateControl,
            ("framerateConversionAlgorithm" Data..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("framerateDenominator" Data..=)
              Prelude.<$> framerateDenominator,
            ("framerateNumerator" Data..=)
              Prelude.<$> framerateNumerator,
            ("interlaceMode" Data..=) Prelude.<$> interlaceMode,
            ("scanTypeConversionMode" Data..=)
              Prelude.<$> scanTypeConversionMode,
            ("slowPal" Data..=) Prelude.<$> slowPal,
            ("telecine" Data..=) Prelude.<$> telecine,
            ("vc3Class" Data..=) Prelude.<$> vc3Class
          ]
      )
