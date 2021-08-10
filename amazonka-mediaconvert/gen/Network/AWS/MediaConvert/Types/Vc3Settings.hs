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
-- Module      : Network.AWS.MediaConvert.Types.Vc3Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Vc3Class
import Network.AWS.MediaConvert.Types.Vc3FramerateControl
import Network.AWS.MediaConvert.Types.Vc3FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vc3InterlaceMode
import Network.AWS.MediaConvert.Types.Vc3ScanTypeConversionMode
import Network.AWS.MediaConvert.Types.Vc3SlowPal
import Network.AWS.MediaConvert.Types.Vc3Telecine
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VC3
--
-- /See:/ 'newVc3Settings' smart constructor.
data Vc3Settings = Vc3Settings'
  { -- | Optional. Choose the scan line type for this output. If you don\'t
    -- specify a value, MediaConvert will create a progressive output.
    interlaceMode :: Prelude.Maybe Vc3InterlaceMode,
    -- | When you do frame rate conversion from 23.976 frames per second (fps) to
    -- 29.97 fps, and your output scan type is interlaced, you can optionally
    -- enable hard telecine (HARD) to create a smoother picture. When you keep
    -- the default value, None (NONE), MediaConvert does a standard frame rate
    -- conversion to 29.97 without doing anything with the field polarity to
    -- create a smoother picture.
    telecine :: Prelude.Maybe Vc3Telecine,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
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
    scanTypeConversionMode :: Prelude.Maybe Vc3ScanTypeConversionMode,
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
    -- | Specify the VC3 class to choose the quality characteristics for this
    -- output. VC3 class, together with the settings Framerate
    -- (framerateNumerator and framerateDenominator) and Resolution (height and
    -- width), determine your output bitrate. For example, say that your video
    -- resolution is 1920x1080 and your framerate is 29.97. Then Class 145
    -- (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps
    -- and Class 220 (CLASS_220) gives you and output with a bitrate of
    -- approximately 220 Mbps. VC3 class also specifies the color bit depth of
    -- your output.
    vc3Class :: Prelude.Maybe Vc3Class,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
    -- per second (fps). Enable slow PAL to create a 25 fps output by
    -- relabeling the video frames and resampling your audio. Note that
    -- enabling this setting will slightly reduce the duration of your video.
    -- Related settings: You must also set Framerate to 25. In your JSON job
    -- specification, set (framerateControl) to (SPECIFIED),
    -- (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Prelude.Maybe Vc3SlowPal
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
-- 'interlaceMode', 'vc3Settings_interlaceMode' - Optional. Choose the scan line type for this output. If you don\'t
-- specify a value, MediaConvert will create a progressive output.
--
-- 'telecine', 'vc3Settings_telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard telecine (HARD) to create a smoother picture. When you keep
-- the default value, None (NONE), MediaConvert does a standard frame rate
-- conversion to 29.97 without doing anything with the field polarity to
-- create a smoother picture.
--
-- 'framerateNumerator', 'vc3Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'framerateDenominator', 'vc3Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
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
-- 'vc3Class', 'vc3Settings_vc3Class' - Specify the VC3 class to choose the quality characteristics for this
-- output. VC3 class, together with the settings Framerate
-- (framerateNumerator and framerateDenominator) and Resolution (height and
-- width), determine your output bitrate. For example, say that your video
-- resolution is 1920x1080 and your framerate is 29.97. Then Class 145
-- (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps
-- and Class 220 (CLASS_220) gives you and output with a bitrate of
-- approximately 220 Mbps. VC3 class also specifies the color bit depth of
-- your output.
--
-- 'slowPal', 'vc3Settings_slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output by
-- relabeling the video frames and resampling your audio. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Related settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
newVc3Settings ::
  Vc3Settings
newVc3Settings =
  Vc3Settings'
    { interlaceMode = Prelude.Nothing,
      telecine = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      scanTypeConversionMode = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      vc3Class = Prelude.Nothing,
      slowPal = Prelude.Nothing
    }

-- | Optional. Choose the scan line type for this output. If you don\'t
-- specify a value, MediaConvert will create a progressive output.
vc3Settings_interlaceMode :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3InterlaceMode)
vc3Settings_interlaceMode = Lens.lens (\Vc3Settings' {interlaceMode} -> interlaceMode) (\s@Vc3Settings' {} a -> s {interlaceMode = a} :: Vc3Settings)

-- | When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard telecine (HARD) to create a smoother picture. When you keep
-- the default value, None (NONE), MediaConvert does a standard frame rate
-- conversion to 29.97 without doing anything with the field polarity to
-- create a smoother picture.
vc3Settings_telecine :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3Telecine)
vc3Settings_telecine = Lens.lens (\Vc3Settings' {telecine} -> telecine) (\s@Vc3Settings' {} a -> s {telecine = a} :: Vc3Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
vc3Settings_framerateNumerator :: Lens.Lens' Vc3Settings (Prelude.Maybe Prelude.Natural)
vc3Settings_framerateNumerator = Lens.lens (\Vc3Settings' {framerateNumerator} -> framerateNumerator) (\s@Vc3Settings' {} a -> s {framerateNumerator = a} :: Vc3Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
vc3Settings_framerateDenominator :: Lens.Lens' Vc3Settings (Prelude.Maybe Prelude.Natural)
vc3Settings_framerateDenominator = Lens.lens (\Vc3Settings' {framerateDenominator} -> framerateDenominator) (\s@Vc3Settings' {} a -> s {framerateDenominator = a} :: Vc3Settings)

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

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output by
-- relabeling the video frames and resampling your audio. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Related settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
vc3Settings_slowPal :: Lens.Lens' Vc3Settings (Prelude.Maybe Vc3SlowPal)
vc3Settings_slowPal = Lens.lens (\Vc3Settings' {slowPal} -> slowPal) (\s@Vc3Settings' {} a -> s {slowPal = a} :: Vc3Settings)

instance Core.FromJSON Vc3Settings where
  parseJSON =
    Core.withObject
      "Vc3Settings"
      ( \x ->
          Vc3Settings'
            Prelude.<$> (x Core..:? "interlaceMode")
            Prelude.<*> (x Core..:? "telecine")
            Prelude.<*> (x Core..:? "framerateNumerator")
            Prelude.<*> (x Core..:? "framerateDenominator")
            Prelude.<*> (x Core..:? "scanTypeConversionMode")
            Prelude.<*> (x Core..:? "framerateControl")
            Prelude.<*> (x Core..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Core..:? "vc3Class")
            Prelude.<*> (x Core..:? "slowPal")
      )

instance Prelude.Hashable Vc3Settings

instance Prelude.NFData Vc3Settings

instance Core.ToJSON Vc3Settings where
  toJSON Vc3Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("interlaceMode" Core..=) Prelude.<$> interlaceMode,
            ("telecine" Core..=) Prelude.<$> telecine,
            ("framerateNumerator" Core..=)
              Prelude.<$> framerateNumerator,
            ("framerateDenominator" Core..=)
              Prelude.<$> framerateDenominator,
            ("scanTypeConversionMode" Core..=)
              Prelude.<$> scanTypeConversionMode,
            ("framerateControl" Core..=)
              Prelude.<$> framerateControl,
            ("framerateConversionAlgorithm" Core..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("vc3Class" Core..=) Prelude.<$> vc3Class,
            ("slowPal" Core..=) Prelude.<$> slowPal
          ]
      )
