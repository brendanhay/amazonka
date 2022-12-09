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
-- Module      : Amazonka.MediaConvert.Types.XavcSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Xavc4kIntraCbgProfileSettings
import Amazonka.MediaConvert.Types.Xavc4kIntraVbrProfileSettings
import Amazonka.MediaConvert.Types.Xavc4kProfileSettings
import Amazonka.MediaConvert.Types.XavcAdaptiveQuantization
import Amazonka.MediaConvert.Types.XavcEntropyEncoding
import Amazonka.MediaConvert.Types.XavcFramerateControl
import Amazonka.MediaConvert.Types.XavcFramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.XavcHdIntraCbgProfileSettings
import Amazonka.MediaConvert.Types.XavcHdProfileSettings
import Amazonka.MediaConvert.Types.XavcProfile
import Amazonka.MediaConvert.Types.XavcSlowPal
import Amazonka.MediaConvert.Types.XavcSpatialAdaptiveQuantization
import Amazonka.MediaConvert.Types.XavcTemporalAdaptiveQuantization
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value XAVC.
--
-- /See:/ 'newXavcSettings' smart constructor.
data XavcSettings = XavcSettings'
  { -- | Keep the default value, Auto (AUTO), for this setting to have
    -- MediaConvert automatically apply the best types of quantization for your
    -- video content. When you want to apply your quantization settings
    -- manually, you must set Adaptive quantization (adaptiveQuantization) to a
    -- value other than Auto (AUTO). Use this setting to specify the strength
    -- of any adaptive quantization filters that you enable. If you don\'t want
    -- MediaConvert to do any adaptive quantization in this transcode, set
    -- Adaptive quantization to Off (OFF). Related settings: The value that you
    -- choose here applies to the following settings: Flicker adaptive
    -- quantization (flickerAdaptiveQuantization), Spatial adaptive
    -- quantization (spatialAdaptiveQuantization), and Temporal adaptive
    -- quantization (temporalAdaptiveQuantization).
    adaptiveQuantization :: Prelude.Maybe XavcAdaptiveQuantization,
    -- | Optional. Choose a specific entropy encoding mode only when you want to
    -- override XAVC recommendations. If you choose the value auto,
    -- MediaConvert uses the mode that the XAVC file format specifies given
    -- this output\'s operating point.
    entropyEncoding :: Prelude.Maybe XavcEntropyEncoding,
    -- | If you are using the console, use the Frame rate setting to specify the
    -- frame rate for this output. If you want to keep the same frame rate as
    -- the input video, choose Follow source. If you want to do frame rate
    -- conversion, choose a frame rate from the dropdown list. The framerates
    -- shown in the dropdown list are decimal approximations of fractions. If
    -- you are creating your transcoding job specification as a JSON file
    -- without the console, use FramerateControl to specify which value the
    -- service uses for the frame rate for this output. Choose
    -- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
    -- from the input. Choose SPECIFIED if you want the service to use the
    -- frame rate that you specify in the settings FramerateNumerator and
    -- FramerateDenominator.
    framerateControl :: Prelude.Maybe XavcFramerateControl,
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
    framerateConversionAlgorithm :: Prelude.Maybe XavcFramerateConversionAlgorithm,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Frame rate. In this example, specify 23.976.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Specify the XAVC profile for this output. For more information, see the
    -- Sony documentation at https:\/\/www.xavc-info.org\/. Note that
    -- MediaConvert doesn\'t support the interlaced video XAVC operating points
    -- for XAVC_HD_INTRA_CBG. To create an interlaced XAVC output, choose the
    -- profile XAVC_HD.
    profile :: Prelude.Maybe XavcProfile,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
    -- per second (fps). Enable slow PAL to create a 25 fps output by
    -- relabeling the video frames and resampling your audio. Note that
    -- enabling this setting will slightly reduce the duration of your video.
    -- Related settings: You must also set Frame rate to 25. In your JSON job
    -- specification, set (framerateControl) to (SPECIFIED),
    -- (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Prelude.Maybe XavcSlowPal,
    -- | Ignore this setting unless your downstream workflow requires that you
    -- specify it explicitly. Otherwise, we recommend that you adjust the
    -- softness of your output by using a lower value for the setting Sharpness
    -- (sharpness) or by enabling a noise reducer filter (noiseReducerFilter).
    -- The Softness (softness) setting specifies the quantization matrices that
    -- the encoder uses. Keep the default value, 0, for flat quantization.
    -- Choose the value 1 or 16 to use the default JVT softening quantization
    -- matricies from the H.264 specification. Choose a value from 17 to 128 to
    -- use planar interpolation. Increasing values from 17 to 128 result in
    -- increasing reduction of high-frequency data. The value 128 results in
    -- the softest video.
    softness :: Prelude.Maybe Prelude.Natural,
    -- | The best way to set up adaptive quantization is to keep the default
    -- value, Auto (AUTO), for the setting Adaptive quantization
    -- (adaptiveQuantization). When you do so, MediaConvert automatically
    -- applies the best types of quantization for your video content. Include
    -- this setting in your JSON job specification only when you choose to
    -- change the default value for Adaptive quantization. For this setting,
    -- keep the default value, Enabled (ENABLED), to adjust quantization within
    -- each frame based on spatial variation of content complexity. When you
    -- enable this feature, the encoder uses fewer bits on areas that can
    -- sustain more distortion with no noticeable visual degradation and uses
    -- more bits on areas where any small distortion will be noticeable. For
    -- example, complex textured blocks are encoded with fewer bits and smooth
    -- textured blocks are encoded with more bits. Enabling this feature will
    -- almost always improve your video quality. Note, though, that this
    -- feature doesn\'t take into account where the viewer\'s attention is
    -- likely to be. If viewers are likely to be focusing their attention on a
    -- part of the screen with a lot of complex texture, you might choose to
    -- disable this feature. Related setting: When you enable spatial adaptive
    -- quantization, set the value for Adaptive quantization
    -- (adaptiveQuantization) depending on your content. For homogeneous
    -- content, such as cartoons and video games, set it to Low. For content
    -- with a wider variety of textures, set it to High or Higher.
    spatialAdaptiveQuantization :: Prelude.Maybe XavcSpatialAdaptiveQuantization,
    -- | The best way to set up adaptive quantization is to keep the default
    -- value, Auto (AUTO), for the setting Adaptive quantization
    -- (adaptiveQuantization). When you do so, MediaConvert automatically
    -- applies the best types of quantization for your video content. Include
    -- this setting in your JSON job specification only when you choose to
    -- change the default value for Adaptive quantization. For this setting,
    -- keep the default value, Enabled (ENABLED), to adjust quantization within
    -- each frame based on temporal variation of content complexity. When you
    -- enable this feature, the encoder uses fewer bits on areas of the frame
    -- that aren\'t moving and uses more bits on complex objects with sharp
    -- edges that move a lot. For example, this feature improves the
    -- readability of text tickers on newscasts and scoreboards on sports
    -- matches. Enabling this feature will almost always improve your video
    -- quality. Note, though, that this feature doesn\'t take into account
    -- where the viewer\'s attention is likely to be. If viewers are likely to
    -- be focusing their attention on a part of the screen that doesn\'t have
    -- moving objects with sharp edges, such as sports athletes\' faces, you
    -- might choose to disable this feature. Related setting: When you enable
    -- temporal adaptive quantization, adjust the strength of the filter with
    -- the setting Adaptive quantization (adaptiveQuantization).
    temporalAdaptiveQuantization :: Prelude.Maybe XavcTemporalAdaptiveQuantization,
    -- | Required when you set (Profile) under
    -- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
    -- XAVC_4K_INTRA_CBG.
    xavc4kIntraCbgProfileSettings :: Prelude.Maybe Xavc4kIntraCbgProfileSettings,
    -- | Required when you set (Profile) under
    -- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
    -- XAVC_4K_INTRA_VBR.
    xavc4kIntraVbrProfileSettings :: Prelude.Maybe Xavc4kIntraVbrProfileSettings,
    -- | Required when you set (Profile) under
    -- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value XAVC_4K.
    xavc4kProfileSettings :: Prelude.Maybe Xavc4kProfileSettings,
    -- | Required when you set (Profile) under
    -- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
    -- XAVC_HD_INTRA_CBG.
    xavcHdIntraCbgProfileSettings :: Prelude.Maybe XavcHdIntraCbgProfileSettings,
    -- | Required when you set (Profile) under
    -- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value XAVC_HD.
    xavcHdProfileSettings :: Prelude.Maybe XavcHdProfileSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'XavcSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adaptiveQuantization', 'xavcSettings_adaptiveQuantization' - Keep the default value, Auto (AUTO), for this setting to have
-- MediaConvert automatically apply the best types of quantization for your
-- video content. When you want to apply your quantization settings
-- manually, you must set Adaptive quantization (adaptiveQuantization) to a
-- value other than Auto (AUTO). Use this setting to specify the strength
-- of any adaptive quantization filters that you enable. If you don\'t want
-- MediaConvert to do any adaptive quantization in this transcode, set
-- Adaptive quantization to Off (OFF). Related settings: The value that you
-- choose here applies to the following settings: Flicker adaptive
-- quantization (flickerAdaptiveQuantization), Spatial adaptive
-- quantization (spatialAdaptiveQuantization), and Temporal adaptive
-- quantization (temporalAdaptiveQuantization).
--
-- 'entropyEncoding', 'xavcSettings_entropyEncoding' - Optional. Choose a specific entropy encoding mode only when you want to
-- override XAVC recommendations. If you choose the value auto,
-- MediaConvert uses the mode that the XAVC file format specifies given
-- this output\'s operating point.
--
-- 'framerateControl', 'xavcSettings_framerateControl' - If you are using the console, use the Frame rate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list. The framerates
-- shown in the dropdown list are decimal approximations of fractions. If
-- you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate that you specify in the settings FramerateNumerator and
-- FramerateDenominator.
--
-- 'framerateConversionAlgorithm', 'xavcSettings_framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or
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
-- 'framerateDenominator', 'xavcSettings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Frame rate. In this example, specify 23.976.
--
-- 'framerateNumerator', 'xavcSettings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'profile', 'xavcSettings_profile' - Specify the XAVC profile for this output. For more information, see the
-- Sony documentation at https:\/\/www.xavc-info.org\/. Note that
-- MediaConvert doesn\'t support the interlaced video XAVC operating points
-- for XAVC_HD_INTRA_CBG. To create an interlaced XAVC output, choose the
-- profile XAVC_HD.
--
-- 'slowPal', 'xavcSettings_slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output by
-- relabeling the video frames and resampling your audio. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Related settings: You must also set Frame rate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- 'softness', 'xavcSettings_softness' - Ignore this setting unless your downstream workflow requires that you
-- specify it explicitly. Otherwise, we recommend that you adjust the
-- softness of your output by using a lower value for the setting Sharpness
-- (sharpness) or by enabling a noise reducer filter (noiseReducerFilter).
-- The Softness (softness) setting specifies the quantization matrices that
-- the encoder uses. Keep the default value, 0, for flat quantization.
-- Choose the value 1 or 16 to use the default JVT softening quantization
-- matricies from the H.264 specification. Choose a value from 17 to 128 to
-- use planar interpolation. Increasing values from 17 to 128 result in
-- increasing reduction of high-frequency data. The value 128 results in
-- the softest video.
--
-- 'spatialAdaptiveQuantization', 'xavcSettings_spatialAdaptiveQuantization' - The best way to set up adaptive quantization is to keep the default
-- value, Auto (AUTO), for the setting Adaptive quantization
-- (adaptiveQuantization). When you do so, MediaConvert automatically
-- applies the best types of quantization for your video content. Include
-- this setting in your JSON job specification only when you choose to
-- change the default value for Adaptive quantization. For this setting,
-- keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on spatial variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas that can
-- sustain more distortion with no noticeable visual degradation and uses
-- more bits on areas where any small distortion will be noticeable. For
-- example, complex textured blocks are encoded with fewer bits and smooth
-- textured blocks are encoded with more bits. Enabling this feature will
-- almost always improve your video quality. Note, though, that this
-- feature doesn\'t take into account where the viewer\'s attention is
-- likely to be. If viewers are likely to be focusing their attention on a
-- part of the screen with a lot of complex texture, you might choose to
-- disable this feature. Related setting: When you enable spatial adaptive
-- quantization, set the value for Adaptive quantization
-- (adaptiveQuantization) depending on your content. For homogeneous
-- content, such as cartoons and video games, set it to Low. For content
-- with a wider variety of textures, set it to High or Higher.
--
-- 'temporalAdaptiveQuantization', 'xavcSettings_temporalAdaptiveQuantization' - The best way to set up adaptive quantization is to keep the default
-- value, Auto (AUTO), for the setting Adaptive quantization
-- (adaptiveQuantization). When you do so, MediaConvert automatically
-- applies the best types of quantization for your video content. Include
-- this setting in your JSON job specification only when you choose to
-- change the default value for Adaptive quantization. For this setting,
-- keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on temporal variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas of the frame
-- that aren\'t moving and uses more bits on complex objects with sharp
-- edges that move a lot. For example, this feature improves the
-- readability of text tickers on newscasts and scoreboards on sports
-- matches. Enabling this feature will almost always improve your video
-- quality. Note, though, that this feature doesn\'t take into account
-- where the viewer\'s attention is likely to be. If viewers are likely to
-- be focusing their attention on a part of the screen that doesn\'t have
-- moving objects with sharp edges, such as sports athletes\' faces, you
-- might choose to disable this feature. Related setting: When you enable
-- temporal adaptive quantization, adjust the strength of the filter with
-- the setting Adaptive quantization (adaptiveQuantization).
--
-- 'xavc4kIntraCbgProfileSettings', 'xavcSettings_xavc4kIntraCbgProfileSettings' - Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
-- XAVC_4K_INTRA_CBG.
--
-- 'xavc4kIntraVbrProfileSettings', 'xavcSettings_xavc4kIntraVbrProfileSettings' - Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
-- XAVC_4K_INTRA_VBR.
--
-- 'xavc4kProfileSettings', 'xavcSettings_xavc4kProfileSettings' - Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value XAVC_4K.
--
-- 'xavcHdIntraCbgProfileSettings', 'xavcSettings_xavcHdIntraCbgProfileSettings' - Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
-- XAVC_HD_INTRA_CBG.
--
-- 'xavcHdProfileSettings', 'xavcSettings_xavcHdProfileSettings' - Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value XAVC_HD.
newXavcSettings ::
  XavcSettings
newXavcSettings =
  XavcSettings'
    { adaptiveQuantization =
        Prelude.Nothing,
      entropyEncoding = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      profile = Prelude.Nothing,
      slowPal = Prelude.Nothing,
      softness = Prelude.Nothing,
      spatialAdaptiveQuantization = Prelude.Nothing,
      temporalAdaptiveQuantization = Prelude.Nothing,
      xavc4kIntraCbgProfileSettings = Prelude.Nothing,
      xavc4kIntraVbrProfileSettings = Prelude.Nothing,
      xavc4kProfileSettings = Prelude.Nothing,
      xavcHdIntraCbgProfileSettings = Prelude.Nothing,
      xavcHdProfileSettings = Prelude.Nothing
    }

-- | Keep the default value, Auto (AUTO), for this setting to have
-- MediaConvert automatically apply the best types of quantization for your
-- video content. When you want to apply your quantization settings
-- manually, you must set Adaptive quantization (adaptiveQuantization) to a
-- value other than Auto (AUTO). Use this setting to specify the strength
-- of any adaptive quantization filters that you enable. If you don\'t want
-- MediaConvert to do any adaptive quantization in this transcode, set
-- Adaptive quantization to Off (OFF). Related settings: The value that you
-- choose here applies to the following settings: Flicker adaptive
-- quantization (flickerAdaptiveQuantization), Spatial adaptive
-- quantization (spatialAdaptiveQuantization), and Temporal adaptive
-- quantization (temporalAdaptiveQuantization).
xavcSettings_adaptiveQuantization :: Lens.Lens' XavcSettings (Prelude.Maybe XavcAdaptiveQuantization)
xavcSettings_adaptiveQuantization = Lens.lens (\XavcSettings' {adaptiveQuantization} -> adaptiveQuantization) (\s@XavcSettings' {} a -> s {adaptiveQuantization = a} :: XavcSettings)

-- | Optional. Choose a specific entropy encoding mode only when you want to
-- override XAVC recommendations. If you choose the value auto,
-- MediaConvert uses the mode that the XAVC file format specifies given
-- this output\'s operating point.
xavcSettings_entropyEncoding :: Lens.Lens' XavcSettings (Prelude.Maybe XavcEntropyEncoding)
xavcSettings_entropyEncoding = Lens.lens (\XavcSettings' {entropyEncoding} -> entropyEncoding) (\s@XavcSettings' {} a -> s {entropyEncoding = a} :: XavcSettings)

-- | If you are using the console, use the Frame rate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list. The framerates
-- shown in the dropdown list are decimal approximations of fractions. If
-- you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate that you specify in the settings FramerateNumerator and
-- FramerateDenominator.
xavcSettings_framerateControl :: Lens.Lens' XavcSettings (Prelude.Maybe XavcFramerateControl)
xavcSettings_framerateControl = Lens.lens (\XavcSettings' {framerateControl} -> framerateControl) (\s@XavcSettings' {} a -> s {framerateControl = a} :: XavcSettings)

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
xavcSettings_framerateConversionAlgorithm :: Lens.Lens' XavcSettings (Prelude.Maybe XavcFramerateConversionAlgorithm)
xavcSettings_framerateConversionAlgorithm = Lens.lens (\XavcSettings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@XavcSettings' {} a -> s {framerateConversionAlgorithm = a} :: XavcSettings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Frame rate. In this example, specify 23.976.
xavcSettings_framerateDenominator :: Lens.Lens' XavcSettings (Prelude.Maybe Prelude.Natural)
xavcSettings_framerateDenominator = Lens.lens (\XavcSettings' {framerateDenominator} -> framerateDenominator) (\s@XavcSettings' {} a -> s {framerateDenominator = a} :: XavcSettings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
xavcSettings_framerateNumerator :: Lens.Lens' XavcSettings (Prelude.Maybe Prelude.Natural)
xavcSettings_framerateNumerator = Lens.lens (\XavcSettings' {framerateNumerator} -> framerateNumerator) (\s@XavcSettings' {} a -> s {framerateNumerator = a} :: XavcSettings)

-- | Specify the XAVC profile for this output. For more information, see the
-- Sony documentation at https:\/\/www.xavc-info.org\/. Note that
-- MediaConvert doesn\'t support the interlaced video XAVC operating points
-- for XAVC_HD_INTRA_CBG. To create an interlaced XAVC output, choose the
-- profile XAVC_HD.
xavcSettings_profile :: Lens.Lens' XavcSettings (Prelude.Maybe XavcProfile)
xavcSettings_profile = Lens.lens (\XavcSettings' {profile} -> profile) (\s@XavcSettings' {} a -> s {profile = a} :: XavcSettings)

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output by
-- relabeling the video frames and resampling your audio. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Related settings: You must also set Frame rate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
xavcSettings_slowPal :: Lens.Lens' XavcSettings (Prelude.Maybe XavcSlowPal)
xavcSettings_slowPal = Lens.lens (\XavcSettings' {slowPal} -> slowPal) (\s@XavcSettings' {} a -> s {slowPal = a} :: XavcSettings)

-- | Ignore this setting unless your downstream workflow requires that you
-- specify it explicitly. Otherwise, we recommend that you adjust the
-- softness of your output by using a lower value for the setting Sharpness
-- (sharpness) or by enabling a noise reducer filter (noiseReducerFilter).
-- The Softness (softness) setting specifies the quantization matrices that
-- the encoder uses. Keep the default value, 0, for flat quantization.
-- Choose the value 1 or 16 to use the default JVT softening quantization
-- matricies from the H.264 specification. Choose a value from 17 to 128 to
-- use planar interpolation. Increasing values from 17 to 128 result in
-- increasing reduction of high-frequency data. The value 128 results in
-- the softest video.
xavcSettings_softness :: Lens.Lens' XavcSettings (Prelude.Maybe Prelude.Natural)
xavcSettings_softness = Lens.lens (\XavcSettings' {softness} -> softness) (\s@XavcSettings' {} a -> s {softness = a} :: XavcSettings)

-- | The best way to set up adaptive quantization is to keep the default
-- value, Auto (AUTO), for the setting Adaptive quantization
-- (adaptiveQuantization). When you do so, MediaConvert automatically
-- applies the best types of quantization for your video content. Include
-- this setting in your JSON job specification only when you choose to
-- change the default value for Adaptive quantization. For this setting,
-- keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on spatial variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas that can
-- sustain more distortion with no noticeable visual degradation and uses
-- more bits on areas where any small distortion will be noticeable. For
-- example, complex textured blocks are encoded with fewer bits and smooth
-- textured blocks are encoded with more bits. Enabling this feature will
-- almost always improve your video quality. Note, though, that this
-- feature doesn\'t take into account where the viewer\'s attention is
-- likely to be. If viewers are likely to be focusing their attention on a
-- part of the screen with a lot of complex texture, you might choose to
-- disable this feature. Related setting: When you enable spatial adaptive
-- quantization, set the value for Adaptive quantization
-- (adaptiveQuantization) depending on your content. For homogeneous
-- content, such as cartoons and video games, set it to Low. For content
-- with a wider variety of textures, set it to High or Higher.
xavcSettings_spatialAdaptiveQuantization :: Lens.Lens' XavcSettings (Prelude.Maybe XavcSpatialAdaptiveQuantization)
xavcSettings_spatialAdaptiveQuantization = Lens.lens (\XavcSettings' {spatialAdaptiveQuantization} -> spatialAdaptiveQuantization) (\s@XavcSettings' {} a -> s {spatialAdaptiveQuantization = a} :: XavcSettings)

-- | The best way to set up adaptive quantization is to keep the default
-- value, Auto (AUTO), for the setting Adaptive quantization
-- (adaptiveQuantization). When you do so, MediaConvert automatically
-- applies the best types of quantization for your video content. Include
-- this setting in your JSON job specification only when you choose to
-- change the default value for Adaptive quantization. For this setting,
-- keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on temporal variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas of the frame
-- that aren\'t moving and uses more bits on complex objects with sharp
-- edges that move a lot. For example, this feature improves the
-- readability of text tickers on newscasts and scoreboards on sports
-- matches. Enabling this feature will almost always improve your video
-- quality. Note, though, that this feature doesn\'t take into account
-- where the viewer\'s attention is likely to be. If viewers are likely to
-- be focusing their attention on a part of the screen that doesn\'t have
-- moving objects with sharp edges, such as sports athletes\' faces, you
-- might choose to disable this feature. Related setting: When you enable
-- temporal adaptive quantization, adjust the strength of the filter with
-- the setting Adaptive quantization (adaptiveQuantization).
xavcSettings_temporalAdaptiveQuantization :: Lens.Lens' XavcSettings (Prelude.Maybe XavcTemporalAdaptiveQuantization)
xavcSettings_temporalAdaptiveQuantization = Lens.lens (\XavcSettings' {temporalAdaptiveQuantization} -> temporalAdaptiveQuantization) (\s@XavcSettings' {} a -> s {temporalAdaptiveQuantization = a} :: XavcSettings)

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
-- XAVC_4K_INTRA_CBG.
xavcSettings_xavc4kIntraCbgProfileSettings :: Lens.Lens' XavcSettings (Prelude.Maybe Xavc4kIntraCbgProfileSettings)
xavcSettings_xavc4kIntraCbgProfileSettings = Lens.lens (\XavcSettings' {xavc4kIntraCbgProfileSettings} -> xavc4kIntraCbgProfileSettings) (\s@XavcSettings' {} a -> s {xavc4kIntraCbgProfileSettings = a} :: XavcSettings)

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
-- XAVC_4K_INTRA_VBR.
xavcSettings_xavc4kIntraVbrProfileSettings :: Lens.Lens' XavcSettings (Prelude.Maybe Xavc4kIntraVbrProfileSettings)
xavcSettings_xavc4kIntraVbrProfileSettings = Lens.lens (\XavcSettings' {xavc4kIntraVbrProfileSettings} -> xavc4kIntraVbrProfileSettings) (\s@XavcSettings' {} a -> s {xavc4kIntraVbrProfileSettings = a} :: XavcSettings)

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value XAVC_4K.
xavcSettings_xavc4kProfileSettings :: Lens.Lens' XavcSettings (Prelude.Maybe Xavc4kProfileSettings)
xavcSettings_xavc4kProfileSettings = Lens.lens (\XavcSettings' {xavc4kProfileSettings} -> xavc4kProfileSettings) (\s@XavcSettings' {} a -> s {xavc4kProfileSettings = a} :: XavcSettings)

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
-- XAVC_HD_INTRA_CBG.
xavcSettings_xavcHdIntraCbgProfileSettings :: Lens.Lens' XavcSettings (Prelude.Maybe XavcHdIntraCbgProfileSettings)
xavcSettings_xavcHdIntraCbgProfileSettings = Lens.lens (\XavcSettings' {xavcHdIntraCbgProfileSettings} -> xavcHdIntraCbgProfileSettings) (\s@XavcSettings' {} a -> s {xavcHdIntraCbgProfileSettings = a} :: XavcSettings)

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value XAVC_HD.
xavcSettings_xavcHdProfileSettings :: Lens.Lens' XavcSettings (Prelude.Maybe XavcHdProfileSettings)
xavcSettings_xavcHdProfileSettings = Lens.lens (\XavcSettings' {xavcHdProfileSettings} -> xavcHdProfileSettings) (\s@XavcSettings' {} a -> s {xavcHdProfileSettings = a} :: XavcSettings)

instance Data.FromJSON XavcSettings where
  parseJSON =
    Data.withObject
      "XavcSettings"
      ( \x ->
          XavcSettings'
            Prelude.<$> (x Data..:? "adaptiveQuantization")
            Prelude.<*> (x Data..:? "entropyEncoding")
            Prelude.<*> (x Data..:? "framerateControl")
            Prelude.<*> (x Data..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Data..:? "framerateDenominator")
            Prelude.<*> (x Data..:? "framerateNumerator")
            Prelude.<*> (x Data..:? "profile")
            Prelude.<*> (x Data..:? "slowPal")
            Prelude.<*> (x Data..:? "softness")
            Prelude.<*> (x Data..:? "spatialAdaptiveQuantization")
            Prelude.<*> (x Data..:? "temporalAdaptiveQuantization")
            Prelude.<*> (x Data..:? "xavc4kIntraCbgProfileSettings")
            Prelude.<*> (x Data..:? "xavc4kIntraVbrProfileSettings")
            Prelude.<*> (x Data..:? "xavc4kProfileSettings")
            Prelude.<*> (x Data..:? "xavcHdIntraCbgProfileSettings")
            Prelude.<*> (x Data..:? "xavcHdProfileSettings")
      )

instance Prelude.Hashable XavcSettings where
  hashWithSalt _salt XavcSettings' {..} =
    _salt `Prelude.hashWithSalt` adaptiveQuantization
      `Prelude.hashWithSalt` entropyEncoding
      `Prelude.hashWithSalt` framerateControl
      `Prelude.hashWithSalt` framerateConversionAlgorithm
      `Prelude.hashWithSalt` framerateDenominator
      `Prelude.hashWithSalt` framerateNumerator
      `Prelude.hashWithSalt` profile
      `Prelude.hashWithSalt` slowPal
      `Prelude.hashWithSalt` softness
      `Prelude.hashWithSalt` spatialAdaptiveQuantization
      `Prelude.hashWithSalt` temporalAdaptiveQuantization
      `Prelude.hashWithSalt` xavc4kIntraCbgProfileSettings
      `Prelude.hashWithSalt` xavc4kIntraVbrProfileSettings
      `Prelude.hashWithSalt` xavc4kProfileSettings
      `Prelude.hashWithSalt` xavcHdIntraCbgProfileSettings
      `Prelude.hashWithSalt` xavcHdProfileSettings

instance Prelude.NFData XavcSettings where
  rnf XavcSettings' {..} =
    Prelude.rnf adaptiveQuantization
      `Prelude.seq` Prelude.rnf entropyEncoding
      `Prelude.seq` Prelude.rnf framerateControl
      `Prelude.seq` Prelude.rnf framerateConversionAlgorithm
      `Prelude.seq` Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf framerateNumerator
      `Prelude.seq` Prelude.rnf profile
      `Prelude.seq` Prelude.rnf slowPal
      `Prelude.seq` Prelude.rnf softness
      `Prelude.seq` Prelude.rnf spatialAdaptiveQuantization
      `Prelude.seq` Prelude.rnf temporalAdaptiveQuantization
      `Prelude.seq` Prelude.rnf xavc4kIntraCbgProfileSettings
      `Prelude.seq` Prelude.rnf xavc4kIntraVbrProfileSettings
      `Prelude.seq` Prelude.rnf xavc4kProfileSettings
      `Prelude.seq` Prelude.rnf
        xavcHdIntraCbgProfileSettings
      `Prelude.seq` Prelude.rnf xavcHdProfileSettings

instance Data.ToJSON XavcSettings where
  toJSON XavcSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adaptiveQuantization" Data..=)
              Prelude.<$> adaptiveQuantization,
            ("entropyEncoding" Data..=)
              Prelude.<$> entropyEncoding,
            ("framerateControl" Data..=)
              Prelude.<$> framerateControl,
            ("framerateConversionAlgorithm" Data..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("framerateDenominator" Data..=)
              Prelude.<$> framerateDenominator,
            ("framerateNumerator" Data..=)
              Prelude.<$> framerateNumerator,
            ("profile" Data..=) Prelude.<$> profile,
            ("slowPal" Data..=) Prelude.<$> slowPal,
            ("softness" Data..=) Prelude.<$> softness,
            ("spatialAdaptiveQuantization" Data..=)
              Prelude.<$> spatialAdaptiveQuantization,
            ("temporalAdaptiveQuantization" Data..=)
              Prelude.<$> temporalAdaptiveQuantization,
            ("xavc4kIntraCbgProfileSettings" Data..=)
              Prelude.<$> xavc4kIntraCbgProfileSettings,
            ("xavc4kIntraVbrProfileSettings" Data..=)
              Prelude.<$> xavc4kIntraVbrProfileSettings,
            ("xavc4kProfileSettings" Data..=)
              Prelude.<$> xavc4kProfileSettings,
            ("xavcHdIntraCbgProfileSettings" Data..=)
              Prelude.<$> xavcHdIntraCbgProfileSettings,
            ("xavcHdProfileSettings" Data..=)
              Prelude.<$> xavcHdProfileSettings
          ]
      )
