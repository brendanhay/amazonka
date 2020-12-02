{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.PresetWatermark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.PresetWatermark where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video.
--
--
-- Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.
--
-- When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.
--
--
-- /See:/ 'presetWatermark' smart constructor.
data PresetWatermark = PresetWatermark'
  { _pwVerticalAlign ::
      !(Maybe Text),
    _pwSizingPolicy :: !(Maybe Text),
    _pwHorizontalOffset :: !(Maybe Text),
    _pwMaxHeight :: !(Maybe Text),
    _pwOpacity :: !(Maybe Text),
    _pwVerticalOffset :: !(Maybe Text),
    _pwMaxWidth :: !(Maybe Text),
    _pwId :: !(Maybe Text),
    _pwHorizontalAlign :: !(Maybe Text),
    _pwTarget :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PresetWatermark' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwVerticalAlign' - The vertical position of the watermark unless you specify a non-zero value for @VerticalOffset@ :      * __Top__ : The top edge of the watermark is aligned with the top border of the video.     * __Bottom__ : The bottom edge of the watermark is aligned with the bottom border of the video.     * __Center__ : The watermark is centered between the top and bottom borders.
--
-- * 'pwSizingPolicy' - A value that controls scaling of the watermark:      * __Fit__ : Elastic Transcoder scales the watermark so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.     * __Stretch__ : Elastic Transcoder stretches the watermark to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the watermark and the values of @MaxWidth@ and @MaxHeight@ are different, the watermark will be distorted.     * __ShrinkToFit__ : Elastic Transcoder scales the watermark down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the watermark up.
--
-- * 'pwHorizontalOffset' - The amount by which you want the horizontal position of the watermark to be offset from the position specified by HorizontalAlign:      * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of MaxWidth.     * integer percentage (%): The range of valid values is 0 to 100. For example, if you specify Left for @HorizontalAlign@ and 5px for @HorizontalOffset@ , the left side of the watermark appears 5 pixels from the left border of the output video. @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is @Left@ or @Right@ . If you specify an offset that causes the watermark to extend beyond the left or right border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped. Use the value of @Target@ to specify whether you want to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
--
-- * 'pwMaxHeight' - The maximum height of the watermark in one of the following formats:      * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxHeight@ .     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation. If you specify the value in pixels, it must be less than or equal to the value of @MaxHeight@ .
--
-- * 'pwOpacity' - A percentage that indicates how much you want a watermark to obscure the video in the location where it appears. Valid values are 0 (the watermark is invisible) to 100 (the watermark completely obscures the video in the specified location). The datatype of @Opacity@ is float. Elastic Transcoder supports transparent .png graphics. If you use a transparent .png, the transparent portion of the video appears as if you had specified a value of 0 for @Opacity@ . The .jpg file format doesn't support transparency.
--
-- * 'pwVerticalOffset' - @VerticalOffset@  The amount by which you want the vertical position of the watermark to be offset from the position specified by VerticalAlign:     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of @MaxHeight@ .     * integer percentage (%): The range of valid values is 0 to 100. For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for @VerticalOffset@ , the top of the watermark appears 5 pixels from the top border of the output video. @VerticalOffset@ is only valid when the value of VerticalAlign is Top or Bottom. If you specify an offset that causes the watermark to extend beyond the top or bottom border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
--
-- * 'pwMaxWidth' - The maximum width of the watermark in one of the following formats:      * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxWidth@ .     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation. If you specify the value in pixels, it must be less than or equal to the value of @MaxWidth@ .
--
-- * 'pwId' - A unique identifier for the settings for one watermark. The value of @Id@ can be up to 40 characters long.
--
-- * 'pwHorizontalAlign' - The horizontal position of the watermark unless you specify a non-zero value for @HorizontalOffset@ :      * __Left__ : The left edge of the watermark is aligned with the left border of the video.     * __Right__ : The right edge of the watermark is aligned with the right border of the video.     * __Center__ : The watermark is centered between the left and right borders.
--
-- * 'pwTarget' - A value that determines how Elastic Transcoder interprets values that you specified for @HorizontalOffset@ , @VerticalOffset@ , @MaxWidth@ , and @MaxHeight@ :     * __Content__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any.     * __Frame__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any.
presetWatermark ::
  PresetWatermark
presetWatermark =
  PresetWatermark'
    { _pwVerticalAlign = Nothing,
      _pwSizingPolicy = Nothing,
      _pwHorizontalOffset = Nothing,
      _pwMaxHeight = Nothing,
      _pwOpacity = Nothing,
      _pwVerticalOffset = Nothing,
      _pwMaxWidth = Nothing,
      _pwId = Nothing,
      _pwHorizontalAlign = Nothing,
      _pwTarget = Nothing
    }

-- | The vertical position of the watermark unless you specify a non-zero value for @VerticalOffset@ :      * __Top__ : The top edge of the watermark is aligned with the top border of the video.     * __Bottom__ : The bottom edge of the watermark is aligned with the bottom border of the video.     * __Center__ : The watermark is centered between the top and bottom borders.
pwVerticalAlign :: Lens' PresetWatermark (Maybe Text)
pwVerticalAlign = lens _pwVerticalAlign (\s a -> s {_pwVerticalAlign = a})

-- | A value that controls scaling of the watermark:      * __Fit__ : Elastic Transcoder scales the watermark so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.     * __Stretch__ : Elastic Transcoder stretches the watermark to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the watermark and the values of @MaxWidth@ and @MaxHeight@ are different, the watermark will be distorted.     * __ShrinkToFit__ : Elastic Transcoder scales the watermark down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the watermark up.
pwSizingPolicy :: Lens' PresetWatermark (Maybe Text)
pwSizingPolicy = lens _pwSizingPolicy (\s a -> s {_pwSizingPolicy = a})

-- | The amount by which you want the horizontal position of the watermark to be offset from the position specified by HorizontalAlign:      * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of MaxWidth.     * integer percentage (%): The range of valid values is 0 to 100. For example, if you specify Left for @HorizontalAlign@ and 5px for @HorizontalOffset@ , the left side of the watermark appears 5 pixels from the left border of the output video. @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is @Left@ or @Right@ . If you specify an offset that causes the watermark to extend beyond the left or right border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped. Use the value of @Target@ to specify whether you want to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
pwHorizontalOffset :: Lens' PresetWatermark (Maybe Text)
pwHorizontalOffset = lens _pwHorizontalOffset (\s a -> s {_pwHorizontalOffset = a})

-- | The maximum height of the watermark in one of the following formats:      * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxHeight@ .     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation. If you specify the value in pixels, it must be less than or equal to the value of @MaxHeight@ .
pwMaxHeight :: Lens' PresetWatermark (Maybe Text)
pwMaxHeight = lens _pwMaxHeight (\s a -> s {_pwMaxHeight = a})

-- | A percentage that indicates how much you want a watermark to obscure the video in the location where it appears. Valid values are 0 (the watermark is invisible) to 100 (the watermark completely obscures the video in the specified location). The datatype of @Opacity@ is float. Elastic Transcoder supports transparent .png graphics. If you use a transparent .png, the transparent portion of the video appears as if you had specified a value of 0 for @Opacity@ . The .jpg file format doesn't support transparency.
pwOpacity :: Lens' PresetWatermark (Maybe Text)
pwOpacity = lens _pwOpacity (\s a -> s {_pwOpacity = a})

-- | @VerticalOffset@  The amount by which you want the vertical position of the watermark to be offset from the position specified by VerticalAlign:     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of @MaxHeight@ .     * integer percentage (%): The range of valid values is 0 to 100. For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for @VerticalOffset@ , the top of the watermark appears 5 pixels from the top border of the output video. @VerticalOffset@ is only valid when the value of VerticalAlign is Top or Bottom. If you specify an offset that causes the watermark to extend beyond the top or bottom border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
pwVerticalOffset :: Lens' PresetWatermark (Maybe Text)
pwVerticalOffset = lens _pwVerticalOffset (\s a -> s {_pwVerticalOffset = a})

-- | The maximum width of the watermark in one of the following formats:      * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxWidth@ .     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation. If you specify the value in pixels, it must be less than or equal to the value of @MaxWidth@ .
pwMaxWidth :: Lens' PresetWatermark (Maybe Text)
pwMaxWidth = lens _pwMaxWidth (\s a -> s {_pwMaxWidth = a})

-- | A unique identifier for the settings for one watermark. The value of @Id@ can be up to 40 characters long.
pwId :: Lens' PresetWatermark (Maybe Text)
pwId = lens _pwId (\s a -> s {_pwId = a})

-- | The horizontal position of the watermark unless you specify a non-zero value for @HorizontalOffset@ :      * __Left__ : The left edge of the watermark is aligned with the left border of the video.     * __Right__ : The right edge of the watermark is aligned with the right border of the video.     * __Center__ : The watermark is centered between the left and right borders.
pwHorizontalAlign :: Lens' PresetWatermark (Maybe Text)
pwHorizontalAlign = lens _pwHorizontalAlign (\s a -> s {_pwHorizontalAlign = a})

-- | A value that determines how Elastic Transcoder interprets values that you specified for @HorizontalOffset@ , @VerticalOffset@ , @MaxWidth@ , and @MaxHeight@ :     * __Content__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any.     * __Frame__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any.
pwTarget :: Lens' PresetWatermark (Maybe Text)
pwTarget = lens _pwTarget (\s a -> s {_pwTarget = a})

instance FromJSON PresetWatermark where
  parseJSON =
    withObject
      "PresetWatermark"
      ( \x ->
          PresetWatermark'
            <$> (x .:? "VerticalAlign")
            <*> (x .:? "SizingPolicy")
            <*> (x .:? "HorizontalOffset")
            <*> (x .:? "MaxHeight")
            <*> (x .:? "Opacity")
            <*> (x .:? "VerticalOffset")
            <*> (x .:? "MaxWidth")
            <*> (x .:? "Id")
            <*> (x .:? "HorizontalAlign")
            <*> (x .:? "Target")
      )

instance Hashable PresetWatermark

instance NFData PresetWatermark

instance ToJSON PresetWatermark where
  toJSON PresetWatermark' {..} =
    object
      ( catMaybes
          [ ("VerticalAlign" .=) <$> _pwVerticalAlign,
            ("SizingPolicy" .=) <$> _pwSizingPolicy,
            ("HorizontalOffset" .=) <$> _pwHorizontalOffset,
            ("MaxHeight" .=) <$> _pwMaxHeight,
            ("Opacity" .=) <$> _pwOpacity,
            ("VerticalOffset" .=) <$> _pwVerticalOffset,
            ("MaxWidth" .=) <$> _pwMaxWidth,
            ("Id" .=) <$> _pwId,
            ("HorizontalAlign" .=) <$> _pwHorizontalAlign,
            ("Target" .=) <$> _pwTarget
          ]
      )
