{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Thumbnails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Thumbnails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Thumbnails for videos.
--
--
--
-- /See:/ 'thumbnails' smart constructor.
data Thumbnails = Thumbnails'
  { _tSizingPolicy :: !(Maybe Text),
    _tFormat :: !(Maybe Text),
    _tMaxHeight :: !(Maybe Text),
    _tResolution :: !(Maybe Text),
    _tAspectRatio :: !(Maybe Text),
    _tPaddingPolicy :: !(Maybe Text),
    _tInterval :: !(Maybe Text),
    _tMaxWidth :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Thumbnails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tSizingPolicy' - Specify one of the following values to control scaling of thumbnails:     * @Fit@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail MaxWidth or MaxHeight settings without exceeding the other value.      * @Fill@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings and matches or exceeds the other value. Elastic Transcoder centers the image in thumbnails and then crops in the dimension (if any) that exceeds the maximum value.     * @Stretch@ : Elastic Transcoder stretches thumbnails to match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings. If the relative proportions of the input video and thumbnails are different, the thumbnails will be distorted.     * @Keep@ : Elastic Transcoder does not scale thumbnails. If either dimension of the input video exceeds the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic Transcoder crops the thumbnails.     * @ShrinkToFit@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.     * @ShrinkToFill@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.
--
-- * 'tFormat' - The format of thumbnails, if any. Valid values are @jpg@ and @png@ .  You specify whether you want Elastic Transcoder to create thumbnails when you create a job.
--
-- * 'tMaxHeight' - The maximum height of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 3072.
--
-- * 'tResolution' - /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The width and height of thumbnail files in pixels. Specify a value in the format @/width/ @ x @/height/ @ where both values are even integers. The values cannot exceed the width and height that you specified in the @Video:Resolution@ object.
--
-- * 'tAspectRatio' - /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The aspect ratio of thumbnails. Valid values include: @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the video in the output file.
--
-- * 'tPaddingPolicy' - When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of thumbnails to make the total size of the thumbnails match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
--
-- * 'tInterval' - The approximate number of seconds between thumbnails. Specify an integer value.
--
-- * 'tMaxWidth' - The maximum width of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 4096.
thumbnails ::
  Thumbnails
thumbnails =
  Thumbnails'
    { _tSizingPolicy = Nothing,
      _tFormat = Nothing,
      _tMaxHeight = Nothing,
      _tResolution = Nothing,
      _tAspectRatio = Nothing,
      _tPaddingPolicy = Nothing,
      _tInterval = Nothing,
      _tMaxWidth = Nothing
    }

-- | Specify one of the following values to control scaling of thumbnails:     * @Fit@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail MaxWidth or MaxHeight settings without exceeding the other value.      * @Fill@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings and matches or exceeds the other value. Elastic Transcoder centers the image in thumbnails and then crops in the dimension (if any) that exceeds the maximum value.     * @Stretch@ : Elastic Transcoder stretches thumbnails to match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings. If the relative proportions of the input video and thumbnails are different, the thumbnails will be distorted.     * @Keep@ : Elastic Transcoder does not scale thumbnails. If either dimension of the input video exceeds the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic Transcoder crops the thumbnails.     * @ShrinkToFit@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.     * @ShrinkToFill@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.
tSizingPolicy :: Lens' Thumbnails (Maybe Text)
tSizingPolicy = lens _tSizingPolicy (\s a -> s {_tSizingPolicy = a})

-- | The format of thumbnails, if any. Valid values are @jpg@ and @png@ .  You specify whether you want Elastic Transcoder to create thumbnails when you create a job.
tFormat :: Lens' Thumbnails (Maybe Text)
tFormat = lens _tFormat (\s a -> s {_tFormat = a})

-- | The maximum height of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 3072.
tMaxHeight :: Lens' Thumbnails (Maybe Text)
tMaxHeight = lens _tMaxHeight (\s a -> s {_tMaxHeight = a})

-- | /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The width and height of thumbnail files in pixels. Specify a value in the format @/width/ @ x @/height/ @ where both values are even integers. The values cannot exceed the width and height that you specified in the @Video:Resolution@ object.
tResolution :: Lens' Thumbnails (Maybe Text)
tResolution = lens _tResolution (\s a -> s {_tResolution = a})

-- | /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The aspect ratio of thumbnails. Valid values include: @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the video in the output file.
tAspectRatio :: Lens' Thumbnails (Maybe Text)
tAspectRatio = lens _tAspectRatio (\s a -> s {_tAspectRatio = a})

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of thumbnails to make the total size of the thumbnails match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
tPaddingPolicy :: Lens' Thumbnails (Maybe Text)
tPaddingPolicy = lens _tPaddingPolicy (\s a -> s {_tPaddingPolicy = a})

-- | The approximate number of seconds between thumbnails. Specify an integer value.
tInterval :: Lens' Thumbnails (Maybe Text)
tInterval = lens _tInterval (\s a -> s {_tInterval = a})

-- | The maximum width of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 4096.
tMaxWidth :: Lens' Thumbnails (Maybe Text)
tMaxWidth = lens _tMaxWidth (\s a -> s {_tMaxWidth = a})

instance FromJSON Thumbnails where
  parseJSON =
    withObject
      "Thumbnails"
      ( \x ->
          Thumbnails'
            <$> (x .:? "SizingPolicy")
            <*> (x .:? "Format")
            <*> (x .:? "MaxHeight")
            <*> (x .:? "Resolution")
            <*> (x .:? "AspectRatio")
            <*> (x .:? "PaddingPolicy")
            <*> (x .:? "Interval")
            <*> (x .:? "MaxWidth")
      )

instance Hashable Thumbnails

instance NFData Thumbnails

instance ToJSON Thumbnails where
  toJSON Thumbnails' {..} =
    object
      ( catMaybes
          [ ("SizingPolicy" .=) <$> _tSizingPolicy,
            ("Format" .=) <$> _tFormat,
            ("MaxHeight" .=) <$> _tMaxHeight,
            ("Resolution" .=) <$> _tResolution,
            ("AspectRatio" .=) <$> _tAspectRatio,
            ("PaddingPolicy" .=) <$> _tPaddingPolicy,
            ("Interval" .=) <$> _tInterval,
            ("MaxWidth" .=) <$> _tMaxWidth
          ]
      )
