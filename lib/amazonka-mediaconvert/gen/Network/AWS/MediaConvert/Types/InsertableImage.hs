{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InsertableImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InsertableImage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings that specify how your still graphic overlay appears.
--
-- /See:/ 'insertableImage' smart constructor.
data InsertableImage = InsertableImage'
  { _iiImageX :: !(Maybe Nat),
    _iiHeight :: !(Maybe Nat),
    _iiStartTime :: !(Maybe Text),
    _iiFadeOut :: !(Maybe Nat),
    _iiWidth :: !(Maybe Nat),
    _iiOpacity :: !(Maybe Nat),
    _iiLayer :: !(Maybe Nat),
    _iiDuration :: !(Maybe Nat),
    _iiImageY :: !(Maybe Nat),
    _iiImageInserterInput :: !(Maybe Text),
    _iiFadeIn :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InsertableImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiImageX' - Specify the distance, in pixels, between the inserted image and the left edge of the video frame. Required for any image overlay that you specify.
--
-- * 'iiHeight' - Specify the height of the inserted image in pixels. If you specify a value that's larger than the video resolution height, the service will crop your overlaid image to fit. To use the native height of the image, keep this setting blank.
--
-- * 'iiStartTime' - Specify the timecode of the frame that you want the overlay to first appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format. Remember to take into account your timecode source settings.
--
-- * 'iiFadeOut' - Specify the length of time, in milliseconds, between the end of the time that you have specified for the image overlay Duration and when the overlaid image has faded to total transparency. If you don't specify a value for Fade-out, the image will disappear abruptly at the end of the inserted image duration.
--
-- * 'iiWidth' - Specify the width of the inserted image in pixels. If you specify a value that's larger than the video resolution width, the service will crop your overlaid image to fit. To use the native width of the image, keep this setting blank.
--
-- * 'iiOpacity' - Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
--
-- * 'iiLayer' - Specify how overlapping inserted images appear. Images with higher values for Layer appear on top of images with lower values for Layer.
--
-- * 'iiDuration' - Specify the time, in milliseconds, for the image to remain on the output video. This duration includes fade-in time but not fade-out time.
--
-- * 'iiImageY' - Specify the distance, in pixels, between the overlaid image and the top edge of the video frame. Required for any image overlay that you specify.
--
-- * 'iiImageInserterInput' - Specify the HTTP, HTTPS, or Amazon S3 location of the image that you want to overlay on the video. Use a PNG or TGA file.
--
-- * 'iiFadeIn' - Specify the length of time, in milliseconds, between the Start time that you specify for the image insertion and the time that the image appears at full opacity. Full opacity is the level that you specify for the opacity setting. If you don't specify a value for Fade-in, the image will appear abruptly at the overlay start time.
insertableImage ::
  InsertableImage
insertableImage =
  InsertableImage'
    { _iiImageX = Nothing,
      _iiHeight = Nothing,
      _iiStartTime = Nothing,
      _iiFadeOut = Nothing,
      _iiWidth = Nothing,
      _iiOpacity = Nothing,
      _iiLayer = Nothing,
      _iiDuration = Nothing,
      _iiImageY = Nothing,
      _iiImageInserterInput = Nothing,
      _iiFadeIn = Nothing
    }

-- | Specify the distance, in pixels, between the inserted image and the left edge of the video frame. Required for any image overlay that you specify.
iiImageX :: Lens' InsertableImage (Maybe Natural)
iiImageX = lens _iiImageX (\s a -> s {_iiImageX = a}) . mapping _Nat

-- | Specify the height of the inserted image in pixels. If you specify a value that's larger than the video resolution height, the service will crop your overlaid image to fit. To use the native height of the image, keep this setting blank.
iiHeight :: Lens' InsertableImage (Maybe Natural)
iiHeight = lens _iiHeight (\s a -> s {_iiHeight = a}) . mapping _Nat

-- | Specify the timecode of the frame that you want the overlay to first appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format. Remember to take into account your timecode source settings.
iiStartTime :: Lens' InsertableImage (Maybe Text)
iiStartTime = lens _iiStartTime (\s a -> s {_iiStartTime = a})

-- | Specify the length of time, in milliseconds, between the end of the time that you have specified for the image overlay Duration and when the overlaid image has faded to total transparency. If you don't specify a value for Fade-out, the image will disappear abruptly at the end of the inserted image duration.
iiFadeOut :: Lens' InsertableImage (Maybe Natural)
iiFadeOut = lens _iiFadeOut (\s a -> s {_iiFadeOut = a}) . mapping _Nat

-- | Specify the width of the inserted image in pixels. If you specify a value that's larger than the video resolution width, the service will crop your overlaid image to fit. To use the native width of the image, keep this setting blank.
iiWidth :: Lens' InsertableImage (Maybe Natural)
iiWidth = lens _iiWidth (\s a -> s {_iiWidth = a}) . mapping _Nat

-- | Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
iiOpacity :: Lens' InsertableImage (Maybe Natural)
iiOpacity = lens _iiOpacity (\s a -> s {_iiOpacity = a}) . mapping _Nat

-- | Specify how overlapping inserted images appear. Images with higher values for Layer appear on top of images with lower values for Layer.
iiLayer :: Lens' InsertableImage (Maybe Natural)
iiLayer = lens _iiLayer (\s a -> s {_iiLayer = a}) . mapping _Nat

-- | Specify the time, in milliseconds, for the image to remain on the output video. This duration includes fade-in time but not fade-out time.
iiDuration :: Lens' InsertableImage (Maybe Natural)
iiDuration = lens _iiDuration (\s a -> s {_iiDuration = a}) . mapping _Nat

-- | Specify the distance, in pixels, between the overlaid image and the top edge of the video frame. Required for any image overlay that you specify.
iiImageY :: Lens' InsertableImage (Maybe Natural)
iiImageY = lens _iiImageY (\s a -> s {_iiImageY = a}) . mapping _Nat

-- | Specify the HTTP, HTTPS, or Amazon S3 location of the image that you want to overlay on the video. Use a PNG or TGA file.
iiImageInserterInput :: Lens' InsertableImage (Maybe Text)
iiImageInserterInput = lens _iiImageInserterInput (\s a -> s {_iiImageInserterInput = a})

-- | Specify the length of time, in milliseconds, between the Start time that you specify for the image insertion and the time that the image appears at full opacity. Full opacity is the level that you specify for the opacity setting. If you don't specify a value for Fade-in, the image will appear abruptly at the overlay start time.
iiFadeIn :: Lens' InsertableImage (Maybe Natural)
iiFadeIn = lens _iiFadeIn (\s a -> s {_iiFadeIn = a}) . mapping _Nat

instance FromJSON InsertableImage where
  parseJSON =
    withObject
      "InsertableImage"
      ( \x ->
          InsertableImage'
            <$> (x .:? "imageX")
            <*> (x .:? "height")
            <*> (x .:? "startTime")
            <*> (x .:? "fadeOut")
            <*> (x .:? "width")
            <*> (x .:? "opacity")
            <*> (x .:? "layer")
            <*> (x .:? "duration")
            <*> (x .:? "imageY")
            <*> (x .:? "imageInserterInput")
            <*> (x .:? "fadeIn")
      )

instance Hashable InsertableImage

instance NFData InsertableImage

instance ToJSON InsertableImage where
  toJSON InsertableImage' {..} =
    object
      ( catMaybes
          [ ("imageX" .=) <$> _iiImageX,
            ("height" .=) <$> _iiHeight,
            ("startTime" .=) <$> _iiStartTime,
            ("fadeOut" .=) <$> _iiFadeOut,
            ("width" .=) <$> _iiWidth,
            ("opacity" .=) <$> _iiOpacity,
            ("layer" .=) <$> _iiLayer,
            ("duration" .=) <$> _iiDuration,
            ("imageY" .=) <$> _iiImageY,
            ("imageInserterInput" .=) <$> _iiImageInserterInput,
            ("fadeIn" .=) <$> _iiFadeIn
          ]
      )
