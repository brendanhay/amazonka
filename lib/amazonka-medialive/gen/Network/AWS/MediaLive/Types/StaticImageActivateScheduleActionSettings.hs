{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputLocation
import Network.AWS.Prelude

-- | Settings for the action to activate a static image.
--
-- /See:/ 'staticImageActivateScheduleActionSettings' smart constructor.
data StaticImageActivateScheduleActionSettings = StaticImageActivateScheduleActionSettings'
  { _siasasImageX ::
      !( Maybe
           Nat
       ),
    _siasasHeight ::
      !( Maybe
           Nat
       ),
    _siasasFadeOut ::
      !( Maybe
           Nat
       ),
    _siasasWidth ::
      !( Maybe
           Nat
       ),
    _siasasOpacity ::
      !( Maybe
           Nat
       ),
    _siasasLayer ::
      !( Maybe
           Nat
       ),
    _siasasDuration ::
      !( Maybe
           Nat
       ),
    _siasasImageY ::
      !( Maybe
           Nat
       ),
    _siasasFadeIn ::
      !( Maybe
           Nat
       ),
    _siasasImage ::
      !InputLocation
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StaticImageActivateScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siasasImageX' - Placement of the left edge of the overlay relative to the left edge of the video frame, in pixels. 0 (the default) is the left edge of the frame. If the placement causes the overlay to extend beyond the right edge of the underlying video, then the overlay is cropped on the right.
--
-- * 'siasasHeight' - The height of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified height. Leave blank to use the native height of the overlay.
--
-- * 'siasasFadeOut' - Applies only if a duration is specified. The time in milliseconds for the image to fade out. The fade-out starts when the duration time is hit, so it effectively extends the duration. Default is 0 (no fade-out).
--
-- * 'siasasWidth' - The width of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified width. Leave blank to use the native width of the overlay.
--
-- * 'siasasOpacity' - Opacity of image where 0 is transparent and 100 is fully opaque. Default is 100.
--
-- * 'siasasLayer' - The number of the layer, 0 to 7. There are 8 layers that can be overlaid on the video, each layer with a different image. The layers are in Z order, which means that overlays with higher values of layer are inserted on top of overlays with lower values of layer. Default is 0.
--
-- * 'siasasDuration' - The duration in milliseconds for the image to remain on the video. If omitted or set to 0 the duration is unlimited and the image will remain until it is explicitly deactivated.
--
-- * 'siasasImageY' - Placement of the top edge of the overlay relative to the top edge of the video frame, in pixels. 0 (the default) is the top edge of the frame. If the placement causes the overlay to extend beyond the bottom edge of the underlying video, then the overlay is cropped on the bottom.
--
-- * 'siasasFadeIn' - The time in milliseconds for the image to fade in. The fade-in starts at the start time of the overlay. Default is 0 (no fade-in).
--
-- * 'siasasImage' - The location and filename of the image file to overlay on the video. The file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in pixels) than the input video.
staticImageActivateScheduleActionSettings ::
  -- | 'siasasImage'
  InputLocation ->
  StaticImageActivateScheduleActionSettings
staticImageActivateScheduleActionSettings pImage_ =
  StaticImageActivateScheduleActionSettings'
    { _siasasImageX =
        Nothing,
      _siasasHeight = Nothing,
      _siasasFadeOut = Nothing,
      _siasasWidth = Nothing,
      _siasasOpacity = Nothing,
      _siasasLayer = Nothing,
      _siasasDuration = Nothing,
      _siasasImageY = Nothing,
      _siasasFadeIn = Nothing,
      _siasasImage = pImage_
    }

-- | Placement of the left edge of the overlay relative to the left edge of the video frame, in pixels. 0 (the default) is the left edge of the frame. If the placement causes the overlay to extend beyond the right edge of the underlying video, then the overlay is cropped on the right.
siasasImageX :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasImageX = lens _siasasImageX (\s a -> s {_siasasImageX = a}) . mapping _Nat

-- | The height of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified height. Leave blank to use the native height of the overlay.
siasasHeight :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasHeight = lens _siasasHeight (\s a -> s {_siasasHeight = a}) . mapping _Nat

-- | Applies only if a duration is specified. The time in milliseconds for the image to fade out. The fade-out starts when the duration time is hit, so it effectively extends the duration. Default is 0 (no fade-out).
siasasFadeOut :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasFadeOut = lens _siasasFadeOut (\s a -> s {_siasasFadeOut = a}) . mapping _Nat

-- | The width of the image when inserted into the video, in pixels. The overlay will be scaled up or down to the specified width. Leave blank to use the native width of the overlay.
siasasWidth :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasWidth = lens _siasasWidth (\s a -> s {_siasasWidth = a}) . mapping _Nat

-- | Opacity of image where 0 is transparent and 100 is fully opaque. Default is 100.
siasasOpacity :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasOpacity = lens _siasasOpacity (\s a -> s {_siasasOpacity = a}) . mapping _Nat

-- | The number of the layer, 0 to 7. There are 8 layers that can be overlaid on the video, each layer with a different image. The layers are in Z order, which means that overlays with higher values of layer are inserted on top of overlays with lower values of layer. Default is 0.
siasasLayer :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasLayer = lens _siasasLayer (\s a -> s {_siasasLayer = a}) . mapping _Nat

-- | The duration in milliseconds for the image to remain on the video. If omitted or set to 0 the duration is unlimited and the image will remain until it is explicitly deactivated.
siasasDuration :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasDuration = lens _siasasDuration (\s a -> s {_siasasDuration = a}) . mapping _Nat

-- | Placement of the top edge of the overlay relative to the top edge of the video frame, in pixels. 0 (the default) is the top edge of the frame. If the placement causes the overlay to extend beyond the bottom edge of the underlying video, then the overlay is cropped on the bottom.
siasasImageY :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasImageY = lens _siasasImageY (\s a -> s {_siasasImageY = a}) . mapping _Nat

-- | The time in milliseconds for the image to fade in. The fade-in starts at the start time of the overlay. Default is 0 (no fade-in).
siasasFadeIn :: Lens' StaticImageActivateScheduleActionSettings (Maybe Natural)
siasasFadeIn = lens _siasasFadeIn (\s a -> s {_siasasFadeIn = a}) . mapping _Nat

-- | The location and filename of the image file to overlay on the video. The file must be a 32-bit BMP, PNG, or TGA file, and must not be larger (in pixels) than the input video.
siasasImage :: Lens' StaticImageActivateScheduleActionSettings InputLocation
siasasImage = lens _siasasImage (\s a -> s {_siasasImage = a})

instance FromJSON StaticImageActivateScheduleActionSettings where
  parseJSON =
    withObject
      "StaticImageActivateScheduleActionSettings"
      ( \x ->
          StaticImageActivateScheduleActionSettings'
            <$> (x .:? "imageX")
            <*> (x .:? "height")
            <*> (x .:? "fadeOut")
            <*> (x .:? "width")
            <*> (x .:? "opacity")
            <*> (x .:? "layer")
            <*> (x .:? "duration")
            <*> (x .:? "imageY")
            <*> (x .:? "fadeIn")
            <*> (x .: "image")
      )

instance Hashable StaticImageActivateScheduleActionSettings

instance NFData StaticImageActivateScheduleActionSettings

instance ToJSON StaticImageActivateScheduleActionSettings where
  toJSON StaticImageActivateScheduleActionSettings' {..} =
    object
      ( catMaybes
          [ ("imageX" .=) <$> _siasasImageX,
            ("height" .=) <$> _siasasHeight,
            ("fadeOut" .=) <$> _siasasFadeOut,
            ("width" .=) <$> _siasasWidth,
            ("opacity" .=) <$> _siasasOpacity,
            ("layer" .=) <$> _siasasLayer,
            ("duration" .=) <$> _siasasDuration,
            ("imageY" .=) <$> _siasasImageY,
            ("fadeIn" .=) <$> _siasasFadeIn,
            Just ("image" .= _siasasImage)
          ]
      )
