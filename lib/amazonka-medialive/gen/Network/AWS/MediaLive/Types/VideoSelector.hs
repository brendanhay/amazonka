{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelector where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.VideoSelectorColorSpace
import Network.AWS.MediaLive.Types.VideoSelectorColorSpaceUsage
import Network.AWS.MediaLive.Types.VideoSelectorSettings
import Network.AWS.Prelude

-- | Specifies a particular video stream within an input source. An input may have only a single video selector.
--
-- /See:/ 'videoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { _vsSelectorSettings ::
      !(Maybe VideoSelectorSettings),
    _vsColorSpaceUsage :: !(Maybe VideoSelectorColorSpaceUsage),
    _vsColorSpace :: !(Maybe VideoSelectorColorSpace)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsSelectorSettings' - The video selector settings.
--
-- * 'vsColorSpaceUsage' - Applies only if colorSpace is a value other than follow. This field controls how the value in the colorSpace field will be used. fallback means that when the input does include color space data, that data will be used, but when the input has no color space data, the value in colorSpace will be used. Choose fallback if your input is sometimes missing color space data, but when it does have color space data, that data is correct. force means to always use the value in colorSpace. Choose force if your input usually has no color space data or might have unreliable color space data.
--
-- * 'vsColorSpace' - Specifies the color space of an input. This setting works in tandem with colorSpaceUsage and a video description's colorSpaceSettingsChoice to determine if any conversion will be performed.
videoSelector ::
  VideoSelector
videoSelector =
  VideoSelector'
    { _vsSelectorSettings = Nothing,
      _vsColorSpaceUsage = Nothing,
      _vsColorSpace = Nothing
    }

-- | The video selector settings.
vsSelectorSettings :: Lens' VideoSelector (Maybe VideoSelectorSettings)
vsSelectorSettings = lens _vsSelectorSettings (\s a -> s {_vsSelectorSettings = a})

-- | Applies only if colorSpace is a value other than follow. This field controls how the value in the colorSpace field will be used. fallback means that when the input does include color space data, that data will be used, but when the input has no color space data, the value in colorSpace will be used. Choose fallback if your input is sometimes missing color space data, but when it does have color space data, that data is correct. force means to always use the value in colorSpace. Choose force if your input usually has no color space data or might have unreliable color space data.
vsColorSpaceUsage :: Lens' VideoSelector (Maybe VideoSelectorColorSpaceUsage)
vsColorSpaceUsage = lens _vsColorSpaceUsage (\s a -> s {_vsColorSpaceUsage = a})

-- | Specifies the color space of an input. This setting works in tandem with colorSpaceUsage and a video description's colorSpaceSettingsChoice to determine if any conversion will be performed.
vsColorSpace :: Lens' VideoSelector (Maybe VideoSelectorColorSpace)
vsColorSpace = lens _vsColorSpace (\s a -> s {_vsColorSpace = a})

instance FromJSON VideoSelector where
  parseJSON =
    withObject
      "VideoSelector"
      ( \x ->
          VideoSelector'
            <$> (x .:? "selectorSettings")
            <*> (x .:? "colorSpaceUsage")
            <*> (x .:? "colorSpace")
      )

instance Hashable VideoSelector

instance NFData VideoSelector

instance ToJSON VideoSelector where
  toJSON VideoSelector' {..} =
    object
      ( catMaybes
          [ ("selectorSettings" .=) <$> _vsSelectorSettings,
            ("colorSpaceUsage" .=) <$> _vsColorSpaceUsage,
            ("colorSpace" .=) <$> _vsColorSpace
          ]
      )
