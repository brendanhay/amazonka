{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ColorSpaceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ColorSpaceSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
import Network.AWS.MediaLive.Types.Rec601Settings
import Network.AWS.MediaLive.Types.Rec709Settings
import Network.AWS.Prelude

-- | H264 Color Space Settings
--
-- /See:/ 'h264ColorSpaceSettings' smart constructor.
data H264ColorSpaceSettings = H264ColorSpaceSettings'
  { _hRec709Settings ::
      !(Maybe Rec709Settings),
    _hRec601Settings :: !(Maybe Rec601Settings),
    _hColorSpacePassthroughSettings ::
      !(Maybe ColorSpacePassthroughSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H264ColorSpaceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hRec709Settings' - Undocumented member.
--
-- * 'hRec601Settings' - Undocumented member.
--
-- * 'hColorSpacePassthroughSettings' - Undocumented member.
h264ColorSpaceSettings ::
  H264ColorSpaceSettings
h264ColorSpaceSettings =
  H264ColorSpaceSettings'
    { _hRec709Settings = Nothing,
      _hRec601Settings = Nothing,
      _hColorSpacePassthroughSettings = Nothing
    }

-- | Undocumented member.
hRec709Settings :: Lens' H264ColorSpaceSettings (Maybe Rec709Settings)
hRec709Settings = lens _hRec709Settings (\s a -> s {_hRec709Settings = a})

-- | Undocumented member.
hRec601Settings :: Lens' H264ColorSpaceSettings (Maybe Rec601Settings)
hRec601Settings = lens _hRec601Settings (\s a -> s {_hRec601Settings = a})

-- | Undocumented member.
hColorSpacePassthroughSettings :: Lens' H264ColorSpaceSettings (Maybe ColorSpacePassthroughSettings)
hColorSpacePassthroughSettings = lens _hColorSpacePassthroughSettings (\s a -> s {_hColorSpacePassthroughSettings = a})

instance FromJSON H264ColorSpaceSettings where
  parseJSON =
    withObject
      "H264ColorSpaceSettings"
      ( \x ->
          H264ColorSpaceSettings'
            <$> (x .:? "rec709Settings")
            <*> (x .:? "rec601Settings")
            <*> (x .:? "colorSpacePassthroughSettings")
      )

instance Hashable H264ColorSpaceSettings

instance NFData H264ColorSpaceSettings

instance ToJSON H264ColorSpaceSettings where
  toJSON H264ColorSpaceSettings' {..} =
    object
      ( catMaybes
          [ ("rec709Settings" .=) <$> _hRec709Settings,
            ("rec601Settings" .=) <$> _hRec601Settings,
            ("colorSpacePassthroughSettings" .=)
              <$> _hColorSpacePassthroughSettings
          ]
      )
