{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265ColorSpaceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265ColorSpaceSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
import Network.AWS.MediaLive.Types.Hdr10Settings
import Network.AWS.MediaLive.Types.Rec601Settings
import Network.AWS.MediaLive.Types.Rec709Settings
import Network.AWS.Prelude

-- | H265 Color Space Settings
--
-- /See:/ 'h265ColorSpaceSettings' smart constructor.
data H265ColorSpaceSettings = H265ColorSpaceSettings'
  { _hcssHdr10Settings ::
      !(Maybe Hdr10Settings),
    _hcssRec709Settings ::
      !(Maybe Rec709Settings),
    _hcssRec601Settings ::
      !(Maybe Rec601Settings),
    _hcssColorSpacePassthroughSettings ::
      !(Maybe ColorSpacePassthroughSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H265ColorSpaceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcssHdr10Settings' - Undocumented member.
--
-- * 'hcssRec709Settings' - Undocumented member.
--
-- * 'hcssRec601Settings' - Undocumented member.
--
-- * 'hcssColorSpacePassthroughSettings' - Undocumented member.
h265ColorSpaceSettings ::
  H265ColorSpaceSettings
h265ColorSpaceSettings =
  H265ColorSpaceSettings'
    { _hcssHdr10Settings = Nothing,
      _hcssRec709Settings = Nothing,
      _hcssRec601Settings = Nothing,
      _hcssColorSpacePassthroughSettings = Nothing
    }

-- | Undocumented member.
hcssHdr10Settings :: Lens' H265ColorSpaceSettings (Maybe Hdr10Settings)
hcssHdr10Settings = lens _hcssHdr10Settings (\s a -> s {_hcssHdr10Settings = a})

-- | Undocumented member.
hcssRec709Settings :: Lens' H265ColorSpaceSettings (Maybe Rec709Settings)
hcssRec709Settings = lens _hcssRec709Settings (\s a -> s {_hcssRec709Settings = a})

-- | Undocumented member.
hcssRec601Settings :: Lens' H265ColorSpaceSettings (Maybe Rec601Settings)
hcssRec601Settings = lens _hcssRec601Settings (\s a -> s {_hcssRec601Settings = a})

-- | Undocumented member.
hcssColorSpacePassthroughSettings :: Lens' H265ColorSpaceSettings (Maybe ColorSpacePassthroughSettings)
hcssColorSpacePassthroughSettings = lens _hcssColorSpacePassthroughSettings (\s a -> s {_hcssColorSpacePassthroughSettings = a})

instance FromJSON H265ColorSpaceSettings where
  parseJSON =
    withObject
      "H265ColorSpaceSettings"
      ( \x ->
          H265ColorSpaceSettings'
            <$> (x .:? "hdr10Settings")
            <*> (x .:? "rec709Settings")
            <*> (x .:? "rec601Settings")
            <*> (x .:? "colorSpacePassthroughSettings")
      )

instance Hashable H265ColorSpaceSettings

instance NFData H265ColorSpaceSettings

instance ToJSON H265ColorSpaceSettings where
  toJSON H265ColorSpaceSettings' {..} =
    object
      ( catMaybes
          [ ("hdr10Settings" .=) <$> _hcssHdr10Settings,
            ("rec709Settings" .=) <$> _hcssRec709Settings,
            ("rec601Settings" .=) <$> _hcssRec601Settings,
            ("colorSpacePassthroughSettings" .=)
              <$> _hcssColorSpacePassthroughSettings
          ]
      )
