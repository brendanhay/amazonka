{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeBurnin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeBurnin where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.TimecodeBurninPosition
import Network.AWS.Prelude

-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
--
-- /See:/ 'timecodeBurnin' smart constructor.
data TimecodeBurnin = TimecodeBurnin'
  { _tbPrefix :: !(Maybe Text),
    _tbFontSize :: !(Maybe Nat),
    _tbPosition :: !(Maybe TimecodeBurninPosition)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimecodeBurnin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbPrefix' - Use Prefix (Prefix) to place ASCII characters before any burned-in timecode. For example, a prefix of "EZ-" will result in the timecode "EZ-00:00:00:00". Provide either the characters themselves or the ASCII code equivalents. The supported range of characters is 0x20 through 0x7e. This includes letters, numbers, and all special characters represented on a standard English keyboard.
--
-- * 'tbFontSize' - Use Font Size (FontSize) to set the font size of any burned-in timecode. Valid values are 10, 16, 32, 48.
--
-- * 'tbPosition' - Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
timecodeBurnin ::
  TimecodeBurnin
timecodeBurnin =
  TimecodeBurnin'
    { _tbPrefix = Nothing,
      _tbFontSize = Nothing,
      _tbPosition = Nothing
    }

-- | Use Prefix (Prefix) to place ASCII characters before any burned-in timecode. For example, a prefix of "EZ-" will result in the timecode "EZ-00:00:00:00". Provide either the characters themselves or the ASCII code equivalents. The supported range of characters is 0x20 through 0x7e. This includes letters, numbers, and all special characters represented on a standard English keyboard.
tbPrefix :: Lens' TimecodeBurnin (Maybe Text)
tbPrefix = lens _tbPrefix (\s a -> s {_tbPrefix = a})

-- | Use Font Size (FontSize) to set the font size of any burned-in timecode. Valid values are 10, 16, 32, 48.
tbFontSize :: Lens' TimecodeBurnin (Maybe Natural)
tbFontSize = lens _tbFontSize (\s a -> s {_tbFontSize = a}) . mapping _Nat

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
tbPosition :: Lens' TimecodeBurnin (Maybe TimecodeBurninPosition)
tbPosition = lens _tbPosition (\s a -> s {_tbPosition = a})

instance FromJSON TimecodeBurnin where
  parseJSON =
    withObject
      "TimecodeBurnin"
      ( \x ->
          TimecodeBurnin'
            <$> (x .:? "prefix") <*> (x .:? "fontSize") <*> (x .:? "position")
      )

instance Hashable TimecodeBurnin

instance NFData TimecodeBurnin

instance ToJSON TimecodeBurnin where
  toJSON TimecodeBurnin' {..} =
    object
      ( catMaybes
          [ ("prefix" .=) <$> _tbPrefix,
            ("fontSize" .=) <$> _tbFontSize,
            ("position" .=) <$> _tbPosition
          ]
      )
