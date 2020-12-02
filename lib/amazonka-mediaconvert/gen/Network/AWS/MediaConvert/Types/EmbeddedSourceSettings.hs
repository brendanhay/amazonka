{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EmbeddedSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EmbeddedSourceSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.EmbeddedConvert608To708
import Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions
import Network.AWS.Prelude

-- | Settings for embedded captions Source
--
-- /See:/ 'embeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { _essConvert608To708 ::
      !(Maybe EmbeddedConvert608To708),
    _essTerminateCaptions ::
      !(Maybe EmbeddedTerminateCaptions),
    _essSource608TrackNumber :: !(Maybe Nat),
    _essSource608ChannelNumber :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmbeddedSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'essConvert608To708' - Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
--
-- * 'essTerminateCaptions' - By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
--
-- * 'essSource608TrackNumber' - Specifies the video track index used for extracting captions. The system only supports one input video track, so this should always be set to '1'.
--
-- * 'essSource608ChannelNumber' - Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
embeddedSourceSettings ::
  EmbeddedSourceSettings
embeddedSourceSettings =
  EmbeddedSourceSettings'
    { _essConvert608To708 = Nothing,
      _essTerminateCaptions = Nothing,
      _essSource608TrackNumber = Nothing,
      _essSource608ChannelNumber = Nothing
    }

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
essConvert608To708 :: Lens' EmbeddedSourceSettings (Maybe EmbeddedConvert608To708)
essConvert608To708 = lens _essConvert608To708 (\s a -> s {_essConvert608To708 = a})

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
essTerminateCaptions :: Lens' EmbeddedSourceSettings (Maybe EmbeddedTerminateCaptions)
essTerminateCaptions = lens _essTerminateCaptions (\s a -> s {_essTerminateCaptions = a})

-- | Specifies the video track index used for extracting captions. The system only supports one input video track, so this should always be set to '1'.
essSource608TrackNumber :: Lens' EmbeddedSourceSettings (Maybe Natural)
essSource608TrackNumber = lens _essSource608TrackNumber (\s a -> s {_essSource608TrackNumber = a}) . mapping _Nat

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
essSource608ChannelNumber :: Lens' EmbeddedSourceSettings (Maybe Natural)
essSource608ChannelNumber = lens _essSource608ChannelNumber (\s a -> s {_essSource608ChannelNumber = a}) . mapping _Nat

instance FromJSON EmbeddedSourceSettings where
  parseJSON =
    withObject
      "EmbeddedSourceSettings"
      ( \x ->
          EmbeddedSourceSettings'
            <$> (x .:? "convert608To708")
            <*> (x .:? "terminateCaptions")
            <*> (x .:? "source608TrackNumber")
            <*> (x .:? "source608ChannelNumber")
      )

instance Hashable EmbeddedSourceSettings

instance NFData EmbeddedSourceSettings

instance ToJSON EmbeddedSourceSettings where
  toJSON EmbeddedSourceSettings' {..} =
    object
      ( catMaybes
          [ ("convert608To708" .=) <$> _essConvert608To708,
            ("terminateCaptions" .=) <$> _essTerminateCaptions,
            ("source608TrackNumber" .=) <$> _essSource608TrackNumber,
            ("source608ChannelNumber" .=) <$> _essSource608ChannelNumber
          ]
      )
