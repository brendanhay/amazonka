{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedSourceSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.EmbeddedConvert608To708
import Network.AWS.MediaLive.Types.EmbeddedScte20Detection
import Network.AWS.Prelude

-- | Embedded Source Settings
--
-- /See:/ 'embeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { _essConvert608To708 ::
      !(Maybe EmbeddedConvert608To708),
    _essScte20Detection ::
      !(Maybe EmbeddedScte20Detection),
    _essSource608TrackNumber :: !(Maybe Nat),
    _essSource608ChannelNumber :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmbeddedSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'essConvert608To708' - If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
--
-- * 'essScte20Detection' - Set to "auto" to handle streams with intermittent and/or non-aligned SCTE-20 and Embedded captions.
--
-- * 'essSource608TrackNumber' - This field is unused and deprecated.
--
-- * 'essSource608ChannelNumber' - Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
embeddedSourceSettings ::
  EmbeddedSourceSettings
embeddedSourceSettings =
  EmbeddedSourceSettings'
    { _essConvert608To708 = Nothing,
      _essScte20Detection = Nothing,
      _essSource608TrackNumber = Nothing,
      _essSource608ChannelNumber = Nothing
    }

-- | If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
essConvert608To708 :: Lens' EmbeddedSourceSettings (Maybe EmbeddedConvert608To708)
essConvert608To708 = lens _essConvert608To708 (\s a -> s {_essConvert608To708 = a})

-- | Set to "auto" to handle streams with intermittent and/or non-aligned SCTE-20 and Embedded captions.
essScte20Detection :: Lens' EmbeddedSourceSettings (Maybe EmbeddedScte20Detection)
essScte20Detection = lens _essScte20Detection (\s a -> s {_essScte20Detection = a})

-- | This field is unused and deprecated.
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
            <*> (x .:? "scte20Detection")
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
            ("scte20Detection" .=) <$> _essScte20Detection,
            ("source608TrackNumber" .=) <$> _essSource608TrackNumber,
            ("source608ChannelNumber" .=) <$> _essSource608ChannelNumber
          ]
      )
