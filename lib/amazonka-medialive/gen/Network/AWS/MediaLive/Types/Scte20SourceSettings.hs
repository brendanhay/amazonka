{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte20SourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20SourceSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Scte20Convert608To708
import Network.AWS.Prelude

-- | Scte20 Source Settings
--
-- /See:/ 'scte20SourceSettings' smart constructor.
data Scte20SourceSettings = Scte20SourceSettings'
  { _sssConvert608To708 ::
      !(Maybe Scte20Convert608To708),
    _sssSource608ChannelNumber :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte20SourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sssConvert608To708' - If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
--
-- * 'sssSource608ChannelNumber' - Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
scte20SourceSettings ::
  Scte20SourceSettings
scte20SourceSettings =
  Scte20SourceSettings'
    { _sssConvert608To708 = Nothing,
      _sssSource608ChannelNumber = Nothing
    }

-- | If upconvert, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
sssConvert608To708 :: Lens' Scte20SourceSettings (Maybe Scte20Convert608To708)
sssConvert608To708 = lens _sssConvert608To708 (\s a -> s {_sssConvert608To708 = a})

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
sssSource608ChannelNumber :: Lens' Scte20SourceSettings (Maybe Natural)
sssSource608ChannelNumber = lens _sssSource608ChannelNumber (\s a -> s {_sssSource608ChannelNumber = a}) . mapping _Nat

instance FromJSON Scte20SourceSettings where
  parseJSON =
    withObject
      "Scte20SourceSettings"
      ( \x ->
          Scte20SourceSettings'
            <$> (x .:? "convert608To708") <*> (x .:? "source608ChannelNumber")
      )

instance Hashable Scte20SourceSettings

instance NFData Scte20SourceSettings

instance ToJSON Scte20SourceSettings where
  toJSON Scte20SourceSettings' {..} =
    object
      ( catMaybes
          [ ("convert608To708" .=) <$> _sssConvert608To708,
            ("source608ChannelNumber" .=) <$> _sssSource608ChannelNumber
          ]
      )
