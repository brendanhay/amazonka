{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RemixSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RemixSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AudioChannelMapping
import Network.AWS.Prelude

-- | Remix Settings
--
-- /See:/ 'remixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { _rsChannelsIn :: !(Maybe Nat),
    _rsChannelsOut :: !(Maybe Nat),
    _rsChannelMappings :: ![AudioChannelMapping]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemixSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsChannelsIn' - Number of input channels to be used.
--
-- * 'rsChannelsOut' - Number of output channels to be produced. Valid values: 1, 2, 4, 6, 8
--
-- * 'rsChannelMappings' - Mapping of input channels to output channels, with appropriate gain adjustments.
remixSettings ::
  RemixSettings
remixSettings =
  RemixSettings'
    { _rsChannelsIn = Nothing,
      _rsChannelsOut = Nothing,
      _rsChannelMappings = mempty
    }

-- | Number of input channels to be used.
rsChannelsIn :: Lens' RemixSettings (Maybe Natural)
rsChannelsIn = lens _rsChannelsIn (\s a -> s {_rsChannelsIn = a}) . mapping _Nat

-- | Number of output channels to be produced. Valid values: 1, 2, 4, 6, 8
rsChannelsOut :: Lens' RemixSettings (Maybe Natural)
rsChannelsOut = lens _rsChannelsOut (\s a -> s {_rsChannelsOut = a}) . mapping _Nat

-- | Mapping of input channels to output channels, with appropriate gain adjustments.
rsChannelMappings :: Lens' RemixSettings [AudioChannelMapping]
rsChannelMappings = lens _rsChannelMappings (\s a -> s {_rsChannelMappings = a}) . _Coerce

instance FromJSON RemixSettings where
  parseJSON =
    withObject
      "RemixSettings"
      ( \x ->
          RemixSettings'
            <$> (x .:? "channelsIn")
            <*> (x .:? "channelsOut")
            <*> (x .:? "channelMappings" .!= mempty)
      )

instance Hashable RemixSettings

instance NFData RemixSettings

instance ToJSON RemixSettings where
  toJSON RemixSettings' {..} =
    object
      ( catMaybes
          [ ("channelsIn" .=) <$> _rsChannelsIn,
            ("channelsOut" .=) <$> _rsChannelsOut,
            Just ("channelMappings" .= _rsChannelMappings)
          ]
      )
