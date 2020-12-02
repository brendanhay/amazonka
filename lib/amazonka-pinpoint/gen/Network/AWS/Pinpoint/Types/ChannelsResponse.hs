{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ChannelsResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ChannelResponse
import Network.AWS.Prelude

-- | Provides information about the general settings and status of all channels for an application, including channels that aren't enabled for the application.
--
--
--
-- /See:/ 'channelsResponse' smart constructor.
newtype ChannelsResponse = ChannelsResponse'
  { _cChannels ::
      Map Text (ChannelResponse)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cChannels' - A map that contains a multipart response for each channel. For each item in this object, the ChannelType is the key and the Channel is the value.
channelsResponse ::
  ChannelsResponse
channelsResponse = ChannelsResponse' {_cChannels = mempty}

-- | A map that contains a multipart response for each channel. For each item in this object, the ChannelType is the key and the Channel is the value.
cChannels :: Lens' ChannelsResponse (HashMap Text (ChannelResponse))
cChannels = lens _cChannels (\s a -> s {_cChannels = a}) . _Map

instance FromJSON ChannelsResponse where
  parseJSON =
    withObject
      "ChannelsResponse"
      (\x -> ChannelsResponse' <$> (x .:? "Channels" .!= mempty))

instance Hashable ChannelsResponse

instance NFData ChannelsResponse
