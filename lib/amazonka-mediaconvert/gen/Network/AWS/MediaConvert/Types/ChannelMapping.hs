{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ChannelMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ChannelMapping where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.OutputChannelMapping
import Network.AWS.Prelude

-- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
--
-- /See:/ 'channelMapping' smart constructor.
newtype ChannelMapping = ChannelMapping'
  { _cmOutputChannels ::
      Maybe [OutputChannelMapping]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmOutputChannels' - List of output channels
channelMapping ::
  ChannelMapping
channelMapping = ChannelMapping' {_cmOutputChannels = Nothing}

-- | List of output channels
cmOutputChannels :: Lens' ChannelMapping [OutputChannelMapping]
cmOutputChannels = lens _cmOutputChannels (\s a -> s {_cmOutputChannels = a}) . _Default . _Coerce

instance FromJSON ChannelMapping where
  parseJSON =
    withObject
      "ChannelMapping"
      (\x -> ChannelMapping' <$> (x .:? "outputChannels" .!= mempty))

instance Hashable ChannelMapping

instance NFData ChannelMapping

instance ToJSON ChannelMapping where
  toJSON ChannelMapping' {..} =
    object (catMaybes [("outputChannels" .=) <$> _cmOutputChannels])
