{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelEgressEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelEgressEndpoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Placeholder documentation for ChannelEgressEndpoint
--
-- /See:/ 'channelEgressEndpoint' smart constructor.
newtype ChannelEgressEndpoint = ChannelEgressEndpoint'
  { _ceeSourceIP ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelEgressEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceeSourceIP' - Public IP of where a channel's output comes from
channelEgressEndpoint ::
  ChannelEgressEndpoint
channelEgressEndpoint =
  ChannelEgressEndpoint' {_ceeSourceIP = Nothing}

-- | Public IP of where a channel's output comes from
ceeSourceIP :: Lens' ChannelEgressEndpoint (Maybe Text)
ceeSourceIP = lens _ceeSourceIP (\s a -> s {_ceeSourceIP = a})

instance FromJSON ChannelEgressEndpoint where
  parseJSON =
    withObject
      "ChannelEgressEndpoint"
      (\x -> ChannelEgressEndpoint' <$> (x .:? "sourceIp"))

instance Hashable ChannelEgressEndpoint

instance NFData ChannelEgressEndpoint
