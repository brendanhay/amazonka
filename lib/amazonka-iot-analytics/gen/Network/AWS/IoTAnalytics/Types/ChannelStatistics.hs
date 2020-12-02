{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStatistics where

import Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Statistics information about the channel.
--
--
--
-- /See:/ 'channelStatistics' smart constructor.
newtype ChannelStatistics = ChannelStatistics'
  { _csSize ::
      Maybe EstimatedResourceSize
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csSize' - The estimated size of the channel.
channelStatistics ::
  ChannelStatistics
channelStatistics = ChannelStatistics' {_csSize = Nothing}

-- | The estimated size of the channel.
csSize :: Lens' ChannelStatistics (Maybe EstimatedResourceSize)
csSize = lens _csSize (\s a -> s {_csSize = a})

instance FromJSON ChannelStatistics where
  parseJSON =
    withObject
      "ChannelStatistics"
      (\x -> ChannelStatistics' <$> (x .:? "size"))

instance Hashable ChannelStatistics

instance NFData ChannelStatistics
