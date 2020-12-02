{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Channel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Channel where

import Network.AWS.IoTAnalytics.Types.ChannelStatus
import Network.AWS.IoTAnalytics.Types.ChannelStorage
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of data from an MQTT topic. Channels archive the raw, unprocessed messages before publishing the data to a pipeline.
--
--
--
-- /See:/ 'channel' smart constructor.
data Channel = Channel'
  { _cCreationTime :: !(Maybe POSIX),
    _cStatus :: !(Maybe ChannelStatus),
    _cLastMessageArrivalTime :: !(Maybe POSIX),
    _cArn :: !(Maybe Text),
    _cStorage :: !(Maybe ChannelStorage),
    _cRetentionPeriod :: !(Maybe RetentionPeriod),
    _cName :: !(Maybe Text),
    _cLastUpdateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCreationTime' - When the channel was created.
--
-- * 'cStatus' - The status of the channel.
--
-- * 'cLastMessageArrivalTime' - The last time when a new message arrived in the channel. AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation. This feature only applies to messages that arrived in the data store after October 23, 2020.
--
-- * 'cArn' - The ARN of the channel.
--
-- * 'cStorage' - Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- * 'cRetentionPeriod' - How long, in days, message data is kept for the channel.
--
-- * 'cName' - The name of the channel.
--
-- * 'cLastUpdateTime' - When the channel was last updated.
channel ::
  Channel
channel =
  Channel'
    { _cCreationTime = Nothing,
      _cStatus = Nothing,
      _cLastMessageArrivalTime = Nothing,
      _cArn = Nothing,
      _cStorage = Nothing,
      _cRetentionPeriod = Nothing,
      _cName = Nothing,
      _cLastUpdateTime = Nothing
    }

-- | When the channel was created.
cCreationTime :: Lens' Channel (Maybe UTCTime)
cCreationTime = lens _cCreationTime (\s a -> s {_cCreationTime = a}) . mapping _Time

-- | The status of the channel.
cStatus :: Lens' Channel (Maybe ChannelStatus)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The last time when a new message arrived in the channel. AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation. This feature only applies to messages that arrived in the data store after October 23, 2020.
cLastMessageArrivalTime :: Lens' Channel (Maybe UTCTime)
cLastMessageArrivalTime = lens _cLastMessageArrivalTime (\s a -> s {_cLastMessageArrivalTime = a}) . mapping _Time

-- | The ARN of the channel.
cArn :: Lens' Channel (Maybe Text)
cArn = lens _cArn (\s a -> s {_cArn = a})

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
cStorage :: Lens' Channel (Maybe ChannelStorage)
cStorage = lens _cStorage (\s a -> s {_cStorage = a})

-- | How long, in days, message data is kept for the channel.
cRetentionPeriod :: Lens' Channel (Maybe RetentionPeriod)
cRetentionPeriod = lens _cRetentionPeriod (\s a -> s {_cRetentionPeriod = a})

-- | The name of the channel.
cName :: Lens' Channel (Maybe Text)
cName = lens _cName (\s a -> s {_cName = a})

-- | When the channel was last updated.
cLastUpdateTime :: Lens' Channel (Maybe UTCTime)
cLastUpdateTime = lens _cLastUpdateTime (\s a -> s {_cLastUpdateTime = a}) . mapping _Time

instance FromJSON Channel where
  parseJSON =
    withObject
      "Channel"
      ( \x ->
          Channel'
            <$> (x .:? "creationTime")
            <*> (x .:? "status")
            <*> (x .:? "lastMessageArrivalTime")
            <*> (x .:? "arn")
            <*> (x .:? "storage")
            <*> (x .:? "retentionPeriod")
            <*> (x .:? "name")
            <*> (x .:? "lastUpdateTime")
      )

instance Hashable Channel

instance NFData Channel
