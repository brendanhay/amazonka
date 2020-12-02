{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.EventTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.EventTopic where

import Network.AWS.DirectoryService.Types.TopicStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about SNS topic and AWS Directory Service directory associations.
--
--
--
-- /See:/ 'eventTopic' smart constructor.
data EventTopic = EventTopic'
  { _etStatus :: !(Maybe TopicStatus),
    _etDirectoryId :: !(Maybe Text),
    _etTopicName :: !(Maybe Text),
    _etTopicARN :: !(Maybe Text),
    _etCreatedDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etStatus' - The topic registration status.
--
-- * 'etDirectoryId' - The Directory ID of an AWS Directory Service directory that will publish status messages to an SNS topic.
--
-- * 'etTopicName' - The name of an AWS SNS topic the receives status messages from the directory.
--
-- * 'etTopicARN' - The SNS topic ARN (Amazon Resource Name).
--
-- * 'etCreatedDateTime' - The date and time of when you associated your directory with the SNS topic.
eventTopic ::
  EventTopic
eventTopic =
  EventTopic'
    { _etStatus = Nothing,
      _etDirectoryId = Nothing,
      _etTopicName = Nothing,
      _etTopicARN = Nothing,
      _etCreatedDateTime = Nothing
    }

-- | The topic registration status.
etStatus :: Lens' EventTopic (Maybe TopicStatus)
etStatus = lens _etStatus (\s a -> s {_etStatus = a})

-- | The Directory ID of an AWS Directory Service directory that will publish status messages to an SNS topic.
etDirectoryId :: Lens' EventTopic (Maybe Text)
etDirectoryId = lens _etDirectoryId (\s a -> s {_etDirectoryId = a})

-- | The name of an AWS SNS topic the receives status messages from the directory.
etTopicName :: Lens' EventTopic (Maybe Text)
etTopicName = lens _etTopicName (\s a -> s {_etTopicName = a})

-- | The SNS topic ARN (Amazon Resource Name).
etTopicARN :: Lens' EventTopic (Maybe Text)
etTopicARN = lens _etTopicARN (\s a -> s {_etTopicARN = a})

-- | The date and time of when you associated your directory with the SNS topic.
etCreatedDateTime :: Lens' EventTopic (Maybe UTCTime)
etCreatedDateTime = lens _etCreatedDateTime (\s a -> s {_etCreatedDateTime = a}) . mapping _Time

instance FromJSON EventTopic where
  parseJSON =
    withObject
      "EventTopic"
      ( \x ->
          EventTopic'
            <$> (x .:? "Status")
            <*> (x .:? "DirectoryId")
            <*> (x .:? "TopicName")
            <*> (x .:? "TopicArn")
            <*> (x .:? "CreatedDateTime")
      )

instance Hashable EventTopic

instance NFData EventTopic
