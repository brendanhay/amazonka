{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NotificationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).
--
--
--
-- /See:/ 'notificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { _ncTopicStatus ::
      !(Maybe Text),
    _ncTopicARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncTopicStatus' - The current state of the topic.
--
-- * 'ncTopicARN' - The Amazon Resource Name (ARN) that identifies the topic.
notificationConfiguration ::
  NotificationConfiguration
notificationConfiguration =
  NotificationConfiguration'
    { _ncTopicStatus = Nothing,
      _ncTopicARN = Nothing
    }

-- | The current state of the topic.
ncTopicStatus :: Lens' NotificationConfiguration (Maybe Text)
ncTopicStatus = lens _ncTopicStatus (\s a -> s {_ncTopicStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the topic.
ncTopicARN :: Lens' NotificationConfiguration (Maybe Text)
ncTopicARN = lens _ncTopicARN (\s a -> s {_ncTopicARN = a})

instance FromXML NotificationConfiguration where
  parseXML x =
    NotificationConfiguration'
      <$> (x .@? "TopicStatus") <*> (x .@? "TopicArn")

instance Hashable NotificationConfiguration

instance NFData NotificationConfiguration
