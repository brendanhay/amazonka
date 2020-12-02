{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NotificationConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LambdaFunctionConfiguration
import Network.AWS.S3.Types.QueueConfiguration
import Network.AWS.S3.Types.TopicConfiguration

-- | A container for specifying the notification configuration of the bucket. If this element is empty, notifications are turned off for the bucket.
--
--
--
-- /See:/ 'notificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { _ncQueueConfigurations ::
      !(Maybe [QueueConfiguration]),
    _ncTopicConfigurations ::
      !(Maybe [TopicConfiguration]),
    _ncLambdaFunctionConfigurations ::
      !(Maybe [LambdaFunctionConfiguration])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncQueueConfigurations' - The Amazon Simple Queue Service queues to publish messages to and the events for which to publish messages.
--
-- * 'ncTopicConfigurations' - The topic to which notifications are sent and the events for which notifications are generated.
--
-- * 'ncLambdaFunctionConfigurations' - Describes the AWS Lambda functions to invoke and the events for which to invoke them.
notificationConfiguration ::
  NotificationConfiguration
notificationConfiguration =
  NotificationConfiguration'
    { _ncQueueConfigurations = Nothing,
      _ncTopicConfigurations = Nothing,
      _ncLambdaFunctionConfigurations = Nothing
    }

-- | The Amazon Simple Queue Service queues to publish messages to and the events for which to publish messages.
ncQueueConfigurations :: Lens' NotificationConfiguration [QueueConfiguration]
ncQueueConfigurations = lens _ncQueueConfigurations (\s a -> s {_ncQueueConfigurations = a}) . _Default . _Coerce

-- | The topic to which notifications are sent and the events for which notifications are generated.
ncTopicConfigurations :: Lens' NotificationConfiguration [TopicConfiguration]
ncTopicConfigurations = lens _ncTopicConfigurations (\s a -> s {_ncTopicConfigurations = a}) . _Default . _Coerce

-- | Describes the AWS Lambda functions to invoke and the events for which to invoke them.
ncLambdaFunctionConfigurations :: Lens' NotificationConfiguration [LambdaFunctionConfiguration]
ncLambdaFunctionConfigurations = lens _ncLambdaFunctionConfigurations (\s a -> s {_ncLambdaFunctionConfigurations = a}) . _Default . _Coerce

instance FromXML NotificationConfiguration where
  parseXML x =
    NotificationConfiguration'
      <$> (may (parseXMLList "QueueConfiguration") x)
      <*> (may (parseXMLList "TopicConfiguration") x)
      <*> (may (parseXMLList "CloudFunctionConfiguration") x)

instance Hashable NotificationConfiguration

instance NFData NotificationConfiguration

instance ToXML NotificationConfiguration where
  toXML NotificationConfiguration' {..} =
    mconcat
      [ toXML (toXMLList "QueueConfiguration" <$> _ncQueueConfigurations),
        toXML (toXMLList "TopicConfiguration" <$> _ncTopicConfigurations),
        toXML
          ( toXMLList "CloudFunctionConfiguration"
              <$> _ncLambdaFunctionConfigurations
          )
      ]
