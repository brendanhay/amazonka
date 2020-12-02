{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.TopicConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.TopicConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.NotificationConfigurationFilter

-- | A container for specifying the configuration for publication of messages to an Amazon Simple Notification Service (Amazon SNS) topic when Amazon S3 detects specified events.
--
--
--
-- /See:/ 'topicConfiguration' smart constructor.
data TopicConfiguration = TopicConfiguration'
  { _tcId ::
      !(Maybe Text),
    _tcFilter :: !(Maybe NotificationConfigurationFilter),
    _tcTopicARN :: !Text,
    _tcEvents :: ![Event]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TopicConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcId' - Undocumented member.
--
-- * 'tcFilter' - Undocumented member.
--
-- * 'tcTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon S3 publishes a message when it detects events of the specified type.
--
-- * 'tcEvents' - The Amazon S3 bucket event about which to send notifications. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
topicConfiguration ::
  -- | 'tcTopicARN'
  Text ->
  TopicConfiguration
topicConfiguration pTopicARN_ =
  TopicConfiguration'
    { _tcId = Nothing,
      _tcFilter = Nothing,
      _tcTopicARN = pTopicARN_,
      _tcEvents = mempty
    }

-- | Undocumented member.
tcId :: Lens' TopicConfiguration (Maybe Text)
tcId = lens _tcId (\s a -> s {_tcId = a})

-- | Undocumented member.
tcFilter :: Lens' TopicConfiguration (Maybe NotificationConfigurationFilter)
tcFilter = lens _tcFilter (\s a -> s {_tcFilter = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which Amazon S3 publishes a message when it detects events of the specified type.
tcTopicARN :: Lens' TopicConfiguration Text
tcTopicARN = lens _tcTopicARN (\s a -> s {_tcTopicARN = a})

-- | The Amazon S3 bucket event about which to send notifications. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
tcEvents :: Lens' TopicConfiguration [Event]
tcEvents = lens _tcEvents (\s a -> s {_tcEvents = a}) . _Coerce

instance FromXML TopicConfiguration where
  parseXML x =
    TopicConfiguration'
      <$> (x .@? "Id")
      <*> (x .@? "Filter")
      <*> (x .@ "Topic")
      <*> (parseXMLList "Event" x)

instance Hashable TopicConfiguration

instance NFData TopicConfiguration

instance ToXML TopicConfiguration where
  toXML TopicConfiguration' {..} =
    mconcat
      [ "Id" @= _tcId,
        "Filter" @= _tcFilter,
        "Topic" @= _tcTopicARN,
        toXMLList "Event" _tcEvents
      ]
