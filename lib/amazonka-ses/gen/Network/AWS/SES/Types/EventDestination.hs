{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.EventDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.EventDestination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.CloudWatchDestination
import Network.AWS.SES.Types.EventType
import Network.AWS.SES.Types.KinesisFirehoseDestination
import Network.AWS.SES.Types.SNSDestination

-- | Contains information about the event destination that the specified email sending events will be published to.
--
--
-- Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'eventDestination' smart constructor.
data EventDestination = EventDestination'
  { _edEnabled ::
      !(Maybe Bool),
    _edKinesisFirehoseDestination ::
      !(Maybe KinesisFirehoseDestination),
    _edCloudWatchDestination ::
      !(Maybe CloudWatchDestination),
    _edSNSDestination :: !(Maybe SNSDestination),
    _edName :: !Text,
    _edMatchingEventTypes :: ![EventType]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edEnabled' - Sets whether Amazon SES publishes events to this destination when you send an email with the associated configuration set. Set to @true@ to enable publishing to this destination; set to @false@ to prevent publishing to this destination. The default value is @false@ .
--
-- * 'edKinesisFirehoseDestination' - An object that contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
--
-- * 'edCloudWatchDestination' - An object that contains the names, default values, and sources of the dimensions associated with an Amazon CloudWatch event destination.
--
-- * 'edSNSDestination' - An object that contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
--
-- * 'edName' - The name of the event destination. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 64 characters.
--
-- * 'edMatchingEventTypes' - The type of email sending events to publish to the event destination.
eventDestination ::
  -- | 'edName'
  Text ->
  EventDestination
eventDestination pName_ =
  EventDestination'
    { _edEnabled = Nothing,
      _edKinesisFirehoseDestination = Nothing,
      _edCloudWatchDestination = Nothing,
      _edSNSDestination = Nothing,
      _edName = pName_,
      _edMatchingEventTypes = mempty
    }

-- | Sets whether Amazon SES publishes events to this destination when you send an email with the associated configuration set. Set to @true@ to enable publishing to this destination; set to @false@ to prevent publishing to this destination. The default value is @false@ .
edEnabled :: Lens' EventDestination (Maybe Bool)
edEnabled = lens _edEnabled (\s a -> s {_edEnabled = a})

-- | An object that contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
edKinesisFirehoseDestination :: Lens' EventDestination (Maybe KinesisFirehoseDestination)
edKinesisFirehoseDestination = lens _edKinesisFirehoseDestination (\s a -> s {_edKinesisFirehoseDestination = a})

-- | An object that contains the names, default values, and sources of the dimensions associated with an Amazon CloudWatch event destination.
edCloudWatchDestination :: Lens' EventDestination (Maybe CloudWatchDestination)
edCloudWatchDestination = lens _edCloudWatchDestination (\s a -> s {_edCloudWatchDestination = a})

-- | An object that contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
edSNSDestination :: Lens' EventDestination (Maybe SNSDestination)
edSNSDestination = lens _edSNSDestination (\s a -> s {_edSNSDestination = a})

-- | The name of the event destination. The name must:     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain less than 64 characters.
edName :: Lens' EventDestination Text
edName = lens _edName (\s a -> s {_edName = a})

-- | The type of email sending events to publish to the event destination.
edMatchingEventTypes :: Lens' EventDestination [EventType]
edMatchingEventTypes = lens _edMatchingEventTypes (\s a -> s {_edMatchingEventTypes = a}) . _Coerce

instance FromXML EventDestination where
  parseXML x =
    EventDestination'
      <$> (x .@? "Enabled")
      <*> (x .@? "KinesisFirehoseDestination")
      <*> (x .@? "CloudWatchDestination")
      <*> (x .@? "SNSDestination")
      <*> (x .@ "Name")
      <*> (x .@? "MatchingEventTypes" .!@ mempty >>= parseXMLList "member")

instance Hashable EventDestination

instance NFData EventDestination

instance ToQuery EventDestination where
  toQuery EventDestination' {..} =
    mconcat
      [ "Enabled" =: _edEnabled,
        "KinesisFirehoseDestination" =: _edKinesisFirehoseDestination,
        "CloudWatchDestination" =: _edCloudWatchDestination,
        "SNSDestination" =: _edSNSDestination,
        "Name" =: _edName,
        "MatchingEventTypes" =: toQueryList "member" _edMatchingEventTypes
      ]
