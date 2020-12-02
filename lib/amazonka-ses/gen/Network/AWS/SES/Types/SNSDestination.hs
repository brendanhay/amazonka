{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SNSDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SNSDestination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the topic ARN associated with an Amazon Simple Notification Service (Amazon SNS) event destination.
--
--
-- Event destinations, such as Amazon SNS, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'snsDestination' smart constructor.
newtype SNSDestination = SNSDestination' {_sdTopicARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SNSDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdTopicARN' - The ARN of the Amazon SNS topic that email sending events will be published to. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
snsDestination ::
  -- | 'sdTopicARN'
  Text ->
  SNSDestination
snsDestination pTopicARN_ =
  SNSDestination' {_sdTopicARN = pTopicARN_}

-- | The ARN of the Amazon SNS topic that email sending events will be published to. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
sdTopicARN :: Lens' SNSDestination Text
sdTopicARN = lens _sdTopicARN (\s a -> s {_sdTopicARN = a})

instance FromXML SNSDestination where
  parseXML x = SNSDestination' <$> (x .@ "TopicARN")

instance Hashable SNSDestination

instance NFData SNSDestination

instance ToQuery SNSDestination where
  toQuery SNSDestination' {..} = mconcat ["TopicARN" =: _sdTopicARN]
