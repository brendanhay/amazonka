{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Notification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Notification where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.JobState

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
--
--
-- When the notification settings are defined during job creation, you can choose to notify based on a specific set of job states using the @JobStatesToNotify@ array of strings, or you can specify that you want to have Amazon SNS notifications sent out for all job states with @NotifyAll@ set to true.
--
--
-- /See:/ 'notification' smart constructor.
data Notification = Notification'
  { _nNotifyAll :: !(Maybe Bool),
    _nSNSTopicARN :: !(Maybe Text),
    _nJobStatesToNotify :: !(Maybe [JobState])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Notification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nNotifyAll' - Any change in job state will trigger a notification for this job.
--
-- * 'nSNSTopicARN' - The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action. You can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
--
-- * 'nJobStatesToNotify' - The list of job states that will trigger a notification for this job.
notification ::
  Notification
notification =
  Notification'
    { _nNotifyAll = Nothing,
      _nSNSTopicARN = Nothing,
      _nJobStatesToNotify = Nothing
    }

-- | Any change in job state will trigger a notification for this job.
nNotifyAll :: Lens' Notification (Maybe Bool)
nNotifyAll = lens _nNotifyAll (\s a -> s {_nNotifyAll = a})

-- | The new SNS @TopicArn@ that you want to associate with this job. You can create Amazon Resource Names (ARNs) for topics by using the <https://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html CreateTopic> Amazon SNS API action. You can subscribe email addresses to an Amazon SNS topic through the AWS Management Console, or by using the <https://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html Subscribe> AWS Simple Notification Service (SNS) API action.
nSNSTopicARN :: Lens' Notification (Maybe Text)
nSNSTopicARN = lens _nSNSTopicARN (\s a -> s {_nSNSTopicARN = a})

-- | The list of job states that will trigger a notification for this job.
nJobStatesToNotify :: Lens' Notification [JobState]
nJobStatesToNotify = lens _nJobStatesToNotify (\s a -> s {_nJobStatesToNotify = a}) . _Default . _Coerce

instance FromJSON Notification where
  parseJSON =
    withObject
      "Notification"
      ( \x ->
          Notification'
            <$> (x .:? "NotifyAll")
            <*> (x .:? "SnsTopicARN")
            <*> (x .:? "JobStatesToNotify" .!= mempty)
      )

instance Hashable Notification

instance NFData Notification

instance ToJSON Notification where
  toJSON Notification' {..} =
    object
      ( catMaybes
          [ ("NotifyAll" .=) <$> _nNotifyAll,
            ("SnsTopicARN" .=) <$> _nSNSTopicARN,
            ("JobStatesToNotify" .=) <$> _nJobStatesToNotify
          ]
      )
