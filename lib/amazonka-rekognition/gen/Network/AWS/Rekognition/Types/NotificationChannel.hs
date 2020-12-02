{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.NotificationChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.NotificationChannel where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon Simple Notification Service topic to which Amazon Rekognition publishes the completion status of a video analysis operation. For more information, see 'api-video' .
--
--
--
-- /See:/ 'notificationChannel' smart constructor.
data NotificationChannel = NotificationChannel'
  { _ncSNSTopicARN ::
      !Text,
    _ncRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncSNSTopicARN' - The Amazon SNS topic to which Amazon Rekognition to posts the completion status.
--
-- * 'ncRoleARN' - The ARN of an IAM role that gives Amazon Rekognition publishing permissions to the Amazon SNS topic.
notificationChannel ::
  -- | 'ncSNSTopicARN'
  Text ->
  -- | 'ncRoleARN'
  Text ->
  NotificationChannel
notificationChannel pSNSTopicARN_ pRoleARN_ =
  NotificationChannel'
    { _ncSNSTopicARN = pSNSTopicARN_,
      _ncRoleARN = pRoleARN_
    }

-- | The Amazon SNS topic to which Amazon Rekognition to posts the completion status.
ncSNSTopicARN :: Lens' NotificationChannel Text
ncSNSTopicARN = lens _ncSNSTopicARN (\s a -> s {_ncSNSTopicARN = a})

-- | The ARN of an IAM role that gives Amazon Rekognition publishing permissions to the Amazon SNS topic.
ncRoleARN :: Lens' NotificationChannel Text
ncRoleARN = lens _ncRoleARN (\s a -> s {_ncRoleARN = a})

instance Hashable NotificationChannel

instance NFData NotificationChannel

instance ToJSON NotificationChannel where
  toJSON NotificationChannel' {..} =
    object
      ( catMaybes
          [ Just ("SNSTopicArn" .= _ncSNSTopicARN),
            Just ("RoleArn" .= _ncRoleARN)
          ]
      )
