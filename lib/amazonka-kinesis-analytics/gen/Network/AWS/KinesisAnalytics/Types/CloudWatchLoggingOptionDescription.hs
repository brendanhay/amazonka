{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Description of the CloudWatch logging option.
--
--
--
-- /See:/ 'cloudWatchLoggingOptionDescription' smart constructor.
data CloudWatchLoggingOptionDescription = CloudWatchLoggingOptionDescription'
  { _cwlodCloudWatchLoggingOptionId ::
      !(Maybe Text),
    _cwlodLogStreamARN ::
      !Text,
    _cwlodRoleARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchLoggingOptionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwlodCloudWatchLoggingOptionId' - ID of the CloudWatch logging option description.
--
-- * 'cwlodLogStreamARN' - ARN of the CloudWatch log to receive application messages.
--
-- * 'cwlodRoleARN' - IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
cloudWatchLoggingOptionDescription ::
  -- | 'cwlodLogStreamARN'
  Text ->
  -- | 'cwlodRoleARN'
  Text ->
  CloudWatchLoggingOptionDescription
cloudWatchLoggingOptionDescription pLogStreamARN_ pRoleARN_ =
  CloudWatchLoggingOptionDescription'
    { _cwlodCloudWatchLoggingOptionId =
        Nothing,
      _cwlodLogStreamARN = pLogStreamARN_,
      _cwlodRoleARN = pRoleARN_
    }

-- | ID of the CloudWatch logging option description.
cwlodCloudWatchLoggingOptionId :: Lens' CloudWatchLoggingOptionDescription (Maybe Text)
cwlodCloudWatchLoggingOptionId = lens _cwlodCloudWatchLoggingOptionId (\s a -> s {_cwlodCloudWatchLoggingOptionId = a})

-- | ARN of the CloudWatch log to receive application messages.
cwlodLogStreamARN :: Lens' CloudWatchLoggingOptionDescription Text
cwlodLogStreamARN = lens _cwlodLogStreamARN (\s a -> s {_cwlodLogStreamARN = a})

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
cwlodRoleARN :: Lens' CloudWatchLoggingOptionDescription Text
cwlodRoleARN = lens _cwlodRoleARN (\s a -> s {_cwlodRoleARN = a})

instance FromJSON CloudWatchLoggingOptionDescription where
  parseJSON =
    withObject
      "CloudWatchLoggingOptionDescription"
      ( \x ->
          CloudWatchLoggingOptionDescription'
            <$> (x .:? "CloudWatchLoggingOptionId")
            <*> (x .: "LogStreamARN")
            <*> (x .: "RoleARN")
      )

instance Hashable CloudWatchLoggingOptionDescription

instance NFData CloudWatchLoggingOptionDescription
