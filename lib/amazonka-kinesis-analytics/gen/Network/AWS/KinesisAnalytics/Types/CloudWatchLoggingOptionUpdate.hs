{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes CloudWatch logging option updates.
--
--
--
-- /See:/ 'cloudWatchLoggingOptionUpdate' smart constructor.
data CloudWatchLoggingOptionUpdate = CloudWatchLoggingOptionUpdate'
  { _cwlouRoleARNUpdate ::
      !(Maybe Text),
    _cwlouLogStreamARNUpdate ::
      !(Maybe Text),
    _cwlouCloudWatchLoggingOptionId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchLoggingOptionUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwlouRoleARNUpdate' - IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
--
-- * 'cwlouLogStreamARNUpdate' - ARN of the CloudWatch log to receive application messages.
--
-- * 'cwlouCloudWatchLoggingOptionId' - ID of the CloudWatch logging option to update
cloudWatchLoggingOptionUpdate ::
  -- | 'cwlouCloudWatchLoggingOptionId'
  Text ->
  CloudWatchLoggingOptionUpdate
cloudWatchLoggingOptionUpdate pCloudWatchLoggingOptionId_ =
  CloudWatchLoggingOptionUpdate'
    { _cwlouRoleARNUpdate = Nothing,
      _cwlouLogStreamARNUpdate = Nothing,
      _cwlouCloudWatchLoggingOptionId = pCloudWatchLoggingOptionId_
    }

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role used must have the @PutLogEvents@ policy action enabled.
cwlouRoleARNUpdate :: Lens' CloudWatchLoggingOptionUpdate (Maybe Text)
cwlouRoleARNUpdate = lens _cwlouRoleARNUpdate (\s a -> s {_cwlouRoleARNUpdate = a})

-- | ARN of the CloudWatch log to receive application messages.
cwlouLogStreamARNUpdate :: Lens' CloudWatchLoggingOptionUpdate (Maybe Text)
cwlouLogStreamARNUpdate = lens _cwlouLogStreamARNUpdate (\s a -> s {_cwlouLogStreamARNUpdate = a})

-- | ID of the CloudWatch logging option to update
cwlouCloudWatchLoggingOptionId :: Lens' CloudWatchLoggingOptionUpdate Text
cwlouCloudWatchLoggingOptionId = lens _cwlouCloudWatchLoggingOptionId (\s a -> s {_cwlouCloudWatchLoggingOptionId = a})

instance Hashable CloudWatchLoggingOptionUpdate

instance NFData CloudWatchLoggingOptionUpdate

instance ToJSON CloudWatchLoggingOptionUpdate where
  toJSON CloudWatchLoggingOptionUpdate' {..} =
    object
      ( catMaybes
          [ ("RoleARNUpdate" .=) <$> _cwlouRoleARNUpdate,
            ("LogStreamARNUpdate" .=) <$> _cwlouLogStreamARNUpdate,
            Just
              ("CloudWatchLoggingOptionId" .= _cwlouCloudWatchLoggingOptionId)
          ]
      )
