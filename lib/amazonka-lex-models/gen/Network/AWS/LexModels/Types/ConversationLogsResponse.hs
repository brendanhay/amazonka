{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ConversationLogsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ConversationLogsResponse where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.LogSettingsResponse
import Network.AWS.Prelude

-- | Contains information about conversation log settings.
--
--
--
-- /See:/ 'conversationLogsResponse' smart constructor.
data ConversationLogsResponse = ConversationLogsResponse'
  { _clIamRoleARN ::
      !(Maybe Text),
    _clLogSettings ::
      !(Maybe [LogSettingsResponse])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConversationLogsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clIamRoleARN' - The Amazon Resource Name (ARN) of the IAM role used to write your logs to CloudWatch Logs or an S3 bucket.
--
-- * 'clLogSettings' - The settings for your conversation logs. You can log text, audio, or both.
conversationLogsResponse ::
  ConversationLogsResponse
conversationLogsResponse =
  ConversationLogsResponse'
    { _clIamRoleARN = Nothing,
      _clLogSettings = Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role used to write your logs to CloudWatch Logs or an S3 bucket.
clIamRoleARN :: Lens' ConversationLogsResponse (Maybe Text)
clIamRoleARN = lens _clIamRoleARN (\s a -> s {_clIamRoleARN = a})

-- | The settings for your conversation logs. You can log text, audio, or both.
clLogSettings :: Lens' ConversationLogsResponse [LogSettingsResponse]
clLogSettings = lens _clLogSettings (\s a -> s {_clLogSettings = a}) . _Default . _Coerce

instance FromJSON ConversationLogsResponse where
  parseJSON =
    withObject
      "ConversationLogsResponse"
      ( \x ->
          ConversationLogsResponse'
            <$> (x .:? "iamRoleArn") <*> (x .:? "logSettings" .!= mempty)
      )

instance Hashable ConversationLogsResponse

instance NFData ConversationLogsResponse
