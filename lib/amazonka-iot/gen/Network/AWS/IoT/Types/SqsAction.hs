{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SqsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SqsAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action to publish data to an Amazon SQS queue.
--
--
--
-- /See:/ 'sqsAction' smart constructor.
data SqsAction = SqsAction'
  { _saUseBase64 :: !(Maybe Bool),
    _saRoleARN :: !Text,
    _saQueueURL :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SqsAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saUseBase64' - Specifies whether to use Base64 encoding.
--
-- * 'saRoleARN' - The ARN of the IAM role that grants access.
--
-- * 'saQueueURL' - The URL of the Amazon SQS queue.
sqsAction ::
  -- | 'saRoleARN'
  Text ->
  -- | 'saQueueURL'
  Text ->
  SqsAction
sqsAction pRoleARN_ pQueueURL_ =
  SqsAction'
    { _saUseBase64 = Nothing,
      _saRoleARN = pRoleARN_,
      _saQueueURL = pQueueURL_
    }

-- | Specifies whether to use Base64 encoding.
saUseBase64 :: Lens' SqsAction (Maybe Bool)
saUseBase64 = lens _saUseBase64 (\s a -> s {_saUseBase64 = a})

-- | The ARN of the IAM role that grants access.
saRoleARN :: Lens' SqsAction Text
saRoleARN = lens _saRoleARN (\s a -> s {_saRoleARN = a})

-- | The URL of the Amazon SQS queue.
saQueueURL :: Lens' SqsAction Text
saQueueURL = lens _saQueueURL (\s a -> s {_saQueueURL = a})

instance FromJSON SqsAction where
  parseJSON =
    withObject
      "SqsAction"
      ( \x ->
          SqsAction'
            <$> (x .:? "useBase64") <*> (x .: "roleArn") <*> (x .: "queueUrl")
      )

instance Hashable SqsAction

instance NFData SqsAction

instance ToJSON SqsAction where
  toJSON SqsAction' {..} =
    object
      ( catMaybes
          [ ("useBase64" .=) <$> _saUseBase64,
            Just ("roleArn" .= _saRoleARN),
            Just ("queueUrl" .= _saQueueURL)
          ]
      )
