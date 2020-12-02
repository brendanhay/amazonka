{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SNSAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SNSAction where

import Network.AWS.IoT.Types.MessageFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action to publish to an Amazon SNS topic.
--
--
--
-- /See:/ 'snsAction' smart constructor.
data SNSAction = SNSAction'
  { _snsaMessageFormat ::
      !(Maybe MessageFormat),
    _snsaTargetARN :: !Text,
    _snsaRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SNSAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snsaMessageFormat' - (Optional) The message format of the message to publish. Accepted values are "JSON" and "RAW". The default value of the attribute is "RAW". SNS uses this setting to determine if the payload should be parsed and relevant platform-specific bits of the payload should be extracted. To read more about SNS message formats, see <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to their official documentation.
--
-- * 'snsaTargetARN' - The ARN of the SNS topic.
--
-- * 'snsaRoleARN' - The ARN of the IAM role that grants access.
snsAction ::
  -- | 'snsaTargetARN'
  Text ->
  -- | 'snsaRoleARN'
  Text ->
  SNSAction
snsAction pTargetARN_ pRoleARN_ =
  SNSAction'
    { _snsaMessageFormat = Nothing,
      _snsaTargetARN = pTargetARN_,
      _snsaRoleARN = pRoleARN_
    }

-- | (Optional) The message format of the message to publish. Accepted values are "JSON" and "RAW". The default value of the attribute is "RAW". SNS uses this setting to determine if the payload should be parsed and relevant platform-specific bits of the payload should be extracted. To read more about SNS message formats, see <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to their official documentation.
snsaMessageFormat :: Lens' SNSAction (Maybe MessageFormat)
snsaMessageFormat = lens _snsaMessageFormat (\s a -> s {_snsaMessageFormat = a})

-- | The ARN of the SNS topic.
snsaTargetARN :: Lens' SNSAction Text
snsaTargetARN = lens _snsaTargetARN (\s a -> s {_snsaTargetARN = a})

-- | The ARN of the IAM role that grants access.
snsaRoleARN :: Lens' SNSAction Text
snsaRoleARN = lens _snsaRoleARN (\s a -> s {_snsaRoleARN = a})

instance FromJSON SNSAction where
  parseJSON =
    withObject
      "SNSAction"
      ( \x ->
          SNSAction'
            <$> (x .:? "messageFormat")
            <*> (x .: "targetArn")
            <*> (x .: "roleArn")
      )

instance Hashable SNSAction

instance NFData SNSAction

instance ToJSON SNSAction where
  toJSON SNSAction' {..} =
    object
      ( catMaybes
          [ ("messageFormat" .=) <$> _snsaMessageFormat,
            Just ("targetArn" .= _snsaTargetARN),
            Just ("roleArn" .= _snsaRoleARN)
          ]
      )
