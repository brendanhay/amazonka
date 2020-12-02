{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.DeadLetterConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq dead-letter queue> for failed asynchronous invocations.
--
--
--
-- /See:/ 'deadLetterConfig' smart constructor.
newtype DeadLetterConfig = DeadLetterConfig'
  { _dlcTargetARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeadLetterConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcTargetARN' - The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic.
deadLetterConfig ::
  DeadLetterConfig
deadLetterConfig = DeadLetterConfig' {_dlcTargetARN = Nothing}

-- | The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic.
dlcTargetARN :: Lens' DeadLetterConfig (Maybe Text)
dlcTargetARN = lens _dlcTargetARN (\s a -> s {_dlcTargetARN = a})

instance FromJSON DeadLetterConfig where
  parseJSON =
    withObject
      "DeadLetterConfig"
      (\x -> DeadLetterConfig' <$> (x .:? "TargetArn"))

instance Hashable DeadLetterConfig

instance NFData DeadLetterConfig

instance ToJSON DeadLetterConfig where
  toJSON DeadLetterConfig' {..} =
    object (catMaybes [("TargetArn" .=) <$> _dlcTargetARN])
