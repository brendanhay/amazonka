{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LogSettingsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.LogSettingsRequest where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.Destination
import Network.AWS.LexModels.Types.LogType
import Network.AWS.Prelude

-- | Settings used to configure delivery mode and destination for conversation logs.
--
--
--
-- /See:/ 'logSettingsRequest' smart constructor.
data LogSettingsRequest = LogSettingsRequest'
  { _lsrKmsKeyARN ::
      !(Maybe Text),
    _lsrLogType :: !LogType,
    _lsrDestination :: !Destination,
    _lsrResourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogSettingsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrKmsKeyARN' - The Amazon Resource Name (ARN) of the AWS KMS customer managed key for encrypting audio logs delivered to an S3 bucket. The key does not apply to CloudWatch Logs and is optional for S3 buckets.
--
-- * 'lsrLogType' - The type of logging to enable. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
--
-- * 'lsrDestination' - Where the logs will be delivered. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
--
-- * 'lsrResourceARN' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs should be delivered.
logSettingsRequest ::
  -- | 'lsrLogType'
  LogType ->
  -- | 'lsrDestination'
  Destination ->
  -- | 'lsrResourceARN'
  Text ->
  LogSettingsRequest
logSettingsRequest pLogType_ pDestination_ pResourceARN_ =
  LogSettingsRequest'
    { _lsrKmsKeyARN = Nothing,
      _lsrLogType = pLogType_,
      _lsrDestination = pDestination_,
      _lsrResourceARN = pResourceARN_
    }

-- | The Amazon Resource Name (ARN) of the AWS KMS customer managed key for encrypting audio logs delivered to an S3 bucket. The key does not apply to CloudWatch Logs and is optional for S3 buckets.
lsrKmsKeyARN :: Lens' LogSettingsRequest (Maybe Text)
lsrKmsKeyARN = lens _lsrKmsKeyARN (\s a -> s {_lsrKmsKeyARN = a})

-- | The type of logging to enable. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
lsrLogType :: Lens' LogSettingsRequest LogType
lsrLogType = lens _lsrLogType (\s a -> s {_lsrLogType = a})

-- | Where the logs will be delivered. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
lsrDestination :: Lens' LogSettingsRequest Destination
lsrDestination = lens _lsrDestination (\s a -> s {_lsrDestination = a})

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs should be delivered.
lsrResourceARN :: Lens' LogSettingsRequest Text
lsrResourceARN = lens _lsrResourceARN (\s a -> s {_lsrResourceARN = a})

instance Hashable LogSettingsRequest

instance NFData LogSettingsRequest

instance ToJSON LogSettingsRequest where
  toJSON LogSettingsRequest' {..} =
    object
      ( catMaybes
          [ ("kmsKeyArn" .=) <$> _lsrKmsKeyARN,
            Just ("logType" .= _lsrLogType),
            Just ("destination" .= _lsrDestination),
            Just ("resourceArn" .= _lsrResourceARN)
          ]
      )
