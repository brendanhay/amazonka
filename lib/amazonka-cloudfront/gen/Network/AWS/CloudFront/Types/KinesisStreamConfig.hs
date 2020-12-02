{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KinesisStreamConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
--
--
-- /See:/ 'kinesisStreamConfig' smart constructor.
data KinesisStreamConfig = KinesisStreamConfig'
  { _kscRoleARN ::
      !Text,
    _kscStreamARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kscRoleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that CloudFront can use to send real-time log data to your Kinesis data stream. For more information the IAM role, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-iam-role Real-time log configuration IAM role> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'kscStreamARN' - The Amazon Resource Name (ARN) of the Kinesis data stream where you are sending real-time log data.
kinesisStreamConfig ::
  -- | 'kscRoleARN'
  Text ->
  -- | 'kscStreamARN'
  Text ->
  KinesisStreamConfig
kinesisStreamConfig pRoleARN_ pStreamARN_ =
  KinesisStreamConfig'
    { _kscRoleARN = pRoleARN_,
      _kscStreamARN = pStreamARN_
    }

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that CloudFront can use to send real-time log data to your Kinesis data stream. For more information the IAM role, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-iam-role Real-time log configuration IAM role> in the /Amazon CloudFront Developer Guide/ .
kscRoleARN :: Lens' KinesisStreamConfig Text
kscRoleARN = lens _kscRoleARN (\s a -> s {_kscRoleARN = a})

-- | The Amazon Resource Name (ARN) of the Kinesis data stream where you are sending real-time log data.
kscStreamARN :: Lens' KinesisStreamConfig Text
kscStreamARN = lens _kscStreamARN (\s a -> s {_kscStreamARN = a})

instance FromXML KinesisStreamConfig where
  parseXML x =
    KinesisStreamConfig' <$> (x .@ "RoleARN") <*> (x .@ "StreamARN")

instance Hashable KinesisStreamConfig

instance NFData KinesisStreamConfig

instance ToXML KinesisStreamConfig where
  toXML KinesisStreamConfig' {..} =
    mconcat ["RoleARN" @= _kscRoleARN, "StreamARN" @= _kscStreamARN]
