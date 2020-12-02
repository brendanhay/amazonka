{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.KinesisStreamSourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KinesisStreamSourceDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about a Kinesis data stream used as the source for a Kinesis Data Firehose delivery stream.
--
--
--
-- /See:/ 'kinesisStreamSourceDescription' smart constructor.
data KinesisStreamSourceDescription = KinesisStreamSourceDescription'
  { _kssdDeliveryStartTimestamp ::
      !(Maybe POSIX),
    _kssdKinesisStreamARN ::
      !(Maybe Text),
    _kssdRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamSourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kssdDeliveryStartTimestamp' - Kinesis Data Firehose starts retrieving records from the Kinesis data stream starting with this timestamp.
--
-- * 'kssdKinesisStreamARN' - The Amazon Resource Name (ARN) of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
--
-- * 'kssdRoleARN' - The ARN of the role used by the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
kinesisStreamSourceDescription ::
  KinesisStreamSourceDescription
kinesisStreamSourceDescription =
  KinesisStreamSourceDescription'
    { _kssdDeliveryStartTimestamp =
        Nothing,
      _kssdKinesisStreamARN = Nothing,
      _kssdRoleARN = Nothing
    }

-- | Kinesis Data Firehose starts retrieving records from the Kinesis data stream starting with this timestamp.
kssdDeliveryStartTimestamp :: Lens' KinesisStreamSourceDescription (Maybe UTCTime)
kssdDeliveryStartTimestamp = lens _kssdDeliveryStartTimestamp (\s a -> s {_kssdDeliveryStartTimestamp = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
kssdKinesisStreamARN :: Lens' KinesisStreamSourceDescription (Maybe Text)
kssdKinesisStreamARN = lens _kssdKinesisStreamARN (\s a -> s {_kssdKinesisStreamARN = a})

-- | The ARN of the role used by the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
kssdRoleARN :: Lens' KinesisStreamSourceDescription (Maybe Text)
kssdRoleARN = lens _kssdRoleARN (\s a -> s {_kssdRoleARN = a})

instance FromJSON KinesisStreamSourceDescription where
  parseJSON =
    withObject
      "KinesisStreamSourceDescription"
      ( \x ->
          KinesisStreamSourceDescription'
            <$> (x .:? "DeliveryStartTimestamp")
            <*> (x .:? "KinesisStreamARN")
            <*> (x .:? "RoleARN")
      )

instance Hashable KinesisStreamSourceDescription

instance NFData KinesisStreamSourceDescription
