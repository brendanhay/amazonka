{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The stream and role Amazon Resource Names (ARNs) for a Kinesis data stream used as the source for a delivery stream.
--
--
--
-- /See:/ 'kinesisStreamSourceConfiguration' smart constructor.
data KinesisStreamSourceConfiguration = KinesisStreamSourceConfiguration'
  { _ksscKinesisStreamARN ::
      !Text,
    _ksscRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamSourceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksscKinesisStreamARN' - The ARN of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
--
-- * 'ksscRoleARN' - The ARN of the role that provides access to the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
kinesisStreamSourceConfiguration ::
  -- | 'ksscKinesisStreamARN'
  Text ->
  -- | 'ksscRoleARN'
  Text ->
  KinesisStreamSourceConfiguration
kinesisStreamSourceConfiguration pKinesisStreamARN_ pRoleARN_ =
  KinesisStreamSourceConfiguration'
    { _ksscKinesisStreamARN =
        pKinesisStreamARN_,
      _ksscRoleARN = pRoleARN_
    }

-- | The ARN of the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Kinesis Data Streams ARN Format> .
ksscKinesisStreamARN :: Lens' KinesisStreamSourceConfiguration Text
ksscKinesisStreamARN = lens _ksscKinesisStreamARN (\s a -> s {_ksscKinesisStreamARN = a})

-- | The ARN of the role that provides access to the source Kinesis data stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM) ARN Format> .
ksscRoleARN :: Lens' KinesisStreamSourceConfiguration Text
ksscRoleARN = lens _ksscRoleARN (\s a -> s {_ksscRoleARN = a})

instance Hashable KinesisStreamSourceConfiguration

instance NFData KinesisStreamSourceConfiguration

instance ToJSON KinesisStreamSourceConfiguration where
  toJSON KinesisStreamSourceConfiguration' {..} =
    object
      ( catMaybes
          [ Just ("KinesisStreamARN" .= _ksscKinesisStreamARN),
            Just ("RoleARN" .= _ksscRoleARN)
          ]
      )
