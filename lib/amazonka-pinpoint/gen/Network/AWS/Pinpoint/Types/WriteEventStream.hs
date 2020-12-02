{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteEventStream where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the Amazon Resource Name (ARN) of an event stream to publish events to and the AWS Identity and Access Management (IAM) role to use when publishing those events.
--
--
--
-- /See:/ 'writeEventStream' smart constructor.
data WriteEventStream = WriteEventStream'
  { _wesRoleARN :: !Text,
    _wesDestinationStreamARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WriteEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wesRoleARN' - The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
--
-- * 'wesDestinationStreamARN' - The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream that you want to publish event data to. For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
writeEventStream ::
  -- | 'wesRoleARN'
  Text ->
  -- | 'wesDestinationStreamARN'
  Text ->
  WriteEventStream
writeEventStream pRoleARN_ pDestinationStreamARN_ =
  WriteEventStream'
    { _wesRoleARN = pRoleARN_,
      _wesDestinationStreamARN = pDestinationStreamARN_
    }

-- | The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
wesRoleARN :: Lens' WriteEventStream Text
wesRoleARN = lens _wesRoleARN (\s a -> s {_wesRoleARN = a})

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream that you want to publish event data to. For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
wesDestinationStreamARN :: Lens' WriteEventStream Text
wesDestinationStreamARN = lens _wesDestinationStreamARN (\s a -> s {_wesDestinationStreamARN = a})

instance Hashable WriteEventStream

instance NFData WriteEventStream

instance ToJSON WriteEventStream where
  toJSON WriteEventStream' {..} =
    object
      ( catMaybes
          [ Just ("RoleArn" .= _wesRoleARN),
            Just ("DestinationStreamArn" .= _wesDestinationStreamARN)
          ]
      )
