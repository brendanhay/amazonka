{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DescribeStreamConsumer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To get the description of a registered consumer, provide the ARN of the consumer. Alternatively, you can provide the ARN of the data stream and the name you gave the consumer when you registered it. You may also provide all three parameters, as long as they don't conflict with each other. If you don't know the name or ARN of the consumer that you want to describe, you can use the 'ListStreamConsumers' operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream.
--
--
-- This operation has a limit of 20 transactions per second per stream.
module Network.AWS.Kinesis.DescribeStreamConsumer
  ( -- * Creating a Request
    describeStreamConsumer,
    DescribeStreamConsumer,

    -- * Request Lenses
    dConsumerARN,
    dStreamARN,
    dConsumerName,

    -- * Destructuring the Response
    describeStreamConsumerResponse,
    DescribeStreamConsumerResponse,

    -- * Response Lenses
    dscrsResponseStatus,
    dscrsConsumerDescription,
  )
where

import Network.AWS.Kinesis.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeStreamConsumer' smart constructor.
data DescribeStreamConsumer = DescribeStreamConsumer'
  { _dConsumerARN ::
      !(Maybe Text),
    _dStreamARN :: !(Maybe Text),
    _dConsumerName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStreamConsumer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dConsumerARN' - The ARN returned by Kinesis Data Streams when you registered the consumer.
--
-- * 'dStreamARN' - The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'dConsumerName' - The name that you gave to the consumer.
describeStreamConsumer ::
  DescribeStreamConsumer
describeStreamConsumer =
  DescribeStreamConsumer'
    { _dConsumerARN = Nothing,
      _dStreamARN = Nothing,
      _dConsumerName = Nothing
    }

-- | The ARN returned by Kinesis Data Streams when you registered the consumer.
dConsumerARN :: Lens' DescribeStreamConsumer (Maybe Text)
dConsumerARN = lens _dConsumerARN (\s a -> s {_dConsumerARN = a})

-- | The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
dStreamARN :: Lens' DescribeStreamConsumer (Maybe Text)
dStreamARN = lens _dStreamARN (\s a -> s {_dStreamARN = a})

-- | The name that you gave to the consumer.
dConsumerName :: Lens' DescribeStreamConsumer (Maybe Text)
dConsumerName = lens _dConsumerName (\s a -> s {_dConsumerName = a})

instance AWSRequest DescribeStreamConsumer where
  type Rs DescribeStreamConsumer = DescribeStreamConsumerResponse
  request = postJSON kinesis
  response =
    receiveJSON
      ( \s h x ->
          DescribeStreamConsumerResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "ConsumerDescription")
      )

instance Hashable DescribeStreamConsumer

instance NFData DescribeStreamConsumer

instance ToHeaders DescribeStreamConsumer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Kinesis_20131202.DescribeStreamConsumer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeStreamConsumer where
  toJSON DescribeStreamConsumer' {..} =
    object
      ( catMaybes
          [ ("ConsumerARN" .=) <$> _dConsumerARN,
            ("StreamARN" .=) <$> _dStreamARN,
            ("ConsumerName" .=) <$> _dConsumerName
          ]
      )

instance ToPath DescribeStreamConsumer where
  toPath = const "/"

instance ToQuery DescribeStreamConsumer where
  toQuery = const mempty

-- | /See:/ 'describeStreamConsumerResponse' smart constructor.
data DescribeStreamConsumerResponse = DescribeStreamConsumerResponse'
  { _dscrsResponseStatus ::
      !Int,
    _dscrsConsumerDescription ::
      !ConsumerDescription
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeStreamConsumerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrsResponseStatus' - -- | The response status code.
--
-- * 'dscrsConsumerDescription' - An object that represents the details of the consumer.
describeStreamConsumerResponse ::
  -- | 'dscrsResponseStatus'
  Int ->
  -- | 'dscrsConsumerDescription'
  ConsumerDescription ->
  DescribeStreamConsumerResponse
describeStreamConsumerResponse
  pResponseStatus_
  pConsumerDescription_ =
    DescribeStreamConsumerResponse'
      { _dscrsResponseStatus =
          pResponseStatus_,
        _dscrsConsumerDescription = pConsumerDescription_
      }

-- | -- | The response status code.
dscrsResponseStatus :: Lens' DescribeStreamConsumerResponse Int
dscrsResponseStatus = lens _dscrsResponseStatus (\s a -> s {_dscrsResponseStatus = a})

-- | An object that represents the details of the consumer.
dscrsConsumerDescription :: Lens' DescribeStreamConsumerResponse ConsumerDescription
dscrsConsumerDescription = lens _dscrsConsumerDescription (\s a -> s {_dscrsConsumerDescription = a})

instance NFData DescribeStreamConsumerResponse
