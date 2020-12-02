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
-- Module      : Network.AWS.Kinesis.RegisterStreamConsumer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a consumer with a Kinesis data stream. When you use this operation, the consumer you register can then call 'SubscribeToShard' to receive data from the stream using enhanced fan-out, at a rate of up to 2 MiB per second for every shard you subscribe to. This rate is unaffected by the total number of consumers that read from the same stream.
--
--
-- You can register up to 20 consumers per stream. A given consumer can only be registered with one stream at a time.
--
-- For an example of how to use this operations, see </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API> .
--
-- The use of this operation has a limit of five transactions per second per account. Also, only 5 consumers can be created simultaneously. In other words, you cannot have more than 5 consumers in a @CREATING@ status at the same time. Registering a 6th consumer while there are 5 in a @CREATING@ status results in a @LimitExceededException@ .
module Network.AWS.Kinesis.RegisterStreamConsumer
  ( -- * Creating a Request
    registerStreamConsumer,
    RegisterStreamConsumer,

    -- * Request Lenses
    rscStreamARN,
    rscConsumerName,

    -- * Destructuring the Response
    registerStreamConsumerResponse,
    RegisterStreamConsumerResponse,

    -- * Response Lenses
    rscrsResponseStatus,
    rscrsConsumer,
  )
where

import Network.AWS.Kinesis.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerStreamConsumer' smart constructor.
data RegisterStreamConsumer = RegisterStreamConsumer'
  { _rscStreamARN ::
      !Text,
    _rscConsumerName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterStreamConsumer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rscStreamARN' - The ARN of the Kinesis data stream that you want to register the consumer with. For more info, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'rscConsumerName' - For a given Kinesis data stream, each consumer must have a unique name. However, consumer names don't have to be unique across data streams.
registerStreamConsumer ::
  -- | 'rscStreamARN'
  Text ->
  -- | 'rscConsumerName'
  Text ->
  RegisterStreamConsumer
registerStreamConsumer pStreamARN_ pConsumerName_ =
  RegisterStreamConsumer'
    { _rscStreamARN = pStreamARN_,
      _rscConsumerName = pConsumerName_
    }

-- | The ARN of the Kinesis data stream that you want to register the consumer with. For more info, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
rscStreamARN :: Lens' RegisterStreamConsumer Text
rscStreamARN = lens _rscStreamARN (\s a -> s {_rscStreamARN = a})

-- | For a given Kinesis data stream, each consumer must have a unique name. However, consumer names don't have to be unique across data streams.
rscConsumerName :: Lens' RegisterStreamConsumer Text
rscConsumerName = lens _rscConsumerName (\s a -> s {_rscConsumerName = a})

instance AWSRequest RegisterStreamConsumer where
  type Rs RegisterStreamConsumer = RegisterStreamConsumerResponse
  request = postJSON kinesis
  response =
    receiveJSON
      ( \s h x ->
          RegisterStreamConsumerResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "Consumer")
      )

instance Hashable RegisterStreamConsumer

instance NFData RegisterStreamConsumer

instance ToHeaders RegisterStreamConsumer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Kinesis_20131202.RegisterStreamConsumer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterStreamConsumer where
  toJSON RegisterStreamConsumer' {..} =
    object
      ( catMaybes
          [ Just ("StreamARN" .= _rscStreamARN),
            Just ("ConsumerName" .= _rscConsumerName)
          ]
      )

instance ToPath RegisterStreamConsumer where
  toPath = const "/"

instance ToQuery RegisterStreamConsumer where
  toQuery = const mempty

-- | /See:/ 'registerStreamConsumerResponse' smart constructor.
data RegisterStreamConsumerResponse = RegisterStreamConsumerResponse'
  { _rscrsResponseStatus ::
      !Int,
    _rscrsConsumer :: !Consumer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterStreamConsumerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rscrsResponseStatus' - -- | The response status code.
--
-- * 'rscrsConsumer' - An object that represents the details of the consumer you registered. When you register a consumer, it gets an ARN that is generated by Kinesis Data Streams.
registerStreamConsumerResponse ::
  -- | 'rscrsResponseStatus'
  Int ->
  -- | 'rscrsConsumer'
  Consumer ->
  RegisterStreamConsumerResponse
registerStreamConsumerResponse pResponseStatus_ pConsumer_ =
  RegisterStreamConsumerResponse'
    { _rscrsResponseStatus =
        pResponseStatus_,
      _rscrsConsumer = pConsumer_
    }

-- | -- | The response status code.
rscrsResponseStatus :: Lens' RegisterStreamConsumerResponse Int
rscrsResponseStatus = lens _rscrsResponseStatus (\s a -> s {_rscrsResponseStatus = a})

-- | An object that represents the details of the consumer you registered. When you register a consumer, it gets an ARN that is generated by Kinesis Data Streams.
rscrsConsumer :: Lens' RegisterStreamConsumerResponse Consumer
rscrsConsumer = lens _rscrsConsumer (\s a -> s {_rscrsConsumer = a})

instance NFData RegisterStreamConsumerResponse
