{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DeregisterStreamConsumer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To deregister a consumer, provide its ARN. Alternatively, you can provide the ARN of the data stream and the name you gave the consumer when you registered it. You may also provide all three parameters, as long as they don't conflict with each other. If you don't know the name or ARN of the consumer that you want to deregister, you can use the 'ListStreamConsumers' operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream. The description of a consumer contains its name and ARN.
--
--
-- This operation has a limit of five transactions per second per account.
--
module Network.AWS.Kinesis.DeregisterStreamConsumer
    (
    -- * Creating a Request
      deregisterStreamConsumer
    , DeregisterStreamConsumer
    -- * Request Lenses
    , dscConsumerARN
    , dscStreamARN
    , dscConsumerName

    -- * Destructuring the Response
    , deregisterStreamConsumerResponse
    , DeregisterStreamConsumerResponse
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterStreamConsumer' smart constructor.
data DeregisterStreamConsumer = DeregisterStreamConsumer'
  { _dscConsumerARN  :: !(Maybe Text)
  , _dscStreamARN    :: !(Maybe Text)
  , _dscConsumerName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterStreamConsumer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscConsumerARN' - The ARN returned by Kinesis Data Streams when you registered the consumer. If you don't know the ARN of the consumer that you want to deregister, you can use the ListStreamConsumers operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream. The description of a consumer contains its ARN.
--
-- * 'dscStreamARN' - The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'dscConsumerName' - The name that you gave to the consumer.
deregisterStreamConsumer
    :: DeregisterStreamConsumer
deregisterStreamConsumer =
  DeregisterStreamConsumer'
    { _dscConsumerARN = Nothing
    , _dscStreamARN = Nothing
    , _dscConsumerName = Nothing
    }


-- | The ARN returned by Kinesis Data Streams when you registered the consumer. If you don't know the ARN of the consumer that you want to deregister, you can use the ListStreamConsumers operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream. The description of a consumer contains its ARN.
dscConsumerARN :: Lens' DeregisterStreamConsumer (Maybe Text)
dscConsumerARN = lens _dscConsumerARN (\ s a -> s{_dscConsumerARN = a})

-- | The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
dscStreamARN :: Lens' DeregisterStreamConsumer (Maybe Text)
dscStreamARN = lens _dscStreamARN (\ s a -> s{_dscStreamARN = a})

-- | The name that you gave to the consumer.
dscConsumerName :: Lens' DeregisterStreamConsumer (Maybe Text)
dscConsumerName = lens _dscConsumerName (\ s a -> s{_dscConsumerName = a})

instance AWSRequest DeregisterStreamConsumer where
        type Rs DeregisterStreamConsumer =
             DeregisterStreamConsumerResponse
        request = postJSON kinesis
        response
          = receiveNull DeregisterStreamConsumerResponse'

instance Hashable DeregisterStreamConsumer where

instance NFData DeregisterStreamConsumer where

instance ToHeaders DeregisterStreamConsumer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.DeregisterStreamConsumer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterStreamConsumer where
        toJSON DeregisterStreamConsumer'{..}
          = object
              (catMaybes
                 [("ConsumerARN" .=) <$> _dscConsumerARN,
                  ("StreamARN" .=) <$> _dscStreamARN,
                  ("ConsumerName" .=) <$> _dscConsumerName])

instance ToPath DeregisterStreamConsumer where
        toPath = const "/"

instance ToQuery DeregisterStreamConsumer where
        toQuery = const mempty

-- | /See:/ 'deregisterStreamConsumerResponse' smart constructor.
data DeregisterStreamConsumerResponse =
  DeregisterStreamConsumerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterStreamConsumerResponse' with the minimum fields required to make a request.
--
deregisterStreamConsumerResponse
    :: DeregisterStreamConsumerResponse
deregisterStreamConsumerResponse = DeregisterStreamConsumerResponse'


instance NFData DeregisterStreamConsumerResponse
         where
