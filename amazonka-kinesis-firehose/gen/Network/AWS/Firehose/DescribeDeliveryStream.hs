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
-- Module      : Network.AWS.Firehose.DescribeDeliveryStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified delivery stream and gets the status. For example, after your delivery stream is created, call @DescribeDeliveryStream@ to see whether the delivery stream is @ACTIVE@ and therefore ready for data to be sent to it.
--
--
module Network.AWS.Firehose.DescribeDeliveryStream
    (
    -- * Creating a Request
      describeDeliveryStream
    , DescribeDeliveryStream
    -- * Request Lenses
    , ddsExclusiveStartDestinationId
    , ddsLimit
    , ddsDeliveryStreamName

    -- * Destructuring the Response
    , describeDeliveryStreamResponse
    , DescribeDeliveryStreamResponse
    -- * Response Lenses
    , ddsrsResponseStatus
    , ddsrsDeliveryStreamDescription
    ) where

import Network.AWS.Firehose.Types
import Network.AWS.Firehose.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDeliveryStream' smart constructor.
data DescribeDeliveryStream = DescribeDeliveryStream'
  { _ddsExclusiveStartDestinationId :: !(Maybe Text)
  , _ddsLimit                       :: !(Maybe Nat)
  , _ddsDeliveryStreamName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDeliveryStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsExclusiveStartDestinationId' - The ID of the destination to start returning the destination information. Kinesis Data Firehose supports one destination per delivery stream.
--
-- * 'ddsLimit' - The limit on the number of destinations to return. You can have one destination per delivery stream.
--
-- * 'ddsDeliveryStreamName' - The name of the delivery stream.
describeDeliveryStream
    :: Text -- ^ 'ddsDeliveryStreamName'
    -> DescribeDeliveryStream
describeDeliveryStream pDeliveryStreamName_ =
  DescribeDeliveryStream'
    { _ddsExclusiveStartDestinationId = Nothing
    , _ddsLimit = Nothing
    , _ddsDeliveryStreamName = pDeliveryStreamName_
    }


-- | The ID of the destination to start returning the destination information. Kinesis Data Firehose supports one destination per delivery stream.
ddsExclusiveStartDestinationId :: Lens' DescribeDeliveryStream (Maybe Text)
ddsExclusiveStartDestinationId = lens _ddsExclusiveStartDestinationId (\ s a -> s{_ddsExclusiveStartDestinationId = a})

-- | The limit on the number of destinations to return. You can have one destination per delivery stream.
ddsLimit :: Lens' DescribeDeliveryStream (Maybe Natural)
ddsLimit = lens _ddsLimit (\ s a -> s{_ddsLimit = a}) . mapping _Nat

-- | The name of the delivery stream.
ddsDeliveryStreamName :: Lens' DescribeDeliveryStream Text
ddsDeliveryStreamName = lens _ddsDeliveryStreamName (\ s a -> s{_ddsDeliveryStreamName = a})

instance AWSRequest DescribeDeliveryStream where
        type Rs DescribeDeliveryStream =
             DescribeDeliveryStreamResponse
        request = postJSON firehose
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDeliveryStreamResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .:> "DeliveryStreamDescription"))

instance Hashable DescribeDeliveryStream where

instance NFData DescribeDeliveryStream where

instance ToHeaders DescribeDeliveryStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.DescribeDeliveryStream" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDeliveryStream where
        toJSON DescribeDeliveryStream'{..}
          = object
              (catMaybes
                 [("ExclusiveStartDestinationId" .=) <$>
                    _ddsExclusiveStartDestinationId,
                  ("Limit" .=) <$> _ddsLimit,
                  Just
                    ("DeliveryStreamName" .= _ddsDeliveryStreamName)])

instance ToPath DescribeDeliveryStream where
        toPath = const "/"

instance ToQuery DescribeDeliveryStream where
        toQuery = const mempty

-- | /See:/ 'describeDeliveryStreamResponse' smart constructor.
data DescribeDeliveryStreamResponse = DescribeDeliveryStreamResponse'
  { _ddsrsResponseStatus            :: !Int
  , _ddsrsDeliveryStreamDescription :: !DeliveryStreamDescription
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsrsResponseStatus' - -- | The response status code.
--
-- * 'ddsrsDeliveryStreamDescription' - Information about the delivery stream.
describeDeliveryStreamResponse
    :: Int -- ^ 'ddsrsResponseStatus'
    -> DeliveryStreamDescription -- ^ 'ddsrsDeliveryStreamDescription'
    -> DescribeDeliveryStreamResponse
describeDeliveryStreamResponse pResponseStatus_ pDeliveryStreamDescription_ =
  DescribeDeliveryStreamResponse'
    { _ddsrsResponseStatus = pResponseStatus_
    , _ddsrsDeliveryStreamDescription = pDeliveryStreamDescription_
    }


-- | -- | The response status code.
ddsrsResponseStatus :: Lens' DescribeDeliveryStreamResponse Int
ddsrsResponseStatus = lens _ddsrsResponseStatus (\ s a -> s{_ddsrsResponseStatus = a})

-- | Information about the delivery stream.
ddsrsDeliveryStreamDescription :: Lens' DescribeDeliveryStreamResponse DeliveryStreamDescription
ddsrsDeliveryStreamDescription = lens _ddsrsDeliveryStreamDescription (\ s a -> s{_ddsrsDeliveryStreamDescription = a})

instance NFData DescribeDeliveryStreamResponse where
