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
-- Module      : Network.AWS.Firehose.PutRecordBatch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes multiple data records into a delivery stream in a single call, which can achieve higher throughput per producer than when writing single records. To write single data records into a delivery stream, use 'PutRecord' . Applications using these operations are referred to as producers.
--
--
-- By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. If you use 'PutRecord' and 'PutRecordBatch' , the limits are an aggregate across these two operations for each delivery stream. For more information about limits, see <http://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Data Firehose Limits> .
--
-- Each 'PutRecordBatch' request supports up to 500 records. Each record in the request can be as large as 1,000 KB (before 64-bit encoding), up to a limit of 4 MB for the entire request. These limits cannot be changed.
--
-- You must specify the name of the delivery stream and the data record when using 'PutRecord' . The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data. For example, it could be a segment from a log file, geographic location data, website clickstream data, and so on.
--
-- Kinesis Data Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (@\n@ ) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.
--
-- The 'PutRecordBatch' response includes a count of failed records, __FailedPutCount__ , and an array of responses, __RequestResponses__ . Each entry in the __RequestResponses__ array provides additional information about the processed record. It directly correlates with a record in the request array using the same ordering, from the top to the bottom. The response array always includes the same number of records as the request array. __RequestResponses__ includes both successfully and unsuccessfully processed records. Kinesis Data Firehose tries to process all records in each 'PutRecordBatch' request. A single record failure does not stop the processing of subsequent records.
--
-- A successfully processed record includes a __RecordId__ value, which is unique for the record. An unsuccessfully processed record includes __ErrorCode__ and __ErrorMessage__ values. __ErrorCode__ reflects the type of error, and is one of the following values: @ServiceUnavailable@ or @InternalFailure@ . __ErrorMessage__ provides more detailed information about the error.
--
-- If there is an internal server error or a timeout, the write might have completed or it might have failed. If __FailedPutCount__ is greater than 0, retry the request, resending only those records that might have failed processing. This minimizes the possible duplicate records and also reduces the total bytes sent (and corresponding charges). We recommend that you handle any duplicates at the destination.
--
-- If 'PutRecordBatch' throws __ServiceUnavailableException__ , back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream.
--
-- Data records sent to Kinesis Data Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.
--
module Network.AWS.Firehose.PutRecordBatch
    (
    -- * Creating a Request
      putRecordBatch
    , PutRecordBatch
    -- * Request Lenses
    , prbDeliveryStreamName
    , prbRecords

    -- * Destructuring the Response
    , putRecordBatchResponse
    , PutRecordBatchResponse
    -- * Response Lenses
    , prbrsResponseStatus
    , prbrsFailedPutCount
    , prbrsRequestResponses
    ) where

import Network.AWS.Firehose.Types
import Network.AWS.Firehose.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRecordBatch' smart constructor.
data PutRecordBatch = PutRecordBatch'
  { _prbDeliveryStreamName :: !Text
  , _prbRecords            :: !(List1 Record)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRecordBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prbDeliveryStreamName' - The name of the delivery stream.
--
-- * 'prbRecords' - One or more records.
putRecordBatch
    :: Text -- ^ 'prbDeliveryStreamName'
    -> NonEmpty Record -- ^ 'prbRecords'
    -> PutRecordBatch
putRecordBatch pDeliveryStreamName_ pRecords_ =
  PutRecordBatch'
    { _prbDeliveryStreamName = pDeliveryStreamName_
    , _prbRecords = _List1 # pRecords_
    }


-- | The name of the delivery stream.
prbDeliveryStreamName :: Lens' PutRecordBatch Text
prbDeliveryStreamName = lens _prbDeliveryStreamName (\ s a -> s{_prbDeliveryStreamName = a})

-- | One or more records.
prbRecords :: Lens' PutRecordBatch (NonEmpty Record)
prbRecords = lens _prbRecords (\ s a -> s{_prbRecords = a}) . _List1

instance AWSRequest PutRecordBatch where
        type Rs PutRecordBatch = PutRecordBatchResponse
        request = postJSON firehose
        response
          = receiveJSON
              (\ s h x ->
                 PutRecordBatchResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "FailedPutCount") <*>
                     (x .:> "RequestResponses"))

instance Hashable PutRecordBatch where

instance NFData PutRecordBatch where

instance ToHeaders PutRecordBatch where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.PutRecordBatch" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRecordBatch where
        toJSON PutRecordBatch'{..}
          = object
              (catMaybes
                 [Just
                    ("DeliveryStreamName" .= _prbDeliveryStreamName),
                  Just ("Records" .= _prbRecords)])

instance ToPath PutRecordBatch where
        toPath = const "/"

instance ToQuery PutRecordBatch where
        toQuery = const mempty

-- | /See:/ 'putRecordBatchResponse' smart constructor.
data PutRecordBatchResponse = PutRecordBatchResponse'
  { _prbrsResponseStatus   :: !Int
  , _prbrsFailedPutCount   :: !Nat
  , _prbrsRequestResponses :: !(List1 PutRecordBatchResponseEntry)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRecordBatchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prbrsResponseStatus' - -- | The response status code.
--
-- * 'prbrsFailedPutCount' - The number of records that might have failed processing.
--
-- * 'prbrsRequestResponses' - The results array. For each record, the index of the response element is the same as the index used in the request array.
putRecordBatchResponse
    :: Int -- ^ 'prbrsResponseStatus'
    -> Natural -- ^ 'prbrsFailedPutCount'
    -> NonEmpty PutRecordBatchResponseEntry -- ^ 'prbrsRequestResponses'
    -> PutRecordBatchResponse
putRecordBatchResponse pResponseStatus_ pFailedPutCount_ pRequestResponses_ =
  PutRecordBatchResponse'
    { _prbrsResponseStatus = pResponseStatus_
    , _prbrsFailedPutCount = _Nat # pFailedPutCount_
    , _prbrsRequestResponses = _List1 # pRequestResponses_
    }


-- | -- | The response status code.
prbrsResponseStatus :: Lens' PutRecordBatchResponse Int
prbrsResponseStatus = lens _prbrsResponseStatus (\ s a -> s{_prbrsResponseStatus = a})

-- | The number of records that might have failed processing.
prbrsFailedPutCount :: Lens' PutRecordBatchResponse Natural
prbrsFailedPutCount = lens _prbrsFailedPutCount (\ s a -> s{_prbrsFailedPutCount = a}) . _Nat

-- | The results array. For each record, the index of the response element is the same as the index used in the request array.
prbrsRequestResponses :: Lens' PutRecordBatchResponse (NonEmpty PutRecordBatchResponseEntry)
prbrsRequestResponses = lens _prbrsRequestResponses (\ s a -> s{_prbrsRequestResponses = a}) . _List1

instance NFData PutRecordBatchResponse where
