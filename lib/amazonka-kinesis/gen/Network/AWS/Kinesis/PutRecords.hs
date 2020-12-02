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
-- Module      : Network.AWS.Kinesis.PutRecords
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes multiple data records into a Kinesis data stream in a single call (also referred to as a @PutRecords@ request). Use this operation to send data into the stream for data ingestion and processing.
--
--
-- Each @PutRecords@ request can support up to 500 records. Each record in the request can be as large as 1 MB, up to a limit of 5 MB for the entire request, including partition keys. Each shard can support writes up to 1,000 records per second, up to a maximum data write total of 1 MB per second.
--
-- You must specify the name of the stream that captures, stores, and transports the data; and an array of request @Records@ , with each record in the array requiring a partition key and data blob. The record size limit applies to the total size of the partition key and data blob.
--
-- The data blob can be any type of data; for example, a segment from a log file, geographic/location data, website clickstream data, and so on.
--
-- The partition key is used by Kinesis Data Streams as input to a hash function that maps the partition key and associated data to a specific shard. An MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream. For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream Adding Data to a Stream> in the /Amazon Kinesis Data Streams Developer Guide/ .
--
-- Each record in the @Records@ array may include an optional parameter, @ExplicitHashKey@ , which overrides the partition key to shard mapping. This parameter allows a data producer to determine explicitly the shard where the record is stored. For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-putrecords Adding Multiple Records with PutRecords> in the /Amazon Kinesis Data Streams Developer Guide/ .
--
-- The @PutRecords@ response includes an array of response @Records@ . Each record in the response array directly correlates with a record in the request array using natural ordering, from the top to the bottom of the request and response. The response @Records@ array always includes the same number of records as the request array.
--
-- The response @Records@ array includes both successfully and unsuccessfully processed records. Kinesis Data Streams attempts to process all records in each @PutRecords@ request. A single record failure does not stop the processing of subsequent records.
--
-- A successfully processed record includes @ShardId@ and @SequenceNumber@ values. The @ShardId@ parameter identifies the shard in the stream where the record is stored. The @SequenceNumber@ parameter is an identifier assigned to the put record, unique to all records in the stream.
--
-- An unsuccessfully processed record includes @ErrorCode@ and @ErrorMessage@ values. @ErrorCode@ reflects the type of error and can be one of the following values: @ProvisionedThroughputExceededException@ or @InternalFailure@ . @ErrorMessage@ provides more detailed information about the @ProvisionedThroughputExceededException@ exception including the account ID, stream name, and shard ID of the record that was throttled. For more information about partially successful responses, see <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html#kinesis-using-sdk-java-putrecords Adding Multiple Records with PutRecords> in the /Amazon Kinesis Data Streams Developer Guide/ .
--
-- By default, data records are accessible for 24 hours from the time that they are added to a stream. You can use 'IncreaseStreamRetentionPeriod' or 'DecreaseStreamRetentionPeriod' to modify this retention period.
--
module Network.AWS.Kinesis.PutRecords
    (
    -- * Creating a Request
      putRecords
    , PutRecords
    -- * Request Lenses
    , pRecordEntries
    , pStreamName

    -- * Destructuring the Response
    , putRecordsResponse
    , PutRecordsResponse
    -- * Response Lenses
    , prsEncryptionType
    , prsFailedRecordCount
    , prsResponseStatus
    , prsRecords
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A @PutRecords@ request.
--
--
--
-- /See:/ 'putRecords' smart constructor.
data PutRecords = PutRecords'
  { _pRecordEntries :: !(List1 PutRecordsRequestEntry)
  , _pStreamName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRecords' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pRecordEntries' - The records associated with the request.
--
-- * 'pStreamName' - The stream name associated with the request.
putRecords
    :: NonEmpty PutRecordsRequestEntry -- ^ 'pRecordEntries'
    -> Text -- ^ 'pStreamName'
    -> PutRecords
putRecords pRecordEntries_ pStreamName_ =
  PutRecords'
    {_pRecordEntries = _List1 # pRecordEntries_, _pStreamName = pStreamName_}


-- | The records associated with the request.
pRecordEntries :: Lens' PutRecords (NonEmpty PutRecordsRequestEntry)
pRecordEntries = lens _pRecordEntries (\ s a -> s{_pRecordEntries = a}) . _List1

-- | The stream name associated with the request.
pStreamName :: Lens' PutRecords Text
pStreamName = lens _pStreamName (\ s a -> s{_pStreamName = a})

instance AWSRequest PutRecords where
        type Rs PutRecords = PutRecordsResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 PutRecordsResponse' <$>
                   (x .?> "EncryptionType") <*>
                     (x .?> "FailedRecordCount")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "Records"))

instance Hashable PutRecords where

instance NFData PutRecords where

instance ToHeaders PutRecords where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.PutRecords" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRecords where
        toJSON PutRecords'{..}
          = object
              (catMaybes
                 [Just ("Records" .= _pRecordEntries),
                  Just ("StreamName" .= _pStreamName)])

instance ToPath PutRecords where
        toPath = const "/"

instance ToQuery PutRecords where
        toQuery = const mempty

-- | @PutRecords@ results.
--
--
--
-- /See:/ 'putRecordsResponse' smart constructor.
data PutRecordsResponse = PutRecordsResponse'
  { _prsEncryptionType    :: !(Maybe EncryptionType)
  , _prsFailedRecordCount :: !(Maybe Nat)
  , _prsResponseStatus    :: !Int
  , _prsRecords           :: !(List1 PutRecordsResultEntry)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRecordsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prsEncryptionType' - The encryption type used on the records. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records.     * @KMS@ : Use server-side encryption on the records using a customer-managed AWS KMS key.
--
-- * 'prsFailedRecordCount' - The number of unsuccessfully processed records in a @PutRecords@ request.
--
-- * 'prsResponseStatus' - -- | The response status code.
--
-- * 'prsRecords' - An array of successfully and unsuccessfully processed record results, correlated with the request by natural ordering. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to a stream includes @ErrorCode@ and @ErrorMessage@ in the result.
putRecordsResponse
    :: Int -- ^ 'prsResponseStatus'
    -> NonEmpty PutRecordsResultEntry -- ^ 'prsRecords'
    -> PutRecordsResponse
putRecordsResponse pResponseStatus_ pRecords_ =
  PutRecordsResponse'
    { _prsEncryptionType = Nothing
    , _prsFailedRecordCount = Nothing
    , _prsResponseStatus = pResponseStatus_
    , _prsRecords = _List1 # pRecords_
    }


-- | The encryption type used on the records. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records.     * @KMS@ : Use server-side encryption on the records using a customer-managed AWS KMS key.
prsEncryptionType :: Lens' PutRecordsResponse (Maybe EncryptionType)
prsEncryptionType = lens _prsEncryptionType (\ s a -> s{_prsEncryptionType = a})

-- | The number of unsuccessfully processed records in a @PutRecords@ request.
prsFailedRecordCount :: Lens' PutRecordsResponse (Maybe Natural)
prsFailedRecordCount = lens _prsFailedRecordCount (\ s a -> s{_prsFailedRecordCount = a}) . mapping _Nat

-- | -- | The response status code.
prsResponseStatus :: Lens' PutRecordsResponse Int
prsResponseStatus = lens _prsResponseStatus (\ s a -> s{_prsResponseStatus = a})

-- | An array of successfully and unsuccessfully processed record results, correlated with the request by natural ordering. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to a stream includes @ErrorCode@ and @ErrorMessage@ in the result.
prsRecords :: Lens' PutRecordsResponse (NonEmpty PutRecordsResultEntry)
prsRecords = lens _prsRecords (\ s a -> s{_prsRecords = a}) . _List1

instance NFData PutRecordsResponse where
