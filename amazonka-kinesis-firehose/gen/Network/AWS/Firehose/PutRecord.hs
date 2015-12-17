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
-- Module      : Network.AWS.Firehose.PutRecord
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes a single data record into an Amazon Kinesis Firehose delivery
-- stream. To write multiple data records into a delivery stream, use
-- PutRecordBatch. Applications using these operations are referred to as
-- producers.
--
-- By default, each delivery stream can take in up to 2,000 transactions
-- per second, 5,000 records per second, or 5 MB per second. Note that if
-- you use PutRecord and PutRecordBatch, the limits are an aggregate across
-- these two operations for each delivery stream. For more information
-- about limits and how to request an increase, see
-- <http://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Firehose Limits>.
--
-- You must specify the name of the delivery stream and the data record
-- when using PutRecord. The data record consists of a data blob that can
-- be up to 1,000 KB in size, and any kind of data, for example, a segment
-- from a log file, geographic location data, web site clickstream data,
-- etc.
--
-- Amazon Kinesis Firehose buffers records before delivering them to the
-- destination. To disambiguate the data blobs at the destination, a common
-- solution is to use delimiters in the data, such as a newline ('\\n') or
-- some other character unique within the data. This allows the consumer
-- application(s) to parse individual data items when reading the data from
-- the destination.
--
-- Amazon Kinesis Firehose does not maintain data record ordering. If the
-- destination data needs to be re-ordered by the consumer application, the
-- producer should include some form of sequence number in each data
-- record.
--
-- The PutRecord operation returns a 'RecordId', which is a unique string
-- assigned to each record. Producer applications can use this ID for
-- purposes such as auditability and investigation.
--
-- If the PutRecord operation throws a 'ServiceUnavailableException', back
-- off and retry. If the exception persists, it is possible that the
-- throughput limits have been exceeded for the delivery stream.
--
-- Data records sent to Amazon Kinesis Firehose are stored for 24 hours
-- from the time they are added to a delivery stream as it attempts to send
-- the records to the destination. If the destination is unreachable for
-- more than 24 hours, the data is no longer available.
--
-- /See:/ <http://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecord.html AWS API Reference> for PutRecord.
module Network.AWS.Firehose.PutRecord
    (
    -- * Creating a Request
      putRecord
    , PutRecord
    -- * Request Lenses
    , prDeliveryStreamName
    , prRecord

    -- * Destructuring the Response
    , putRecordResponse
    , PutRecordResponse
    -- * Response Lenses
    , prrsResponseStatus
    , prrsRecordId
    ) where

import           Network.AWS.Firehose.Types
import           Network.AWS.Firehose.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for PutRecord.
--
-- /See:/ 'putRecord' smart constructor.
data PutRecord = PutRecord'
    { _prDeliveryStreamName :: !Text
    , _prRecord             :: !Record
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prDeliveryStreamName'
--
-- * 'prRecord'
putRecord
    :: Text -- ^ 'prDeliveryStreamName'
    -> Record -- ^ 'prRecord'
    -> PutRecord
putRecord pDeliveryStreamName_ pRecord_ =
    PutRecord'
    { _prDeliveryStreamName = pDeliveryStreamName_
    , _prRecord = pRecord_
    }

-- | The name of the delivery stream.
prDeliveryStreamName :: Lens' PutRecord Text
prDeliveryStreamName = lens _prDeliveryStreamName (\ s a -> s{_prDeliveryStreamName = a});

-- | The record.
prRecord :: Lens' PutRecord Record
prRecord = lens _prRecord (\ s a -> s{_prRecord = a});

instance AWSRequest PutRecord where
        type Rs PutRecord = PutRecordResponse
        request = postJSON firehose
        response
          = receiveJSON
              (\ s h x ->
                 PutRecordResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "RecordId"))

instance ToHeaders PutRecord where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.PutRecord" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRecord where
        toJSON PutRecord'{..}
          = object
              (catMaybes
                 [Just
                    ("DeliveryStreamName" .= _prDeliveryStreamName),
                  Just ("Record" .= _prRecord)])

instance ToPath PutRecord where
        toPath = const "/"

instance ToQuery PutRecord where
        toQuery = const mempty

-- | Contains the output of PutRecord.
--
-- /See:/ 'putRecordResponse' smart constructor.
data PutRecordResponse = PutRecordResponse'
    { _prrsResponseStatus :: !Int
    , _prrsRecordId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutRecordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prrsResponseStatus'
--
-- * 'prrsRecordId'
putRecordResponse
    :: Int -- ^ 'prrsResponseStatus'
    -> Text -- ^ 'prrsRecordId'
    -> PutRecordResponse
putRecordResponse pResponseStatus_ pRecordId_ =
    PutRecordResponse'
    { _prrsResponseStatus = pResponseStatus_
    , _prrsRecordId = pRecordId_
    }

-- | The response status code.
prrsResponseStatus :: Lens' PutRecordResponse Int
prrsResponseStatus = lens _prrsResponseStatus (\ s a -> s{_prrsResponseStatus = a});

-- | The ID of the record.
prrsRecordId :: Lens' PutRecordResponse Text
prrsRecordId = lens _prrsRecordId (\ s a -> s{_prrsRecordId = a});
