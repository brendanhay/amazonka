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
-- Module      : Network.AWS.Firehose.CreateDeliveryStream
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delivery stream.
--
-- CreateDeliveryStream is an asynchronous operation that immediately
-- returns. The initial status of the delivery stream is 'CREATING'. After
-- the delivery stream is created, its status is 'ACTIVE' and it now
-- accepts data. Attempts to send data to a delivery stream that is not in
-- the 'ACTIVE' state cause an exception. To check the state of a delivery
-- stream, use DescribeDeliveryStream.
--
-- The name of a delivery stream identifies it. You can\'t have two
-- delivery streams with the same name in the same region. Two delivery
-- streams in different AWS accounts or different regions in the same AWS
-- account can have the same name.
--
-- By default, you can create up to 5 delivery streams per region.
--
-- A delivery stream can only be configured with a single destination,
-- Amazon S3 or Amazon Redshift. For correct CreateDeliveryStream request
-- syntax, specify only one destination configuration parameter: either
-- 'RedshiftDestinationConfiguration' or 'S3DestinationConfiguration'
--
-- As part of 'S3DestinationConfiguration', optional values
-- 'BufferingHints', 'EncryptionConfiguration', and 'CompressionFormat' can
-- be provided. By default, if no 'BufferingHints' value is provided,
-- Amazon Kinesis Firehose buffers data up to 5 MB or for 5 minutes,
-- whichever condition is satisfied first. Note that 'BufferingHints' is a
-- hint, so there are some cases where the service cannot adhere to these
-- conditions strictly; for example, record boundaries are such that the
-- size is a little over or under the configured buffering size. By
-- default, no encryption is performed. We strongly recommend that you
-- enable encryption to ensure secure data storage in Amazon S3.
--
-- A few notes about 'RedshiftDestinationConfiguration':
--
-- -   An Amazon Redshift destination requires an S3 bucket as intermediate
--     location, as Amazon Kinesis Firehose first delivers data to S3 and
--     then uses 'COPY' syntax to load data into an Amazon Redshift table.
--     This is specified in the
--     'RedshiftDestinationConfiguration.S3Configuration' parameter
--     element.
-- -   The compression formats 'SNAPPY' or 'ZIP' cannot be specified in
--     'RedshiftDestinationConfiguration.S3Configuration' because the
--     Amazon Redshift 'COPY' operation that reads from the S3 bucket
--     doesn\'t support these compression formats.
-- -   We strongly recommend that the username and password provided is
--     used exclusively for Amazon Kinesis Firehose purposes, and that the
--     permissions for the account are restricted for Amazon Redshift
--     'INSERT' permissions.
--
-- Amazon Kinesis Firehose assumes the IAM role that is configured as part
-- of destinations. The IAM role should allow the Amazon Kinesis Firehose
-- principal to assume the role, and the role should have permissions that
-- allows the service to deliver the data. For more information, see
-- <http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3 Amazon S3 Bucket Access>
-- in the /Amazon Kinesis Firehose Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/firehose/latest/APIReference/API_CreateDeliveryStream.html AWS API Reference> for CreateDeliveryStream.
module Network.AWS.Firehose.CreateDeliveryStream
    (
    -- * Creating a Request
      createDeliveryStream
    , CreateDeliveryStream
    -- * Request Lenses
    , cdsS3DestinationConfiguration
    , cdsRedshiftDestinationConfiguration
    , cdsDeliveryStreamName

    -- * Destructuring the Response
    , createDeliveryStreamResponse
    , CreateDeliveryStreamResponse
    -- * Response Lenses
    , cdsrsDeliveryStreamARN
    , cdsrsResponseStatus
    ) where

import           Network.AWS.Firehose.Types
import           Network.AWS.Firehose.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CreateDeliveryStream.
--
-- /See:/ 'createDeliveryStream' smart constructor.
data CreateDeliveryStream = CreateDeliveryStream'
    { _cdsS3DestinationConfiguration       :: !(Maybe S3DestinationConfiguration)
    , _cdsRedshiftDestinationConfiguration :: !(Maybe RedshiftDestinationConfiguration)
    , _cdsDeliveryStreamName               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDeliveryStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsS3DestinationConfiguration'
--
-- * 'cdsRedshiftDestinationConfiguration'
--
-- * 'cdsDeliveryStreamName'
createDeliveryStream
    :: Text -- ^ 'cdsDeliveryStreamName'
    -> CreateDeliveryStream
createDeliveryStream pDeliveryStreamName_ =
    CreateDeliveryStream'
    { _cdsS3DestinationConfiguration = Nothing
    , _cdsRedshiftDestinationConfiguration = Nothing
    , _cdsDeliveryStreamName = pDeliveryStreamName_
    }

-- | The destination in Amazon S3. This value must be specified if
-- 'RedshiftDestinationConfiguration' is specified (see restrictions listed
-- above).
cdsS3DestinationConfiguration :: Lens' CreateDeliveryStream (Maybe S3DestinationConfiguration)
cdsS3DestinationConfiguration = lens _cdsS3DestinationConfiguration (\ s a -> s{_cdsS3DestinationConfiguration = a});

-- | The destination in Amazon Redshift. This value cannot be specified if
-- Amazon S3 is the desired destination (see restrictions listed above).
cdsRedshiftDestinationConfiguration :: Lens' CreateDeliveryStream (Maybe RedshiftDestinationConfiguration)
cdsRedshiftDestinationConfiguration = lens _cdsRedshiftDestinationConfiguration (\ s a -> s{_cdsRedshiftDestinationConfiguration = a});

-- | The name of the delivery stream.
cdsDeliveryStreamName :: Lens' CreateDeliveryStream Text
cdsDeliveryStreamName = lens _cdsDeliveryStreamName (\ s a -> s{_cdsDeliveryStreamName = a});

instance AWSRequest CreateDeliveryStream where
        type Rs CreateDeliveryStream =
             CreateDeliveryStreamResponse
        request = postJSON firehose
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeliveryStreamResponse' <$>
                   (x .?> "DeliveryStreamARN") <*> (pure (fromEnum s)))

instance ToHeaders CreateDeliveryStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.CreateDeliveryStream" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDeliveryStream where
        toJSON CreateDeliveryStream'{..}
          = object
              (catMaybes
                 [("S3DestinationConfiguration" .=) <$>
                    _cdsS3DestinationConfiguration,
                  ("RedshiftDestinationConfiguration" .=) <$>
                    _cdsRedshiftDestinationConfiguration,
                  Just
                    ("DeliveryStreamName" .= _cdsDeliveryStreamName)])

instance ToPath CreateDeliveryStream where
        toPath = const "/"

instance ToQuery CreateDeliveryStream where
        toQuery = const mempty

-- | Contains the output of CreateDeliveryStream.
--
-- /See:/ 'createDeliveryStreamResponse' smart constructor.
data CreateDeliveryStreamResponse = CreateDeliveryStreamResponse'
    { _cdsrsDeliveryStreamARN :: !(Maybe Text)
    , _cdsrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsrsDeliveryStreamARN'
--
-- * 'cdsrsResponseStatus'
createDeliveryStreamResponse
    :: Int -- ^ 'cdsrsResponseStatus'
    -> CreateDeliveryStreamResponse
createDeliveryStreamResponse pResponseStatus_ =
    CreateDeliveryStreamResponse'
    { _cdsrsDeliveryStreamARN = Nothing
    , _cdsrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the delivery stream.
cdsrsDeliveryStreamARN :: Lens' CreateDeliveryStreamResponse (Maybe Text)
cdsrsDeliveryStreamARN = lens _cdsrsDeliveryStreamARN (\ s a -> s{_cdsrsDeliveryStreamARN = a});

-- | The response status code.
cdsrsResponseStatus :: Lens' CreateDeliveryStreamResponse Int
cdsrsResponseStatus = lens _cdsrsResponseStatus (\ s a -> s{_cdsrsResponseStatus = a});
