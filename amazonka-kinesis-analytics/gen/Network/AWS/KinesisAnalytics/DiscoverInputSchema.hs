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
-- Module      : Network.AWS.KinesisAnalytics.DiscoverInputSchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Infers a schema by evaluating sample records on the specified streaming source (Amazon Kinesis stream or Amazon Kinesis Firehose delivery stream) or S3 object. In the response, the operation returns the inferred schema and also the sample records that the operation used to infer the schema.
--
--
-- You can use the inferred schema when configuring a streaming source for your application. For conceptual information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . Note that when you create an application using the Amazon Kinesis Analytics console, the console uses this operation to infer a schema and show it in the console user interface.
--
-- This operation requires permissions to perform the @kinesisanalytics:DiscoverInputSchema@ action.
--
module Network.AWS.KinesisAnalytics.DiscoverInputSchema
    (
    -- * Creating a Request
      discoverInputSchema
    , DiscoverInputSchema
    -- * Request Lenses
    , disInputStartingPositionConfiguration
    , disInputProcessingConfiguration
    , disS3Configuration
    , disResourceARN
    , disRoleARN

    -- * Destructuring the Response
    , discoverInputSchemaResponse
    , DiscoverInputSchemaResponse
    -- * Response Lenses
    , disrsRawInputRecords
    , disrsInputSchema
    , disrsProcessedInputRecords
    , disrsParsedInputRecords
    , disrsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'discoverInputSchema' smart constructor.
data DiscoverInputSchema = DiscoverInputSchema'
  { _disInputStartingPositionConfiguration :: !(Maybe InputStartingPositionConfiguration)
  , _disInputProcessingConfiguration :: !(Maybe InputProcessingConfiguration)
  , _disS3Configuration :: !(Maybe S3Configuration)
  , _disResourceARN :: !(Maybe Text)
  , _disRoleARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiscoverInputSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disInputStartingPositionConfiguration' - Point at which you want Amazon Kinesis Analytics to start reading records from the specified streaming source discovery purposes.
--
-- * 'disInputProcessingConfiguration' - The 'InputProcessingConfiguration' to use to preprocess the records before discovering the schema of the records.
--
-- * 'disS3Configuration' - Specify this parameter to discover a schema from data in an S3 object.
--
-- * 'disResourceARN' - Amazon Resource Name (ARN) of the streaming source.
--
-- * 'disRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf.
discoverInputSchema
    :: DiscoverInputSchema
discoverInputSchema =
  DiscoverInputSchema'
    { _disInputStartingPositionConfiguration = Nothing
    , _disInputProcessingConfiguration = Nothing
    , _disS3Configuration = Nothing
    , _disResourceARN = Nothing
    , _disRoleARN = Nothing
    }


-- | Point at which you want Amazon Kinesis Analytics to start reading records from the specified streaming source discovery purposes.
disInputStartingPositionConfiguration :: Lens' DiscoverInputSchema (Maybe InputStartingPositionConfiguration)
disInputStartingPositionConfiguration = lens _disInputStartingPositionConfiguration (\ s a -> s{_disInputStartingPositionConfiguration = a})

-- | The 'InputProcessingConfiguration' to use to preprocess the records before discovering the schema of the records.
disInputProcessingConfiguration :: Lens' DiscoverInputSchema (Maybe InputProcessingConfiguration)
disInputProcessingConfiguration = lens _disInputProcessingConfiguration (\ s a -> s{_disInputProcessingConfiguration = a})

-- | Specify this parameter to discover a schema from data in an S3 object.
disS3Configuration :: Lens' DiscoverInputSchema (Maybe S3Configuration)
disS3Configuration = lens _disS3Configuration (\ s a -> s{_disS3Configuration = a})

-- | Amazon Resource Name (ARN) of the streaming source.
disResourceARN :: Lens' DiscoverInputSchema (Maybe Text)
disResourceARN = lens _disResourceARN (\ s a -> s{_disResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf.
disRoleARN :: Lens' DiscoverInputSchema (Maybe Text)
disRoleARN = lens _disRoleARN (\ s a -> s{_disRoleARN = a})

instance AWSRequest DiscoverInputSchema where
        type Rs DiscoverInputSchema =
             DiscoverInputSchemaResponse
        request = postJSON kinesisAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 DiscoverInputSchemaResponse' <$>
                   (x .?> "RawInputRecords" .!@ mempty) <*>
                     (x .?> "InputSchema")
                     <*> (x .?> "ProcessedInputRecords" .!@ mempty)
                     <*> (x .?> "ParsedInputRecords" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DiscoverInputSchema where

instance NFData DiscoverInputSchema where

instance ToHeaders DiscoverInputSchema where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.DiscoverInputSchema" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DiscoverInputSchema where
        toJSON DiscoverInputSchema'{..}
          = object
              (catMaybes
                 [("InputStartingPositionConfiguration" .=) <$>
                    _disInputStartingPositionConfiguration,
                  ("InputProcessingConfiguration" .=) <$>
                    _disInputProcessingConfiguration,
                  ("S3Configuration" .=) <$> _disS3Configuration,
                  ("ResourceARN" .=) <$> _disResourceARN,
                  ("RoleARN" .=) <$> _disRoleARN])

instance ToPath DiscoverInputSchema where
        toPath = const "/"

instance ToQuery DiscoverInputSchema where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'discoverInputSchemaResponse' smart constructor.
data DiscoverInputSchemaResponse = DiscoverInputSchemaResponse'
  { _disrsRawInputRecords       :: !(Maybe [Text])
  , _disrsInputSchema           :: !(Maybe SourceSchema)
  , _disrsProcessedInputRecords :: !(Maybe [Text])
  , _disrsParsedInputRecords    :: !(Maybe [[Text]])
  , _disrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiscoverInputSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsRawInputRecords' - Raw stream data that was sampled to infer the schema.
--
-- * 'disrsInputSchema' - Schema inferred from the streaming source. It identifies the format of the data in the streaming source and how each data element maps to corresponding columns in the in-application stream that you can create.
--
-- * 'disrsProcessedInputRecords' - Stream data that was modified by the processor specified in the @InputProcessingConfiguration@ parameter.
--
-- * 'disrsParsedInputRecords' - An array of elements, where each element corresponds to a row in a stream record (a stream record can have more than one row).
--
-- * 'disrsResponseStatus' - -- | The response status code.
discoverInputSchemaResponse
    :: Int -- ^ 'disrsResponseStatus'
    -> DiscoverInputSchemaResponse
discoverInputSchemaResponse pResponseStatus_ =
  DiscoverInputSchemaResponse'
    { _disrsRawInputRecords = Nothing
    , _disrsInputSchema = Nothing
    , _disrsProcessedInputRecords = Nothing
    , _disrsParsedInputRecords = Nothing
    , _disrsResponseStatus = pResponseStatus_
    }


-- | Raw stream data that was sampled to infer the schema.
disrsRawInputRecords :: Lens' DiscoverInputSchemaResponse [Text]
disrsRawInputRecords = lens _disrsRawInputRecords (\ s a -> s{_disrsRawInputRecords = a}) . _Default . _Coerce

-- | Schema inferred from the streaming source. It identifies the format of the data in the streaming source and how each data element maps to corresponding columns in the in-application stream that you can create.
disrsInputSchema :: Lens' DiscoverInputSchemaResponse (Maybe SourceSchema)
disrsInputSchema = lens _disrsInputSchema (\ s a -> s{_disrsInputSchema = a})

-- | Stream data that was modified by the processor specified in the @InputProcessingConfiguration@ parameter.
disrsProcessedInputRecords :: Lens' DiscoverInputSchemaResponse [Text]
disrsProcessedInputRecords = lens _disrsProcessedInputRecords (\ s a -> s{_disrsProcessedInputRecords = a}) . _Default . _Coerce

-- | An array of elements, where each element corresponds to a row in a stream record (a stream record can have more than one row).
disrsParsedInputRecords :: Lens' DiscoverInputSchemaResponse [[Text]]
disrsParsedInputRecords = lens _disrsParsedInputRecords (\ s a -> s{_disrsParsedInputRecords = a}) . _Default . _Coerce

-- | -- | The response status code.
disrsResponseStatus :: Lens' DiscoverInputSchemaResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\ s a -> s{_disrsResponseStatus = a})

instance NFData DiscoverInputSchemaResponse where
