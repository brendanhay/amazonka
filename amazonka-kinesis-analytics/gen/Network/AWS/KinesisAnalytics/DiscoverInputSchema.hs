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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Infers a schema by evaluating sample records on the specified streaming source (Amazon Kinesis stream or Amazon Kinesis Firehose delivery stream). In the response, the operation returns the inferred schema and also the sample records that the operation used to infer the schema.
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
    , disResourceARN
    , disRoleARN
    , disInputStartingPositionConfiguration

    -- * Destructuring the Response
    , discoverInputSchemaResponse
    , DiscoverInputSchemaResponse
    -- * Response Lenses
    , disrsRawInputRecords
    , disrsInputSchema
    , disrsParsedInputRecords
    , disrsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'discoverInputSchema' smart constructor.
data DiscoverInputSchema = DiscoverInputSchema'
  { _disResourceARN :: {-# NOUNPACK #-}!Text
  , _disRoleARN :: {-# NOUNPACK #-}!Text
  , _disInputStartingPositionConfiguration :: {-# NOUNPACK #-}!InputStartingPositionConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiscoverInputSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disResourceARN' - Amazon Resource Name (ARN) of the streaming source.
--
-- * 'disRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf.
--
-- * 'disInputStartingPositionConfiguration' - Point at which you want Amazon Kinesis Analytics to start reading records from the specified streaming source discovery purposes.
discoverInputSchema
    :: Text -- ^ 'disResourceARN'
    -> Text -- ^ 'disRoleARN'
    -> InputStartingPositionConfiguration -- ^ 'disInputStartingPositionConfiguration'
    -> DiscoverInputSchema
discoverInputSchema pResourceARN_ pRoleARN_ pInputStartingPositionConfiguration_ =
  DiscoverInputSchema'
  { _disResourceARN = pResourceARN_
  , _disRoleARN = pRoleARN_
  , _disInputStartingPositionConfiguration =
      pInputStartingPositionConfiguration_
  }


-- | Amazon Resource Name (ARN) of the streaming source.
disResourceARN :: Lens' DiscoverInputSchema Text
disResourceARN = lens _disResourceARN (\ s a -> s{_disResourceARN = a});

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf.
disRoleARN :: Lens' DiscoverInputSchema Text
disRoleARN = lens _disRoleARN (\ s a -> s{_disRoleARN = a});

-- | Point at which you want Amazon Kinesis Analytics to start reading records from the specified streaming source discovery purposes.
disInputStartingPositionConfiguration :: Lens' DiscoverInputSchema InputStartingPositionConfiguration
disInputStartingPositionConfiguration = lens _disInputStartingPositionConfiguration (\ s a -> s{_disInputStartingPositionConfiguration = a});

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
                 [Just ("ResourceARN" .= _disResourceARN),
                  Just ("RoleARN" .= _disRoleARN),
                  Just
                    ("InputStartingPositionConfiguration" .=
                       _disInputStartingPositionConfiguration)])

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
  { _disrsRawInputRecords    :: {-# NOUNPACK #-}!(Maybe [Text])
  , _disrsInputSchema        :: {-# NOUNPACK #-}!(Maybe SourceSchema)
  , _disrsParsedInputRecords :: {-# NOUNPACK #-}!(Maybe [[Text]])
  , _disrsResponseStatus     :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiscoverInputSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsRawInputRecords' - Raw stream data that was sampled to infer the schema.
--
-- * 'disrsInputSchema' - Schema inferred from the streaming source. It identifies the format of the data in the streaming source and how each data element maps to corresponding columns in the in-application stream that you can create.
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
  , _disrsParsedInputRecords = Nothing
  , _disrsResponseStatus = pResponseStatus_
  }


-- | Raw stream data that was sampled to infer the schema.
disrsRawInputRecords :: Lens' DiscoverInputSchemaResponse [Text]
disrsRawInputRecords = lens _disrsRawInputRecords (\ s a -> s{_disrsRawInputRecords = a}) . _Default . _Coerce;

-- | Schema inferred from the streaming source. It identifies the format of the data in the streaming source and how each data element maps to corresponding columns in the in-application stream that you can create.
disrsInputSchema :: Lens' DiscoverInputSchemaResponse (Maybe SourceSchema)
disrsInputSchema = lens _disrsInputSchema (\ s a -> s{_disrsInputSchema = a});

-- | An array of elements, where each element corresponds to a row in a stream record (a stream record can have more than one row).
disrsParsedInputRecords :: Lens' DiscoverInputSchemaResponse [[Text]]
disrsParsedInputRecords = lens _disrsParsedInputRecords (\ s a -> s{_disrsParsedInputRecords = a}) . _Default . _Coerce;

-- | -- | The response status code.
disrsResponseStatus :: Lens' DiscoverInputSchemaResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\ s a -> s{_disrsResponseStatus = a});

instance NFData DiscoverInputSchemaResponse where
