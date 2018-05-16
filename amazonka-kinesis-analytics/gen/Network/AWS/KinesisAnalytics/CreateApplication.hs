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
-- Module      : Network.AWS.KinesisAnalytics.CreateApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Kinesis Analytics application. You can configure each application with one streaming source as input, application code to process the input, and up to three destinations where you want Amazon Kinesis Analytics to write the output data from your application. For an overview, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works.html How it Works> .
--
--
-- In the input configuration, you map the streaming source to an in-application stream, which you can think of as a constantly updating table. In the mapping, you must provide a schema for the in-application stream and map each data column in the in-application stream to a data element in the streaming source.
--
-- Your application code is one or more SQL statements that read input data, transform it, and generate output. Your application code can create one or more SQL artifacts like SQL streams or pumps.
--
-- In the output configuration, you can configure the application to write data from in-application streams created in your applications to up to three destinations.
--
-- To read data from your source stream or write data to destination streams, Amazon Kinesis Analytics needs your permissions. You grant these permissions by creating IAM roles. This operation requires permissions to perform the @kinesisanalytics:CreateApplication@ action.
--
-- For introductory exercises to create an Amazon Kinesis Analytics application, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/getting-started.html Getting Started> .
--
module Network.AWS.KinesisAnalytics.CreateApplication
    (
    -- * Creating a Request
      createApplication
    , CreateApplication
    -- * Request Lenses
    , caApplicationDescription
    , caInputs
    , caCloudWatchLoggingOptions
    , caOutputs
    , caApplicationCode
    , caApplicationName

    -- * Destructuring the Response
    , createApplicationResponse
    , CreateApplicationResponse
    -- * Response Lenses
    , carsResponseStatus
    , carsApplicationSummary
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | TBD
--
--
--
-- /See:/ 'createApplication' smart constructor.
data CreateApplication = CreateApplication'
  { _caApplicationDescription   :: !(Maybe Text)
  , _caInputs                   :: !(Maybe [Input])
  , _caCloudWatchLoggingOptions :: !(Maybe [CloudWatchLoggingOption])
  , _caOutputs                  :: !(Maybe [Output])
  , _caApplicationCode          :: !(Maybe Text)
  , _caApplicationName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caApplicationDescription' - Summary description of the application.
--
-- * 'caInputs' - Use this parameter to configure the application input. You can configure your application to receive input from a single streaming source. In this configuration, you map this streaming source to an in-application stream that is created. Your application code can then query the in-application stream like a table (you can think of it as a constantly updating table). For the streaming source, you provide its Amazon Resource Name (ARN) and format of data on the stream (for example, JSON, CSV, etc.). You also must provide an IAM role that Amazon Kinesis Analytics can assume to read this stream on your behalf. To create the in-application stream, you need to specify a schema to transform your data into a schematized version used in SQL. In the schema, you provide the necessary mapping of the data elements in the streaming source to record columns in the in-app stream.
--
-- * 'caCloudWatchLoggingOptions' - Use this parameter to configure a CloudWatch log stream to monitor application configuration errors. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
--
-- * 'caOutputs' - You can configure application output to write data from any of the in-application streams to up to three destinations. These destinations can be Amazon Kinesis streams, Amazon Kinesis Firehose delivery streams, Amazon Lambda destinations, or any combination of the three. In the configuration, you specify the in-application stream name, the destination stream or Lambda function Amazon Resource Name (ARN), and the format to use when writing data. You must also provide an IAM role that Amazon Kinesis Analytics can assume to write to the destination stream or Lambda function on your behalf. In the output configuration, you also provide the output stream or Lambda function ARN. For stream destinations, you provide the format of data in the stream (for example, JSON, CSV). You also must provide an IAM role that Amazon Kinesis Analytics can assume to write to the stream or Lambda function on your behalf.
--
-- * 'caApplicationCode' - One or more SQL statements that read input data, transform it, and generate output. For example, you can write a SQL statement that reads data from one in-application stream, generates a running average of the number of advertisement clicks by vendor, and insert resulting rows in another in-application stream using pumps. For more information about the typical pattern, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-app-code.html Application Code> .  You can provide such series of SQL statements, where output of one statement can be used as the input for the next statement. You store intermediate results by creating in-application streams and pumps. Note that the application code must create the streams with names specified in the @Outputs@ . For example, if your @Outputs@ defines output streams named @ExampleOutputStream1@ and @ExampleOutputStream2@ , then your application code must create these streams.
--
-- * 'caApplicationName' - Name of your Amazon Kinesis Analytics application (for example, @sample-app@ ).
createApplication
    :: Text -- ^ 'caApplicationName'
    -> CreateApplication
createApplication pApplicationName_ =
  CreateApplication'
    { _caApplicationDescription = Nothing
    , _caInputs = Nothing
    , _caCloudWatchLoggingOptions = Nothing
    , _caOutputs = Nothing
    , _caApplicationCode = Nothing
    , _caApplicationName = pApplicationName_
    }


-- | Summary description of the application.
caApplicationDescription :: Lens' CreateApplication (Maybe Text)
caApplicationDescription = lens _caApplicationDescription (\ s a -> s{_caApplicationDescription = a})

-- | Use this parameter to configure the application input. You can configure your application to receive input from a single streaming source. In this configuration, you map this streaming source to an in-application stream that is created. Your application code can then query the in-application stream like a table (you can think of it as a constantly updating table). For the streaming source, you provide its Amazon Resource Name (ARN) and format of data on the stream (for example, JSON, CSV, etc.). You also must provide an IAM role that Amazon Kinesis Analytics can assume to read this stream on your behalf. To create the in-application stream, you need to specify a schema to transform your data into a schematized version used in SQL. In the schema, you provide the necessary mapping of the data elements in the streaming source to record columns in the in-app stream.
caInputs :: Lens' CreateApplication [Input]
caInputs = lens _caInputs (\ s a -> s{_caInputs = a}) . _Default . _Coerce

-- | Use this parameter to configure a CloudWatch log stream to monitor application configuration errors. For more information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
caCloudWatchLoggingOptions :: Lens' CreateApplication [CloudWatchLoggingOption]
caCloudWatchLoggingOptions = lens _caCloudWatchLoggingOptions (\ s a -> s{_caCloudWatchLoggingOptions = a}) . _Default . _Coerce

-- | You can configure application output to write data from any of the in-application streams to up to three destinations. These destinations can be Amazon Kinesis streams, Amazon Kinesis Firehose delivery streams, Amazon Lambda destinations, or any combination of the three. In the configuration, you specify the in-application stream name, the destination stream or Lambda function Amazon Resource Name (ARN), and the format to use when writing data. You must also provide an IAM role that Amazon Kinesis Analytics can assume to write to the destination stream or Lambda function on your behalf. In the output configuration, you also provide the output stream or Lambda function ARN. For stream destinations, you provide the format of data in the stream (for example, JSON, CSV). You also must provide an IAM role that Amazon Kinesis Analytics can assume to write to the stream or Lambda function on your behalf.
caOutputs :: Lens' CreateApplication [Output]
caOutputs = lens _caOutputs (\ s a -> s{_caOutputs = a}) . _Default . _Coerce

-- | One or more SQL statements that read input data, transform it, and generate output. For example, you can write a SQL statement that reads data from one in-application stream, generates a running average of the number of advertisement clicks by vendor, and insert resulting rows in another in-application stream using pumps. For more information about the typical pattern, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-app-code.html Application Code> .  You can provide such series of SQL statements, where output of one statement can be used as the input for the next statement. You store intermediate results by creating in-application streams and pumps. Note that the application code must create the streams with names specified in the @Outputs@ . For example, if your @Outputs@ defines output streams named @ExampleOutputStream1@ and @ExampleOutputStream2@ , then your application code must create these streams.
caApplicationCode :: Lens' CreateApplication (Maybe Text)
caApplicationCode = lens _caApplicationCode (\ s a -> s{_caApplicationCode = a})

-- | Name of your Amazon Kinesis Analytics application (for example, @sample-app@ ).
caApplicationName :: Lens' CreateApplication Text
caApplicationName = lens _caApplicationName (\ s a -> s{_caApplicationName = a})

instance AWSRequest CreateApplication where
        type Rs CreateApplication = CreateApplicationResponse
        request = postJSON kinesisAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 CreateApplicationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ApplicationSummary"))

instance Hashable CreateApplication where

instance NFData CreateApplication where

instance ToHeaders CreateApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.CreateApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateApplication where
        toJSON CreateApplication'{..}
          = object
              (catMaybes
                 [("ApplicationDescription" .=) <$>
                    _caApplicationDescription,
                  ("Inputs" .=) <$> _caInputs,
                  ("CloudWatchLoggingOptions" .=) <$>
                    _caCloudWatchLoggingOptions,
                  ("Outputs" .=) <$> _caOutputs,
                  ("ApplicationCode" .=) <$> _caApplicationCode,
                  Just ("ApplicationName" .= _caApplicationName)])

instance ToPath CreateApplication where
        toPath = const "/"

instance ToQuery CreateApplication where
        toQuery = const mempty

-- | TBD
--
--
--
-- /See:/ 'createApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { _carsResponseStatus     :: !Int
  , _carsApplicationSummary :: !ApplicationSummary
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsResponseStatus' - -- | The response status code.
--
-- * 'carsApplicationSummary' - In response to your @CreateApplication@ request, Amazon Kinesis Analytics returns a response with a summary of the application it created, including the application Amazon Resource Name (ARN), name, and status.
createApplicationResponse
    :: Int -- ^ 'carsResponseStatus'
    -> ApplicationSummary -- ^ 'carsApplicationSummary'
    -> CreateApplicationResponse
createApplicationResponse pResponseStatus_ pApplicationSummary_ =
  CreateApplicationResponse'
    { _carsResponseStatus = pResponseStatus_
    , _carsApplicationSummary = pApplicationSummary_
    }


-- | -- | The response status code.
carsResponseStatus :: Lens' CreateApplicationResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

-- | In response to your @CreateApplication@ request, Amazon Kinesis Analytics returns a response with a summary of the application it created, including the application Amazon Resource Name (ARN), name, and status.
carsApplicationSummary :: Lens' CreateApplicationResponse ApplicationSummary
carsApplicationSummary = lens _carsApplicationSummary (\ s a -> s{_carsApplicationSummary = a})

instance NFData CreateApplicationResponse where
