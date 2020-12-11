{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.CreateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Kinesis Analytics application. You can configure each application with one streaming source as input, application code to process the input, and up to three destinations where you want Amazon Kinesis Analytics to write the output data from your application. For an overview, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works.html How it Works> .
--
-- In the input configuration, you map the streaming source to an in-application stream, which you can think of as a constantly updating table. In the mapping, you must provide a schema for the in-application stream and map each data column in the in-application stream to a data element in the streaming source.
-- Your application code is one or more SQL statements that read input data, transform it, and generate output. Your application code can create one or more SQL artifacts like SQL streams or pumps.
-- In the output configuration, you can configure the application to write data from in-application streams created in your applications to up to three destinations.
-- To read data from your source stream or write data to destination streams, Amazon Kinesis Analytics needs your permissions. You grant these permissions by creating IAM roles. This operation requires permissions to perform the @kinesisanalytics:CreateApplication@ action.
-- For introductory exercises to create an Amazon Kinesis Analytics application, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/getting-started.html Getting Started> .
module Network.AWS.KinesisAnalytics.CreateApplication
  ( -- * Creating a request
    CreateApplication (..),
    mkCreateApplication,

    -- ** Request lenses
    caApplicationDescription,
    caInputs,
    caCloudWatchLoggingOptions,
    caOutputs,
    caApplicationCode,
    caTags,
    caApplicationName,

    -- * Destructuring the response
    CreateApplicationResponse (..),
    mkCreateApplicationResponse,

    -- ** Response lenses
    carsResponseStatus,
    carsApplicationSummary,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | TBD
--
-- /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { applicationDescription ::
      Lude.Maybe Lude.Text,
    inputs :: Lude.Maybe [Input],
    cloudWatchLoggingOptions ::
      Lude.Maybe [CloudWatchLoggingOption],
    outputs :: Lude.Maybe [Output],
    applicationCode :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    applicationName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- * 'applicationCode' - One or more SQL statements that read input data, transform it, and generate output. For example, you can write a SQL statement that reads data from one in-application stream, generates a running average of the number of advertisement clicks by vendor, and insert resulting rows in another in-application stream using pumps. For more information about the typical pattern, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-app-code.html Application Code> .
--
-- You can provide such series of SQL statements, where output of one statement can be used as the input for the next statement. You store intermediate results by creating in-application streams and pumps.
-- Note that the application code must create the streams with names specified in the @Outputs@ . For example, if your @Outputs@ defines output streams named @ExampleOutputStream1@ and @ExampleOutputStream2@ , then your application code must create these streams.
-- * 'applicationDescription' - Summary description of the application.
-- * 'applicationName' - Name of your Amazon Kinesis Analytics application (for example, @sample-app@ ).
-- * 'cloudWatchLoggingOptions' - Use this parameter to configure a CloudWatch log stream to monitor application configuration errors. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
-- * 'inputs' - Use this parameter to configure the application input.
--
-- You can configure your application to receive input from a single streaming source. In this configuration, you map this streaming source to an in-application stream that is created. Your application code can then query the in-application stream like a table (you can think of it as a constantly updating table).
-- For the streaming source, you provide its Amazon Resource Name (ARN) and format of data on the stream (for example, JSON, CSV, etc.). You also must provide an IAM role that Amazon Kinesis Analytics can assume to read this stream on your behalf.
-- To create the in-application stream, you need to specify a schema to transform your data into a schematized version used in SQL. In the schema, you provide the necessary mapping of the data elements in the streaming source to record columns in the in-app stream.
-- * 'outputs' - You can configure application output to write data from any of the in-application streams to up to three destinations.
--
-- These destinations can be Amazon Kinesis streams, Amazon Kinesis Firehose delivery streams, AWS Lambda destinations, or any combination of the three.
-- In the configuration, you specify the in-application stream name, the destination stream or Lambda function Amazon Resource Name (ARN), and the format to use when writing data. You must also provide an IAM role that Amazon Kinesis Analytics can assume to write to the destination stream or Lambda function on your behalf.
-- In the output configuration, you also provide the output stream or Lambda function ARN. For stream destinations, you provide the format of data in the stream (for example, JSON, CSV). You also must provide an IAM role that Amazon Kinesis Analytics can assume to write to the stream or Lambda function on your behalf.
-- * 'tags' - A list of one or more tags to assign to the application. A tag is a key-value pair that identifies an application. Note that the maximum number of application tags includes system tags. The maximum number of user-defined application tags is 50. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-tagging.html Using Tagging> .
mkCreateApplication ::
  -- | 'applicationName'
  Lude.Text ->
  CreateApplication
mkCreateApplication pApplicationName_ =
  CreateApplication'
    { applicationDescription = Lude.Nothing,
      inputs = Lude.Nothing,
      cloudWatchLoggingOptions = Lude.Nothing,
      outputs = Lude.Nothing,
      applicationCode = Lude.Nothing,
      tags = Lude.Nothing,
      applicationName = pApplicationName_
    }

-- | Summary description of the application.
--
-- /Note:/ Consider using 'applicationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationDescription :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caApplicationDescription = Lens.lens (applicationDescription :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {applicationDescription = a} :: CreateApplication)
{-# DEPRECATED caApplicationDescription "Use generic-lens or generic-optics with 'applicationDescription' instead." #-}

-- | Use this parameter to configure the application input.
--
-- You can configure your application to receive input from a single streaming source. In this configuration, you map this streaming source to an in-application stream that is created. Your application code can then query the in-application stream like a table (you can think of it as a constantly updating table).
-- For the streaming source, you provide its Amazon Resource Name (ARN) and format of data on the stream (for example, JSON, CSV, etc.). You also must provide an IAM role that Amazon Kinesis Analytics can assume to read this stream on your behalf.
-- To create the in-application stream, you need to specify a schema to transform your data into a schematized version used in SQL. In the schema, you provide the necessary mapping of the data elements in the streaming source to record columns in the in-app stream.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caInputs :: Lens.Lens' CreateApplication (Lude.Maybe [Input])
caInputs = Lens.lens (inputs :: CreateApplication -> Lude.Maybe [Input]) (\s a -> s {inputs = a} :: CreateApplication)
{-# DEPRECATED caInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | Use this parameter to configure a CloudWatch log stream to monitor application configuration errors. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCloudWatchLoggingOptions :: Lens.Lens' CreateApplication (Lude.Maybe [CloudWatchLoggingOption])
caCloudWatchLoggingOptions = Lens.lens (cloudWatchLoggingOptions :: CreateApplication -> Lude.Maybe [CloudWatchLoggingOption]) (\s a -> s {cloudWatchLoggingOptions = a} :: CreateApplication)
{-# DEPRECATED caCloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead." #-}

-- | You can configure application output to write data from any of the in-application streams to up to three destinations.
--
-- These destinations can be Amazon Kinesis streams, Amazon Kinesis Firehose delivery streams, AWS Lambda destinations, or any combination of the three.
-- In the configuration, you specify the in-application stream name, the destination stream or Lambda function Amazon Resource Name (ARN), and the format to use when writing data. You must also provide an IAM role that Amazon Kinesis Analytics can assume to write to the destination stream or Lambda function on your behalf.
-- In the output configuration, you also provide the output stream or Lambda function ARN. For stream destinations, you provide the format of data in the stream (for example, JSON, CSV). You also must provide an IAM role that Amazon Kinesis Analytics can assume to write to the stream or Lambda function on your behalf.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOutputs :: Lens.Lens' CreateApplication (Lude.Maybe [Output])
caOutputs = Lens.lens (outputs :: CreateApplication -> Lude.Maybe [Output]) (\s a -> s {outputs = a} :: CreateApplication)
{-# DEPRECATED caOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | One or more SQL statements that read input data, transform it, and generate output. For example, you can write a SQL statement that reads data from one in-application stream, generates a running average of the number of advertisement clicks by vendor, and insert resulting rows in another in-application stream using pumps. For more information about the typical pattern, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-app-code.html Application Code> .
--
-- You can provide such series of SQL statements, where output of one statement can be used as the input for the next statement. You store intermediate results by creating in-application streams and pumps.
-- Note that the application code must create the streams with names specified in the @Outputs@ . For example, if your @Outputs@ defines output streams named @ExampleOutputStream1@ and @ExampleOutputStream2@ , then your application code must create these streams.
--
-- /Note:/ Consider using 'applicationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationCode :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caApplicationCode = Lens.lens (applicationCode :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {applicationCode = a} :: CreateApplication)
{-# DEPRECATED caApplicationCode "Use generic-lens or generic-optics with 'applicationCode' instead." #-}

-- | A list of one or more tags to assign to the application. A tag is a key-value pair that identifies an application. Note that the maximum number of application tags includes system tags. The maximum number of user-defined application tags is 50. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-tagging.html Using Tagging> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApplication (Lude.Maybe (Lude.NonEmpty Tag))
caTags = Lens.lens (tags :: CreateApplication -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateApplication)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Name of your Amazon Kinesis Analytics application (for example, @sample-app@ ).
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationName :: Lens.Lens' CreateApplication Lude.Text
caApplicationName = Lens.lens (applicationName :: CreateApplication -> Lude.Text) (\s a -> s {applicationName = a} :: CreateApplication)
{-# DEPRECATED caApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest CreateApplication where
  type Rs CreateApplication = CreateApplicationResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ApplicationSummary")
      )

instance Lude.ToHeaders CreateApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("KinesisAnalytics_20150814.CreateApplication" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ApplicationDescription" Lude..=)
              Lude.<$> applicationDescription,
            ("Inputs" Lude..=) Lude.<$> inputs,
            ("CloudWatchLoggingOptions" Lude..=)
              Lude.<$> cloudWatchLoggingOptions,
            ("Outputs" Lude..=) Lude.<$> outputs,
            ("ApplicationCode" Lude..=) Lude.<$> applicationCode,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ApplicationName" Lude..= applicationName)
          ]
      )

instance Lude.ToPath CreateApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateApplication where
  toQuery = Lude.const Lude.mempty

-- | TBD
--
-- /See:/ 'mkCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { responseStatus ::
      Lude.Int,
    applicationSummary ::
      ApplicationSummary
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplicationResponse' with the minimum fields required to make a request.
--
-- * 'applicationSummary' - In response to your @CreateApplication@ request, Amazon Kinesis Analytics returns a response with a summary of the application it created, including the application Amazon Resource Name (ARN), name, and status.
-- * 'responseStatus' - The response status code.
mkCreateApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'applicationSummary'
  ApplicationSummary ->
  CreateApplicationResponse
mkCreateApplicationResponse pResponseStatus_ pApplicationSummary_ =
  CreateApplicationResponse'
    { responseStatus = pResponseStatus_,
      applicationSummary = pApplicationSummary_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateApplicationResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateApplicationResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | In response to your @CreateApplication@ request, Amazon Kinesis Analytics returns a response with a summary of the application it created, including the application Amazon Resource Name (ARN), name, and status.
--
-- /Note:/ Consider using 'applicationSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsApplicationSummary :: Lens.Lens' CreateApplicationResponse ApplicationSummary
carsApplicationSummary = Lens.lens (applicationSummary :: CreateApplicationResponse -> ApplicationSummary) (\s a -> s {applicationSummary = a} :: CreateApplicationResponse)
{-# DEPRECATED carsApplicationSummary "Use generic-lens or generic-optics with 'applicationSummary' instead." #-}
