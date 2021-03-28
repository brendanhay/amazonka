{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateApplication (..)
    , mkCreateApplication
    -- ** Request lenses
    , caApplicationName
    , caApplicationCode
    , caApplicationDescription
    , caCloudWatchLoggingOptions
    , caInputs
    , caOutputs
    , caTags

    -- * Destructuring the response
    , CreateApplicationResponse (..)
    , mkCreateApplicationResponse
    -- ** Response lenses
    , carrsApplicationSummary
    , carrsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | TBD
--
-- /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { applicationName :: Types.ApplicationName
    -- ^ Name of your Amazon Kinesis Analytics application (for example, @sample-app@ ).
  , applicationCode :: Core.Maybe Types.ApplicationCode
    -- ^ One or more SQL statements that read input data, transform it, and generate output. For example, you can write a SQL statement that reads data from one in-application stream, generates a running average of the number of advertisement clicks by vendor, and insert resulting rows in another in-application stream using pumps. For more information about the typical pattern, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-app-code.html Application Code> . 
--
-- You can provide such series of SQL statements, where output of one statement can be used as the input for the next statement. You store intermediate results by creating in-application streams and pumps.
-- Note that the application code must create the streams with names specified in the @Outputs@ . For example, if your @Outputs@ defines output streams named @ExampleOutputStream1@ and @ExampleOutputStream2@ , then your application code must create these streams. 
  , applicationDescription :: Core.Maybe Types.ApplicationDescription
    -- ^ Summary description of the application.
  , cloudWatchLoggingOptions :: Core.Maybe [Types.CloudWatchLoggingOption]
    -- ^ Use this parameter to configure a CloudWatch log stream to monitor application configuration errors. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
  , inputs :: Core.Maybe [Types.Input]
    -- ^ Use this parameter to configure the application input.
--
-- You can configure your application to receive input from a single streaming source. In this configuration, you map this streaming source to an in-application stream that is created. Your application code can then query the in-application stream like a table (you can think of it as a constantly updating table).
-- For the streaming source, you provide its Amazon Resource Name (ARN) and format of data on the stream (for example, JSON, CSV, etc.). You also must provide an IAM role that Amazon Kinesis Analytics can assume to read this stream on your behalf.
-- To create the in-application stream, you need to specify a schema to transform your data into a schematized version used in SQL. In the schema, you provide the necessary mapping of the data elements in the streaming source to record columns in the in-app stream.
  , outputs :: Core.Maybe [Types.Output]
    -- ^ You can configure application output to write data from any of the in-application streams to up to three destinations.
--
-- These destinations can be Amazon Kinesis streams, Amazon Kinesis Firehose delivery streams, AWS Lambda destinations, or any combination of the three.
-- In the configuration, you specify the in-application stream name, the destination stream or Lambda function Amazon Resource Name (ARN), and the format to use when writing data. You must also provide an IAM role that Amazon Kinesis Analytics can assume to write to the destination stream or Lambda function on your behalf.
-- In the output configuration, you also provide the output stream or Lambda function ARN. For stream destinations, you provide the format of data in the stream (for example, JSON, CSV). You also must provide an IAM role that Amazon Kinesis Analytics can assume to write to the stream or Lambda function on your behalf.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ A list of one or more tags to assign to the application. A tag is a key-value pair that identifies an application. Note that the maximum number of application tags includes system tags. The maximum number of user-defined application tags is 50. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-tagging.html Using Tagging> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplication' value with any optional fields omitted.
mkCreateApplication
    :: Types.ApplicationName -- ^ 'applicationName'
    -> CreateApplication
mkCreateApplication applicationName
  = CreateApplication'{applicationName,
                       applicationCode = Core.Nothing,
                       applicationDescription = Core.Nothing,
                       cloudWatchLoggingOptions = Core.Nothing, inputs = Core.Nothing,
                       outputs = Core.Nothing, tags = Core.Nothing}

-- | Name of your Amazon Kinesis Analytics application (for example, @sample-app@ ).
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationName :: Lens.Lens' CreateApplication Types.ApplicationName
caApplicationName = Lens.field @"applicationName"
{-# INLINEABLE caApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | One or more SQL statements that read input data, transform it, and generate output. For example, you can write a SQL statement that reads data from one in-application stream, generates a running average of the number of advertisement clicks by vendor, and insert resulting rows in another in-application stream using pumps. For more information about the typical pattern, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-app-code.html Application Code> . 
--
-- You can provide such series of SQL statements, where output of one statement can be used as the input for the next statement. You store intermediate results by creating in-application streams and pumps.
-- Note that the application code must create the streams with names specified in the @Outputs@ . For example, if your @Outputs@ defines output streams named @ExampleOutputStream1@ and @ExampleOutputStream2@ , then your application code must create these streams. 
--
-- /Note:/ Consider using 'applicationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationCode :: Lens.Lens' CreateApplication (Core.Maybe Types.ApplicationCode)
caApplicationCode = Lens.field @"applicationCode"
{-# INLINEABLE caApplicationCode #-}
{-# DEPRECATED applicationCode "Use generic-lens or generic-optics with 'applicationCode' instead"  #-}

-- | Summary description of the application.
--
-- /Note:/ Consider using 'applicationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationDescription :: Lens.Lens' CreateApplication (Core.Maybe Types.ApplicationDescription)
caApplicationDescription = Lens.field @"applicationDescription"
{-# INLINEABLE caApplicationDescription #-}
{-# DEPRECATED applicationDescription "Use generic-lens or generic-optics with 'applicationDescription' instead"  #-}

-- | Use this parameter to configure a CloudWatch log stream to monitor application configuration errors. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
--
-- /Note:/ Consider using 'cloudWatchLoggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caCloudWatchLoggingOptions :: Lens.Lens' CreateApplication (Core.Maybe [Types.CloudWatchLoggingOption])
caCloudWatchLoggingOptions = Lens.field @"cloudWatchLoggingOptions"
{-# INLINEABLE caCloudWatchLoggingOptions #-}
{-# DEPRECATED cloudWatchLoggingOptions "Use generic-lens or generic-optics with 'cloudWatchLoggingOptions' instead"  #-}

-- | Use this parameter to configure the application input.
--
-- You can configure your application to receive input from a single streaming source. In this configuration, you map this streaming source to an in-application stream that is created. Your application code can then query the in-application stream like a table (you can think of it as a constantly updating table).
-- For the streaming source, you provide its Amazon Resource Name (ARN) and format of data on the stream (for example, JSON, CSV, etc.). You also must provide an IAM role that Amazon Kinesis Analytics can assume to read this stream on your behalf.
-- To create the in-application stream, you need to specify a schema to transform your data into a schematized version used in SQL. In the schema, you provide the necessary mapping of the data elements in the streaming source to record columns in the in-app stream.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caInputs :: Lens.Lens' CreateApplication (Core.Maybe [Types.Input])
caInputs = Lens.field @"inputs"
{-# INLINEABLE caInputs #-}
{-# DEPRECATED inputs "Use generic-lens or generic-optics with 'inputs' instead"  #-}

-- | You can configure application output to write data from any of the in-application streams to up to three destinations.
--
-- These destinations can be Amazon Kinesis streams, Amazon Kinesis Firehose delivery streams, AWS Lambda destinations, or any combination of the three.
-- In the configuration, you specify the in-application stream name, the destination stream or Lambda function Amazon Resource Name (ARN), and the format to use when writing data. You must also provide an IAM role that Amazon Kinesis Analytics can assume to write to the destination stream or Lambda function on your behalf.
-- In the output configuration, you also provide the output stream or Lambda function ARN. For stream destinations, you provide the format of data in the stream (for example, JSON, CSV). You also must provide an IAM role that Amazon Kinesis Analytics can assume to write to the stream or Lambda function on your behalf.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOutputs :: Lens.Lens' CreateApplication (Core.Maybe [Types.Output])
caOutputs = Lens.field @"outputs"
{-# INLINEABLE caOutputs #-}
{-# DEPRECATED outputs "Use generic-lens or generic-optics with 'outputs' instead"  #-}

-- | A list of one or more tags to assign to the application. A tag is a key-value pair that identifies an application. Note that the maximum number of application tags includes system tags. The maximum number of user-defined application tags is 50. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-tagging.html Using Tagging> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApplication (Core.Maybe (Core.NonEmpty Types.Tag))
caTags = Lens.field @"tags"
{-# INLINEABLE caTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApplication where
        toHeaders CreateApplication{..}
          = Core.pure
              ("X-Amz-Target", "KinesisAnalytics_20150814.CreateApplication")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateApplication where
        toJSON CreateApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ApplicationName" Core..= applicationName),
                  ("ApplicationCode" Core..=) Core.<$> applicationCode,
                  ("ApplicationDescription" Core..=) Core.<$> applicationDescription,
                  ("CloudWatchLoggingOptions" Core..=) Core.<$>
                    cloudWatchLoggingOptions,
                  ("Inputs" Core..=) Core.<$> inputs,
                  ("Outputs" Core..=) Core.<$> outputs,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateApplication where
        type Rs CreateApplication = CreateApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateApplicationResponse' Core.<$>
                   (x Core..: "ApplicationSummary") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | TBD
--
-- /See:/ 'mkCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { applicationSummary :: Types.ApplicationSummary
    -- ^ In response to your @CreateApplication@ request, Amazon Kinesis Analytics returns a response with a summary of the application it created, including the application Amazon Resource Name (ARN), name, and status.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplicationResponse' value with any optional fields omitted.
mkCreateApplicationResponse
    :: Types.ApplicationSummary -- ^ 'applicationSummary'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateApplicationResponse
mkCreateApplicationResponse applicationSummary responseStatus
  = CreateApplicationResponse'{applicationSummary, responseStatus}

-- | In response to your @CreateApplication@ request, Amazon Kinesis Analytics returns a response with a summary of the application it created, including the application Amazon Resource Name (ARN), name, and status.
--
-- /Note:/ Consider using 'applicationSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsApplicationSummary :: Lens.Lens' CreateApplicationResponse Types.ApplicationSummary
carrsApplicationSummary = Lens.field @"applicationSummary"
{-# INLINEABLE carrsApplicationSummary #-}
{-# DEPRECATED applicationSummary "Use generic-lens or generic-optics with 'applicationSummary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateApplicationResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
