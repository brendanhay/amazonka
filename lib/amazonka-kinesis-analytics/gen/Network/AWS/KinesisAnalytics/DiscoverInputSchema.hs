{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DiscoverInputSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Infers a schema by evaluating sample records on the specified streaming source (Amazon Kinesis stream or Amazon Kinesis Firehose delivery stream) or S3 object. In the response, the operation returns the inferred schema and also the sample records that the operation used to infer the schema.
--
-- You can use the inferred schema when configuring a streaming source for your application. For conceptual information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . Note that when you create an application using the Amazon Kinesis Analytics console, the console uses this operation to infer a schema and show it in the console user interface.
-- This operation requires permissions to perform the @kinesisanalytics:DiscoverInputSchema@ action.
module Network.AWS.KinesisAnalytics.DiscoverInputSchema
  ( -- * Creating a request
    DiscoverInputSchema (..),
    mkDiscoverInputSchema,

    -- ** Request lenses
    disInputStartingPositionConfiguration,
    disInputProcessingConfiguration,
    disS3Configuration,
    disResourceARN,
    disRoleARN,

    -- * Destructuring the response
    DiscoverInputSchemaResponse (..),
    mkDiscoverInputSchemaResponse,

    -- ** Response lenses
    disrsRawInputRecords,
    disrsInputSchema,
    disrsProcessedInputRecords,
    disrsParsedInputRecords,
    disrsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDiscoverInputSchema' smart constructor.
data DiscoverInputSchema = DiscoverInputSchema'
  { inputStartingPositionConfiguration ::
      Lude.Maybe InputStartingPositionConfiguration,
    inputProcessingConfiguration ::
      Lude.Maybe InputProcessingConfiguration,
    s3Configuration :: Lude.Maybe S3Configuration,
    resourceARN :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiscoverInputSchema' with the minimum fields required to make a request.
--
-- * 'inputProcessingConfiguration' - The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> to use to preprocess the records before discovering the schema of the records.
-- * 'inputStartingPositionConfiguration' - Point at which you want Amazon Kinesis Analytics to start reading records from the specified streaming source discovery purposes.
-- * 'resourceARN' - Amazon Resource Name (ARN) of the streaming source.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf.
-- * 's3Configuration' - Specify this parameter to discover a schema from data in an Amazon S3 object.
mkDiscoverInputSchema ::
  DiscoverInputSchema
mkDiscoverInputSchema =
  DiscoverInputSchema'
    { inputStartingPositionConfiguration =
        Lude.Nothing,
      inputProcessingConfiguration = Lude.Nothing,
      s3Configuration = Lude.Nothing,
      resourceARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Point at which you want Amazon Kinesis Analytics to start reading records from the specified streaming source discovery purposes.
--
-- /Note:/ Consider using 'inputStartingPositionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disInputStartingPositionConfiguration :: Lens.Lens' DiscoverInputSchema (Lude.Maybe InputStartingPositionConfiguration)
disInputStartingPositionConfiguration = Lens.lens (inputStartingPositionConfiguration :: DiscoverInputSchema -> Lude.Maybe InputStartingPositionConfiguration) (\s a -> s {inputStartingPositionConfiguration = a} :: DiscoverInputSchema)
{-# DEPRECATED disInputStartingPositionConfiguration "Use generic-lens or generic-optics with 'inputStartingPositionConfiguration' instead." #-}

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> to use to preprocess the records before discovering the schema of the records.
--
-- /Note:/ Consider using 'inputProcessingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disInputProcessingConfiguration :: Lens.Lens' DiscoverInputSchema (Lude.Maybe InputProcessingConfiguration)
disInputProcessingConfiguration = Lens.lens (inputProcessingConfiguration :: DiscoverInputSchema -> Lude.Maybe InputProcessingConfiguration) (\s a -> s {inputProcessingConfiguration = a} :: DiscoverInputSchema)
{-# DEPRECATED disInputProcessingConfiguration "Use generic-lens or generic-optics with 'inputProcessingConfiguration' instead." #-}

-- | Specify this parameter to discover a schema from data in an Amazon S3 object.
--
-- /Note:/ Consider using 's3Configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disS3Configuration :: Lens.Lens' DiscoverInputSchema (Lude.Maybe S3Configuration)
disS3Configuration = Lens.lens (s3Configuration :: DiscoverInputSchema -> Lude.Maybe S3Configuration) (\s a -> s {s3Configuration = a} :: DiscoverInputSchema)
{-# DEPRECATED disS3Configuration "Use generic-lens or generic-optics with 's3Configuration' instead." #-}

-- | Amazon Resource Name (ARN) of the streaming source.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disResourceARN :: Lens.Lens' DiscoverInputSchema (Lude.Maybe Lude.Text)
disResourceARN = Lens.lens (resourceARN :: DiscoverInputSchema -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: DiscoverInputSchema)
{-# DEPRECATED disResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disRoleARN :: Lens.Lens' DiscoverInputSchema (Lude.Maybe Lude.Text)
disRoleARN = Lens.lens (roleARN :: DiscoverInputSchema -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DiscoverInputSchema)
{-# DEPRECATED disRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest DiscoverInputSchema where
  type Rs DiscoverInputSchema = DiscoverInputSchemaResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DiscoverInputSchemaResponse'
            Lude.<$> (x Lude..?> "RawInputRecords" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "InputSchema")
            Lude.<*> (x Lude..?> "ProcessedInputRecords" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ParsedInputRecords" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DiscoverInputSchema where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.DiscoverInputSchema" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DiscoverInputSchema where
  toJSON DiscoverInputSchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InputStartingPositionConfiguration" Lude..=)
              Lude.<$> inputStartingPositionConfiguration,
            ("InputProcessingConfiguration" Lude..=)
              Lude.<$> inputProcessingConfiguration,
            ("S3Configuration" Lude..=) Lude.<$> s3Configuration,
            ("ResourceARN" Lude..=) Lude.<$> resourceARN,
            ("RoleARN" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath DiscoverInputSchema where
  toPath = Lude.const "/"

instance Lude.ToQuery DiscoverInputSchema where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDiscoverInputSchemaResponse' smart constructor.
data DiscoverInputSchemaResponse = DiscoverInputSchemaResponse'
  { rawInputRecords ::
      Lude.Maybe [Lude.Text],
    inputSchema ::
      Lude.Maybe SourceSchema,
    processedInputRecords ::
      Lude.Maybe [Lude.Text],
    parsedInputRecords ::
      Lude.Maybe [[Lude.Text]],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiscoverInputSchemaResponse' with the minimum fields required to make a request.
--
-- * 'inputSchema' - Schema inferred from the streaming source. It identifies the format of the data in the streaming source and how each data element maps to corresponding columns in the in-application stream that you can create.
-- * 'parsedInputRecords' - An array of elements, where each element corresponds to a row in a stream record (a stream record can have more than one row).
-- * 'processedInputRecords' - Stream data that was modified by the processor specified in the @InputProcessingConfiguration@ parameter.
-- * 'rawInputRecords' - Raw stream data that was sampled to infer the schema.
-- * 'responseStatus' - The response status code.
mkDiscoverInputSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DiscoverInputSchemaResponse
mkDiscoverInputSchemaResponse pResponseStatus_ =
  DiscoverInputSchemaResponse'
    { rawInputRecords = Lude.Nothing,
      inputSchema = Lude.Nothing,
      processedInputRecords = Lude.Nothing,
      parsedInputRecords = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Raw stream data that was sampled to infer the schema.
--
-- /Note:/ Consider using 'rawInputRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsRawInputRecords :: Lens.Lens' DiscoverInputSchemaResponse (Lude.Maybe [Lude.Text])
disrsRawInputRecords = Lens.lens (rawInputRecords :: DiscoverInputSchemaResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {rawInputRecords = a} :: DiscoverInputSchemaResponse)
{-# DEPRECATED disrsRawInputRecords "Use generic-lens or generic-optics with 'rawInputRecords' instead." #-}

-- | Schema inferred from the streaming source. It identifies the format of the data in the streaming source and how each data element maps to corresponding columns in the in-application stream that you can create.
--
-- /Note:/ Consider using 'inputSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsInputSchema :: Lens.Lens' DiscoverInputSchemaResponse (Lude.Maybe SourceSchema)
disrsInputSchema = Lens.lens (inputSchema :: DiscoverInputSchemaResponse -> Lude.Maybe SourceSchema) (\s a -> s {inputSchema = a} :: DiscoverInputSchemaResponse)
{-# DEPRECATED disrsInputSchema "Use generic-lens or generic-optics with 'inputSchema' instead." #-}

-- | Stream data that was modified by the processor specified in the @InputProcessingConfiguration@ parameter.
--
-- /Note:/ Consider using 'processedInputRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsProcessedInputRecords :: Lens.Lens' DiscoverInputSchemaResponse (Lude.Maybe [Lude.Text])
disrsProcessedInputRecords = Lens.lens (processedInputRecords :: DiscoverInputSchemaResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {processedInputRecords = a} :: DiscoverInputSchemaResponse)
{-# DEPRECATED disrsProcessedInputRecords "Use generic-lens or generic-optics with 'processedInputRecords' instead." #-}

-- | An array of elements, where each element corresponds to a row in a stream record (a stream record can have more than one row).
--
-- /Note:/ Consider using 'parsedInputRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsParsedInputRecords :: Lens.Lens' DiscoverInputSchemaResponse (Lude.Maybe [[Lude.Text]])
disrsParsedInputRecords = Lens.lens (parsedInputRecords :: DiscoverInputSchemaResponse -> Lude.Maybe [[Lude.Text]]) (\s a -> s {parsedInputRecords = a} :: DiscoverInputSchemaResponse)
{-# DEPRECATED disrsParsedInputRecords "Use generic-lens or generic-optics with 'parsedInputRecords' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsResponseStatus :: Lens.Lens' DiscoverInputSchemaResponse Lude.Int
disrsResponseStatus = Lens.lens (responseStatus :: DiscoverInputSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DiscoverInputSchemaResponse)
{-# DEPRECATED disrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
