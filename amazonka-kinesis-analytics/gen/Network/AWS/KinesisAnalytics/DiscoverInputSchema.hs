{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DiscoverInputSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This documentation is for version 1 of the Amazon Kinesis Data Analytics
-- API, which only supports SQL applications. Version 2 of the API supports
-- SQL and Java applications. For more information about version 2, see
-- </kinesisanalytics/latest/apiv2/Welcome.html Amazon Kinesis Data Analytics API V2 Documentation>.
--
-- Infers a schema by evaluating sample records on the specified streaming
-- source (Amazon Kinesis stream or Amazon Kinesis Firehose delivery
-- stream) or S3 object. In the response, the operation returns the
-- inferred schema and also the sample records that the operation used to
-- infer the schema.
--
-- You can use the inferred schema when configuring a streaming source for
-- your application. For conceptual information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
-- Note that when you create an application using the Amazon Kinesis
-- Analytics console, the console uses this operation to infer a schema and
-- show it in the console user interface.
--
-- This operation requires permissions to perform the
-- @kinesisanalytics:DiscoverInputSchema@ action.
module Network.AWS.KinesisAnalytics.DiscoverInputSchema
  ( -- * Creating a Request
    DiscoverInputSchema (..),
    newDiscoverInputSchema,

    -- * Request Lenses
    discoverInputSchema_resourceARN,
    discoverInputSchema_inputStartingPositionConfiguration,
    discoverInputSchema_roleARN,
    discoverInputSchema_s3Configuration,
    discoverInputSchema_inputProcessingConfiguration,

    -- * Destructuring the Response
    DiscoverInputSchemaResponse (..),
    newDiscoverInputSchemaResponse,

    -- * Response Lenses
    discoverInputSchemaResponse_inputSchema,
    discoverInputSchemaResponse_processedInputRecords,
    discoverInputSchemaResponse_rawInputRecords,
    discoverInputSchemaResponse_parsedInputRecords,
    discoverInputSchemaResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDiscoverInputSchema' smart constructor.
data DiscoverInputSchema = DiscoverInputSchema'
  { -- | Amazon Resource Name (ARN) of the streaming source.
    resourceARN :: Core.Maybe Core.Text,
    -- | Point at which you want Amazon Kinesis Analytics to start reading
    -- records from the specified streaming source discovery purposes.
    inputStartingPositionConfiguration :: Core.Maybe InputStartingPositionConfiguration,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream on your behalf.
    roleARN :: Core.Maybe Core.Text,
    -- | Specify this parameter to discover a schema from data in an Amazon S3
    -- object.
    s3Configuration :: Core.Maybe S3Configuration,
    -- | The
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
    -- to use to preprocess the records before discovering the schema of the
    -- records.
    inputProcessingConfiguration :: Core.Maybe InputProcessingConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiscoverInputSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'discoverInputSchema_resourceARN' - Amazon Resource Name (ARN) of the streaming source.
--
-- 'inputStartingPositionConfiguration', 'discoverInputSchema_inputStartingPositionConfiguration' - Point at which you want Amazon Kinesis Analytics to start reading
-- records from the specified streaming source discovery purposes.
--
-- 'roleARN', 'discoverInputSchema_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf.
--
-- 's3Configuration', 'discoverInputSchema_s3Configuration' - Specify this parameter to discover a schema from data in an Amazon S3
-- object.
--
-- 'inputProcessingConfiguration', 'discoverInputSchema_inputProcessingConfiguration' - The
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
-- to use to preprocess the records before discovering the schema of the
-- records.
newDiscoverInputSchema ::
  DiscoverInputSchema
newDiscoverInputSchema =
  DiscoverInputSchema'
    { resourceARN = Core.Nothing,
      inputStartingPositionConfiguration = Core.Nothing,
      roleARN = Core.Nothing,
      s3Configuration = Core.Nothing,
      inputProcessingConfiguration = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the streaming source.
discoverInputSchema_resourceARN :: Lens.Lens' DiscoverInputSchema (Core.Maybe Core.Text)
discoverInputSchema_resourceARN = Lens.lens (\DiscoverInputSchema' {resourceARN} -> resourceARN) (\s@DiscoverInputSchema' {} a -> s {resourceARN = a} :: DiscoverInputSchema)

-- | Point at which you want Amazon Kinesis Analytics to start reading
-- records from the specified streaming source discovery purposes.
discoverInputSchema_inputStartingPositionConfiguration :: Lens.Lens' DiscoverInputSchema (Core.Maybe InputStartingPositionConfiguration)
discoverInputSchema_inputStartingPositionConfiguration = Lens.lens (\DiscoverInputSchema' {inputStartingPositionConfiguration} -> inputStartingPositionConfiguration) (\s@DiscoverInputSchema' {} a -> s {inputStartingPositionConfiguration = a} :: DiscoverInputSchema)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf.
discoverInputSchema_roleARN :: Lens.Lens' DiscoverInputSchema (Core.Maybe Core.Text)
discoverInputSchema_roleARN = Lens.lens (\DiscoverInputSchema' {roleARN} -> roleARN) (\s@DiscoverInputSchema' {} a -> s {roleARN = a} :: DiscoverInputSchema)

-- | Specify this parameter to discover a schema from data in an Amazon S3
-- object.
discoverInputSchema_s3Configuration :: Lens.Lens' DiscoverInputSchema (Core.Maybe S3Configuration)
discoverInputSchema_s3Configuration = Lens.lens (\DiscoverInputSchema' {s3Configuration} -> s3Configuration) (\s@DiscoverInputSchema' {} a -> s {s3Configuration = a} :: DiscoverInputSchema)

-- | The
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
-- to use to preprocess the records before discovering the schema of the
-- records.
discoverInputSchema_inputProcessingConfiguration :: Lens.Lens' DiscoverInputSchema (Core.Maybe InputProcessingConfiguration)
discoverInputSchema_inputProcessingConfiguration = Lens.lens (\DiscoverInputSchema' {inputProcessingConfiguration} -> inputProcessingConfiguration) (\s@DiscoverInputSchema' {} a -> s {inputProcessingConfiguration = a} :: DiscoverInputSchema)

instance Core.AWSRequest DiscoverInputSchema where
  type
    AWSResponse DiscoverInputSchema =
      DiscoverInputSchemaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DiscoverInputSchemaResponse'
            Core.<$> (x Core..?> "InputSchema")
            Core.<*> ( x Core..?> "ProcessedInputRecords"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "RawInputRecords" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "ParsedInputRecords"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DiscoverInputSchema

instance Core.NFData DiscoverInputSchema

instance Core.ToHeaders DiscoverInputSchema where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20150814.DiscoverInputSchema" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DiscoverInputSchema where
  toJSON DiscoverInputSchema' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceARN" Core..=) Core.<$> resourceARN,
            ("InputStartingPositionConfiguration" Core..=)
              Core.<$> inputStartingPositionConfiguration,
            ("RoleARN" Core..=) Core.<$> roleARN,
            ("S3Configuration" Core..=) Core.<$> s3Configuration,
            ("InputProcessingConfiguration" Core..=)
              Core.<$> inputProcessingConfiguration
          ]
      )

instance Core.ToPath DiscoverInputSchema where
  toPath = Core.const "/"

instance Core.ToQuery DiscoverInputSchema where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDiscoverInputSchemaResponse' smart constructor.
data DiscoverInputSchemaResponse = DiscoverInputSchemaResponse'
  { -- | Schema inferred from the streaming source. It identifies the format of
    -- the data in the streaming source and how each data element maps to
    -- corresponding columns in the in-application stream that you can create.
    inputSchema :: Core.Maybe SourceSchema,
    -- | Stream data that was modified by the processor specified in the
    -- @InputProcessingConfiguration@ parameter.
    processedInputRecords :: Core.Maybe [Core.Text],
    -- | Raw stream data that was sampled to infer the schema.
    rawInputRecords :: Core.Maybe [Core.Text],
    -- | An array of elements, where each element corresponds to a row in a
    -- stream record (a stream record can have more than one row).
    parsedInputRecords :: Core.Maybe [[Core.Text]],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiscoverInputSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSchema', 'discoverInputSchemaResponse_inputSchema' - Schema inferred from the streaming source. It identifies the format of
-- the data in the streaming source and how each data element maps to
-- corresponding columns in the in-application stream that you can create.
--
-- 'processedInputRecords', 'discoverInputSchemaResponse_processedInputRecords' - Stream data that was modified by the processor specified in the
-- @InputProcessingConfiguration@ parameter.
--
-- 'rawInputRecords', 'discoverInputSchemaResponse_rawInputRecords' - Raw stream data that was sampled to infer the schema.
--
-- 'parsedInputRecords', 'discoverInputSchemaResponse_parsedInputRecords' - An array of elements, where each element corresponds to a row in a
-- stream record (a stream record can have more than one row).
--
-- 'httpStatus', 'discoverInputSchemaResponse_httpStatus' - The response's http status code.
newDiscoverInputSchemaResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DiscoverInputSchemaResponse
newDiscoverInputSchemaResponse pHttpStatus_ =
  DiscoverInputSchemaResponse'
    { inputSchema =
        Core.Nothing,
      processedInputRecords = Core.Nothing,
      rawInputRecords = Core.Nothing,
      parsedInputRecords = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Schema inferred from the streaming source. It identifies the format of
-- the data in the streaming source and how each data element maps to
-- corresponding columns in the in-application stream that you can create.
discoverInputSchemaResponse_inputSchema :: Lens.Lens' DiscoverInputSchemaResponse (Core.Maybe SourceSchema)
discoverInputSchemaResponse_inputSchema = Lens.lens (\DiscoverInputSchemaResponse' {inputSchema} -> inputSchema) (\s@DiscoverInputSchemaResponse' {} a -> s {inputSchema = a} :: DiscoverInputSchemaResponse)

-- | Stream data that was modified by the processor specified in the
-- @InputProcessingConfiguration@ parameter.
discoverInputSchemaResponse_processedInputRecords :: Lens.Lens' DiscoverInputSchemaResponse (Core.Maybe [Core.Text])
discoverInputSchemaResponse_processedInputRecords = Lens.lens (\DiscoverInputSchemaResponse' {processedInputRecords} -> processedInputRecords) (\s@DiscoverInputSchemaResponse' {} a -> s {processedInputRecords = a} :: DiscoverInputSchemaResponse) Core.. Lens.mapping Lens._Coerce

-- | Raw stream data that was sampled to infer the schema.
discoverInputSchemaResponse_rawInputRecords :: Lens.Lens' DiscoverInputSchemaResponse (Core.Maybe [Core.Text])
discoverInputSchemaResponse_rawInputRecords = Lens.lens (\DiscoverInputSchemaResponse' {rawInputRecords} -> rawInputRecords) (\s@DiscoverInputSchemaResponse' {} a -> s {rawInputRecords = a} :: DiscoverInputSchemaResponse) Core.. Lens.mapping Lens._Coerce

-- | An array of elements, where each element corresponds to a row in a
-- stream record (a stream record can have more than one row).
discoverInputSchemaResponse_parsedInputRecords :: Lens.Lens' DiscoverInputSchemaResponse (Core.Maybe [[Core.Text]])
discoverInputSchemaResponse_parsedInputRecords = Lens.lens (\DiscoverInputSchemaResponse' {parsedInputRecords} -> parsedInputRecords) (\s@DiscoverInputSchemaResponse' {} a -> s {parsedInputRecords = a} :: DiscoverInputSchemaResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
discoverInputSchemaResponse_httpStatus :: Lens.Lens' DiscoverInputSchemaResponse Core.Int
discoverInputSchemaResponse_httpStatus = Lens.lens (\DiscoverInputSchemaResponse' {httpStatus} -> httpStatus) (\s@DiscoverInputSchemaResponse' {} a -> s {httpStatus = a} :: DiscoverInputSchemaResponse)

instance Core.NFData DiscoverInputSchemaResponse
