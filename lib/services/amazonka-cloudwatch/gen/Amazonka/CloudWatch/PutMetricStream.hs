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
-- Module      : Amazonka.CloudWatch.PutMetricStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a metric stream. Metric streams can automatically
-- stream CloudWatch metrics to Amazon Web Services destinations including
-- Amazon S3 and to many third-party solutions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Metric-Streams.html Using Metric Streams>.
--
-- To create a metric stream, you must be logged on to an account that has
-- the @iam:PassRole@ permission and either the @CloudWatchFullAccess@
-- policy or the @cloudwatch:PutMetricStream@ permission.
--
-- When you create or update a metric stream, you choose one of the
-- following:
--
-- -   Stream metrics from all metric namespaces in the account.
--
-- -   Stream metrics from all metric namespaces in the account, except for
--     the namespaces that you list in @ExcludeFilters@.
--
-- -   Stream metrics from only the metric namespaces that you list in
--     @IncludeFilters@.
--
-- By default, a metric stream always sends the @MAX@, @MIN@, @SUM@, and
-- @SAMPLECOUNT@ statistics for each metric that is streamed. You can use
-- the @StatisticsConfigurations@ parameter to have the metric stream also
-- send additional statistics in the stream. Streaming additional
-- statistics incurs additional costs. For more information, see
-- <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing>.
--
-- When you use @PutMetricStream@ to create a new metric stream, the stream
-- is created in the @running@ state. If you use it to update an existing
-- stream, the state of the stream is not changed.
module Amazonka.CloudWatch.PutMetricStream
  ( -- * Creating a Request
    PutMetricStream (..),
    newPutMetricStream,

    -- * Request Lenses
    putMetricStream_tags,
    putMetricStream_statisticsConfigurations,
    putMetricStream_excludeFilters,
    putMetricStream_includeFilters,
    putMetricStream_name,
    putMetricStream_firehoseArn,
    putMetricStream_roleArn,
    putMetricStream_outputFormat,

    -- * Destructuring the Response
    PutMetricStreamResponse (..),
    newPutMetricStreamResponse,

    -- * Response Lenses
    putMetricStreamResponse_arn,
    putMetricStreamResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutMetricStream' smart constructor.
data PutMetricStream = PutMetricStream'
  { -- | A list of key-value pairs to associate with the metric stream. You can
    -- associate as many as 50 tags with a metric stream.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- You can use this parameter only when you are creating a new metric
    -- stream. If you are using this operation to update an existing metric
    -- stream, any tags you specify in this parameter are ignored. To change
    -- the tags of an existing metric stream, use
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_UntagResource.html UntagResource>.
    tags :: Prelude.Maybe [Tag],
    -- | By default, a metric stream always sends the @MAX@, @MIN@, @SUM@, and
    -- @SAMPLECOUNT@ statistics for each metric that is streamed. You can use
    -- this parameter to have the metric stream also send additional statistics
    -- in the stream. This array can have up to 100 members.
    --
    -- For each entry in this array, you specify one or more metrics and the
    -- list of additional statistics to stream for those metrics. The
    -- additional statistics that you can stream depend on the stream\'s
    -- @OutputFormat@. If the @OutputFormat@ is @json@, you can stream any
    -- additional statistic that is supported by CloudWatch, listed in
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
    -- If the @OutputFormat@ is @opentelemetry0.7@, you can stream percentile
    -- statistics such as p95, p99.9 and so on.
    statisticsConfigurations :: Prelude.Maybe [MetricStreamStatisticsConfiguration],
    -- | If you specify this parameter, the stream sends metrics from all metric
    -- namespaces except for the namespaces that you specify here.
    --
    -- You cannot include @ExcludeFilters@ and @IncludeFilters@ in the same
    -- operation.
    excludeFilters :: Prelude.Maybe [MetricStreamFilter],
    -- | If you specify this parameter, the stream sends only the metrics from
    -- the metric namespaces that you specify here.
    --
    -- You cannot include @IncludeFilters@ and @ExcludeFilters@ in the same
    -- operation.
    includeFilters :: Prelude.Maybe [MetricStreamFilter],
    -- | If you are creating a new metric stream, this is the name for the new
    -- stream. The name must be different than the names of other metric
    -- streams in this account and Region.
    --
    -- If you are updating a metric stream, specify the name of that stream
    -- here.
    --
    -- Valid characters are A-Z, a-z, 0-9, \"-\" and \"_\".
    name :: Prelude.Text,
    -- | The ARN of the Amazon Kinesis Firehose delivery stream to use for this
    -- metric stream. This Amazon Kinesis Firehose delivery stream must already
    -- exist and must be in the same account as the metric stream.
    firehoseArn :: Prelude.Text,
    -- | The ARN of an IAM role that this metric stream will use to access Amazon
    -- Kinesis Firehose resources. This IAM role must already exist and must be
    -- in the same account as the metric stream. This IAM role must include the
    -- following permissions:
    --
    -- -   firehose:PutRecord
    --
    -- -   firehose:PutRecordBatch
    roleArn :: Prelude.Text,
    -- | The output format for the stream. Valid values are @json@ and
    -- @opentelemetry0.7@. For more information about metric stream output
    -- formats, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-metric-streams-formats.html Metric streams output formats>.
    outputFormat :: MetricStreamOutputFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putMetricStream_tags' - A list of key-value pairs to associate with the metric stream. You can
-- associate as many as 50 tags with a metric stream.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- You can use this parameter only when you are creating a new metric
-- stream. If you are using this operation to update an existing metric
-- stream, any tags you specify in this parameter are ignored. To change
-- the tags of an existing metric stream, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_UntagResource.html UntagResource>.
--
-- 'statisticsConfigurations', 'putMetricStream_statisticsConfigurations' - By default, a metric stream always sends the @MAX@, @MIN@, @SUM@, and
-- @SAMPLECOUNT@ statistics for each metric that is streamed. You can use
-- this parameter to have the metric stream also send additional statistics
-- in the stream. This array can have up to 100 members.
--
-- For each entry in this array, you specify one or more metrics and the
-- list of additional statistics to stream for those metrics. The
-- additional statistics that you can stream depend on the stream\'s
-- @OutputFormat@. If the @OutputFormat@ is @json@, you can stream any
-- additional statistic that is supported by CloudWatch, listed in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
-- If the @OutputFormat@ is @opentelemetry0.7@, you can stream percentile
-- statistics such as p95, p99.9 and so on.
--
-- 'excludeFilters', 'putMetricStream_excludeFilters' - If you specify this parameter, the stream sends metrics from all metric
-- namespaces except for the namespaces that you specify here.
--
-- You cannot include @ExcludeFilters@ and @IncludeFilters@ in the same
-- operation.
--
-- 'includeFilters', 'putMetricStream_includeFilters' - If you specify this parameter, the stream sends only the metrics from
-- the metric namespaces that you specify here.
--
-- You cannot include @IncludeFilters@ and @ExcludeFilters@ in the same
-- operation.
--
-- 'name', 'putMetricStream_name' - If you are creating a new metric stream, this is the name for the new
-- stream. The name must be different than the names of other metric
-- streams in this account and Region.
--
-- If you are updating a metric stream, specify the name of that stream
-- here.
--
-- Valid characters are A-Z, a-z, 0-9, \"-\" and \"_\".
--
-- 'firehoseArn', 'putMetricStream_firehoseArn' - The ARN of the Amazon Kinesis Firehose delivery stream to use for this
-- metric stream. This Amazon Kinesis Firehose delivery stream must already
-- exist and must be in the same account as the metric stream.
--
-- 'roleArn', 'putMetricStream_roleArn' - The ARN of an IAM role that this metric stream will use to access Amazon
-- Kinesis Firehose resources. This IAM role must already exist and must be
-- in the same account as the metric stream. This IAM role must include the
-- following permissions:
--
-- -   firehose:PutRecord
--
-- -   firehose:PutRecordBatch
--
-- 'outputFormat', 'putMetricStream_outputFormat' - The output format for the stream. Valid values are @json@ and
-- @opentelemetry0.7@. For more information about metric stream output
-- formats, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-metric-streams-formats.html Metric streams output formats>.
newPutMetricStream ::
  -- | 'name'
  Prelude.Text ->
  -- | 'firehoseArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'outputFormat'
  MetricStreamOutputFormat ->
  PutMetricStream
newPutMetricStream
  pName_
  pFirehoseArn_
  pRoleArn_
  pOutputFormat_ =
    PutMetricStream'
      { tags = Prelude.Nothing,
        statisticsConfigurations = Prelude.Nothing,
        excludeFilters = Prelude.Nothing,
        includeFilters = Prelude.Nothing,
        name = pName_,
        firehoseArn = pFirehoseArn_,
        roleArn = pRoleArn_,
        outputFormat = pOutputFormat_
      }

-- | A list of key-value pairs to associate with the metric stream. You can
-- associate as many as 50 tags with a metric stream.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- You can use this parameter only when you are creating a new metric
-- stream. If you are using this operation to update an existing metric
-- stream, any tags you specify in this parameter are ignored. To change
-- the tags of an existing metric stream, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_UntagResource.html UntagResource>.
putMetricStream_tags :: Lens.Lens' PutMetricStream (Prelude.Maybe [Tag])
putMetricStream_tags = Lens.lens (\PutMetricStream' {tags} -> tags) (\s@PutMetricStream' {} a -> s {tags = a} :: PutMetricStream) Prelude.. Lens.mapping Lens.coerced

-- | By default, a metric stream always sends the @MAX@, @MIN@, @SUM@, and
-- @SAMPLECOUNT@ statistics for each metric that is streamed. You can use
-- this parameter to have the metric stream also send additional statistics
-- in the stream. This array can have up to 100 members.
--
-- For each entry in this array, you specify one or more metrics and the
-- list of additional statistics to stream for those metrics. The
-- additional statistics that you can stream depend on the stream\'s
-- @OutputFormat@. If the @OutputFormat@ is @json@, you can stream any
-- additional statistic that is supported by CloudWatch, listed in
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
-- If the @OutputFormat@ is @opentelemetry0.7@, you can stream percentile
-- statistics such as p95, p99.9 and so on.
putMetricStream_statisticsConfigurations :: Lens.Lens' PutMetricStream (Prelude.Maybe [MetricStreamStatisticsConfiguration])
putMetricStream_statisticsConfigurations = Lens.lens (\PutMetricStream' {statisticsConfigurations} -> statisticsConfigurations) (\s@PutMetricStream' {} a -> s {statisticsConfigurations = a} :: PutMetricStream) Prelude.. Lens.mapping Lens.coerced

-- | If you specify this parameter, the stream sends metrics from all metric
-- namespaces except for the namespaces that you specify here.
--
-- You cannot include @ExcludeFilters@ and @IncludeFilters@ in the same
-- operation.
putMetricStream_excludeFilters :: Lens.Lens' PutMetricStream (Prelude.Maybe [MetricStreamFilter])
putMetricStream_excludeFilters = Lens.lens (\PutMetricStream' {excludeFilters} -> excludeFilters) (\s@PutMetricStream' {} a -> s {excludeFilters = a} :: PutMetricStream) Prelude.. Lens.mapping Lens.coerced

-- | If you specify this parameter, the stream sends only the metrics from
-- the metric namespaces that you specify here.
--
-- You cannot include @IncludeFilters@ and @ExcludeFilters@ in the same
-- operation.
putMetricStream_includeFilters :: Lens.Lens' PutMetricStream (Prelude.Maybe [MetricStreamFilter])
putMetricStream_includeFilters = Lens.lens (\PutMetricStream' {includeFilters} -> includeFilters) (\s@PutMetricStream' {} a -> s {includeFilters = a} :: PutMetricStream) Prelude.. Lens.mapping Lens.coerced

-- | If you are creating a new metric stream, this is the name for the new
-- stream. The name must be different than the names of other metric
-- streams in this account and Region.
--
-- If you are updating a metric stream, specify the name of that stream
-- here.
--
-- Valid characters are A-Z, a-z, 0-9, \"-\" and \"_\".
putMetricStream_name :: Lens.Lens' PutMetricStream Prelude.Text
putMetricStream_name = Lens.lens (\PutMetricStream' {name} -> name) (\s@PutMetricStream' {} a -> s {name = a} :: PutMetricStream)

-- | The ARN of the Amazon Kinesis Firehose delivery stream to use for this
-- metric stream. This Amazon Kinesis Firehose delivery stream must already
-- exist and must be in the same account as the metric stream.
putMetricStream_firehoseArn :: Lens.Lens' PutMetricStream Prelude.Text
putMetricStream_firehoseArn = Lens.lens (\PutMetricStream' {firehoseArn} -> firehoseArn) (\s@PutMetricStream' {} a -> s {firehoseArn = a} :: PutMetricStream)

-- | The ARN of an IAM role that this metric stream will use to access Amazon
-- Kinesis Firehose resources. This IAM role must already exist and must be
-- in the same account as the metric stream. This IAM role must include the
-- following permissions:
--
-- -   firehose:PutRecord
--
-- -   firehose:PutRecordBatch
putMetricStream_roleArn :: Lens.Lens' PutMetricStream Prelude.Text
putMetricStream_roleArn = Lens.lens (\PutMetricStream' {roleArn} -> roleArn) (\s@PutMetricStream' {} a -> s {roleArn = a} :: PutMetricStream)

-- | The output format for the stream. Valid values are @json@ and
-- @opentelemetry0.7@. For more information about metric stream output
-- formats, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-metric-streams-formats.html Metric streams output formats>.
putMetricStream_outputFormat :: Lens.Lens' PutMetricStream MetricStreamOutputFormat
putMetricStream_outputFormat = Lens.lens (\PutMetricStream' {outputFormat} -> outputFormat) (\s@PutMetricStream' {} a -> s {outputFormat = a} :: PutMetricStream)

instance Core.AWSRequest PutMetricStream where
  type
    AWSResponse PutMetricStream =
      PutMetricStreamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PutMetricStreamResult"
      ( \s h x ->
          PutMetricStreamResponse'
            Prelude.<$> (x Core..@? "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutMetricStream where
  hashWithSalt _salt PutMetricStream' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` statisticsConfigurations
      `Prelude.hashWithSalt` excludeFilters
      `Prelude.hashWithSalt` includeFilters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` firehoseArn
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` outputFormat

instance Prelude.NFData PutMetricStream where
  rnf PutMetricStream' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf statisticsConfigurations
      `Prelude.seq` Prelude.rnf excludeFilters
      `Prelude.seq` Prelude.rnf includeFilters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf firehoseArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf outputFormat

instance Core.ToHeaders PutMetricStream where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath PutMetricStream where
  toPath = Prelude.const "/"

instance Core.ToQuery PutMetricStream where
  toQuery PutMetricStream' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("PutMetricStream" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-08-01" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "StatisticsConfigurations"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> statisticsConfigurations
            ),
        "ExcludeFilters"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> excludeFilters
            ),
        "IncludeFilters"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> includeFilters
            ),
        "Name" Core.=: name,
        "FirehoseArn" Core.=: firehoseArn,
        "RoleArn" Core.=: roleArn,
        "OutputFormat" Core.=: outputFormat
      ]

-- | /See:/ 'newPutMetricStreamResponse' smart constructor.
data PutMetricStreamResponse = PutMetricStreamResponse'
  { -- | The ARN of the metric stream.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'putMetricStreamResponse_arn' - The ARN of the metric stream.
--
-- 'httpStatus', 'putMetricStreamResponse_httpStatus' - The response's http status code.
newPutMetricStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutMetricStreamResponse
newPutMetricStreamResponse pHttpStatus_ =
  PutMetricStreamResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the metric stream.
putMetricStreamResponse_arn :: Lens.Lens' PutMetricStreamResponse (Prelude.Maybe Prelude.Text)
putMetricStreamResponse_arn = Lens.lens (\PutMetricStreamResponse' {arn} -> arn) (\s@PutMetricStreamResponse' {} a -> s {arn = a} :: PutMetricStreamResponse)

-- | The response's http status code.
putMetricStreamResponse_httpStatus :: Lens.Lens' PutMetricStreamResponse Prelude.Int
putMetricStreamResponse_httpStatus = Lens.lens (\PutMetricStreamResponse' {httpStatus} -> httpStatus) (\s@PutMetricStreamResponse' {} a -> s {httpStatus = a} :: PutMetricStreamResponse)

instance Prelude.NFData PutMetricStreamResponse where
  rnf PutMetricStreamResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
