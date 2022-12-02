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
-- Module      : Amazonka.CloudWatch.GetMetricStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the metric stream that you specify.
module Amazonka.CloudWatch.GetMetricStream
  ( -- * Creating a Request
    GetMetricStream (..),
    newGetMetricStream,

    -- * Request Lenses
    getMetricStream_name,

    -- * Destructuring the Response
    GetMetricStreamResponse (..),
    newGetMetricStreamResponse,

    -- * Response Lenses
    getMetricStreamResponse_name,
    getMetricStreamResponse_roleArn,
    getMetricStreamResponse_statisticsConfigurations,
    getMetricStreamResponse_arn,
    getMetricStreamResponse_state,
    getMetricStreamResponse_creationDate,
    getMetricStreamResponse_lastUpdateDate,
    getMetricStreamResponse_outputFormat,
    getMetricStreamResponse_excludeFilters,
    getMetricStreamResponse_includeFilters,
    getMetricStreamResponse_firehoseArn,
    getMetricStreamResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMetricStream' smart constructor.
data GetMetricStream = GetMetricStream'
  { -- | The name of the metric stream to retrieve information about.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getMetricStream_name' - The name of the metric stream to retrieve information about.
newGetMetricStream ::
  -- | 'name'
  Prelude.Text ->
  GetMetricStream
newGetMetricStream pName_ =
  GetMetricStream' {name = pName_}

-- | The name of the metric stream to retrieve information about.
getMetricStream_name :: Lens.Lens' GetMetricStream Prelude.Text
getMetricStream_name = Lens.lens (\GetMetricStream' {name} -> name) (\s@GetMetricStream' {} a -> s {name = a} :: GetMetricStream)

instance Core.AWSRequest GetMetricStream where
  type
    AWSResponse GetMetricStream =
      GetMetricStreamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetMetricStreamResult"
      ( \s h x ->
          GetMetricStreamResponse'
            Prelude.<$> (x Data..@? "Name")
            Prelude.<*> (x Data..@? "RoleArn")
            Prelude.<*> ( x Data..@? "StatisticsConfigurations"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "Arn")
            Prelude.<*> (x Data..@? "State")
            Prelude.<*> (x Data..@? "CreationDate")
            Prelude.<*> (x Data..@? "LastUpdateDate")
            Prelude.<*> (x Data..@? "OutputFormat")
            Prelude.<*> ( x Data..@? "ExcludeFilters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "IncludeFilters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "FirehoseArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMetricStream where
  hashWithSalt _salt GetMetricStream' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetMetricStream where
  rnf GetMetricStream' {..} = Prelude.rnf name

instance Data.ToHeaders GetMetricStream where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetMetricStream where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMetricStream where
  toQuery GetMetricStream' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetMetricStream" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "Name" Data.=: name
      ]

-- | /See:/ 'newGetMetricStreamResponse' smart constructor.
data GetMetricStreamResponse = GetMetricStreamResponse'
  { -- | The name of the metric stream.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that is used by this metric stream.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Each entry in this array displays information about one or more metrics
    -- that include additional statistics in the metric stream. For more
    -- information about the additional statistics, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
    statisticsConfigurations :: Prelude.Maybe [MetricStreamStatisticsConfiguration],
    -- | The ARN of the metric stream.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the metric stream. The possible values are @running@ and
    -- @stopped@.
    state :: Prelude.Maybe Prelude.Text,
    -- | The date that the metric stream was created.
    creationDate :: Prelude.Maybe Data.ISO8601,
    -- | The date of the most recent update to the metric stream\'s
    -- configuration.
    lastUpdateDate :: Prelude.Maybe Data.ISO8601,
    -- | The output format for the stream. Valid values are @json@ and
    -- @opentelemetry0.7@. For more information about metric stream output
    -- formats, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-metric-streams-formats.html Metric streams output formats>.
    outputFormat :: Prelude.Maybe MetricStreamOutputFormat,
    -- | If this array of metric namespaces is present, then these namespaces are
    -- the only metric namespaces that are not streamed by this metric stream.
    -- In this case, all other metric namespaces in the account are streamed by
    -- this metric stream.
    excludeFilters :: Prelude.Maybe [MetricStreamFilter],
    -- | If this array of metric namespaces is present, then these namespaces are
    -- the only metric namespaces that are streamed by this metric stream.
    includeFilters :: Prelude.Maybe [MetricStreamFilter],
    -- | The ARN of the Amazon Kinesis Firehose delivery stream that is used by
    -- this metric stream.
    firehoseArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getMetricStreamResponse_name' - The name of the metric stream.
--
-- 'roleArn', 'getMetricStreamResponse_roleArn' - The ARN of the IAM role that is used by this metric stream.
--
-- 'statisticsConfigurations', 'getMetricStreamResponse_statisticsConfigurations' - Each entry in this array displays information about one or more metrics
-- that include additional statistics in the metric stream. For more
-- information about the additional statistics, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
--
-- 'arn', 'getMetricStreamResponse_arn' - The ARN of the metric stream.
--
-- 'state', 'getMetricStreamResponse_state' - The state of the metric stream. The possible values are @running@ and
-- @stopped@.
--
-- 'creationDate', 'getMetricStreamResponse_creationDate' - The date that the metric stream was created.
--
-- 'lastUpdateDate', 'getMetricStreamResponse_lastUpdateDate' - The date of the most recent update to the metric stream\'s
-- configuration.
--
-- 'outputFormat', 'getMetricStreamResponse_outputFormat' - The output format for the stream. Valid values are @json@ and
-- @opentelemetry0.7@. For more information about metric stream output
-- formats, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-metric-streams-formats.html Metric streams output formats>.
--
-- 'excludeFilters', 'getMetricStreamResponse_excludeFilters' - If this array of metric namespaces is present, then these namespaces are
-- the only metric namespaces that are not streamed by this metric stream.
-- In this case, all other metric namespaces in the account are streamed by
-- this metric stream.
--
-- 'includeFilters', 'getMetricStreamResponse_includeFilters' - If this array of metric namespaces is present, then these namespaces are
-- the only metric namespaces that are streamed by this metric stream.
--
-- 'firehoseArn', 'getMetricStreamResponse_firehoseArn' - The ARN of the Amazon Kinesis Firehose delivery stream that is used by
-- this metric stream.
--
-- 'httpStatus', 'getMetricStreamResponse_httpStatus' - The response's http status code.
newGetMetricStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMetricStreamResponse
newGetMetricStreamResponse pHttpStatus_ =
  GetMetricStreamResponse'
    { name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      statisticsConfigurations = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lastUpdateDate = Prelude.Nothing,
      outputFormat = Prelude.Nothing,
      excludeFilters = Prelude.Nothing,
      includeFilters = Prelude.Nothing,
      firehoseArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the metric stream.
getMetricStreamResponse_name :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe Prelude.Text)
getMetricStreamResponse_name = Lens.lens (\GetMetricStreamResponse' {name} -> name) (\s@GetMetricStreamResponse' {} a -> s {name = a} :: GetMetricStreamResponse)

-- | The ARN of the IAM role that is used by this metric stream.
getMetricStreamResponse_roleArn :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe Prelude.Text)
getMetricStreamResponse_roleArn = Lens.lens (\GetMetricStreamResponse' {roleArn} -> roleArn) (\s@GetMetricStreamResponse' {} a -> s {roleArn = a} :: GetMetricStreamResponse)

-- | Each entry in this array displays information about one or more metrics
-- that include additional statistics in the metric stream. For more
-- information about the additional statistics, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Statistics-definitions.html.html CloudWatch statistics definitions>.
getMetricStreamResponse_statisticsConfigurations :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe [MetricStreamStatisticsConfiguration])
getMetricStreamResponse_statisticsConfigurations = Lens.lens (\GetMetricStreamResponse' {statisticsConfigurations} -> statisticsConfigurations) (\s@GetMetricStreamResponse' {} a -> s {statisticsConfigurations = a} :: GetMetricStreamResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the metric stream.
getMetricStreamResponse_arn :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe Prelude.Text)
getMetricStreamResponse_arn = Lens.lens (\GetMetricStreamResponse' {arn} -> arn) (\s@GetMetricStreamResponse' {} a -> s {arn = a} :: GetMetricStreamResponse)

-- | The state of the metric stream. The possible values are @running@ and
-- @stopped@.
getMetricStreamResponse_state :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe Prelude.Text)
getMetricStreamResponse_state = Lens.lens (\GetMetricStreamResponse' {state} -> state) (\s@GetMetricStreamResponse' {} a -> s {state = a} :: GetMetricStreamResponse)

-- | The date that the metric stream was created.
getMetricStreamResponse_creationDate :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe Prelude.UTCTime)
getMetricStreamResponse_creationDate = Lens.lens (\GetMetricStreamResponse' {creationDate} -> creationDate) (\s@GetMetricStreamResponse' {} a -> s {creationDate = a} :: GetMetricStreamResponse) Prelude.. Lens.mapping Data._Time

-- | The date of the most recent update to the metric stream\'s
-- configuration.
getMetricStreamResponse_lastUpdateDate :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe Prelude.UTCTime)
getMetricStreamResponse_lastUpdateDate = Lens.lens (\GetMetricStreamResponse' {lastUpdateDate} -> lastUpdateDate) (\s@GetMetricStreamResponse' {} a -> s {lastUpdateDate = a} :: GetMetricStreamResponse) Prelude.. Lens.mapping Data._Time

-- | The output format for the stream. Valid values are @json@ and
-- @opentelemetry0.7@. For more information about metric stream output
-- formats, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-metric-streams-formats.html Metric streams output formats>.
getMetricStreamResponse_outputFormat :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe MetricStreamOutputFormat)
getMetricStreamResponse_outputFormat = Lens.lens (\GetMetricStreamResponse' {outputFormat} -> outputFormat) (\s@GetMetricStreamResponse' {} a -> s {outputFormat = a} :: GetMetricStreamResponse)

-- | If this array of metric namespaces is present, then these namespaces are
-- the only metric namespaces that are not streamed by this metric stream.
-- In this case, all other metric namespaces in the account are streamed by
-- this metric stream.
getMetricStreamResponse_excludeFilters :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe [MetricStreamFilter])
getMetricStreamResponse_excludeFilters = Lens.lens (\GetMetricStreamResponse' {excludeFilters} -> excludeFilters) (\s@GetMetricStreamResponse' {} a -> s {excludeFilters = a} :: GetMetricStreamResponse) Prelude.. Lens.mapping Lens.coerced

-- | If this array of metric namespaces is present, then these namespaces are
-- the only metric namespaces that are streamed by this metric stream.
getMetricStreamResponse_includeFilters :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe [MetricStreamFilter])
getMetricStreamResponse_includeFilters = Lens.lens (\GetMetricStreamResponse' {includeFilters} -> includeFilters) (\s@GetMetricStreamResponse' {} a -> s {includeFilters = a} :: GetMetricStreamResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the Amazon Kinesis Firehose delivery stream that is used by
-- this metric stream.
getMetricStreamResponse_firehoseArn :: Lens.Lens' GetMetricStreamResponse (Prelude.Maybe Prelude.Text)
getMetricStreamResponse_firehoseArn = Lens.lens (\GetMetricStreamResponse' {firehoseArn} -> firehoseArn) (\s@GetMetricStreamResponse' {} a -> s {firehoseArn = a} :: GetMetricStreamResponse)

-- | The response's http status code.
getMetricStreamResponse_httpStatus :: Lens.Lens' GetMetricStreamResponse Prelude.Int
getMetricStreamResponse_httpStatus = Lens.lens (\GetMetricStreamResponse' {httpStatus} -> httpStatus) (\s@GetMetricStreamResponse' {} a -> s {httpStatus = a} :: GetMetricStreamResponse)

instance Prelude.NFData GetMetricStreamResponse where
  rnf GetMetricStreamResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf statisticsConfigurations
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf outputFormat
      `Prelude.seq` Prelude.rnf excludeFilters
      `Prelude.seq` Prelude.rnf includeFilters
      `Prelude.seq` Prelude.rnf firehoseArn
      `Prelude.seq` Prelude.rnf httpStatus
