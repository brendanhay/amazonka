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
-- Module      : Amazonka.Discovery.StartExportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins the export of a discovered data report to an Amazon S3 bucket
-- managed by Amazon Web Services.
--
-- Exports might provide an estimate of fees and savings based on certain
-- information that you provide. Fee estimates do not include any taxes
-- that might apply. Your actual fees and savings depend on a variety of
-- factors, including your actual usage of Amazon Web Services services,
-- which might vary from the estimates provided in this report.
--
-- If you do not specify @preferences@ or @agentIds@ in the filter, a
-- summary of all servers, applications, tags, and performance is
-- generated. This data is an aggregation of all server data collected
-- through on-premises tooling, file import, application grouping and
-- applying tags.
--
-- If you specify @agentIds@ in a filter, the task exports up to 72 hours
-- of detailed data collected by the identified Application Discovery
-- Agent, including network, process, and performance details. A time range
-- for exported agent data may be set by using @startTime@ and @endTime@.
-- Export of detailed agent data is limited to five concurrently running
-- exports. Export of detailed agent data is limited to two exports per
-- day.
--
-- If you enable @ec2RecommendationsPreferences@ in @preferences@ , an
-- Amazon EC2 instance matching the characteristics of each server in
-- Application Discovery Service is generated. Changing the attributes of
-- the @ec2RecommendationsPreferences@ changes the criteria of the
-- recommendation.
module Amazonka.Discovery.StartExportTask
  ( -- * Creating a Request
    StartExportTask (..),
    newStartExportTask,

    -- * Request Lenses
    startExportTask_endTime,
    startExportTask_exportDataFormat,
    startExportTask_filters,
    startExportTask_preferences,
    startExportTask_startTime,

    -- * Destructuring the Response
    StartExportTaskResponse (..),
    newStartExportTaskResponse,

    -- * Response Lenses
    startExportTaskResponse_exportId,
    startExportTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartExportTask' smart constructor.
data StartExportTask = StartExportTask'
  { -- | The end timestamp for exported data from the single Application
    -- Discovery Agent selected in the filters. If no value is specified,
    -- exported data includes the most recent data collected by the agent.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The file format for the returned export data. Default value is @CSV@.
    -- __Note:__ /The/ @GRAPHML@ /option has been deprecated./
    exportDataFormat :: Prelude.Maybe [ExportDataFormat],
    -- | If a filter is present, it selects the single @agentId@ of the
    -- Application Discovery Agent for which data is exported. The @agentId@
    -- can be found in the results of the @DescribeAgents@ API or CLI. If no
    -- filter is present, @startTime@ and @endTime@ are ignored and exported
    -- data includes both Amazon Web Services Application Discovery Service
    -- Agentless Collector collectors data and summary data from Application
    -- Discovery Agent agents.
    filters :: Prelude.Maybe [ExportFilter],
    -- | Indicates the type of data that needs to be exported. Only one
    -- <https://docs.aws.amazon.com/application-discovery/latest/APIReference/API_ExportPreferences.html ExportPreferences>
    -- can be enabled at any time.
    preferences :: Prelude.Maybe ExportPreferences,
    -- | The start timestamp for exported data from the single Application
    -- Discovery Agent selected in the filters. If no value is specified, data
    -- is exported starting from the first data collected by the agent.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'startExportTask_endTime' - The end timestamp for exported data from the single Application
-- Discovery Agent selected in the filters. If no value is specified,
-- exported data includes the most recent data collected by the agent.
--
-- 'exportDataFormat', 'startExportTask_exportDataFormat' - The file format for the returned export data. Default value is @CSV@.
-- __Note:__ /The/ @GRAPHML@ /option has been deprecated./
--
-- 'filters', 'startExportTask_filters' - If a filter is present, it selects the single @agentId@ of the
-- Application Discovery Agent for which data is exported. The @agentId@
-- can be found in the results of the @DescribeAgents@ API or CLI. If no
-- filter is present, @startTime@ and @endTime@ are ignored and exported
-- data includes both Amazon Web Services Application Discovery Service
-- Agentless Collector collectors data and summary data from Application
-- Discovery Agent agents.
--
-- 'preferences', 'startExportTask_preferences' - Indicates the type of data that needs to be exported. Only one
-- <https://docs.aws.amazon.com/application-discovery/latest/APIReference/API_ExportPreferences.html ExportPreferences>
-- can be enabled at any time.
--
-- 'startTime', 'startExportTask_startTime' - The start timestamp for exported data from the single Application
-- Discovery Agent selected in the filters. If no value is specified, data
-- is exported starting from the first data collected by the agent.
newStartExportTask ::
  StartExportTask
newStartExportTask =
  StartExportTask'
    { endTime = Prelude.Nothing,
      exportDataFormat = Prelude.Nothing,
      filters = Prelude.Nothing,
      preferences = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The end timestamp for exported data from the single Application
-- Discovery Agent selected in the filters. If no value is specified,
-- exported data includes the most recent data collected by the agent.
startExportTask_endTime :: Lens.Lens' StartExportTask (Prelude.Maybe Prelude.UTCTime)
startExportTask_endTime = Lens.lens (\StartExportTask' {endTime} -> endTime) (\s@StartExportTask' {} a -> s {endTime = a} :: StartExportTask) Prelude.. Lens.mapping Data._Time

-- | The file format for the returned export data. Default value is @CSV@.
-- __Note:__ /The/ @GRAPHML@ /option has been deprecated./
startExportTask_exportDataFormat :: Lens.Lens' StartExportTask (Prelude.Maybe [ExportDataFormat])
startExportTask_exportDataFormat = Lens.lens (\StartExportTask' {exportDataFormat} -> exportDataFormat) (\s@StartExportTask' {} a -> s {exportDataFormat = a} :: StartExportTask) Prelude.. Lens.mapping Lens.coerced

-- | If a filter is present, it selects the single @agentId@ of the
-- Application Discovery Agent for which data is exported. The @agentId@
-- can be found in the results of the @DescribeAgents@ API or CLI. If no
-- filter is present, @startTime@ and @endTime@ are ignored and exported
-- data includes both Amazon Web Services Application Discovery Service
-- Agentless Collector collectors data and summary data from Application
-- Discovery Agent agents.
startExportTask_filters :: Lens.Lens' StartExportTask (Prelude.Maybe [ExportFilter])
startExportTask_filters = Lens.lens (\StartExportTask' {filters} -> filters) (\s@StartExportTask' {} a -> s {filters = a} :: StartExportTask) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the type of data that needs to be exported. Only one
-- <https://docs.aws.amazon.com/application-discovery/latest/APIReference/API_ExportPreferences.html ExportPreferences>
-- can be enabled at any time.
startExportTask_preferences :: Lens.Lens' StartExportTask (Prelude.Maybe ExportPreferences)
startExportTask_preferences = Lens.lens (\StartExportTask' {preferences} -> preferences) (\s@StartExportTask' {} a -> s {preferences = a} :: StartExportTask)

-- | The start timestamp for exported data from the single Application
-- Discovery Agent selected in the filters. If no value is specified, data
-- is exported starting from the first data collected by the agent.
startExportTask_startTime :: Lens.Lens' StartExportTask (Prelude.Maybe Prelude.UTCTime)
startExportTask_startTime = Lens.lens (\StartExportTask' {startTime} -> startTime) (\s@StartExportTask' {} a -> s {startTime = a} :: StartExportTask) Prelude.. Lens.mapping Data._Time

instance Core.AWSRequest StartExportTask where
  type
    AWSResponse StartExportTask =
      StartExportTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExportTaskResponse'
            Prelude.<$> (x Data..?> "exportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartExportTask where
  hashWithSalt _salt StartExportTask' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` exportDataFormat
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` preferences
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData StartExportTask where
  rnf StartExportTask' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf exportDataFormat
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf preferences
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToHeaders StartExportTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.StartExportTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartExportTask where
  toJSON StartExportTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endTime" Data..=) Prelude.<$> endTime,
            ("exportDataFormat" Data..=)
              Prelude.<$> exportDataFormat,
            ("filters" Data..=) Prelude.<$> filters,
            ("preferences" Data..=) Prelude.<$> preferences,
            ("startTime" Data..=) Prelude.<$> startTime
          ]
      )

instance Data.ToPath StartExportTask where
  toPath = Prelude.const "/"

instance Data.ToQuery StartExportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartExportTaskResponse' smart constructor.
data StartExportTaskResponse = StartExportTaskResponse'
  { -- | A unique identifier used to query the status of an export request.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportId', 'startExportTaskResponse_exportId' - A unique identifier used to query the status of an export request.
--
-- 'httpStatus', 'startExportTaskResponse_httpStatus' - The response's http status code.
newStartExportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartExportTaskResponse
newStartExportTaskResponse pHttpStatus_ =
  StartExportTaskResponse'
    { exportId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier used to query the status of an export request.
startExportTaskResponse_exportId :: Lens.Lens' StartExportTaskResponse (Prelude.Maybe Prelude.Text)
startExportTaskResponse_exportId = Lens.lens (\StartExportTaskResponse' {exportId} -> exportId) (\s@StartExportTaskResponse' {} a -> s {exportId = a} :: StartExportTaskResponse)

-- | The response's http status code.
startExportTaskResponse_httpStatus :: Lens.Lens' StartExportTaskResponse Prelude.Int
startExportTaskResponse_httpStatus = Lens.lens (\StartExportTaskResponse' {httpStatus} -> httpStatus) (\s@StartExportTaskResponse' {} a -> s {httpStatus = a} :: StartExportTaskResponse)

instance Prelude.NFData StartExportTaskResponse where
  rnf StartExportTaskResponse' {..} =
    Prelude.rnf exportId
      `Prelude.seq` Prelude.rnf httpStatus
