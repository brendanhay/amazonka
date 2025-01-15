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
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins the export of discovered data to an S3 bucket.
--
-- If you specify @agentIds@ in a filter, the task exports up to 72 hours
-- of detailed data collected by the identified Application Discovery
-- Agent, including network, process, and performance details. A time range
-- for exported agent data may be set by using @startTime@ and @endTime@.
-- Export of detailed agent data is limited to five concurrently running
-- exports.
--
-- If you do not include an @agentIds@ filter, summary data is exported
-- that includes both Amazon Web Services Agentless Discovery Connector
-- data and summary data from Amazon Web Services Discovery Agents. Export
-- of summary data is limited to two exports per day.
module Amazonka.Discovery.StartExportTask
  ( -- * Creating a Request
    StartExportTask (..),
    newStartExportTask,

    -- * Request Lenses
    startExportTask_endTime,
    startExportTask_exportDataFormat,
    startExportTask_filters,
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
    -- data includes both Agentless Discovery Connector data and summary data
    -- from Application Discovery agents.
    filters :: Prelude.Maybe [ExportFilter],
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
-- data includes both Agentless Discovery Connector data and summary data
-- from Application Discovery agents.
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
-- data includes both Agentless Discovery Connector data and summary data
-- from Application Discovery agents.
startExportTask_filters :: Lens.Lens' StartExportTask (Prelude.Maybe [ExportFilter])
startExportTask_filters = Lens.lens (\StartExportTask' {filters} -> filters) (\s@StartExportTask' {} a -> s {filters = a} :: StartExportTask) Prelude.. Lens.mapping Lens.coerced

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
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData StartExportTask where
  rnf StartExportTask' {..} =
    Prelude.rnf endTime `Prelude.seq`
      Prelude.rnf exportDataFormat `Prelude.seq`
        Prelude.rnf filters `Prelude.seq`
          Prelude.rnf startTime

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
    Prelude.rnf exportId `Prelude.seq`
      Prelude.rnf httpStatus
