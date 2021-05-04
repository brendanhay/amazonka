{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Discovery.StartExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- that includes both AWS Agentless Discovery Connector data and summary
-- data from AWS Discovery Agents. Export of summary data is limited to two
-- exports per day.
module Network.AWS.Discovery.StartExportTask
  ( -- * Creating a Request
    StartExportTask (..),
    newStartExportTask,

    -- * Request Lenses
    startExportTask_exportDataFormat,
    startExportTask_startTime,
    startExportTask_endTime,
    startExportTask_filters,

    -- * Destructuring the Response
    StartExportTaskResponse (..),
    newStartExportTaskResponse,

    -- * Response Lenses
    startExportTaskResponse_exportId,
    startExportTaskResponse_httpStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartExportTask' smart constructor.
data StartExportTask = StartExportTask'
  { -- | The file format for the returned export data. Default value is @CSV@.
    -- __Note:__ /The/ @GRAPHML@ /option has been deprecated./
    exportDataFormat :: Prelude.Maybe [ExportDataFormat],
    -- | The start timestamp for exported data from the single Application
    -- Discovery Agent selected in the filters. If no value is specified, data
    -- is exported starting from the first data collected by the agent.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The end timestamp for exported data from the single Application
    -- Discovery Agent selected in the filters. If no value is specified,
    -- exported data includes the most recent data collected by the agent.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | If a filter is present, it selects the single @agentId@ of the
    -- Application Discovery Agent for which data is exported. The @agentId@
    -- can be found in the results of the @DescribeAgents@ API or CLI. If no
    -- filter is present, @startTime@ and @endTime@ are ignored and exported
    -- data includes both Agentless Discovery Connector data and summary data
    -- from Application Discovery agents.
    filters :: Prelude.Maybe [ExportFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportDataFormat', 'startExportTask_exportDataFormat' - The file format for the returned export data. Default value is @CSV@.
-- __Note:__ /The/ @GRAPHML@ /option has been deprecated./
--
-- 'startTime', 'startExportTask_startTime' - The start timestamp for exported data from the single Application
-- Discovery Agent selected in the filters. If no value is specified, data
-- is exported starting from the first data collected by the agent.
--
-- 'endTime', 'startExportTask_endTime' - The end timestamp for exported data from the single Application
-- Discovery Agent selected in the filters. If no value is specified,
-- exported data includes the most recent data collected by the agent.
--
-- 'filters', 'startExportTask_filters' - If a filter is present, it selects the single @agentId@ of the
-- Application Discovery Agent for which data is exported. The @agentId@
-- can be found in the results of the @DescribeAgents@ API or CLI. If no
-- filter is present, @startTime@ and @endTime@ are ignored and exported
-- data includes both Agentless Discovery Connector data and summary data
-- from Application Discovery agents.
newStartExportTask ::
  StartExportTask
newStartExportTask =
  StartExportTask'
    { exportDataFormat =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The file format for the returned export data. Default value is @CSV@.
-- __Note:__ /The/ @GRAPHML@ /option has been deprecated./
startExportTask_exportDataFormat :: Lens.Lens' StartExportTask (Prelude.Maybe [ExportDataFormat])
startExportTask_exportDataFormat = Lens.lens (\StartExportTask' {exportDataFormat} -> exportDataFormat) (\s@StartExportTask' {} a -> s {exportDataFormat = a} :: StartExportTask) Prelude.. Lens.mapping Prelude._Coerce

-- | The start timestamp for exported data from the single Application
-- Discovery Agent selected in the filters. If no value is specified, data
-- is exported starting from the first data collected by the agent.
startExportTask_startTime :: Lens.Lens' StartExportTask (Prelude.Maybe Prelude.UTCTime)
startExportTask_startTime = Lens.lens (\StartExportTask' {startTime} -> startTime) (\s@StartExportTask' {} a -> s {startTime = a} :: StartExportTask) Prelude.. Lens.mapping Prelude._Time

-- | The end timestamp for exported data from the single Application
-- Discovery Agent selected in the filters. If no value is specified,
-- exported data includes the most recent data collected by the agent.
startExportTask_endTime :: Lens.Lens' StartExportTask (Prelude.Maybe Prelude.UTCTime)
startExportTask_endTime = Lens.lens (\StartExportTask' {endTime} -> endTime) (\s@StartExportTask' {} a -> s {endTime = a} :: StartExportTask) Prelude.. Lens.mapping Prelude._Time

-- | If a filter is present, it selects the single @agentId@ of the
-- Application Discovery Agent for which data is exported. The @agentId@
-- can be found in the results of the @DescribeAgents@ API or CLI. If no
-- filter is present, @startTime@ and @endTime@ are ignored and exported
-- data includes both Agentless Discovery Connector data and summary data
-- from Application Discovery agents.
startExportTask_filters :: Lens.Lens' StartExportTask (Prelude.Maybe [ExportFilter])
startExportTask_filters = Lens.lens (\StartExportTask' {filters} -> filters) (\s@StartExportTask' {} a -> s {filters = a} :: StartExportTask) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest StartExportTask where
  type Rs StartExportTask = StartExportTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExportTaskResponse'
            Prelude.<$> (x Prelude..?> "exportId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartExportTask

instance Prelude.NFData StartExportTask

instance Prelude.ToHeaders StartExportTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSPoseidonService_V2015_11_01.StartExportTask" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartExportTask where
  toJSON StartExportTask' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("exportDataFormat" Prelude..=)
              Prelude.<$> exportDataFormat,
            ("startTime" Prelude..=) Prelude.<$> startTime,
            ("endTime" Prelude..=) Prelude.<$> endTime,
            ("filters" Prelude..=) Prelude.<$> filters
          ]
      )

instance Prelude.ToPath StartExportTask where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartExportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartExportTaskResponse' smart constructor.
data StartExportTaskResponse = StartExportTaskResponse'
  { -- | A unique identifier used to query the status of an export request.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData StartExportTaskResponse
