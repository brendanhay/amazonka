{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.StartExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins the export of discovered data to an S3 bucket.
--
-- If you specify @agentIds@ in a filter, the task exports up to 72 hours of detailed data collected by the identified Application Discovery Agent, including network, process, and performance details. A time range for exported agent data may be set by using @startTime@ and @endTime@ . Export of detailed agent data is limited to five concurrently running exports.
-- If you do not include an @agentIds@ filter, summary data is exported that includes both AWS Agentless Discovery Connector data and summary data from AWS Discovery Agents. Export of summary data is limited to two exports per day.
module Network.AWS.Discovery.StartExportTask
  ( -- * Creating a request
    StartExportTask (..),
    mkStartExportTask,

    -- ** Request lenses
    setExportDataFormat,
    setStartTime,
    setFilters,
    setEndTime,

    -- * Destructuring the response
    StartExportTaskResponse (..),
    mkStartExportTaskResponse,

    -- ** Response lenses
    setrsExportId,
    setrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartExportTask' smart constructor.
data StartExportTask = StartExportTask'
  { exportDataFormat ::
      Lude.Maybe [ExportDataFormat],
    startTime :: Lude.Maybe Lude.Timestamp,
    filters :: Lude.Maybe [ExportFilter],
    endTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartExportTask' with the minimum fields required to make a request.
--
-- * 'endTime' - The end timestamp for exported data from the single Application Discovery Agent selected in the filters. If no value is specified, exported data includes the most recent data collected by the agent.
-- * 'exportDataFormat' - The file format for the returned export data. Default value is @CSV@ . __Note:__ /The/ @GRAPHML@ /option has been deprecated./
-- * 'filters' - If a filter is present, it selects the single @agentId@ of the Application Discovery Agent for which data is exported. The @agentId@ can be found in the results of the @DescribeAgents@ API or CLI. If no filter is present, @startTime@ and @endTime@ are ignored and exported data includes both Agentless Discovery Connector data and summary data from Application Discovery agents.
-- * 'startTime' - The start timestamp for exported data from the single Application Discovery Agent selected in the filters. If no value is specified, data is exported starting from the first data collected by the agent.
mkStartExportTask ::
  StartExportTask
mkStartExportTask =
  StartExportTask'
    { exportDataFormat = Lude.Nothing,
      startTime = Lude.Nothing,
      filters = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | The file format for the returned export data. Default value is @CSV@ . __Note:__ /The/ @GRAPHML@ /option has been deprecated./
--
-- /Note:/ Consider using 'exportDataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setExportDataFormat :: Lens.Lens' StartExportTask (Lude.Maybe [ExportDataFormat])
setExportDataFormat = Lens.lens (exportDataFormat :: StartExportTask -> Lude.Maybe [ExportDataFormat]) (\s a -> s {exportDataFormat = a} :: StartExportTask)
{-# DEPRECATED setExportDataFormat "Use generic-lens or generic-optics with 'exportDataFormat' instead." #-}

-- | The start timestamp for exported data from the single Application Discovery Agent selected in the filters. If no value is specified, data is exported starting from the first data collected by the agent.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setStartTime :: Lens.Lens' StartExportTask (Lude.Maybe Lude.Timestamp)
setStartTime = Lens.lens (startTime :: StartExportTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: StartExportTask)
{-# DEPRECATED setStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | If a filter is present, it selects the single @agentId@ of the Application Discovery Agent for which data is exported. The @agentId@ can be found in the results of the @DescribeAgents@ API or CLI. If no filter is present, @startTime@ and @endTime@ are ignored and exported data includes both Agentless Discovery Connector data and summary data from Application Discovery agents.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setFilters :: Lens.Lens' StartExportTask (Lude.Maybe [ExportFilter])
setFilters = Lens.lens (filters :: StartExportTask -> Lude.Maybe [ExportFilter]) (\s a -> s {filters = a} :: StartExportTask)
{-# DEPRECATED setFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The end timestamp for exported data from the single Application Discovery Agent selected in the filters. If no value is specified, exported data includes the most recent data collected by the agent.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setEndTime :: Lens.Lens' StartExportTask (Lude.Maybe Lude.Timestamp)
setEndTime = Lens.lens (endTime :: StartExportTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: StartExportTask)
{-# DEPRECATED setEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.AWSRequest StartExportTask where
  type Rs StartExportTask = StartExportTaskResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartExportTaskResponse'
            Lude.<$> (x Lude..?> "exportId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartExportTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.StartExportTask" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartExportTask where
  toJSON StartExportTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("exportDataFormat" Lude..=) Lude.<$> exportDataFormat,
            ("startTime" Lude..=) Lude.<$> startTime,
            ("filters" Lude..=) Lude.<$> filters,
            ("endTime" Lude..=) Lude.<$> endTime
          ]
      )

instance Lude.ToPath StartExportTask where
  toPath = Lude.const "/"

instance Lude.ToQuery StartExportTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartExportTaskResponse' smart constructor.
data StartExportTaskResponse = StartExportTaskResponse'
  { exportId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StartExportTaskResponse' with the minimum fields required to make a request.
--
-- * 'exportId' - A unique identifier used to query the status of an export request.
-- * 'responseStatus' - The response status code.
mkStartExportTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartExportTaskResponse
mkStartExportTaskResponse pResponseStatus_ =
  StartExportTaskResponse'
    { exportId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier used to query the status of an export request.
--
-- /Note:/ Consider using 'exportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setrsExportId :: Lens.Lens' StartExportTaskResponse (Lude.Maybe Lude.Text)
setrsExportId = Lens.lens (exportId :: StartExportTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {exportId = a} :: StartExportTaskResponse)
{-# DEPRECATED setrsExportId "Use generic-lens or generic-optics with 'exportId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setrsResponseStatus :: Lens.Lens' StartExportTaskResponse Lude.Int
setrsResponseStatus = Lens.lens (responseStatus :: StartExportTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartExportTaskResponse)
{-# DEPRECATED setrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
