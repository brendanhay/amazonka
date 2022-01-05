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
-- Module      : Amazonka.SSM.DescribeMaintenanceWindowSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about upcoming executions of a maintenance window.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeMaintenanceWindowSchedule
  ( -- * Creating a Request
    DescribeMaintenanceWindowSchedule (..),
    newDescribeMaintenanceWindowSchedule,

    -- * Request Lenses
    describeMaintenanceWindowSchedule_resourceType,
    describeMaintenanceWindowSchedule_filters,
    describeMaintenanceWindowSchedule_nextToken,
    describeMaintenanceWindowSchedule_targets,
    describeMaintenanceWindowSchedule_maxResults,
    describeMaintenanceWindowSchedule_windowId,

    -- * Destructuring the Response
    DescribeMaintenanceWindowScheduleResponse (..),
    newDescribeMaintenanceWindowScheduleResponse,

    -- * Response Lenses
    describeMaintenanceWindowScheduleResponse_scheduledWindowExecutions,
    describeMaintenanceWindowScheduleResponse_nextToken,
    describeMaintenanceWindowScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowSchedule' smart constructor.
data DescribeMaintenanceWindowSchedule = DescribeMaintenanceWindowSchedule'
  { -- | The type of resource you want to retrieve information about. For
    -- example, @INSTANCE@.
    resourceType :: Prelude.Maybe MaintenanceWindowResourceType,
    -- | Filters used to limit the range of results. For example, you can limit
    -- maintenance window executions to only those scheduled before or after a
    -- certain date and time.
    filters :: Prelude.Maybe [PatchOrchestratorFilter],
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The instance ID or key-value pair to retrieve information about.
    targets :: Prelude.Maybe [Target],
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the maintenance window to retrieve information about.
    windowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'describeMaintenanceWindowSchedule_resourceType' - The type of resource you want to retrieve information about. For
-- example, @INSTANCE@.
--
-- 'filters', 'describeMaintenanceWindowSchedule_filters' - Filters used to limit the range of results. For example, you can limit
-- maintenance window executions to only those scheduled before or after a
-- certain date and time.
--
-- 'nextToken', 'describeMaintenanceWindowSchedule_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'targets', 'describeMaintenanceWindowSchedule_targets' - The instance ID or key-value pair to retrieve information about.
--
-- 'maxResults', 'describeMaintenanceWindowSchedule_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'windowId', 'describeMaintenanceWindowSchedule_windowId' - The ID of the maintenance window to retrieve information about.
newDescribeMaintenanceWindowSchedule ::
  DescribeMaintenanceWindowSchedule
newDescribeMaintenanceWindowSchedule =
  DescribeMaintenanceWindowSchedule'
    { resourceType =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      targets = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      windowId = Prelude.Nothing
    }

-- | The type of resource you want to retrieve information about. For
-- example, @INSTANCE@.
describeMaintenanceWindowSchedule_resourceType :: Lens.Lens' DescribeMaintenanceWindowSchedule (Prelude.Maybe MaintenanceWindowResourceType)
describeMaintenanceWindowSchedule_resourceType = Lens.lens (\DescribeMaintenanceWindowSchedule' {resourceType} -> resourceType) (\s@DescribeMaintenanceWindowSchedule' {} a -> s {resourceType = a} :: DescribeMaintenanceWindowSchedule)

-- | Filters used to limit the range of results. For example, you can limit
-- maintenance window executions to only those scheduled before or after a
-- certain date and time.
describeMaintenanceWindowSchedule_filters :: Lens.Lens' DescribeMaintenanceWindowSchedule (Prelude.Maybe [PatchOrchestratorFilter])
describeMaintenanceWindowSchedule_filters = Lens.lens (\DescribeMaintenanceWindowSchedule' {filters} -> filters) (\s@DescribeMaintenanceWindowSchedule' {} a -> s {filters = a} :: DescribeMaintenanceWindowSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowSchedule_nextToken :: Lens.Lens' DescribeMaintenanceWindowSchedule (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowSchedule_nextToken = Lens.lens (\DescribeMaintenanceWindowSchedule' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowSchedule' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowSchedule)

-- | The instance ID or key-value pair to retrieve information about.
describeMaintenanceWindowSchedule_targets :: Lens.Lens' DescribeMaintenanceWindowSchedule (Prelude.Maybe [Target])
describeMaintenanceWindowSchedule_targets = Lens.lens (\DescribeMaintenanceWindowSchedule' {targets} -> targets) (\s@DescribeMaintenanceWindowSchedule' {} a -> s {targets = a} :: DescribeMaintenanceWindowSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowSchedule_maxResults :: Lens.Lens' DescribeMaintenanceWindowSchedule (Prelude.Maybe Prelude.Natural)
describeMaintenanceWindowSchedule_maxResults = Lens.lens (\DescribeMaintenanceWindowSchedule' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowSchedule' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowSchedule)

-- | The ID of the maintenance window to retrieve information about.
describeMaintenanceWindowSchedule_windowId :: Lens.Lens' DescribeMaintenanceWindowSchedule (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowSchedule_windowId = Lens.lens (\DescribeMaintenanceWindowSchedule' {windowId} -> windowId) (\s@DescribeMaintenanceWindowSchedule' {} a -> s {windowId = a} :: DescribeMaintenanceWindowSchedule)

instance
  Core.AWSPager
    DescribeMaintenanceWindowSchedule
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowScheduleResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowScheduleResponse_scheduledWindowExecutions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeMaintenanceWindowSchedule_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowScheduleResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowSchedule
  where
  type
    AWSResponse DescribeMaintenanceWindowSchedule =
      DescribeMaintenanceWindowScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowScheduleResponse'
            Prelude.<$> ( x Core..?> "ScheduledWindowExecutions"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMaintenanceWindowSchedule
  where
  hashWithSalt
    _salt
    DescribeMaintenanceWindowSchedule' {..} =
      _salt `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` targets
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` windowId

instance
  Prelude.NFData
    DescribeMaintenanceWindowSchedule
  where
  rnf DescribeMaintenanceWindowSchedule' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf windowId

instance
  Core.ToHeaders
    DescribeMaintenanceWindowSchedule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindowSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeMaintenanceWindowSchedule
  where
  toJSON DescribeMaintenanceWindowSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceType" Core..=) Prelude.<$> resourceType,
            ("Filters" Core..=) Prelude.<$> filters,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Targets" Core..=) Prelude.<$> targets,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("WindowId" Core..=) Prelude.<$> windowId
          ]
      )

instance
  Core.ToPath
    DescribeMaintenanceWindowSchedule
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeMaintenanceWindowSchedule
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMaintenanceWindowScheduleResponse' smart constructor.
data DescribeMaintenanceWindowScheduleResponse = DescribeMaintenanceWindowScheduleResponse'
  { -- | Information about maintenance window executions scheduled for the
    -- specified time range.
    scheduledWindowExecutions :: Prelude.Maybe [ScheduledWindowExecution],
    -- | The token for the next set of items to return. (You use this token in
    -- the next call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledWindowExecutions', 'describeMaintenanceWindowScheduleResponse_scheduledWindowExecutions' - Information about maintenance window executions scheduled for the
-- specified time range.
--
-- 'nextToken', 'describeMaintenanceWindowScheduleResponse_nextToken' - The token for the next set of items to return. (You use this token in
-- the next call.)
--
-- 'httpStatus', 'describeMaintenanceWindowScheduleResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMaintenanceWindowScheduleResponse
newDescribeMaintenanceWindowScheduleResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowScheduleResponse'
      { scheduledWindowExecutions =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about maintenance window executions scheduled for the
-- specified time range.
describeMaintenanceWindowScheduleResponse_scheduledWindowExecutions :: Lens.Lens' DescribeMaintenanceWindowScheduleResponse (Prelude.Maybe [ScheduledWindowExecution])
describeMaintenanceWindowScheduleResponse_scheduledWindowExecutions = Lens.lens (\DescribeMaintenanceWindowScheduleResponse' {scheduledWindowExecutions} -> scheduledWindowExecutions) (\s@DescribeMaintenanceWindowScheduleResponse' {} a -> s {scheduledWindowExecutions = a} :: DescribeMaintenanceWindowScheduleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. (You use this token in
-- the next call.)
describeMaintenanceWindowScheduleResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowScheduleResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowScheduleResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowScheduleResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowScheduleResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowScheduleResponse)

-- | The response's http status code.
describeMaintenanceWindowScheduleResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowScheduleResponse Prelude.Int
describeMaintenanceWindowScheduleResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowScheduleResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowScheduleResponse)

instance
  Prelude.NFData
    DescribeMaintenanceWindowScheduleResponse
  where
  rnf DescribeMaintenanceWindowScheduleResponse' {..} =
    Prelude.rnf scheduledWindowExecutions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
