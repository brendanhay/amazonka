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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the executions of a maintenance window. This includes information
-- about when the maintenance window was scheduled to be active, and
-- information about tasks registered and run with the maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowExecutions
  ( -- * Creating a Request
    DescribeMaintenanceWindowExecutions (..),
    newDescribeMaintenanceWindowExecutions,

    -- * Request Lenses
    describeMaintenanceWindowExecutions_nextToken,
    describeMaintenanceWindowExecutions_maxResults,
    describeMaintenanceWindowExecutions_filters,
    describeMaintenanceWindowExecutions_windowId,

    -- * Destructuring the Response
    DescribeMaintenanceWindowExecutionsResponse (..),
    newDescribeMaintenanceWindowExecutionsResponse,

    -- * Response Lenses
    describeMaintenanceWindowExecutionsResponse_nextToken,
    describeMaintenanceWindowExecutionsResponse_windowExecutions,
    describeMaintenanceWindowExecutionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowExecutions' smart constructor.
data DescribeMaintenanceWindowExecutions = DescribeMaintenanceWindowExecutions'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Each entry in the array is a structure containing:
    --
    -- Key (string, between 1 and 128 characters)
    --
    -- Values (array of strings, each string is between 1 and 256 characters)
    --
    -- The supported Keys are ExecutedBefore and ExecutedAfter with the value
    -- being a date\/time string such as 2016-11-04T05:00:00Z.
    filters :: Core.Maybe [MaintenanceWindowFilter],
    -- | The ID of the maintenance window whose executions should be retrieved.
    windowId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowExecutions_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeMaintenanceWindowExecutions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeMaintenanceWindowExecutions_filters' - Each entry in the array is a structure containing:
--
-- Key (string, between 1 and 128 characters)
--
-- Values (array of strings, each string is between 1 and 256 characters)
--
-- The supported Keys are ExecutedBefore and ExecutedAfter with the value
-- being a date\/time string such as 2016-11-04T05:00:00Z.
--
-- 'windowId', 'describeMaintenanceWindowExecutions_windowId' - The ID of the maintenance window whose executions should be retrieved.
newDescribeMaintenanceWindowExecutions ::
  -- | 'windowId'
  Core.Text ->
  DescribeMaintenanceWindowExecutions
newDescribeMaintenanceWindowExecutions pWindowId_ =
  DescribeMaintenanceWindowExecutions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      windowId = pWindowId_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowExecutions_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutions (Core.Maybe Core.Text)
describeMaintenanceWindowExecutions_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutions' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutions' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowExecutions_maxResults :: Lens.Lens' DescribeMaintenanceWindowExecutions (Core.Maybe Core.Natural)
describeMaintenanceWindowExecutions_maxResults = Lens.lens (\DescribeMaintenanceWindowExecutions' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowExecutions' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowExecutions)

-- | Each entry in the array is a structure containing:
--
-- Key (string, between 1 and 128 characters)
--
-- Values (array of strings, each string is between 1 and 256 characters)
--
-- The supported Keys are ExecutedBefore and ExecutedAfter with the value
-- being a date\/time string such as 2016-11-04T05:00:00Z.
describeMaintenanceWindowExecutions_filters :: Lens.Lens' DescribeMaintenanceWindowExecutions (Core.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindowExecutions_filters = Lens.lens (\DescribeMaintenanceWindowExecutions' {filters} -> filters) (\s@DescribeMaintenanceWindowExecutions' {} a -> s {filters = a} :: DescribeMaintenanceWindowExecutions) Core.. Lens.mapping Lens._Coerce

-- | The ID of the maintenance window whose executions should be retrieved.
describeMaintenanceWindowExecutions_windowId :: Lens.Lens' DescribeMaintenanceWindowExecutions Core.Text
describeMaintenanceWindowExecutions_windowId = Lens.lens (\DescribeMaintenanceWindowExecutions' {windowId} -> windowId) (\s@DescribeMaintenanceWindowExecutions' {} a -> s {windowId = a} :: DescribeMaintenanceWindowExecutions)

instance
  Core.AWSPager
    DescribeMaintenanceWindowExecutions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionsResponse_windowExecutions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMaintenanceWindowExecutions_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowExecutionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowExecutions
  where
  type
    AWSResponse DescribeMaintenanceWindowExecutions =
      DescribeMaintenanceWindowExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowExecutionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "WindowExecutions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeMaintenanceWindowExecutions

instance
  Core.NFData
    DescribeMaintenanceWindowExecutions

instance
  Core.ToHeaders
    DescribeMaintenanceWindowExecutions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindowExecutions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeMaintenanceWindowExecutions
  where
  toJSON DescribeMaintenanceWindowExecutions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just ("WindowId" Core..= windowId)
          ]
      )

instance
  Core.ToPath
    DescribeMaintenanceWindowExecutions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeMaintenanceWindowExecutions
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMaintenanceWindowExecutionsResponse' smart constructor.
data DescribeMaintenanceWindowExecutionsResponse = DescribeMaintenanceWindowExecutionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the maintenance window executions.
    windowExecutions :: Core.Maybe [MaintenanceWindowExecution],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowExecutionsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'windowExecutions', 'describeMaintenanceWindowExecutionsResponse_windowExecutions' - Information about the maintenance window executions.
--
-- 'httpStatus', 'describeMaintenanceWindowExecutionsResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowExecutionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMaintenanceWindowExecutionsResponse
newDescribeMaintenanceWindowExecutionsResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowExecutionsResponse'
      { nextToken =
          Core.Nothing,
        windowExecutions =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowExecutionsResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse (Core.Maybe Core.Text)
describeMaintenanceWindowExecutionsResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutionsResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutionsResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionsResponse)

-- | Information about the maintenance window executions.
describeMaintenanceWindowExecutionsResponse_windowExecutions :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse (Core.Maybe [MaintenanceWindowExecution])
describeMaintenanceWindowExecutionsResponse_windowExecutions = Lens.lens (\DescribeMaintenanceWindowExecutionsResponse' {windowExecutions} -> windowExecutions) (\s@DescribeMaintenanceWindowExecutionsResponse' {} a -> s {windowExecutions = a} :: DescribeMaintenanceWindowExecutionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMaintenanceWindowExecutionsResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse Core.Int
describeMaintenanceWindowExecutionsResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowExecutionsResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowExecutionsResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowExecutionsResponse)

instance
  Core.NFData
    DescribeMaintenanceWindowExecutionsResponse
