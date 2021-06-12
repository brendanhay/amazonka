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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets registered with the maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowTargets
  ( -- * Creating a Request
    DescribeMaintenanceWindowTargets (..),
    newDescribeMaintenanceWindowTargets,

    -- * Request Lenses
    describeMaintenanceWindowTargets_nextToken,
    describeMaintenanceWindowTargets_maxResults,
    describeMaintenanceWindowTargets_filters,
    describeMaintenanceWindowTargets_windowId,

    -- * Destructuring the Response
    DescribeMaintenanceWindowTargetsResponse (..),
    newDescribeMaintenanceWindowTargetsResponse,

    -- * Response Lenses
    describeMaintenanceWindowTargetsResponse_nextToken,
    describeMaintenanceWindowTargetsResponse_targets,
    describeMaintenanceWindowTargetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowTargets' smart constructor.
data DescribeMaintenanceWindowTargets = DescribeMaintenanceWindowTargets'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Optional filters that can be used to narrow down the scope of the
    -- returned window targets. The supported filter keys are Type,
    -- WindowTargetId and OwnerInformation.
    filters :: Core.Maybe [MaintenanceWindowFilter],
    -- | The ID of the maintenance window whose targets should be retrieved.
    windowId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowTargets_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeMaintenanceWindowTargets_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeMaintenanceWindowTargets_filters' - Optional filters that can be used to narrow down the scope of the
-- returned window targets. The supported filter keys are Type,
-- WindowTargetId and OwnerInformation.
--
-- 'windowId', 'describeMaintenanceWindowTargets_windowId' - The ID of the maintenance window whose targets should be retrieved.
newDescribeMaintenanceWindowTargets ::
  -- | 'windowId'
  Core.Text ->
  DescribeMaintenanceWindowTargets
newDescribeMaintenanceWindowTargets pWindowId_ =
  DescribeMaintenanceWindowTargets'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      windowId = pWindowId_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowTargets_nextToken :: Lens.Lens' DescribeMaintenanceWindowTargets (Core.Maybe Core.Text)
describeMaintenanceWindowTargets_nextToken = Lens.lens (\DescribeMaintenanceWindowTargets' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowTargets' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowTargets)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowTargets_maxResults :: Lens.Lens' DescribeMaintenanceWindowTargets (Core.Maybe Core.Natural)
describeMaintenanceWindowTargets_maxResults = Lens.lens (\DescribeMaintenanceWindowTargets' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowTargets' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowTargets)

-- | Optional filters that can be used to narrow down the scope of the
-- returned window targets. The supported filter keys are Type,
-- WindowTargetId and OwnerInformation.
describeMaintenanceWindowTargets_filters :: Lens.Lens' DescribeMaintenanceWindowTargets (Core.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindowTargets_filters = Lens.lens (\DescribeMaintenanceWindowTargets' {filters} -> filters) (\s@DescribeMaintenanceWindowTargets' {} a -> s {filters = a} :: DescribeMaintenanceWindowTargets) Core.. Lens.mapping Lens._Coerce

-- | The ID of the maintenance window whose targets should be retrieved.
describeMaintenanceWindowTargets_windowId :: Lens.Lens' DescribeMaintenanceWindowTargets Core.Text
describeMaintenanceWindowTargets_windowId = Lens.lens (\DescribeMaintenanceWindowTargets' {windowId} -> windowId) (\s@DescribeMaintenanceWindowTargets' {} a -> s {windowId = a} :: DescribeMaintenanceWindowTargets)

instance
  Core.AWSPager
    DescribeMaintenanceWindowTargets
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowTargetsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowTargetsResponse_targets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMaintenanceWindowTargets_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowTargetsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowTargets
  where
  type
    AWSResponse DescribeMaintenanceWindowTargets =
      DescribeMaintenanceWindowTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowTargetsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Targets" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeMaintenanceWindowTargets

instance Core.NFData DescribeMaintenanceWindowTargets

instance
  Core.ToHeaders
    DescribeMaintenanceWindowTargets
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindowTargets" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeMaintenanceWindowTargets where
  toJSON DescribeMaintenanceWindowTargets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just ("WindowId" Core..= windowId)
          ]
      )

instance Core.ToPath DescribeMaintenanceWindowTargets where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeMaintenanceWindowTargets
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMaintenanceWindowTargetsResponse' smart constructor.
data DescribeMaintenanceWindowTargetsResponse = DescribeMaintenanceWindowTargetsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the targets in the maintenance window.
    targets :: Core.Maybe [MaintenanceWindowTarget],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowTargetsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'targets', 'describeMaintenanceWindowTargetsResponse_targets' - Information about the targets in the maintenance window.
--
-- 'httpStatus', 'describeMaintenanceWindowTargetsResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowTargetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMaintenanceWindowTargetsResponse
newDescribeMaintenanceWindowTargetsResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowTargetsResponse'
      { nextToken =
          Core.Nothing,
        targets = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowTargetsResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse (Core.Maybe Core.Text)
describeMaintenanceWindowTargetsResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowTargetsResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowTargetsResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowTargetsResponse)

-- | Information about the targets in the maintenance window.
describeMaintenanceWindowTargetsResponse_targets :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse (Core.Maybe [MaintenanceWindowTarget])
describeMaintenanceWindowTargetsResponse_targets = Lens.lens (\DescribeMaintenanceWindowTargetsResponse' {targets} -> targets) (\s@DescribeMaintenanceWindowTargetsResponse' {} a -> s {targets = a} :: DescribeMaintenanceWindowTargetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMaintenanceWindowTargetsResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse Core.Int
describeMaintenanceWindowTargetsResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowTargetsResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowTargetsResponse)

instance
  Core.NFData
    DescribeMaintenanceWindowTargetsResponse
