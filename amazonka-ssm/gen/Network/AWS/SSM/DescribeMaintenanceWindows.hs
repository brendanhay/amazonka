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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindows
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the maintenance windows in an AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindows
  ( -- * Creating a Request
    DescribeMaintenanceWindows (..),
    newDescribeMaintenanceWindows,

    -- * Request Lenses
    describeMaintenanceWindows_nextToken,
    describeMaintenanceWindows_maxResults,
    describeMaintenanceWindows_filters,

    -- * Destructuring the Response
    DescribeMaintenanceWindowsResponse (..),
    newDescribeMaintenanceWindowsResponse,

    -- * Response Lenses
    describeMaintenanceWindowsResponse_nextToken,
    describeMaintenanceWindowsResponse_windowIdentities,
    describeMaintenanceWindowsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindows' smart constructor.
data DescribeMaintenanceWindows = DescribeMaintenanceWindows'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Optional filters used to narrow down the scope of the returned
    -- maintenance windows. Supported filter keys are __Name__ and __Enabled__.
    filters :: Core.Maybe [MaintenanceWindowFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindows_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeMaintenanceWindows_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeMaintenanceWindows_filters' - Optional filters used to narrow down the scope of the returned
-- maintenance windows. Supported filter keys are __Name__ and __Enabled__.
newDescribeMaintenanceWindows ::
  DescribeMaintenanceWindows
newDescribeMaintenanceWindows =
  DescribeMaintenanceWindows'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindows_nextToken :: Lens.Lens' DescribeMaintenanceWindows (Core.Maybe Core.Text)
describeMaintenanceWindows_nextToken = Lens.lens (\DescribeMaintenanceWindows' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindows' {} a -> s {nextToken = a} :: DescribeMaintenanceWindows)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindows_maxResults :: Lens.Lens' DescribeMaintenanceWindows (Core.Maybe Core.Natural)
describeMaintenanceWindows_maxResults = Lens.lens (\DescribeMaintenanceWindows' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindows' {} a -> s {maxResults = a} :: DescribeMaintenanceWindows)

-- | Optional filters used to narrow down the scope of the returned
-- maintenance windows. Supported filter keys are __Name__ and __Enabled__.
describeMaintenanceWindows_filters :: Lens.Lens' DescribeMaintenanceWindows (Core.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindows_filters = Lens.lens (\DescribeMaintenanceWindows' {filters} -> filters) (\s@DescribeMaintenanceWindows' {} a -> s {filters = a} :: DescribeMaintenanceWindows) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeMaintenanceWindows where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowsResponse_windowIdentities
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMaintenanceWindows_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeMaintenanceWindows where
  type
    AWSResponse DescribeMaintenanceWindows =
      DescribeMaintenanceWindowsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "WindowIdentities" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMaintenanceWindows

instance Core.NFData DescribeMaintenanceWindows

instance Core.ToHeaders DescribeMaintenanceWindows where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindows" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeMaintenanceWindows where
  toJSON DescribeMaintenanceWindows' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribeMaintenanceWindows where
  toPath = Core.const "/"

instance Core.ToQuery DescribeMaintenanceWindows where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMaintenanceWindowsResponse' smart constructor.
data DescribeMaintenanceWindowsResponse = DescribeMaintenanceWindowsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the maintenance windows.
    windowIdentities :: Core.Maybe [MaintenanceWindowIdentity],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'windowIdentities', 'describeMaintenanceWindowsResponse_windowIdentities' - Information about the maintenance windows.
--
-- 'httpStatus', 'describeMaintenanceWindowsResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMaintenanceWindowsResponse
newDescribeMaintenanceWindowsResponse pHttpStatus_ =
  DescribeMaintenanceWindowsResponse'
    { nextToken =
        Core.Nothing,
      windowIdentities = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowsResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowsResponse (Core.Maybe Core.Text)
describeMaintenanceWindowsResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowsResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowsResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowsResponse)

-- | Information about the maintenance windows.
describeMaintenanceWindowsResponse_windowIdentities :: Lens.Lens' DescribeMaintenanceWindowsResponse (Core.Maybe [MaintenanceWindowIdentity])
describeMaintenanceWindowsResponse_windowIdentities = Lens.lens (\DescribeMaintenanceWindowsResponse' {windowIdentities} -> windowIdentities) (\s@DescribeMaintenanceWindowsResponse' {} a -> s {windowIdentities = a} :: DescribeMaintenanceWindowsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMaintenanceWindowsResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowsResponse Core.Int
describeMaintenanceWindowsResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowsResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowsResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowsResponse)

instance
  Core.NFData
    DescribeMaintenanceWindowsResponse
