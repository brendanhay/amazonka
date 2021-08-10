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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindows' smart constructor.
data DescribeMaintenanceWindows = DescribeMaintenanceWindows'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Optional filters used to narrow down the scope of the returned
    -- maintenance windows. Supported filter keys are __Name__ and __Enabled__.
    filters :: Prelude.Maybe [MaintenanceWindowFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindows_nextToken :: Lens.Lens' DescribeMaintenanceWindows (Prelude.Maybe Prelude.Text)
describeMaintenanceWindows_nextToken = Lens.lens (\DescribeMaintenanceWindows' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindows' {} a -> s {nextToken = a} :: DescribeMaintenanceWindows)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindows_maxResults :: Lens.Lens' DescribeMaintenanceWindows (Prelude.Maybe Prelude.Natural)
describeMaintenanceWindows_maxResults = Lens.lens (\DescribeMaintenanceWindows' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindows' {} a -> s {maxResults = a} :: DescribeMaintenanceWindows)

-- | Optional filters used to narrow down the scope of the returned
-- maintenance windows. Supported filter keys are __Name__ and __Enabled__.
describeMaintenanceWindows_filters :: Lens.Lens' DescribeMaintenanceWindows (Prelude.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindows_filters = Lens.lens (\DescribeMaintenanceWindows' {filters} -> filters) (\s@DescribeMaintenanceWindows' {} a -> s {filters = a} :: DescribeMaintenanceWindows) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeMaintenanceWindows where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowsResponse_windowIdentities
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeMaintenanceWindows_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeMaintenanceWindows where
  type
    AWSResponse DescribeMaintenanceWindows =
      DescribeMaintenanceWindowsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "WindowIdentities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMaintenanceWindows

instance Prelude.NFData DescribeMaintenanceWindows

instance Core.ToHeaders DescribeMaintenanceWindows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindows" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeMaintenanceWindows where
  toJSON DescribeMaintenanceWindows' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath DescribeMaintenanceWindows where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeMaintenanceWindows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMaintenanceWindowsResponse' smart constructor.
data DescribeMaintenanceWindowsResponse = DescribeMaintenanceWindowsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the maintenance windows.
    windowIdentities :: Prelude.Maybe [MaintenanceWindowIdentity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeMaintenanceWindowsResponse
newDescribeMaintenanceWindowsResponse pHttpStatus_ =
  DescribeMaintenanceWindowsResponse'
    { nextToken =
        Prelude.Nothing,
      windowIdentities = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowsResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowsResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowsResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowsResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowsResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowsResponse)

-- | Information about the maintenance windows.
describeMaintenanceWindowsResponse_windowIdentities :: Lens.Lens' DescribeMaintenanceWindowsResponse (Prelude.Maybe [MaintenanceWindowIdentity])
describeMaintenanceWindowsResponse_windowIdentities = Lens.lens (\DescribeMaintenanceWindowsResponse' {windowIdentities} -> windowIdentities) (\s@DescribeMaintenanceWindowsResponse' {} a -> s {windowIdentities = a} :: DescribeMaintenanceWindowsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMaintenanceWindowsResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowsResponse Prelude.Int
describeMaintenanceWindowsResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowsResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowsResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowsResponse)

instance
  Prelude.NFData
    DescribeMaintenanceWindowsResponse
