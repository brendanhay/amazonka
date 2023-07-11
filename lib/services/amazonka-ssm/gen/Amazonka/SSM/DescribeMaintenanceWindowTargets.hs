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
-- Module      : Amazonka.SSM.DescribeMaintenanceWindowTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets registered with the maintenance window.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeMaintenanceWindowTargets
  ( -- * Creating a Request
    DescribeMaintenanceWindowTargets (..),
    newDescribeMaintenanceWindowTargets,

    -- * Request Lenses
    describeMaintenanceWindowTargets_filters,
    describeMaintenanceWindowTargets_maxResults,
    describeMaintenanceWindowTargets_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowTargets' smart constructor.
data DescribeMaintenanceWindowTargets = DescribeMaintenanceWindowTargets'
  { -- | Optional filters that can be used to narrow down the scope of the
    -- returned window targets. The supported filter keys are @Type@,
    -- @WindowTargetId@, and @OwnerInformation@.
    filters :: Prelude.Maybe [MaintenanceWindowFilter],
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window whose targets should be retrieved.
    windowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeMaintenanceWindowTargets_filters' - Optional filters that can be used to narrow down the scope of the
-- returned window targets. The supported filter keys are @Type@,
-- @WindowTargetId@, and @OwnerInformation@.
--
-- 'maxResults', 'describeMaintenanceWindowTargets_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'describeMaintenanceWindowTargets_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'windowId', 'describeMaintenanceWindowTargets_windowId' - The ID of the maintenance window whose targets should be retrieved.
newDescribeMaintenanceWindowTargets ::
  -- | 'windowId'
  Prelude.Text ->
  DescribeMaintenanceWindowTargets
newDescribeMaintenanceWindowTargets pWindowId_ =
  DescribeMaintenanceWindowTargets'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      windowId = pWindowId_
    }

-- | Optional filters that can be used to narrow down the scope of the
-- returned window targets. The supported filter keys are @Type@,
-- @WindowTargetId@, and @OwnerInformation@.
describeMaintenanceWindowTargets_filters :: Lens.Lens' DescribeMaintenanceWindowTargets (Prelude.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindowTargets_filters = Lens.lens (\DescribeMaintenanceWindowTargets' {filters} -> filters) (\s@DescribeMaintenanceWindowTargets' {} a -> s {filters = a} :: DescribeMaintenanceWindowTargets) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowTargets_maxResults :: Lens.Lens' DescribeMaintenanceWindowTargets (Prelude.Maybe Prelude.Natural)
describeMaintenanceWindowTargets_maxResults = Lens.lens (\DescribeMaintenanceWindowTargets' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowTargets' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowTargets)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowTargets_nextToken :: Lens.Lens' DescribeMaintenanceWindowTargets (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowTargets_nextToken = Lens.lens (\DescribeMaintenanceWindowTargets' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowTargets' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowTargets)

-- | The ID of the maintenance window whose targets should be retrieved.
describeMaintenanceWindowTargets_windowId :: Lens.Lens' DescribeMaintenanceWindowTargets Prelude.Text
describeMaintenanceWindowTargets_windowId = Lens.lens (\DescribeMaintenanceWindowTargets' {windowId} -> windowId) (\s@DescribeMaintenanceWindowTargets' {} a -> s {windowId = a} :: DescribeMaintenanceWindowTargets)

instance
  Core.AWSPager
    DescribeMaintenanceWindowTargets
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowTargetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowTargetsResponse_targets
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeMaintenanceWindowTargets_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowTargetsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowTargets
  where
  type
    AWSResponse DescribeMaintenanceWindowTargets =
      DescribeMaintenanceWindowTargetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowTargetsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Targets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMaintenanceWindowTargets
  where
  hashWithSalt
    _salt
    DescribeMaintenanceWindowTargets' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` windowId

instance
  Prelude.NFData
    DescribeMaintenanceWindowTargets
  where
  rnf DescribeMaintenanceWindowTargets' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf windowId

instance
  Data.ToHeaders
    DescribeMaintenanceWindowTargets
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeMaintenanceWindowTargets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMaintenanceWindowTargets where
  toJSON DescribeMaintenanceWindowTargets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("WindowId" Data..= windowId)
          ]
      )

instance Data.ToPath DescribeMaintenanceWindowTargets where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeMaintenanceWindowTargets
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMaintenanceWindowTargetsResponse' smart constructor.
data DescribeMaintenanceWindowTargetsResponse = DescribeMaintenanceWindowTargetsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the targets in the maintenance window.
    targets :: Prelude.Maybe [MaintenanceWindowTarget],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeMaintenanceWindowTargetsResponse
newDescribeMaintenanceWindowTargetsResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowTargetsResponse'
      { nextToken =
          Prelude.Nothing,
        targets = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowTargetsResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowTargetsResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowTargetsResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowTargetsResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowTargetsResponse)

-- | Information about the targets in the maintenance window.
describeMaintenanceWindowTargetsResponse_targets :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse (Prelude.Maybe [MaintenanceWindowTarget])
describeMaintenanceWindowTargetsResponse_targets = Lens.lens (\DescribeMaintenanceWindowTargetsResponse' {targets} -> targets) (\s@DescribeMaintenanceWindowTargetsResponse' {} a -> s {targets = a} :: DescribeMaintenanceWindowTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMaintenanceWindowTargetsResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse Prelude.Int
describeMaintenanceWindowTargetsResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowTargetsResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowTargetsResponse)

instance
  Prelude.NFData
    DescribeMaintenanceWindowTargetsResponse
  where
  rnf DescribeMaintenanceWindowTargetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf httpStatus
