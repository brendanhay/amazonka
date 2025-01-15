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
-- Module      : Amazonka.SSM.DescribeMaintenanceWindowExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the executions of a maintenance window. This includes information
-- about when the maintenance window was scheduled to be active, and
-- information about tasks registered and run with the maintenance window.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeMaintenanceWindowExecutions
  ( -- * Creating a Request
    DescribeMaintenanceWindowExecutions (..),
    newDescribeMaintenanceWindowExecutions,

    -- * Request Lenses
    describeMaintenanceWindowExecutions_filters,
    describeMaintenanceWindowExecutions_maxResults,
    describeMaintenanceWindowExecutions_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowExecutions' smart constructor.
data DescribeMaintenanceWindowExecutions = DescribeMaintenanceWindowExecutions'
  { -- | Each entry in the array is a structure containing:
    --
    -- -   Key. A string between 1 and 128 characters. Supported keys include
    --     @ExecutedBefore@ and @ExecutedAfter@.
    --
    -- -   Values. An array of strings, each between 1 and 256 characters.
    --     Supported values are date\/time strings in a valid ISO 8601
    --     date\/time format, such as @2021-11-04T05:00:00Z@.
    filters :: Prelude.Maybe [MaintenanceWindowFilter],
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window whose executions should be retrieved.
    windowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeMaintenanceWindowExecutions_filters' - Each entry in the array is a structure containing:
--
-- -   Key. A string between 1 and 128 characters. Supported keys include
--     @ExecutedBefore@ and @ExecutedAfter@.
--
-- -   Values. An array of strings, each between 1 and 256 characters.
--     Supported values are date\/time strings in a valid ISO 8601
--     date\/time format, such as @2021-11-04T05:00:00Z@.
--
-- 'maxResults', 'describeMaintenanceWindowExecutions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'describeMaintenanceWindowExecutions_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'windowId', 'describeMaintenanceWindowExecutions_windowId' - The ID of the maintenance window whose executions should be retrieved.
newDescribeMaintenanceWindowExecutions ::
  -- | 'windowId'
  Prelude.Text ->
  DescribeMaintenanceWindowExecutions
newDescribeMaintenanceWindowExecutions pWindowId_ =
  DescribeMaintenanceWindowExecutions'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      windowId = pWindowId_
    }

-- | Each entry in the array is a structure containing:
--
-- -   Key. A string between 1 and 128 characters. Supported keys include
--     @ExecutedBefore@ and @ExecutedAfter@.
--
-- -   Values. An array of strings, each between 1 and 256 characters.
--     Supported values are date\/time strings in a valid ISO 8601
--     date\/time format, such as @2021-11-04T05:00:00Z@.
describeMaintenanceWindowExecutions_filters :: Lens.Lens' DescribeMaintenanceWindowExecutions (Prelude.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindowExecutions_filters = Lens.lens (\DescribeMaintenanceWindowExecutions' {filters} -> filters) (\s@DescribeMaintenanceWindowExecutions' {} a -> s {filters = a} :: DescribeMaintenanceWindowExecutions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowExecutions_maxResults :: Lens.Lens' DescribeMaintenanceWindowExecutions (Prelude.Maybe Prelude.Natural)
describeMaintenanceWindowExecutions_maxResults = Lens.lens (\DescribeMaintenanceWindowExecutions' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowExecutions' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowExecutions)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowExecutions_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutions (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowExecutions_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutions' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutions' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutions)

-- | The ID of the maintenance window whose executions should be retrieved.
describeMaintenanceWindowExecutions_windowId :: Lens.Lens' DescribeMaintenanceWindowExecutions Prelude.Text
describeMaintenanceWindowExecutions_windowId = Lens.lens (\DescribeMaintenanceWindowExecutions' {windowId} -> windowId) (\s@DescribeMaintenanceWindowExecutions' {} a -> s {windowId = a} :: DescribeMaintenanceWindowExecutions)

instance
  Core.AWSPager
    DescribeMaintenanceWindowExecutions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionsResponse_windowExecutions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeMaintenanceWindowExecutions_nextToken
              Lens..~ rs
              Lens.^? describeMaintenanceWindowExecutionsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowExecutions
  where
  type
    AWSResponse DescribeMaintenanceWindowExecutions =
      DescribeMaintenanceWindowExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowExecutionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "WindowExecutions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMaintenanceWindowExecutions
  where
  hashWithSalt
    _salt
    DescribeMaintenanceWindowExecutions' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` windowId

instance
  Prelude.NFData
    DescribeMaintenanceWindowExecutions
  where
  rnf DescribeMaintenanceWindowExecutions' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf windowId

instance
  Data.ToHeaders
    DescribeMaintenanceWindowExecutions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeMaintenanceWindowExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeMaintenanceWindowExecutions
  where
  toJSON DescribeMaintenanceWindowExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("WindowId" Data..= windowId)
          ]
      )

instance
  Data.ToPath
    DescribeMaintenanceWindowExecutions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeMaintenanceWindowExecutions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMaintenanceWindowExecutionsResponse' smart constructor.
data DescribeMaintenanceWindowExecutionsResponse = DescribeMaintenanceWindowExecutionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the maintenance window executions.
    windowExecutions :: Prelude.Maybe [MaintenanceWindowExecution],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeMaintenanceWindowExecutionsResponse
newDescribeMaintenanceWindowExecutionsResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowExecutionsResponse'
      { nextToken =
          Prelude.Nothing,
        windowExecutions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowExecutionsResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowExecutionsResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutionsResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutionsResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionsResponse)

-- | Information about the maintenance window executions.
describeMaintenanceWindowExecutionsResponse_windowExecutions :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse (Prelude.Maybe [MaintenanceWindowExecution])
describeMaintenanceWindowExecutionsResponse_windowExecutions = Lens.lens (\DescribeMaintenanceWindowExecutionsResponse' {windowExecutions} -> windowExecutions) (\s@DescribeMaintenanceWindowExecutionsResponse' {} a -> s {windowExecutions = a} :: DescribeMaintenanceWindowExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMaintenanceWindowExecutionsResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse Prelude.Int
describeMaintenanceWindowExecutionsResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowExecutionsResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowExecutionsResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowExecutionsResponse)

instance
  Prelude.NFData
    DescribeMaintenanceWindowExecutionsResponse
  where
  rnf DescribeMaintenanceWindowExecutionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf windowExecutions `Prelude.seq`
        Prelude.rnf httpStatus
