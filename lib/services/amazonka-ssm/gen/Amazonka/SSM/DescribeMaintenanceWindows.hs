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
-- Module      : Amazonka.SSM.DescribeMaintenanceWindows
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the maintenance windows in an Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeMaintenanceWindows
  ( -- * Creating a Request
    DescribeMaintenanceWindows (..),
    newDescribeMaintenanceWindows,

    -- * Request Lenses
    describeMaintenanceWindows_filters,
    describeMaintenanceWindows_maxResults,
    describeMaintenanceWindows_nextToken,

    -- * Destructuring the Response
    DescribeMaintenanceWindowsResponse (..),
    newDescribeMaintenanceWindowsResponse,

    -- * Response Lenses
    describeMaintenanceWindowsResponse_nextToken,
    describeMaintenanceWindowsResponse_windowIdentities,
    describeMaintenanceWindowsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindows' smart constructor.
data DescribeMaintenanceWindows = DescribeMaintenanceWindows'
  { -- | Optional filters used to narrow down the scope of the returned
    -- maintenance windows. Supported filter keys are @Name@ and @Enabled@. For
    -- example, @Name=MyMaintenanceWindow@ and @Enabled=True@.
    filters :: Prelude.Maybe [MaintenanceWindowFilter],
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filters', 'describeMaintenanceWindows_filters' - Optional filters used to narrow down the scope of the returned
-- maintenance windows. Supported filter keys are @Name@ and @Enabled@. For
-- example, @Name=MyMaintenanceWindow@ and @Enabled=True@.
--
-- 'maxResults', 'describeMaintenanceWindows_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'describeMaintenanceWindows_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
newDescribeMaintenanceWindows ::
  DescribeMaintenanceWindows
newDescribeMaintenanceWindows =
  DescribeMaintenanceWindows'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Optional filters used to narrow down the scope of the returned
-- maintenance windows. Supported filter keys are @Name@ and @Enabled@. For
-- example, @Name=MyMaintenanceWindow@ and @Enabled=True@.
describeMaintenanceWindows_filters :: Lens.Lens' DescribeMaintenanceWindows (Prelude.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindows_filters = Lens.lens (\DescribeMaintenanceWindows' {filters} -> filters) (\s@DescribeMaintenanceWindows' {} a -> s {filters = a} :: DescribeMaintenanceWindows) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindows_maxResults :: Lens.Lens' DescribeMaintenanceWindows (Prelude.Maybe Prelude.Natural)
describeMaintenanceWindows_maxResults = Lens.lens (\DescribeMaintenanceWindows' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindows' {} a -> s {maxResults = a} :: DescribeMaintenanceWindows)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindows_nextToken :: Lens.Lens' DescribeMaintenanceWindows (Prelude.Maybe Prelude.Text)
describeMaintenanceWindows_nextToken = Lens.lens (\DescribeMaintenanceWindows' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindows' {} a -> s {nextToken = a} :: DescribeMaintenanceWindows)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeMaintenanceWindows_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeMaintenanceWindows where
  type
    AWSResponse DescribeMaintenanceWindows =
      DescribeMaintenanceWindowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "WindowIdentities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMaintenanceWindows where
  hashWithSalt _salt DescribeMaintenanceWindows' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeMaintenanceWindows where
  rnf DescribeMaintenanceWindows' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeMaintenanceWindows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeMaintenanceWindows" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMaintenanceWindows where
  toJSON DescribeMaintenanceWindows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeMaintenanceWindows where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMaintenanceWindows where
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
describeMaintenanceWindowsResponse_windowIdentities = Lens.lens (\DescribeMaintenanceWindowsResponse' {windowIdentities} -> windowIdentities) (\s@DescribeMaintenanceWindowsResponse' {} a -> s {windowIdentities = a} :: DescribeMaintenanceWindowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMaintenanceWindowsResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowsResponse Prelude.Int
describeMaintenanceWindowsResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowsResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowsResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowsResponse)

instance
  Prelude.NFData
    DescribeMaintenanceWindowsResponse
  where
  rnf DescribeMaintenanceWindowsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf windowIdentities
      `Prelude.seq` Prelude.rnf httpStatus
