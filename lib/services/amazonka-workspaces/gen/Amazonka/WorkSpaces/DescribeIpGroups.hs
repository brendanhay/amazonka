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
-- Module      : Amazonka.WorkSpaces.DescribeIpGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your IP access control groups.
--
-- This operation returns paginated results.
module Amazonka.WorkSpaces.DescribeIpGroups
  ( -- * Creating a Request
    DescribeIpGroups (..),
    newDescribeIpGroups,

    -- * Request Lenses
    describeIpGroups_groupIds,
    describeIpGroups_maxResults,
    describeIpGroups_nextToken,

    -- * Destructuring the Response
    DescribeIpGroupsResponse (..),
    newDescribeIpGroupsResponse,

    -- * Response Lenses
    describeIpGroupsResponse_nextToken,
    describeIpGroupsResponse_result,
    describeIpGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDescribeIpGroups' smart constructor.
data DescribeIpGroups = DescribeIpGroups'
  { -- | The identifiers of one or more IP access control groups.
    groupIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupIds', 'describeIpGroups_groupIds' - The identifiers of one or more IP access control groups.
--
-- 'maxResults', 'describeIpGroups_maxResults' - The maximum number of items to return.
--
-- 'nextToken', 'describeIpGroups_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
newDescribeIpGroups ::
  DescribeIpGroups
newDescribeIpGroups =
  DescribeIpGroups'
    { groupIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The identifiers of one or more IP access control groups.
describeIpGroups_groupIds :: Lens.Lens' DescribeIpGroups (Prelude.Maybe [Prelude.Text])
describeIpGroups_groupIds = Lens.lens (\DescribeIpGroups' {groupIds} -> groupIds) (\s@DescribeIpGroups' {} a -> s {groupIds = a} :: DescribeIpGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return.
describeIpGroups_maxResults :: Lens.Lens' DescribeIpGroups (Prelude.Maybe Prelude.Natural)
describeIpGroups_maxResults = Lens.lens (\DescribeIpGroups' {maxResults} -> maxResults) (\s@DescribeIpGroups' {} a -> s {maxResults = a} :: DescribeIpGroups)

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeIpGroups_nextToken :: Lens.Lens' DescribeIpGroups (Prelude.Maybe Prelude.Text)
describeIpGroups_nextToken = Lens.lens (\DescribeIpGroups' {nextToken} -> nextToken) (\s@DescribeIpGroups' {} a -> s {nextToken = a} :: DescribeIpGroups)

instance Core.AWSPager DescribeIpGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIpGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIpGroupsResponse_result
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeIpGroups_nextToken
              Lens..~ rs
              Lens.^? describeIpGroupsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeIpGroups where
  type
    AWSResponse DescribeIpGroups =
      DescribeIpGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIpGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Result" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIpGroups where
  hashWithSalt _salt DescribeIpGroups' {..} =
    _salt
      `Prelude.hashWithSalt` groupIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeIpGroups where
  rnf DescribeIpGroups' {..} =
    Prelude.rnf groupIds `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders DescribeIpGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeIpGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeIpGroups where
  toJSON DescribeIpGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupIds" Data..=) Prelude.<$> groupIds,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeIpGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIpGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIpGroupsResponse' smart constructor.
data DescribeIpGroupsResponse = DescribeIpGroupsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the IP access control groups.
    result :: Prelude.Maybe [WorkspacesIpGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeIpGroupsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'result', 'describeIpGroupsResponse_result' - Information about the IP access control groups.
--
-- 'httpStatus', 'describeIpGroupsResponse_httpStatus' - The response's http status code.
newDescribeIpGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIpGroupsResponse
newDescribeIpGroupsResponse pHttpStatus_ =
  DescribeIpGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      result = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeIpGroupsResponse_nextToken :: Lens.Lens' DescribeIpGroupsResponse (Prelude.Maybe Prelude.Text)
describeIpGroupsResponse_nextToken = Lens.lens (\DescribeIpGroupsResponse' {nextToken} -> nextToken) (\s@DescribeIpGroupsResponse' {} a -> s {nextToken = a} :: DescribeIpGroupsResponse)

-- | Information about the IP access control groups.
describeIpGroupsResponse_result :: Lens.Lens' DescribeIpGroupsResponse (Prelude.Maybe [WorkspacesIpGroup])
describeIpGroupsResponse_result = Lens.lens (\DescribeIpGroupsResponse' {result} -> result) (\s@DescribeIpGroupsResponse' {} a -> s {result = a} :: DescribeIpGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeIpGroupsResponse_httpStatus :: Lens.Lens' DescribeIpGroupsResponse Prelude.Int
describeIpGroupsResponse_httpStatus = Lens.lens (\DescribeIpGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeIpGroupsResponse' {} a -> s {httpStatus = a} :: DescribeIpGroupsResponse)

instance Prelude.NFData DescribeIpGroupsResponse where
  rnf DescribeIpGroupsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf result `Prelude.seq`
        Prelude.rnf httpStatus
