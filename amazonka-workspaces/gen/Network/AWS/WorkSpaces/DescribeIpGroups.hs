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
-- Module      : Network.AWS.WorkSpaces.DescribeIpGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your IP access control groups.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeIpGroups
  ( -- * Creating a Request
    DescribeIpGroups (..),
    newDescribeIpGroups,

    -- * Request Lenses
    describeIpGroups_nextToken,
    describeIpGroups_groupIds,
    describeIpGroups_maxResults,

    -- * Destructuring the Response
    DescribeIpGroupsResponse (..),
    newDescribeIpGroupsResponse,

    -- * Response Lenses
    describeIpGroupsResponse_nextToken,
    describeIpGroupsResponse_result,
    describeIpGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeIpGroups' smart constructor.
data DescribeIpGroups = DescribeIpGroups'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of one or more IP access control groups.
    groupIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeIpGroups_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'groupIds', 'describeIpGroups_groupIds' - The identifiers of one or more IP access control groups.
--
-- 'maxResults', 'describeIpGroups_maxResults' - The maximum number of items to return.
newDescribeIpGroups ::
  DescribeIpGroups
newDescribeIpGroups =
  DescribeIpGroups'
    { nextToken = Prelude.Nothing,
      groupIds = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeIpGroups_nextToken :: Lens.Lens' DescribeIpGroups (Prelude.Maybe Prelude.Text)
describeIpGroups_nextToken = Lens.lens (\DescribeIpGroups' {nextToken} -> nextToken) (\s@DescribeIpGroups' {} a -> s {nextToken = a} :: DescribeIpGroups)

-- | The identifiers of one or more IP access control groups.
describeIpGroups_groupIds :: Lens.Lens' DescribeIpGroups (Prelude.Maybe [Prelude.Text])
describeIpGroups_groupIds = Lens.lens (\DescribeIpGroups' {groupIds} -> groupIds) (\s@DescribeIpGroups' {} a -> s {groupIds = a} :: DescribeIpGroups) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of items to return.
describeIpGroups_maxResults :: Lens.Lens' DescribeIpGroups (Prelude.Maybe Prelude.Natural)
describeIpGroups_maxResults = Lens.lens (\DescribeIpGroups' {maxResults} -> maxResults) (\s@DescribeIpGroups' {} a -> s {maxResults = a} :: DescribeIpGroups)

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
            Lens.^? describeIpGroupsResponse_result Prelude.. Lens._Just
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIpGroupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Result" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIpGroups

instance Prelude.NFData DescribeIpGroups

instance Core.ToHeaders DescribeIpGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeIpGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeIpGroups where
  toJSON DescribeIpGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("GroupIds" Core..=) Prelude.<$> groupIds,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeIpGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeIpGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIpGroupsResponse' smart constructor.
data DescribeIpGroupsResponse = DescribeIpGroupsResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
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
-- 'nextToken', 'describeIpGroupsResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
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

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeIpGroupsResponse_nextToken :: Lens.Lens' DescribeIpGroupsResponse (Prelude.Maybe Prelude.Text)
describeIpGroupsResponse_nextToken = Lens.lens (\DescribeIpGroupsResponse' {nextToken} -> nextToken) (\s@DescribeIpGroupsResponse' {} a -> s {nextToken = a} :: DescribeIpGroupsResponse)

-- | Information about the IP access control groups.
describeIpGroupsResponse_result :: Lens.Lens' DescribeIpGroupsResponse (Prelude.Maybe [WorkspacesIpGroup])
describeIpGroupsResponse_result = Lens.lens (\DescribeIpGroupsResponse' {result} -> result) (\s@DescribeIpGroupsResponse' {} a -> s {result = a} :: DescribeIpGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeIpGroupsResponse_httpStatus :: Lens.Lens' DescribeIpGroupsResponse Prelude.Int
describeIpGroupsResponse_httpStatus = Lens.lens (\DescribeIpGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeIpGroupsResponse' {} a -> s {httpStatus = a} :: DescribeIpGroupsResponse)

instance Prelude.NFData DescribeIpGroupsResponse
