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
-- Module      : Network.AWS.DAX.DescribeParameterGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of parameter group descriptions. If a parameter group
-- name is specified, the list will contain only the descriptions for that
-- group.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeParameterGroups
  ( -- * Creating a Request
    DescribeParameterGroups (..),
    newDescribeParameterGroups,

    -- * Request Lenses
    describeParameterGroups_nextToken,
    describeParameterGroups_maxResults,
    describeParameterGroups_parameterGroupNames,

    -- * Destructuring the Response
    DescribeParameterGroupsResponse (..),
    newDescribeParameterGroupsResponse,

    -- * Response Lenses
    describeParameterGroupsResponse_nextToken,
    describeParameterGroupsResponse_parameterGroups,
    describeParameterGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeParameterGroups' smart constructor.
data DescribeParameterGroups = DescribeParameterGroups'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Core.Maybe Core.Int,
    -- | The names of the parameter groups.
    parameterGroupNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeParameterGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeParameterGroups_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'maxResults', 'describeParameterGroups_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- 'parameterGroupNames', 'describeParameterGroups_parameterGroupNames' - The names of the parameter groups.
newDescribeParameterGroups ::
  DescribeParameterGroups
newDescribeParameterGroups =
  DescribeParameterGroups'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      parameterGroupNames = Core.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
describeParameterGroups_nextToken :: Lens.Lens' DescribeParameterGroups (Core.Maybe Core.Text)
describeParameterGroups_nextToken = Lens.lens (\DescribeParameterGroups' {nextToken} -> nextToken) (\s@DescribeParameterGroups' {} a -> s {nextToken = a} :: DescribeParameterGroups)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
describeParameterGroups_maxResults :: Lens.Lens' DescribeParameterGroups (Core.Maybe Core.Int)
describeParameterGroups_maxResults = Lens.lens (\DescribeParameterGroups' {maxResults} -> maxResults) (\s@DescribeParameterGroups' {} a -> s {maxResults = a} :: DescribeParameterGroups)

-- | The names of the parameter groups.
describeParameterGroups_parameterGroupNames :: Lens.Lens' DescribeParameterGroups (Core.Maybe [Core.Text])
describeParameterGroups_parameterGroupNames = Lens.lens (\DescribeParameterGroups' {parameterGroupNames} -> parameterGroupNames) (\s@DescribeParameterGroups' {} a -> s {parameterGroupNames = a} :: DescribeParameterGroups) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeParameterGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeParameterGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeParameterGroupsResponse_parameterGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeParameterGroups_nextToken
          Lens..~ rs
          Lens.^? describeParameterGroupsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeParameterGroups where
  type
    AWSResponse DescribeParameterGroups =
      DescribeParameterGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeParameterGroupsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ParameterGroups" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeParameterGroups

instance Core.NFData DescribeParameterGroups

instance Core.ToHeaders DescribeParameterGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.DescribeParameterGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeParameterGroups where
  toJSON DescribeParameterGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ParameterGroupNames" Core..=)
              Core.<$> parameterGroupNames
          ]
      )

instance Core.ToPath DescribeParameterGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeParameterGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeParameterGroupsResponse' smart constructor.
data DescribeParameterGroupsResponse = DescribeParameterGroupsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of parameter groups. Each element in the array represents one
    -- parameter group.
    parameterGroups :: Core.Maybe [ParameterGroup],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeParameterGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeParameterGroupsResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'parameterGroups', 'describeParameterGroupsResponse_parameterGroups' - An array of parameter groups. Each element in the array represents one
-- parameter group.
--
-- 'httpStatus', 'describeParameterGroupsResponse_httpStatus' - The response's http status code.
newDescribeParameterGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeParameterGroupsResponse
newDescribeParameterGroupsResponse pHttpStatus_ =
  DescribeParameterGroupsResponse'
    { nextToken =
        Core.Nothing,
      parameterGroups = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeParameterGroupsResponse_nextToken :: Lens.Lens' DescribeParameterGroupsResponse (Core.Maybe Core.Text)
describeParameterGroupsResponse_nextToken = Lens.lens (\DescribeParameterGroupsResponse' {nextToken} -> nextToken) (\s@DescribeParameterGroupsResponse' {} a -> s {nextToken = a} :: DescribeParameterGroupsResponse)

-- | An array of parameter groups. Each element in the array represents one
-- parameter group.
describeParameterGroupsResponse_parameterGroups :: Lens.Lens' DescribeParameterGroupsResponse (Core.Maybe [ParameterGroup])
describeParameterGroupsResponse_parameterGroups = Lens.lens (\DescribeParameterGroupsResponse' {parameterGroups} -> parameterGroups) (\s@DescribeParameterGroupsResponse' {} a -> s {parameterGroups = a} :: DescribeParameterGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeParameterGroupsResponse_httpStatus :: Lens.Lens' DescribeParameterGroupsResponse Core.Int
describeParameterGroupsResponse_httpStatus = Lens.lens (\DescribeParameterGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeParameterGroupsResponse' {} a -> s {httpStatus = a} :: DescribeParameterGroupsResponse)

instance Core.NFData DescribeParameterGroupsResponse
