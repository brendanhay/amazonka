{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeParameterGroups' smart constructor.
data DescribeParameterGroups = DescribeParameterGroups'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The names of the parameter groups.
    parameterGroupNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      parameterGroupNames = Prelude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
describeParameterGroups_nextToken :: Lens.Lens' DescribeParameterGroups (Prelude.Maybe Prelude.Text)
describeParameterGroups_nextToken = Lens.lens (\DescribeParameterGroups' {nextToken} -> nextToken) (\s@DescribeParameterGroups' {} a -> s {nextToken = a} :: DescribeParameterGroups)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
describeParameterGroups_maxResults :: Lens.Lens' DescribeParameterGroups (Prelude.Maybe Prelude.Int)
describeParameterGroups_maxResults = Lens.lens (\DescribeParameterGroups' {maxResults} -> maxResults) (\s@DescribeParameterGroups' {} a -> s {maxResults = a} :: DescribeParameterGroups)

-- | The names of the parameter groups.
describeParameterGroups_parameterGroupNames :: Lens.Lens' DescribeParameterGroups (Prelude.Maybe [Prelude.Text])
describeParameterGroups_parameterGroupNames = Lens.lens (\DescribeParameterGroups' {parameterGroupNames} -> parameterGroupNames) (\s@DescribeParameterGroups' {} a -> s {parameterGroupNames = a} :: DescribeParameterGroups) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager DescribeParameterGroups where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeParameterGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeParameterGroupsResponse_parameterGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeParameterGroups_nextToken
          Lens..~ rs
          Lens.^? describeParameterGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeParameterGroups where
  type
    Rs DescribeParameterGroups =
      DescribeParameterGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeParameterGroupsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ParameterGroups"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeParameterGroups

instance Prelude.NFData DescribeParameterGroups

instance Prelude.ToHeaders DescribeParameterGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDAXV3.DescribeParameterGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeParameterGroups where
  toJSON DescribeParameterGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("ParameterGroupNames" Prelude..=)
              Prelude.<$> parameterGroupNames
          ]
      )

instance Prelude.ToPath DescribeParameterGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeParameterGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeParameterGroupsResponse' smart constructor.
data DescribeParameterGroupsResponse = DescribeParameterGroupsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of parameter groups. Each element in the array represents one
    -- parameter group.
    parameterGroups :: Prelude.Maybe [ParameterGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeParameterGroupsResponse
newDescribeParameterGroupsResponse pHttpStatus_ =
  DescribeParameterGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      parameterGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeParameterGroupsResponse_nextToken :: Lens.Lens' DescribeParameterGroupsResponse (Prelude.Maybe Prelude.Text)
describeParameterGroupsResponse_nextToken = Lens.lens (\DescribeParameterGroupsResponse' {nextToken} -> nextToken) (\s@DescribeParameterGroupsResponse' {} a -> s {nextToken = a} :: DescribeParameterGroupsResponse)

-- | An array of parameter groups. Each element in the array represents one
-- parameter group.
describeParameterGroupsResponse_parameterGroups :: Lens.Lens' DescribeParameterGroupsResponse (Prelude.Maybe [ParameterGroup])
describeParameterGroupsResponse_parameterGroups = Lens.lens (\DescribeParameterGroupsResponse' {parameterGroups} -> parameterGroups) (\s@DescribeParameterGroupsResponse' {} a -> s {parameterGroups = a} :: DescribeParameterGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeParameterGroupsResponse_httpStatus :: Lens.Lens' DescribeParameterGroupsResponse Prelude.Int
describeParameterGroupsResponse_httpStatus = Lens.lens (\DescribeParameterGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeParameterGroupsResponse' {} a -> s {httpStatus = a} :: DescribeParameterGroupsResponse)

instance
  Prelude.NFData
    DescribeParameterGroupsResponse
