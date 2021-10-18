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
-- Module      : Network.AWS.DAX.DescribeSubnetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of subnet group descriptions. If a subnet group name is
-- specified, the list will contain only the description of that group.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeSubnetGroups
  ( -- * Creating a Request
    DescribeSubnetGroups (..),
    newDescribeSubnetGroups,

    -- * Request Lenses
    describeSubnetGroups_nextToken,
    describeSubnetGroups_maxResults,
    describeSubnetGroups_subnetGroupNames,

    -- * Destructuring the Response
    DescribeSubnetGroupsResponse (..),
    newDescribeSubnetGroupsResponse,

    -- * Response Lenses
    describeSubnetGroupsResponse_subnetGroups,
    describeSubnetGroupsResponse_nextToken,
    describeSubnetGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSubnetGroups' smart constructor.
data DescribeSubnetGroups = DescribeSubnetGroups'
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
    -- | The name of the subnet group.
    subnetGroupNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSubnetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSubnetGroups_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'maxResults', 'describeSubnetGroups_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- 'subnetGroupNames', 'describeSubnetGroups_subnetGroupNames' - The name of the subnet group.
newDescribeSubnetGroups ::
  DescribeSubnetGroups
newDescribeSubnetGroups =
  DescribeSubnetGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      subnetGroupNames = Prelude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
describeSubnetGroups_nextToken :: Lens.Lens' DescribeSubnetGroups (Prelude.Maybe Prelude.Text)
describeSubnetGroups_nextToken = Lens.lens (\DescribeSubnetGroups' {nextToken} -> nextToken) (\s@DescribeSubnetGroups' {} a -> s {nextToken = a} :: DescribeSubnetGroups)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
describeSubnetGroups_maxResults :: Lens.Lens' DescribeSubnetGroups (Prelude.Maybe Prelude.Int)
describeSubnetGroups_maxResults = Lens.lens (\DescribeSubnetGroups' {maxResults} -> maxResults) (\s@DescribeSubnetGroups' {} a -> s {maxResults = a} :: DescribeSubnetGroups)

-- | The name of the subnet group.
describeSubnetGroups_subnetGroupNames :: Lens.Lens' DescribeSubnetGroups (Prelude.Maybe [Prelude.Text])
describeSubnetGroups_subnetGroupNames = Lens.lens (\DescribeSubnetGroups' {subnetGroupNames} -> subnetGroupNames) (\s@DescribeSubnetGroups' {} a -> s {subnetGroupNames = a} :: DescribeSubnetGroups) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeSubnetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSubnetGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSubnetGroupsResponse_subnetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSubnetGroups_nextToken
          Lens..~ rs
          Lens.^? describeSubnetGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSubnetGroups where
  type
    AWSResponse DescribeSubnetGroups =
      DescribeSubnetGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubnetGroupsResponse'
            Prelude.<$> (x Core..?> "SubnetGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSubnetGroups

instance Prelude.NFData DescribeSubnetGroups

instance Core.ToHeaders DescribeSubnetGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.DescribeSubnetGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSubnetGroups where
  toJSON DescribeSubnetGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SubnetGroupNames" Core..=)
              Prelude.<$> subnetGroupNames
          ]
      )

instance Core.ToPath DescribeSubnetGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSubnetGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSubnetGroupsResponse' smart constructor.
data DescribeSubnetGroupsResponse = DescribeSubnetGroupsResponse'
  { -- | An array of subnet groups. Each element in the array represents a single
    -- subnet group.
    subnetGroups :: Prelude.Maybe [SubnetGroup],
    -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetGroups', 'describeSubnetGroupsResponse_subnetGroups' - An array of subnet groups. Each element in the array represents a single
-- subnet group.
--
-- 'nextToken', 'describeSubnetGroupsResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeSubnetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSubnetGroupsResponse
newDescribeSubnetGroupsResponse pHttpStatus_ =
  DescribeSubnetGroupsResponse'
    { subnetGroups =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of subnet groups. Each element in the array represents a single
-- subnet group.
describeSubnetGroupsResponse_subnetGroups :: Lens.Lens' DescribeSubnetGroupsResponse (Prelude.Maybe [SubnetGroup])
describeSubnetGroupsResponse_subnetGroups = Lens.lens (\DescribeSubnetGroupsResponse' {subnetGroups} -> subnetGroups) (\s@DescribeSubnetGroupsResponse' {} a -> s {subnetGroups = a} :: DescribeSubnetGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
describeSubnetGroupsResponse_nextToken :: Lens.Lens' DescribeSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeSubnetGroupsResponse_nextToken = Lens.lens (\DescribeSubnetGroupsResponse' {nextToken} -> nextToken) (\s@DescribeSubnetGroupsResponse' {} a -> s {nextToken = a} :: DescribeSubnetGroupsResponse)

-- | The response's http status code.
describeSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeSubnetGroupsResponse Prelude.Int
describeSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeSubnetGroupsResponse)

instance Prelude.NFData DescribeSubnetGroupsResponse
