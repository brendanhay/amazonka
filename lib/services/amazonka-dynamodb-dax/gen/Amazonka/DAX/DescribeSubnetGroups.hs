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
-- Module      : Amazonka.DAX.DescribeSubnetGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of subnet group descriptions. If a subnet group name is
-- specified, the list will contain only the description of that group.
--
-- This operation returns paginated results.
module Amazonka.DAX.DescribeSubnetGroups
  ( -- * Creating a Request
    DescribeSubnetGroups (..),
    newDescribeSubnetGroups,

    -- * Request Lenses
    describeSubnetGroups_maxResults,
    describeSubnetGroups_nextToken,
    describeSubnetGroups_subnetGroupNames,

    -- * Destructuring the Response
    DescribeSubnetGroupsResponse (..),
    newDescribeSubnetGroupsResponse,

    -- * Response Lenses
    describeSubnetGroupsResponse_nextToken,
    describeSubnetGroupsResponse_subnetGroups,
    describeSubnetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSubnetGroups' smart constructor.
data DescribeSubnetGroups = DescribeSubnetGroups'
  { -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'describeSubnetGroups_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- 'nextToken', 'describeSubnetGroups_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'subnetGroupNames', 'describeSubnetGroups_subnetGroupNames' - The name of the subnet group.
newDescribeSubnetGroups ::
  DescribeSubnetGroups
newDescribeSubnetGroups =
  DescribeSubnetGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      subnetGroupNames = Prelude.Nothing
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
describeSubnetGroups_maxResults :: Lens.Lens' DescribeSubnetGroups (Prelude.Maybe Prelude.Int)
describeSubnetGroups_maxResults = Lens.lens (\DescribeSubnetGroups' {maxResults} -> maxResults) (\s@DescribeSubnetGroups' {} a -> s {maxResults = a} :: DescribeSubnetGroups)

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
describeSubnetGroups_nextToken :: Lens.Lens' DescribeSubnetGroups (Prelude.Maybe Prelude.Text)
describeSubnetGroups_nextToken = Lens.lens (\DescribeSubnetGroups' {nextToken} -> nextToken) (\s@DescribeSubnetGroups' {} a -> s {nextToken = a} :: DescribeSubnetGroups)

-- | The name of the subnet group.
describeSubnetGroups_subnetGroupNames :: Lens.Lens' DescribeSubnetGroups (Prelude.Maybe [Prelude.Text])
describeSubnetGroups_subnetGroupNames = Lens.lens (\DescribeSubnetGroups' {subnetGroupNames} -> subnetGroupNames) (\s@DescribeSubnetGroups' {} a -> s {subnetGroupNames = a} :: DescribeSubnetGroups) Prelude.. Lens.mapping Lens.coerced

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeSubnetGroups_nextToken
          Lens..~ rs
          Lens.^? describeSubnetGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeSubnetGroups where
  type
    AWSResponse DescribeSubnetGroups =
      DescribeSubnetGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubnetGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SubnetGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSubnetGroups where
  hashWithSalt _salt DescribeSubnetGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` subnetGroupNames

instance Prelude.NFData DescribeSubnetGroups where
  rnf DescribeSubnetGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subnetGroupNames

instance Data.ToHeaders DescribeSubnetGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDAXV3.DescribeSubnetGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSubnetGroups where
  toJSON DescribeSubnetGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SubnetGroupNames" Data..=)
              Prelude.<$> subnetGroupNames
          ]
      )

instance Data.ToPath DescribeSubnetGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSubnetGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSubnetGroupsResponse' smart constructor.
data DescribeSubnetGroupsResponse = DescribeSubnetGroupsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of subnet groups. Each element in the array represents a single
    -- subnet group.
    subnetGroups :: Prelude.Maybe [SubnetGroup],
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
-- 'nextToken', 'describeSubnetGroupsResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'subnetGroups', 'describeSubnetGroupsResponse_subnetGroups' - An array of subnet groups. Each element in the array represents a single
-- subnet group.
--
-- 'httpStatus', 'describeSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeSubnetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSubnetGroupsResponse
newDescribeSubnetGroupsResponse pHttpStatus_ =
  DescribeSubnetGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      subnetGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeSubnetGroupsResponse_nextToken :: Lens.Lens' DescribeSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeSubnetGroupsResponse_nextToken = Lens.lens (\DescribeSubnetGroupsResponse' {nextToken} -> nextToken) (\s@DescribeSubnetGroupsResponse' {} a -> s {nextToken = a} :: DescribeSubnetGroupsResponse)

-- | An array of subnet groups. Each element in the array represents a single
-- subnet group.
describeSubnetGroupsResponse_subnetGroups :: Lens.Lens' DescribeSubnetGroupsResponse (Prelude.Maybe [SubnetGroup])
describeSubnetGroupsResponse_subnetGroups = Lens.lens (\DescribeSubnetGroupsResponse' {subnetGroups} -> subnetGroups) (\s@DescribeSubnetGroupsResponse' {} a -> s {subnetGroups = a} :: DescribeSubnetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeSubnetGroupsResponse Prelude.Int
describeSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeSubnetGroupsResponse)

instance Prelude.NFData DescribeSubnetGroupsResponse where
  rnf DescribeSubnetGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subnetGroups
      `Prelude.seq` Prelude.rnf httpStatus
