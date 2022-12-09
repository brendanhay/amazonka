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
-- Module      : Amazonka.MemoryDb.DescribeSubnetGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of subnet group descriptions. If a subnet group name is
-- specified, the list contains only the description of that group.
module Amazonka.MemoryDb.DescribeSubnetGroups
  ( -- * Creating a Request
    DescribeSubnetGroups (..),
    newDescribeSubnetGroups,

    -- * Request Lenses
    describeSubnetGroups_maxResults,
    describeSubnetGroups_nextToken,
    describeSubnetGroups_subnetGroupName,

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
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSubnetGroups' smart constructor.
data DescribeSubnetGroups = DescribeSubnetGroups'
  { -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the subnet group to return details for.
    subnetGroupName :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'describeSubnetGroups_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'describeSubnetGroups_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'subnetGroupName', 'describeSubnetGroups_subnetGroupName' - The name of the subnet group to return details for.
newDescribeSubnetGroups ::
  DescribeSubnetGroups
newDescribeSubnetGroups =
  DescribeSubnetGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      subnetGroupName = Prelude.Nothing
    }

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
describeSubnetGroups_maxResults :: Lens.Lens' DescribeSubnetGroups (Prelude.Maybe Prelude.Int)
describeSubnetGroups_maxResults = Lens.lens (\DescribeSubnetGroups' {maxResults} -> maxResults) (\s@DescribeSubnetGroups' {} a -> s {maxResults = a} :: DescribeSubnetGroups)

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeSubnetGroups_nextToken :: Lens.Lens' DescribeSubnetGroups (Prelude.Maybe Prelude.Text)
describeSubnetGroups_nextToken = Lens.lens (\DescribeSubnetGroups' {nextToken} -> nextToken) (\s@DescribeSubnetGroups' {} a -> s {nextToken = a} :: DescribeSubnetGroups)

-- | The name of the subnet group to return details for.
describeSubnetGroups_subnetGroupName :: Lens.Lens' DescribeSubnetGroups (Prelude.Maybe Prelude.Text)
describeSubnetGroups_subnetGroupName = Lens.lens (\DescribeSubnetGroups' {subnetGroupName} -> subnetGroupName) (\s@DescribeSubnetGroups' {} a -> s {subnetGroupName = a} :: DescribeSubnetGroups)

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
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` subnetGroupName

instance Prelude.NFData DescribeSubnetGroups where
  rnf DescribeSubnetGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subnetGroupName

instance Data.ToHeaders DescribeSubnetGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.DescribeSubnetGroups" ::
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
            ("SubnetGroupName" Data..=)
              Prelude.<$> subnetGroupName
          ]
      )

instance Data.ToPath DescribeSubnetGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSubnetGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSubnetGroupsResponse' smart constructor.
data DescribeSubnetGroupsResponse = DescribeSubnetGroupsResponse'
  { -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of subnet groups. Each element in the list contains detailed
    -- information about one group.
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
-- 'nextToken', 'describeSubnetGroupsResponse_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'subnetGroups', 'describeSubnetGroupsResponse_subnetGroups' - A list of subnet groups. Each element in the list contains detailed
-- information about one group.
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

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeSubnetGroupsResponse_nextToken :: Lens.Lens' DescribeSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeSubnetGroupsResponse_nextToken = Lens.lens (\DescribeSubnetGroupsResponse' {nextToken} -> nextToken) (\s@DescribeSubnetGroupsResponse' {} a -> s {nextToken = a} :: DescribeSubnetGroupsResponse)

-- | A list of subnet groups. Each element in the list contains detailed
-- information about one group.
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
