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
-- Module      : Amazonka.MemoryDb.DescribeACLs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ACLs
module Amazonka.MemoryDb.DescribeACLs
  ( -- * Creating a Request
    DescribeACLs (..),
    newDescribeACLs,

    -- * Request Lenses
    describeACLs_nextToken,
    describeACLs_aCLName,
    describeACLs_maxResults,

    -- * Destructuring the Response
    DescribeACLsResponse (..),
    newDescribeACLsResponse,

    -- * Response Lenses
    describeACLsResponse_nextToken,
    describeACLsResponse_aCLs,
    describeACLsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeACLs' smart constructor.
data DescribeACLs = DescribeACLs'
  { -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the ACL
    aCLName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeACLs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeACLs_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'aCLName', 'describeACLs_aCLName' - The name of the ACL
--
-- 'maxResults', 'describeACLs_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
newDescribeACLs ::
  DescribeACLs
newDescribeACLs =
  DescribeACLs'
    { nextToken = Prelude.Nothing,
      aCLName = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeACLs_nextToken :: Lens.Lens' DescribeACLs (Prelude.Maybe Prelude.Text)
describeACLs_nextToken = Lens.lens (\DescribeACLs' {nextToken} -> nextToken) (\s@DescribeACLs' {} a -> s {nextToken = a} :: DescribeACLs)

-- | The name of the ACL
describeACLs_aCLName :: Lens.Lens' DescribeACLs (Prelude.Maybe Prelude.Text)
describeACLs_aCLName = Lens.lens (\DescribeACLs' {aCLName} -> aCLName) (\s@DescribeACLs' {} a -> s {aCLName = a} :: DescribeACLs)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
describeACLs_maxResults :: Lens.Lens' DescribeACLs (Prelude.Maybe Prelude.Int)
describeACLs_maxResults = Lens.lens (\DescribeACLs' {maxResults} -> maxResults) (\s@DescribeACLs' {} a -> s {maxResults = a} :: DescribeACLs)

instance Core.AWSRequest DescribeACLs where
  type AWSResponse DescribeACLs = DescribeACLsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeACLsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ACLs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeACLs where
  hashWithSalt _salt DescribeACLs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` aCLName
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeACLs where
  rnf DescribeACLs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf aCLName
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeACLs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.DescribeACLs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeACLs where
  toJSON DescribeACLs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ACLName" Data..=) Prelude.<$> aCLName,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeACLs where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeACLs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeACLsResponse' smart constructor.
data DescribeACLsResponse = DescribeACLsResponse'
  { -- | If nextToken is returned, there are more results available. The value of
    -- nextToken is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of ACLs
    aCLs :: Prelude.Maybe [ACL],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeACLsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeACLsResponse_nextToken' - If nextToken is returned, there are more results available. The value of
-- nextToken is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
--
-- 'aCLs', 'describeACLsResponse_aCLs' - The list of ACLs
--
-- 'httpStatus', 'describeACLsResponse_httpStatus' - The response's http status code.
newDescribeACLsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeACLsResponse
newDescribeACLsResponse pHttpStatus_ =
  DescribeACLsResponse'
    { nextToken = Prelude.Nothing,
      aCLs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If nextToken is returned, there are more results available. The value of
-- nextToken is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged.
describeACLsResponse_nextToken :: Lens.Lens' DescribeACLsResponse (Prelude.Maybe Prelude.Text)
describeACLsResponse_nextToken = Lens.lens (\DescribeACLsResponse' {nextToken} -> nextToken) (\s@DescribeACLsResponse' {} a -> s {nextToken = a} :: DescribeACLsResponse)

-- | The list of ACLs
describeACLsResponse_aCLs :: Lens.Lens' DescribeACLsResponse (Prelude.Maybe [ACL])
describeACLsResponse_aCLs = Lens.lens (\DescribeACLsResponse' {aCLs} -> aCLs) (\s@DescribeACLsResponse' {} a -> s {aCLs = a} :: DescribeACLsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeACLsResponse_httpStatus :: Lens.Lens' DescribeACLsResponse Prelude.Int
describeACLsResponse_httpStatus = Lens.lens (\DescribeACLsResponse' {httpStatus} -> httpStatus) (\s@DescribeACLsResponse' {} a -> s {httpStatus = a} :: DescribeACLsResponse)

instance Prelude.NFData DescribeACLsResponse where
  rnf DescribeACLsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf aCLs
      `Prelude.seq` Prelude.rnf httpStatus
