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
-- Module      : Amazonka.Synthetics.DescribeCanaries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a list of the canaries in your account, along
-- with full details about each canary.
--
-- This operation does not have resource-level authorization, so if a user
-- is able to use @DescribeCanaries@, the user can see all of the canaries
-- in the account. A deny policy can only be used to restrict access to all
-- canaries. It cannot be used on specific resources.
module Amazonka.Synthetics.DescribeCanaries
  ( -- * Creating a Request
    DescribeCanaries (..),
    newDescribeCanaries,

    -- * Request Lenses
    describeCanaries_nextToken,
    describeCanaries_maxResults,

    -- * Destructuring the Response
    DescribeCanariesResponse (..),
    newDescribeCanariesResponse,

    -- * Response Lenses
    describeCanariesResponse_canaries,
    describeCanariesResponse_nextToken,
    describeCanariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newDescribeCanaries' smart constructor.
data DescribeCanaries = DescribeCanaries'
  { -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent operation to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specify this parameter to limit how many canaries are returned each time
    -- you use the @DescribeCanaries@ operation. If you omit this parameter,
    -- the default of 100 is used.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCanaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCanaries_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
--
-- 'maxResults', 'describeCanaries_maxResults' - Specify this parameter to limit how many canaries are returned each time
-- you use the @DescribeCanaries@ operation. If you omit this parameter,
-- the default of 100 is used.
newDescribeCanaries ::
  DescribeCanaries
newDescribeCanaries =
  DescribeCanaries'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent operation to retrieve the next set of
-- results.
describeCanaries_nextToken :: Lens.Lens' DescribeCanaries (Prelude.Maybe Prelude.Text)
describeCanaries_nextToken = Lens.lens (\DescribeCanaries' {nextToken} -> nextToken) (\s@DescribeCanaries' {} a -> s {nextToken = a} :: DescribeCanaries)

-- | Specify this parameter to limit how many canaries are returned each time
-- you use the @DescribeCanaries@ operation. If you omit this parameter,
-- the default of 100 is used.
describeCanaries_maxResults :: Lens.Lens' DescribeCanaries (Prelude.Maybe Prelude.Natural)
describeCanaries_maxResults = Lens.lens (\DescribeCanaries' {maxResults} -> maxResults) (\s@DescribeCanaries' {} a -> s {maxResults = a} :: DescribeCanaries)

instance Core.AWSRequest DescribeCanaries where
  type
    AWSResponse DescribeCanaries =
      DescribeCanariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCanariesResponse'
            Prelude.<$> (x Core..?> "Canaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCanaries where
  hashWithSalt salt' DescribeCanaries' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeCanaries where
  rnf DescribeCanaries' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeCanaries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCanaries where
  toJSON DescribeCanaries' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeCanaries where
  toPath = Prelude.const "/canaries"

instance Core.ToQuery DescribeCanaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCanariesResponse' smart constructor.
data DescribeCanariesResponse = DescribeCanariesResponse'
  { -- | Returns an array. Each item in the array contains the full information
    -- about one canary.
    canaries :: Prelude.Maybe [Canary],
    -- | A token that indicates that there is more data available. You can use
    -- this token in a subsequent @DescribeCanaries@ operation to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCanariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canaries', 'describeCanariesResponse_canaries' - Returns an array. Each item in the array contains the full information
-- about one canary.
--
-- 'nextToken', 'describeCanariesResponse_nextToken' - A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeCanaries@ operation to retrieve the
-- next set of results.
--
-- 'httpStatus', 'describeCanariesResponse_httpStatus' - The response's http status code.
newDescribeCanariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCanariesResponse
newDescribeCanariesResponse pHttpStatus_ =
  DescribeCanariesResponse'
    { canaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns an array. Each item in the array contains the full information
-- about one canary.
describeCanariesResponse_canaries :: Lens.Lens' DescribeCanariesResponse (Prelude.Maybe [Canary])
describeCanariesResponse_canaries = Lens.lens (\DescribeCanariesResponse' {canaries} -> canaries) (\s@DescribeCanariesResponse' {} a -> s {canaries = a} :: DescribeCanariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates that there is more data available. You can use
-- this token in a subsequent @DescribeCanaries@ operation to retrieve the
-- next set of results.
describeCanariesResponse_nextToken :: Lens.Lens' DescribeCanariesResponse (Prelude.Maybe Prelude.Text)
describeCanariesResponse_nextToken = Lens.lens (\DescribeCanariesResponse' {nextToken} -> nextToken) (\s@DescribeCanariesResponse' {} a -> s {nextToken = a} :: DescribeCanariesResponse)

-- | The response's http status code.
describeCanariesResponse_httpStatus :: Lens.Lens' DescribeCanariesResponse Prelude.Int
describeCanariesResponse_httpStatus = Lens.lens (\DescribeCanariesResponse' {httpStatus} -> httpStatus) (\s@DescribeCanariesResponse' {} a -> s {httpStatus = a} :: DescribeCanariesResponse)

instance Prelude.NFData DescribeCanariesResponse where
  rnf DescribeCanariesResponse' {..} =
    Prelude.rnf canaries
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
