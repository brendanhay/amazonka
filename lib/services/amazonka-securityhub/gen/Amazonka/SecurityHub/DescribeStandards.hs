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
-- Module      : Amazonka.SecurityHub.DescribeStandards
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available standards in Security Hub.
--
-- For each standard, the results include the standard ARN, the name, and a
-- description.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.DescribeStandards
  ( -- * Creating a Request
    DescribeStandards (..),
    newDescribeStandards,

    -- * Request Lenses
    describeStandards_nextToken,
    describeStandards_maxResults,

    -- * Destructuring the Response
    DescribeStandardsResponse (..),
    newDescribeStandardsResponse,

    -- * Response Lenses
    describeStandardsResponse_nextToken,
    describeStandardsResponse_standards,
    describeStandardsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDescribeStandards' smart constructor.
data DescribeStandards = DescribeStandards'
  { -- | The token that is required for pagination. On your first call to the
    -- @DescribeStandards@ operation, set the value of this parameter to
    -- @NULL@.
    --
    -- For subsequent calls to the operation, to continue listing data, set the
    -- value of this parameter to the value returned from the previous
    -- response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of standards to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStandards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStandards_nextToken' - The token that is required for pagination. On your first call to the
-- @DescribeStandards@ operation, set the value of this parameter to
-- @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
--
-- 'maxResults', 'describeStandards_maxResults' - The maximum number of standards to return.
newDescribeStandards ::
  DescribeStandards
newDescribeStandards =
  DescribeStandards'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token that is required for pagination. On your first call to the
-- @DescribeStandards@ operation, set the value of this parameter to
-- @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
describeStandards_nextToken :: Lens.Lens' DescribeStandards (Prelude.Maybe Prelude.Text)
describeStandards_nextToken = Lens.lens (\DescribeStandards' {nextToken} -> nextToken) (\s@DescribeStandards' {} a -> s {nextToken = a} :: DescribeStandards)

-- | The maximum number of standards to return.
describeStandards_maxResults :: Lens.Lens' DescribeStandards (Prelude.Maybe Prelude.Natural)
describeStandards_maxResults = Lens.lens (\DescribeStandards' {maxResults} -> maxResults) (\s@DescribeStandards' {} a -> s {maxResults = a} :: DescribeStandards)

instance Core.AWSPager DescribeStandards where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStandardsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStandardsResponse_standards
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeStandards_nextToken
          Lens..~ rs
          Lens.^? describeStandardsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeStandards where
  type
    AWSResponse DescribeStandards =
      DescribeStandardsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStandardsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Standards" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStandards where
  hashWithSalt _salt DescribeStandards' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeStandards where
  rnf DescribeStandards' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeStandards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeStandards where
  toPath = Prelude.const "/standards"

instance Data.ToQuery DescribeStandards where
  toQuery DescribeStandards' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newDescribeStandardsResponse' smart constructor.
data DescribeStandardsResponse = DescribeStandardsResponse'
  { -- | The pagination token to use to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of available standards.
    standards :: Prelude.Maybe [Standard],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStandardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStandardsResponse_nextToken' - The pagination token to use to request the next page of results.
--
-- 'standards', 'describeStandardsResponse_standards' - A list of available standards.
--
-- 'httpStatus', 'describeStandardsResponse_httpStatus' - The response's http status code.
newDescribeStandardsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStandardsResponse
newDescribeStandardsResponse pHttpStatus_ =
  DescribeStandardsResponse'
    { nextToken =
        Prelude.Nothing,
      standards = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to request the next page of results.
describeStandardsResponse_nextToken :: Lens.Lens' DescribeStandardsResponse (Prelude.Maybe Prelude.Text)
describeStandardsResponse_nextToken = Lens.lens (\DescribeStandardsResponse' {nextToken} -> nextToken) (\s@DescribeStandardsResponse' {} a -> s {nextToken = a} :: DescribeStandardsResponse)

-- | A list of available standards.
describeStandardsResponse_standards :: Lens.Lens' DescribeStandardsResponse (Prelude.Maybe [Standard])
describeStandardsResponse_standards = Lens.lens (\DescribeStandardsResponse' {standards} -> standards) (\s@DescribeStandardsResponse' {} a -> s {standards = a} :: DescribeStandardsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeStandardsResponse_httpStatus :: Lens.Lens' DescribeStandardsResponse Prelude.Int
describeStandardsResponse_httpStatus = Lens.lens (\DescribeStandardsResponse' {httpStatus} -> httpStatus) (\s@DescribeStandardsResponse' {} a -> s {httpStatus = a} :: DescribeStandardsResponse)

instance Prelude.NFData DescribeStandardsResponse where
  rnf DescribeStandardsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf standards
      `Prelude.seq` Prelude.rnf httpStatus
