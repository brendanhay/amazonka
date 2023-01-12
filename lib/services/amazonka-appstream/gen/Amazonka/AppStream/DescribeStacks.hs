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
-- Module      : Amazonka.AppStream.DescribeStacks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified stacks, if the
-- stack names are provided. Otherwise, all stacks in the account are
-- described.
--
-- This operation returns paginated results.
module Amazonka.AppStream.DescribeStacks
  ( -- * Creating a Request
    DescribeStacks (..),
    newDescribeStacks,

    -- * Request Lenses
    describeStacks_names,
    describeStacks_nextToken,

    -- * Destructuring the Response
    DescribeStacksResponse (..),
    newDescribeStacksResponse,

    -- * Response Lenses
    describeStacksResponse_nextToken,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { -- | The names of the stacks to describe.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStacks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'describeStacks_names' - The names of the stacks to describe.
--
-- 'nextToken', 'describeStacks_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
newDescribeStacks ::
  DescribeStacks
newDescribeStacks =
  DescribeStacks'
    { names = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The names of the stacks to describe.
describeStacks_names :: Lens.Lens' DescribeStacks (Prelude.Maybe [Prelude.Text])
describeStacks_names = Lens.lens (\DescribeStacks' {names} -> names) (\s@DescribeStacks' {} a -> s {names = a} :: DescribeStacks) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeStacks_nextToken :: Lens.Lens' DescribeStacks (Prelude.Maybe Prelude.Text)
describeStacks_nextToken = Lens.lens (\DescribeStacks' {nextToken} -> nextToken) (\s@DescribeStacks' {} a -> s {nextToken = a} :: DescribeStacks)

instance Core.AWSPager DescribeStacks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStacksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStacksResponse_stacks Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeStacks_nextToken
          Lens..~ rs
          Lens.^? describeStacksResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeStacks where
  type
    AWSResponse DescribeStacks =
      DescribeStacksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStacksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Stacks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStacks where
  hashWithSalt _salt DescribeStacks' {..} =
    _salt `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeStacks where
  rnf DescribeStacks' {..} =
    Prelude.rnf names
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeStacks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DescribeStacks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStacks where
  toJSON DescribeStacks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Names" Data..=) Prelude.<$> names,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeStacks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStacks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the stacks.
    stacks :: Prelude.Maybe [Stack],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStacksResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'stacks', 'describeStacksResponse_stacks' - Information about the stacks.
--
-- 'httpStatus', 'describeStacksResponse_httpStatus' - The response's http status code.
newDescribeStacksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStacksResponse
newDescribeStacksResponse pHttpStatus_ =
  DescribeStacksResponse'
    { nextToken =
        Prelude.Nothing,
      stacks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeStacksResponse_nextToken :: Lens.Lens' DescribeStacksResponse (Prelude.Maybe Prelude.Text)
describeStacksResponse_nextToken = Lens.lens (\DescribeStacksResponse' {nextToken} -> nextToken) (\s@DescribeStacksResponse' {} a -> s {nextToken = a} :: DescribeStacksResponse)

-- | Information about the stacks.
describeStacksResponse_stacks :: Lens.Lens' DescribeStacksResponse (Prelude.Maybe [Stack])
describeStacksResponse_stacks = Lens.lens (\DescribeStacksResponse' {stacks} -> stacks) (\s@DescribeStacksResponse' {} a -> s {stacks = a} :: DescribeStacksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeStacksResponse_httpStatus :: Lens.Lens' DescribeStacksResponse Prelude.Int
describeStacksResponse_httpStatus = Lens.lens (\DescribeStacksResponse' {httpStatus} -> httpStatus) (\s@DescribeStacksResponse' {} a -> s {httpStatus = a} :: DescribeStacksResponse)

instance Prelude.NFData DescribeStacksResponse where
  rnf DescribeStacksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stacks
      `Prelude.seq` Prelude.rnf httpStatus
