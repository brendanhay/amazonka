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
-- Module      : Network.AWS.AppStream.DescribeStacks
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.AppStream.DescribeStacks
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

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { -- | The names of the stacks to describe.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeStacks_names = Lens.lens (\DescribeStacks' {names} -> names) (\s@DescribeStacks' {} a -> s {names = a} :: DescribeStacks) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeStacks_nextToken :: Lens.Lens' DescribeStacks (Prelude.Maybe Prelude.Text)
describeStacks_nextToken = Lens.lens (\DescribeStacks' {nextToken} -> nextToken) (\s@DescribeStacks' {} a -> s {nextToken = a} :: DescribeStacks)

instance Pager.AWSPager DescribeStacks where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeStacksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeStacksResponse_stacks Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeStacks_nextToken
          Lens..~ rs
          Lens.^? describeStacksResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeStacks where
  type Rs DescribeStacks = DescribeStacksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStacksResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Stacks" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStacks

instance Prelude.NFData DescribeStacks

instance Prelude.ToHeaders DescribeStacks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.DescribeStacks" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeStacks where
  toJSON DescribeStacks' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Names" Prelude..=) Prelude.<$> names,
            ("NextToken" Prelude..=) Prelude.<$> nextToken
          ]
      )

instance Prelude.ToPath DescribeStacks where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeStacks where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeStacksResponse_stacks = Lens.lens (\DescribeStacksResponse' {stacks} -> stacks) (\s@DescribeStacksResponse' {} a -> s {stacks = a} :: DescribeStacksResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeStacksResponse_httpStatus :: Lens.Lens' DescribeStacksResponse Prelude.Int
describeStacksResponse_httpStatus = Lens.lens (\DescribeStacksResponse' {httpStatus} -> httpStatus) (\s@DescribeStacksResponse' {} a -> s {httpStatus = a} :: DescribeStacksResponse)

instance Prelude.NFData DescribeStacksResponse
