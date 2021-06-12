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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { -- | The names of the stacks to describe.
    names :: Core.Maybe [Core.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { names = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The names of the stacks to describe.
describeStacks_names :: Lens.Lens' DescribeStacks (Core.Maybe [Core.Text])
describeStacks_names = Lens.lens (\DescribeStacks' {names} -> names) (\s@DescribeStacks' {} a -> s {names = a} :: DescribeStacks) Core.. Lens.mapping Lens._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeStacks_nextToken :: Lens.Lens' DescribeStacks (Core.Maybe Core.Text)
describeStacks_nextToken = Lens.lens (\DescribeStacks' {nextToken} -> nextToken) (\s@DescribeStacks' {} a -> s {nextToken = a} :: DescribeStacks)

instance Core.AWSPager DescribeStacks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStacksResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStacksResponse_stacks Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeStacks_nextToken
          Lens..~ rs
          Lens.^? describeStacksResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeStacks where
  type
    AWSResponse DescribeStacks =
      DescribeStacksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStacksResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Stacks" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStacks

instance Core.NFData DescribeStacks

instance Core.ToHeaders DescribeStacks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeStacks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeStacks where
  toJSON DescribeStacks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Names" Core..=) Core.<$> names,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.ToPath DescribeStacks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeStacks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the stacks.
    stacks :: Core.Maybe [Stack],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeStacksResponse
newDescribeStacksResponse pHttpStatus_ =
  DescribeStacksResponse'
    { nextToken = Core.Nothing,
      stacks = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeStacksResponse_nextToken :: Lens.Lens' DescribeStacksResponse (Core.Maybe Core.Text)
describeStacksResponse_nextToken = Lens.lens (\DescribeStacksResponse' {nextToken} -> nextToken) (\s@DescribeStacksResponse' {} a -> s {nextToken = a} :: DescribeStacksResponse)

-- | Information about the stacks.
describeStacksResponse_stacks :: Lens.Lens' DescribeStacksResponse (Core.Maybe [Stack])
describeStacksResponse_stacks = Lens.lens (\DescribeStacksResponse' {stacks} -> stacks) (\s@DescribeStacksResponse' {} a -> s {stacks = a} :: DescribeStacksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeStacksResponse_httpStatus :: Lens.Lens' DescribeStacksResponse Core.Int
describeStacksResponse_httpStatus = Lens.lens (\DescribeStacksResponse' {httpStatus} -> httpStatus) (\s@DescribeStacksResponse' {} a -> s {httpStatus = a} :: DescribeStacksResponse)

instance Core.NFData DescribeStacksResponse
