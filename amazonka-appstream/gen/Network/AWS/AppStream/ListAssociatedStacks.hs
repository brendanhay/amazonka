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
-- Module      : Network.AWS.AppStream.ListAssociatedStacks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the name of the stack with which the specified fleet is
-- associated.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.ListAssociatedStacks
  ( -- * Creating a Request
    ListAssociatedStacks (..),
    newListAssociatedStacks,

    -- * Request Lenses
    listAssociatedStacks_nextToken,
    listAssociatedStacks_fleetName,

    -- * Destructuring the Response
    ListAssociatedStacksResponse (..),
    newListAssociatedStacksResponse,

    -- * Response Lenses
    listAssociatedStacksResponse_names,
    listAssociatedStacksResponse_nextToken,
    listAssociatedStacksResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssociatedStacks' smart constructor.
data ListAssociatedStacks = ListAssociatedStacks'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the fleet.
    fleetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociatedStacks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedStacks_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'fleetName', 'listAssociatedStacks_fleetName' - The name of the fleet.
newListAssociatedStacks ::
  -- | 'fleetName'
  Core.Text ->
  ListAssociatedStacks
newListAssociatedStacks pFleetName_ =
  ListAssociatedStacks'
    { nextToken = Core.Nothing,
      fleetName = pFleetName_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listAssociatedStacks_nextToken :: Lens.Lens' ListAssociatedStacks (Core.Maybe Core.Text)
listAssociatedStacks_nextToken = Lens.lens (\ListAssociatedStacks' {nextToken} -> nextToken) (\s@ListAssociatedStacks' {} a -> s {nextToken = a} :: ListAssociatedStacks)

-- | The name of the fleet.
listAssociatedStacks_fleetName :: Lens.Lens' ListAssociatedStacks Core.Text
listAssociatedStacks_fleetName = Lens.lens (\ListAssociatedStacks' {fleetName} -> fleetName) (\s@ListAssociatedStacks' {} a -> s {fleetName = a} :: ListAssociatedStacks)

instance Core.AWSPager ListAssociatedStacks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociatedStacksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociatedStacksResponse_names Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAssociatedStacks_nextToken
          Lens..~ rs
          Lens.^? listAssociatedStacksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAssociatedStacks where
  type
    AWSResponse ListAssociatedStacks =
      ListAssociatedStacksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedStacksResponse'
            Core.<$> (x Core..?> "Names" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAssociatedStacks

instance Core.NFData ListAssociatedStacks

instance Core.ToHeaders ListAssociatedStacks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.ListAssociatedStacks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAssociatedStacks where
  toJSON ListAssociatedStacks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("FleetName" Core..= fleetName)
          ]
      )

instance Core.ToPath ListAssociatedStacks where
  toPath = Core.const "/"

instance Core.ToQuery ListAssociatedStacks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssociatedStacksResponse' smart constructor.
data ListAssociatedStacksResponse = ListAssociatedStacksResponse'
  { -- | The name of the stack.
    names :: Core.Maybe [Core.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociatedStacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'listAssociatedStacksResponse_names' - The name of the stack.
--
-- 'nextToken', 'listAssociatedStacksResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'listAssociatedStacksResponse_httpStatus' - The response's http status code.
newListAssociatedStacksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssociatedStacksResponse
newListAssociatedStacksResponse pHttpStatus_ =
  ListAssociatedStacksResponse'
    { names = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the stack.
listAssociatedStacksResponse_names :: Lens.Lens' ListAssociatedStacksResponse (Core.Maybe [Core.Text])
listAssociatedStacksResponse_names = Lens.lens (\ListAssociatedStacksResponse' {names} -> names) (\s@ListAssociatedStacksResponse' {} a -> s {names = a} :: ListAssociatedStacksResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listAssociatedStacksResponse_nextToken :: Lens.Lens' ListAssociatedStacksResponse (Core.Maybe Core.Text)
listAssociatedStacksResponse_nextToken = Lens.lens (\ListAssociatedStacksResponse' {nextToken} -> nextToken) (\s@ListAssociatedStacksResponse' {} a -> s {nextToken = a} :: ListAssociatedStacksResponse)

-- | The response's http status code.
listAssociatedStacksResponse_httpStatus :: Lens.Lens' ListAssociatedStacksResponse Core.Int
listAssociatedStacksResponse_httpStatus = Lens.lens (\ListAssociatedStacksResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedStacksResponse' {} a -> s {httpStatus = a} :: ListAssociatedStacksResponse)

instance Core.NFData ListAssociatedStacksResponse
