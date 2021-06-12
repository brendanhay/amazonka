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
-- Module      : Network.AWS.AppStream.ListAssociatedFleets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the name of the fleet that is associated with the specified
-- stack.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.ListAssociatedFleets
  ( -- * Creating a Request
    ListAssociatedFleets (..),
    newListAssociatedFleets,

    -- * Request Lenses
    listAssociatedFleets_nextToken,
    listAssociatedFleets_stackName,

    -- * Destructuring the Response
    ListAssociatedFleetsResponse (..),
    newListAssociatedFleetsResponse,

    -- * Response Lenses
    listAssociatedFleetsResponse_names,
    listAssociatedFleetsResponse_nextToken,
    listAssociatedFleetsResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssociatedFleets' smart constructor.
data ListAssociatedFleets = ListAssociatedFleets'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the stack.
    stackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociatedFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedFleets_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'stackName', 'listAssociatedFleets_stackName' - The name of the stack.
newListAssociatedFleets ::
  -- | 'stackName'
  Core.Text ->
  ListAssociatedFleets
newListAssociatedFleets pStackName_ =
  ListAssociatedFleets'
    { nextToken = Core.Nothing,
      stackName = pStackName_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listAssociatedFleets_nextToken :: Lens.Lens' ListAssociatedFleets (Core.Maybe Core.Text)
listAssociatedFleets_nextToken = Lens.lens (\ListAssociatedFleets' {nextToken} -> nextToken) (\s@ListAssociatedFleets' {} a -> s {nextToken = a} :: ListAssociatedFleets)

-- | The name of the stack.
listAssociatedFleets_stackName :: Lens.Lens' ListAssociatedFleets Core.Text
listAssociatedFleets_stackName = Lens.lens (\ListAssociatedFleets' {stackName} -> stackName) (\s@ListAssociatedFleets' {} a -> s {stackName = a} :: ListAssociatedFleets)

instance Core.AWSPager ListAssociatedFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociatedFleetsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociatedFleetsResponse_names Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAssociatedFleets_nextToken
          Lens..~ rs
          Lens.^? listAssociatedFleetsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAssociatedFleets where
  type
    AWSResponse ListAssociatedFleets =
      ListAssociatedFleetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedFleetsResponse'
            Core.<$> (x Core..?> "Names" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAssociatedFleets

instance Core.NFData ListAssociatedFleets

instance Core.ToHeaders ListAssociatedFleets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.ListAssociatedFleets" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAssociatedFleets where
  toJSON ListAssociatedFleets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("StackName" Core..= stackName)
          ]
      )

instance Core.ToPath ListAssociatedFleets where
  toPath = Core.const "/"

instance Core.ToQuery ListAssociatedFleets where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAssociatedFleetsResponse' smart constructor.
data ListAssociatedFleetsResponse = ListAssociatedFleetsResponse'
  { -- | The name of the fleet.
    names :: Core.Maybe [Core.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAssociatedFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'listAssociatedFleetsResponse_names' - The name of the fleet.
--
-- 'nextToken', 'listAssociatedFleetsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'listAssociatedFleetsResponse_httpStatus' - The response's http status code.
newListAssociatedFleetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAssociatedFleetsResponse
newListAssociatedFleetsResponse pHttpStatus_ =
  ListAssociatedFleetsResponse'
    { names = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the fleet.
listAssociatedFleetsResponse_names :: Lens.Lens' ListAssociatedFleetsResponse (Core.Maybe [Core.Text])
listAssociatedFleetsResponse_names = Lens.lens (\ListAssociatedFleetsResponse' {names} -> names) (\s@ListAssociatedFleetsResponse' {} a -> s {names = a} :: ListAssociatedFleetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listAssociatedFleetsResponse_nextToken :: Lens.Lens' ListAssociatedFleetsResponse (Core.Maybe Core.Text)
listAssociatedFleetsResponse_nextToken = Lens.lens (\ListAssociatedFleetsResponse' {nextToken} -> nextToken) (\s@ListAssociatedFleetsResponse' {} a -> s {nextToken = a} :: ListAssociatedFleetsResponse)

-- | The response's http status code.
listAssociatedFleetsResponse_httpStatus :: Lens.Lens' ListAssociatedFleetsResponse Core.Int
listAssociatedFleetsResponse_httpStatus = Lens.lens (\ListAssociatedFleetsResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedFleetsResponse' {} a -> s {httpStatus = a} :: ListAssociatedFleetsResponse)

instance Core.NFData ListAssociatedFleetsResponse
