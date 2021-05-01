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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssociatedStacks' smart constructor.
data ListAssociatedStacks = ListAssociatedStacks'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet.
    fleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListAssociatedStacks
newListAssociatedStacks pFleetName_ =
  ListAssociatedStacks'
    { nextToken = Prelude.Nothing,
      fleetName = pFleetName_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listAssociatedStacks_nextToken :: Lens.Lens' ListAssociatedStacks (Prelude.Maybe Prelude.Text)
listAssociatedStacks_nextToken = Lens.lens (\ListAssociatedStacks' {nextToken} -> nextToken) (\s@ListAssociatedStacks' {} a -> s {nextToken = a} :: ListAssociatedStacks)

-- | The name of the fleet.
listAssociatedStacks_fleetName :: Lens.Lens' ListAssociatedStacks Prelude.Text
listAssociatedStacks_fleetName = Lens.lens (\ListAssociatedStacks' {fleetName} -> fleetName) (\s@ListAssociatedStacks' {} a -> s {fleetName = a} :: ListAssociatedStacks)

instance Pager.AWSPager ListAssociatedStacks where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listAssociatedStacksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listAssociatedStacksResponse_names
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listAssociatedStacks_nextToken
          Lens..~ rs
          Lens.^? listAssociatedStacksResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListAssociatedStacks where
  type
    Rs ListAssociatedStacks =
      ListAssociatedStacksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedStacksResponse'
            Prelude.<$> (x Prelude..?> "Names" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociatedStacks

instance Prelude.NFData ListAssociatedStacks

instance Prelude.ToHeaders ListAssociatedStacks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.ListAssociatedStacks" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListAssociatedStacks where
  toJSON ListAssociatedStacks' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            Prelude.Just ("FleetName" Prelude..= fleetName)
          ]
      )

instance Prelude.ToPath ListAssociatedStacks where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListAssociatedStacks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssociatedStacksResponse' smart constructor.
data ListAssociatedStacksResponse = ListAssociatedStacksResponse'
  { -- | The name of the stack.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListAssociatedStacksResponse
newListAssociatedStacksResponse pHttpStatus_ =
  ListAssociatedStacksResponse'
    { names =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the stack.
listAssociatedStacksResponse_names :: Lens.Lens' ListAssociatedStacksResponse (Prelude.Maybe [Prelude.Text])
listAssociatedStacksResponse_names = Lens.lens (\ListAssociatedStacksResponse' {names} -> names) (\s@ListAssociatedStacksResponse' {} a -> s {names = a} :: ListAssociatedStacksResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listAssociatedStacksResponse_nextToken :: Lens.Lens' ListAssociatedStacksResponse (Prelude.Maybe Prelude.Text)
listAssociatedStacksResponse_nextToken = Lens.lens (\ListAssociatedStacksResponse' {nextToken} -> nextToken) (\s@ListAssociatedStacksResponse' {} a -> s {nextToken = a} :: ListAssociatedStacksResponse)

-- | The response's http status code.
listAssociatedStacksResponse_httpStatus :: Lens.Lens' ListAssociatedStacksResponse Prelude.Int
listAssociatedStacksResponse_httpStatus = Lens.lens (\ListAssociatedStacksResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedStacksResponse' {} a -> s {httpStatus = a} :: ListAssociatedStacksResponse)

instance Prelude.NFData ListAssociatedStacksResponse
