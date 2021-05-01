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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAssociatedFleets' smart constructor.
data ListAssociatedFleets = ListAssociatedFleets'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the stack.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListAssociatedFleets
newListAssociatedFleets pStackName_ =
  ListAssociatedFleets'
    { nextToken = Prelude.Nothing,
      stackName = pStackName_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listAssociatedFleets_nextToken :: Lens.Lens' ListAssociatedFleets (Prelude.Maybe Prelude.Text)
listAssociatedFleets_nextToken = Lens.lens (\ListAssociatedFleets' {nextToken} -> nextToken) (\s@ListAssociatedFleets' {} a -> s {nextToken = a} :: ListAssociatedFleets)

-- | The name of the stack.
listAssociatedFleets_stackName :: Lens.Lens' ListAssociatedFleets Prelude.Text
listAssociatedFleets_stackName = Lens.lens (\ListAssociatedFleets' {stackName} -> stackName) (\s@ListAssociatedFleets' {} a -> s {stackName = a} :: ListAssociatedFleets)

instance Pager.AWSPager ListAssociatedFleets where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listAssociatedFleetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listAssociatedFleetsResponse_names
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listAssociatedFleets_nextToken
          Lens..~ rs
          Lens.^? listAssociatedFleetsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListAssociatedFleets where
  type
    Rs ListAssociatedFleets =
      ListAssociatedFleetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedFleetsResponse'
            Prelude.<$> (x Prelude..?> "Names" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociatedFleets

instance Prelude.NFData ListAssociatedFleets

instance Prelude.ToHeaders ListAssociatedFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.ListAssociatedFleets" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListAssociatedFleets where
  toJSON ListAssociatedFleets' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            Prelude.Just ("StackName" Prelude..= stackName)
          ]
      )

instance Prelude.ToPath ListAssociatedFleets where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListAssociatedFleets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssociatedFleetsResponse' smart constructor.
data ListAssociatedFleetsResponse = ListAssociatedFleetsResponse'
  { -- | The name of the fleet.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListAssociatedFleetsResponse
newListAssociatedFleetsResponse pHttpStatus_ =
  ListAssociatedFleetsResponse'
    { names =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the fleet.
listAssociatedFleetsResponse_names :: Lens.Lens' ListAssociatedFleetsResponse (Prelude.Maybe [Prelude.Text])
listAssociatedFleetsResponse_names = Lens.lens (\ListAssociatedFleetsResponse' {names} -> names) (\s@ListAssociatedFleetsResponse' {} a -> s {names = a} :: ListAssociatedFleetsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listAssociatedFleetsResponse_nextToken :: Lens.Lens' ListAssociatedFleetsResponse (Prelude.Maybe Prelude.Text)
listAssociatedFleetsResponse_nextToken = Lens.lens (\ListAssociatedFleetsResponse' {nextToken} -> nextToken) (\s@ListAssociatedFleetsResponse' {} a -> s {nextToken = a} :: ListAssociatedFleetsResponse)

-- | The response's http status code.
listAssociatedFleetsResponse_httpStatus :: Lens.Lens' ListAssociatedFleetsResponse Prelude.Int
listAssociatedFleetsResponse_httpStatus = Lens.lens (\ListAssociatedFleetsResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedFleetsResponse' {} a -> s {httpStatus = a} :: ListAssociatedFleetsResponse)

instance Prelude.NFData ListAssociatedFleetsResponse
