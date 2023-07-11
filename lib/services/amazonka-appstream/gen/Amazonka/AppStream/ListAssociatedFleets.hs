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
-- Module      : Amazonka.AppStream.ListAssociatedFleets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the name of the fleet that is associated with the specified
-- stack.
--
-- This operation returns paginated results.
module Amazonka.AppStream.ListAssociatedFleets
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssociatedFleets' smart constructor.
data ListAssociatedFleets = ListAssociatedFleets'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the stack.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSPager ListAssociatedFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociatedFleetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociatedFleetsResponse_names
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAssociatedFleets_nextToken
          Lens..~ rs
          Lens.^? listAssociatedFleetsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAssociatedFleets where
  type
    AWSResponse ListAssociatedFleets =
      ListAssociatedFleetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedFleetsResponse'
            Prelude.<$> (x Data..?> "Names" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociatedFleets where
  hashWithSalt _salt ListAssociatedFleets' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData ListAssociatedFleets where
  rnf ListAssociatedFleets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stackName

instance Data.ToHeaders ListAssociatedFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.ListAssociatedFleets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAssociatedFleets where
  toJSON ListAssociatedFleets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("StackName" Data..= stackName)
          ]
      )

instance Data.ToPath ListAssociatedFleets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAssociatedFleets where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listAssociatedFleetsResponse_names = Lens.lens (\ListAssociatedFleetsResponse' {names} -> names) (\s@ListAssociatedFleetsResponse' {} a -> s {names = a} :: ListAssociatedFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listAssociatedFleetsResponse_nextToken :: Lens.Lens' ListAssociatedFleetsResponse (Prelude.Maybe Prelude.Text)
listAssociatedFleetsResponse_nextToken = Lens.lens (\ListAssociatedFleetsResponse' {nextToken} -> nextToken) (\s@ListAssociatedFleetsResponse' {} a -> s {nextToken = a} :: ListAssociatedFleetsResponse)

-- | The response's http status code.
listAssociatedFleetsResponse_httpStatus :: Lens.Lens' ListAssociatedFleetsResponse Prelude.Int
listAssociatedFleetsResponse_httpStatus = Lens.lens (\ListAssociatedFleetsResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedFleetsResponse' {} a -> s {httpStatus = a} :: ListAssociatedFleetsResponse)

instance Prelude.NFData ListAssociatedFleetsResponse where
  rnf ListAssociatedFleetsResponse' {..} =
    Prelude.rnf names
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
