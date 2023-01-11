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
-- Module      : Amazonka.MacieV2.ListManagedDataIdentifiers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the managed data identifiers that Amazon
-- Macie currently provides.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListManagedDataIdentifiers
  ( -- * Creating a Request
    ListManagedDataIdentifiers (..),
    newListManagedDataIdentifiers,

    -- * Request Lenses
    listManagedDataIdentifiers_nextToken,

    -- * Destructuring the Response
    ListManagedDataIdentifiersResponse (..),
    newListManagedDataIdentifiersResponse,

    -- * Response Lenses
    listManagedDataIdentifiersResponse_items,
    listManagedDataIdentifiersResponse_nextToken,
    listManagedDataIdentifiersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListManagedDataIdentifiers' smart constructor.
data ListManagedDataIdentifiers = ListManagedDataIdentifiers'
  { -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedDataIdentifiers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listManagedDataIdentifiers_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
newListManagedDataIdentifiers ::
  ListManagedDataIdentifiers
newListManagedDataIdentifiers =
  ListManagedDataIdentifiers'
    { nextToken =
        Prelude.Nothing
    }

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listManagedDataIdentifiers_nextToken :: Lens.Lens' ListManagedDataIdentifiers (Prelude.Maybe Prelude.Text)
listManagedDataIdentifiers_nextToken = Lens.lens (\ListManagedDataIdentifiers' {nextToken} -> nextToken) (\s@ListManagedDataIdentifiers' {} a -> s {nextToken = a} :: ListManagedDataIdentifiers)

instance Core.AWSPager ListManagedDataIdentifiers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listManagedDataIdentifiersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listManagedDataIdentifiersResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listManagedDataIdentifiers_nextToken
          Lens..~ rs
          Lens.^? listManagedDataIdentifiersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListManagedDataIdentifiers where
  type
    AWSResponse ListManagedDataIdentifiers =
      ListManagedDataIdentifiersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedDataIdentifiersResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListManagedDataIdentifiers where
  hashWithSalt _salt ListManagedDataIdentifiers' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListManagedDataIdentifiers where
  rnf ListManagedDataIdentifiers' {..} =
    Prelude.rnf nextToken

instance Data.ToHeaders ListManagedDataIdentifiers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListManagedDataIdentifiers where
  toJSON ListManagedDataIdentifiers' {..} =
    Data.object
      ( Prelude.catMaybes
          [("nextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath ListManagedDataIdentifiers where
  toPath =
    Prelude.const "/managed-data-identifiers/list"

instance Data.ToQuery ListManagedDataIdentifiers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListManagedDataIdentifiersResponse' smart constructor.
data ListManagedDataIdentifiersResponse = ListManagedDataIdentifiersResponse'
  { -- | An array of objects, one for each managed data identifier.
    items :: Prelude.Maybe [ManagedDataIdentifierSummary],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedDataIdentifiersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listManagedDataIdentifiersResponse_items' - An array of objects, one for each managed data identifier.
--
-- 'nextToken', 'listManagedDataIdentifiersResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'httpStatus', 'listManagedDataIdentifiersResponse_httpStatus' - The response's http status code.
newListManagedDataIdentifiersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedDataIdentifiersResponse
newListManagedDataIdentifiersResponse pHttpStatus_ =
  ListManagedDataIdentifiersResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each managed data identifier.
listManagedDataIdentifiersResponse_items :: Lens.Lens' ListManagedDataIdentifiersResponse (Prelude.Maybe [ManagedDataIdentifierSummary])
listManagedDataIdentifiersResponse_items = Lens.lens (\ListManagedDataIdentifiersResponse' {items} -> items) (\s@ListManagedDataIdentifiersResponse' {} a -> s {items = a} :: ListManagedDataIdentifiersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listManagedDataIdentifiersResponse_nextToken :: Lens.Lens' ListManagedDataIdentifiersResponse (Prelude.Maybe Prelude.Text)
listManagedDataIdentifiersResponse_nextToken = Lens.lens (\ListManagedDataIdentifiersResponse' {nextToken} -> nextToken) (\s@ListManagedDataIdentifiersResponse' {} a -> s {nextToken = a} :: ListManagedDataIdentifiersResponse)

-- | The response's http status code.
listManagedDataIdentifiersResponse_httpStatus :: Lens.Lens' ListManagedDataIdentifiersResponse Prelude.Int
listManagedDataIdentifiersResponse_httpStatus = Lens.lens (\ListManagedDataIdentifiersResponse' {httpStatus} -> httpStatus) (\s@ListManagedDataIdentifiersResponse' {} a -> s {httpStatus = a} :: ListManagedDataIdentifiersResponse)

instance
  Prelude.NFData
    ListManagedDataIdentifiersResponse
  where
  rnf ListManagedDataIdentifiersResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
