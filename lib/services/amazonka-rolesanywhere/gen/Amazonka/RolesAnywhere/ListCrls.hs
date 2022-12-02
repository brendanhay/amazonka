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
-- Module      : Amazonka.RolesAnywhere.ListCrls
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Crls in the authenticated account and Amazon Web Services
-- Region.
--
-- __Required permissions:__ @rolesanywhere:ListCrls@.
--
-- This operation returns paginated results.
module Amazonka.RolesAnywhere.ListCrls
  ( -- * Creating a Request
    ListCrls (..),
    newListCrls,

    -- * Request Lenses
    listCrls_nextToken,
    listCrls_pageSize,

    -- * Destructuring the Response
    ListCrlsResponse (..),
    newListCrlsResponse,

    -- * Response Lenses
    listCrlsResponse_nextToken,
    listCrlsResponse_crls,
    listCrlsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newListCrls' smart constructor.
data ListCrls = ListCrls'
  { -- | A token that indicates where the output should continue from, if a
    -- previous operation did not show all results. To get the next results,
    -- call the operation again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of resources in the paginated list.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCrls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCrls_nextToken' - A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
--
-- 'pageSize', 'listCrls_pageSize' - The number of resources in the paginated list.
newListCrls ::
  ListCrls
newListCrls =
  ListCrls'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
listCrls_nextToken :: Lens.Lens' ListCrls (Prelude.Maybe Prelude.Text)
listCrls_nextToken = Lens.lens (\ListCrls' {nextToken} -> nextToken) (\s@ListCrls' {} a -> s {nextToken = a} :: ListCrls)

-- | The number of resources in the paginated list.
listCrls_pageSize :: Lens.Lens' ListCrls (Prelude.Maybe Prelude.Int)
listCrls_pageSize = Lens.lens (\ListCrls' {pageSize} -> pageSize) (\s@ListCrls' {} a -> s {pageSize = a} :: ListCrls)

instance Core.AWSPager ListCrls where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCrlsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCrlsResponse_crls Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCrls_nextToken
          Lens..~ rs
          Lens.^? listCrlsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListCrls where
  type AWSResponse ListCrls = ListCrlsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCrlsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "crls" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCrls where
  hashWithSalt _salt ListCrls' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListCrls where
  rnf ListCrls' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToHeaders ListCrls where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCrls where
  toPath = Prelude.const "/crls"

instance Data.ToQuery ListCrls where
  toQuery ListCrls' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "pageSize" Data.=: pageSize
      ]

-- | /See:/ 'newListCrlsResponse' smart constructor.
data ListCrlsResponse = ListCrlsResponse'
  { -- | A token that indicates where the output should continue from, if a
    -- previous operation did not show all results. To get the next results,
    -- call the operation again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of certificate revocation lists (CRL).
    crls :: Prelude.Maybe [CrlDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCrlsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCrlsResponse_nextToken' - A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
--
-- 'crls', 'listCrlsResponse_crls' - A list of certificate revocation lists (CRL).
--
-- 'httpStatus', 'listCrlsResponse_httpStatus' - The response's http status code.
newListCrlsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCrlsResponse
newListCrlsResponse pHttpStatus_ =
  ListCrlsResponse'
    { nextToken = Prelude.Nothing,
      crls = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
listCrlsResponse_nextToken :: Lens.Lens' ListCrlsResponse (Prelude.Maybe Prelude.Text)
listCrlsResponse_nextToken = Lens.lens (\ListCrlsResponse' {nextToken} -> nextToken) (\s@ListCrlsResponse' {} a -> s {nextToken = a} :: ListCrlsResponse)

-- | A list of certificate revocation lists (CRL).
listCrlsResponse_crls :: Lens.Lens' ListCrlsResponse (Prelude.Maybe [CrlDetail])
listCrlsResponse_crls = Lens.lens (\ListCrlsResponse' {crls} -> crls) (\s@ListCrlsResponse' {} a -> s {crls = a} :: ListCrlsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCrlsResponse_httpStatus :: Lens.Lens' ListCrlsResponse Prelude.Int
listCrlsResponse_httpStatus = Lens.lens (\ListCrlsResponse' {httpStatus} -> httpStatus) (\s@ListCrlsResponse' {} a -> s {httpStatus = a} :: ListCrlsResponse)

instance Prelude.NFData ListCrlsResponse where
  rnf ListCrlsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf crls
      `Prelude.seq` Prelude.rnf httpStatus
