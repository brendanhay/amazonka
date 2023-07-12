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
-- Module      : Amazonka.RolesAnywhere.ListTrustAnchors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trust anchors in the authenticated account and Amazon Web
-- Services Region.
--
-- __Required permissions:__ @rolesanywhere:ListTrustAnchors@.
--
-- This operation returns paginated results.
module Amazonka.RolesAnywhere.ListTrustAnchors
  ( -- * Creating a Request
    ListTrustAnchors (..),
    newListTrustAnchors,

    -- * Request Lenses
    listTrustAnchors_nextToken,
    listTrustAnchors_pageSize,

    -- * Destructuring the Response
    ListTrustAnchorsResponse (..),
    newListTrustAnchorsResponse,

    -- * Response Lenses
    listTrustAnchorsResponse_nextToken,
    listTrustAnchorsResponse_trustAnchors,
    listTrustAnchorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newListTrustAnchors' smart constructor.
data ListTrustAnchors = ListTrustAnchors'
  { -- | A token that indicates where the output should continue from, if a
    -- previous operation did not show all results. To get the next results,
    -- call the operation again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of resources in the paginated list.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrustAnchors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrustAnchors_nextToken' - A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
--
-- 'pageSize', 'listTrustAnchors_pageSize' - The number of resources in the paginated list.
newListTrustAnchors ::
  ListTrustAnchors
newListTrustAnchors =
  ListTrustAnchors'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
listTrustAnchors_nextToken :: Lens.Lens' ListTrustAnchors (Prelude.Maybe Prelude.Text)
listTrustAnchors_nextToken = Lens.lens (\ListTrustAnchors' {nextToken} -> nextToken) (\s@ListTrustAnchors' {} a -> s {nextToken = a} :: ListTrustAnchors)

-- | The number of resources in the paginated list.
listTrustAnchors_pageSize :: Lens.Lens' ListTrustAnchors (Prelude.Maybe Prelude.Int)
listTrustAnchors_pageSize = Lens.lens (\ListTrustAnchors' {pageSize} -> pageSize) (\s@ListTrustAnchors' {} a -> s {pageSize = a} :: ListTrustAnchors)

instance Core.AWSPager ListTrustAnchors where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTrustAnchorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTrustAnchorsResponse_trustAnchors
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTrustAnchors_nextToken
          Lens..~ rs
          Lens.^? listTrustAnchorsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTrustAnchors where
  type
    AWSResponse ListTrustAnchors =
      ListTrustAnchorsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrustAnchorsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "trustAnchors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTrustAnchors where
  hashWithSalt _salt ListTrustAnchors' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListTrustAnchors where
  rnf ListTrustAnchors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToHeaders ListTrustAnchors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTrustAnchors where
  toPath = Prelude.const "/trustanchors"

instance Data.ToQuery ListTrustAnchors where
  toQuery ListTrustAnchors' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "pageSize" Data.=: pageSize
      ]

-- | /See:/ 'newListTrustAnchorsResponse' smart constructor.
data ListTrustAnchorsResponse = ListTrustAnchorsResponse'
  { -- | A token that indicates where the output should continue from, if a
    -- previous operation did not show all results. To get the next results,
    -- call the operation again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of trust anchors.
    trustAnchors :: Prelude.Maybe [TrustAnchorDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrustAnchorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrustAnchorsResponse_nextToken' - A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
--
-- 'trustAnchors', 'listTrustAnchorsResponse_trustAnchors' - A list of trust anchors.
--
-- 'httpStatus', 'listTrustAnchorsResponse_httpStatus' - The response's http status code.
newListTrustAnchorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTrustAnchorsResponse
newListTrustAnchorsResponse pHttpStatus_ =
  ListTrustAnchorsResponse'
    { nextToken =
        Prelude.Nothing,
      trustAnchors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where the output should continue from, if a
-- previous operation did not show all results. To get the next results,
-- call the operation again with this value.
listTrustAnchorsResponse_nextToken :: Lens.Lens' ListTrustAnchorsResponse (Prelude.Maybe Prelude.Text)
listTrustAnchorsResponse_nextToken = Lens.lens (\ListTrustAnchorsResponse' {nextToken} -> nextToken) (\s@ListTrustAnchorsResponse' {} a -> s {nextToken = a} :: ListTrustAnchorsResponse)

-- | A list of trust anchors.
listTrustAnchorsResponse_trustAnchors :: Lens.Lens' ListTrustAnchorsResponse (Prelude.Maybe [TrustAnchorDetail])
listTrustAnchorsResponse_trustAnchors = Lens.lens (\ListTrustAnchorsResponse' {trustAnchors} -> trustAnchors) (\s@ListTrustAnchorsResponse' {} a -> s {trustAnchors = a} :: ListTrustAnchorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTrustAnchorsResponse_httpStatus :: Lens.Lens' ListTrustAnchorsResponse Prelude.Int
listTrustAnchorsResponse_httpStatus = Lens.lens (\ListTrustAnchorsResponse' {httpStatus} -> httpStatus) (\s@ListTrustAnchorsResponse' {} a -> s {httpStatus = a} :: ListTrustAnchorsResponse)

instance Prelude.NFData ListTrustAnchorsResponse where
  rnf ListTrustAnchorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trustAnchors
      `Prelude.seq` Prelude.rnf httpStatus
