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
-- Module      : Amazonka.Nimble.ListEulas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List EULAs.
--
-- This operation returns paginated results.
module Amazonka.Nimble.ListEulas
  ( -- * Creating a Request
    ListEulas (..),
    newListEulas,

    -- * Request Lenses
    listEulas_eulaIds,
    listEulas_nextToken,

    -- * Destructuring the Response
    ListEulasResponse (..),
    newListEulasResponse,

    -- * Response Lenses
    listEulasResponse_eulas,
    listEulasResponse_nextToken,
    listEulasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEulas' smart constructor.
data ListEulas = ListEulas'
  { -- | The list of EULA IDs that should be returned
    eulaIds :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEulas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eulaIds', 'listEulas_eulaIds' - The list of EULA IDs that should be returned
--
-- 'nextToken', 'listEulas_nextToken' - The token for the next set of results, or null if there are no more
-- results.
newListEulas ::
  ListEulas
newListEulas =
  ListEulas'
    { eulaIds = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The list of EULA IDs that should be returned
listEulas_eulaIds :: Lens.Lens' ListEulas (Prelude.Maybe [Prelude.Text])
listEulas_eulaIds = Lens.lens (\ListEulas' {eulaIds} -> eulaIds) (\s@ListEulas' {} a -> s {eulaIds = a} :: ListEulas) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listEulas_nextToken :: Lens.Lens' ListEulas (Prelude.Maybe Prelude.Text)
listEulas_nextToken = Lens.lens (\ListEulas' {nextToken} -> nextToken) (\s@ListEulas' {} a -> s {nextToken = a} :: ListEulas)

instance Core.AWSPager ListEulas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEulasResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEulasResponse_eulas
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEulas_nextToken
          Lens..~ rs
          Lens.^? listEulasResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEulas where
  type AWSResponse ListEulas = ListEulasResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEulasResponse'
            Prelude.<$> (x Data..?> "eulas" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEulas where
  hashWithSalt _salt ListEulas' {..} =
    _salt
      `Prelude.hashWithSalt` eulaIds
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEulas where
  rnf ListEulas' {..} =
    Prelude.rnf eulaIds
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEulas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEulas where
  toPath = Prelude.const "/2020-08-01/eulas"

instance Data.ToQuery ListEulas where
  toQuery ListEulas' {..} =
    Prelude.mconcat
      [ "eulaIds"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> eulaIds),
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEulasResponse' smart constructor.
data ListEulasResponse = ListEulasResponse'
  { -- | A collection of EULA resources.
    eulas :: Prelude.Maybe [Eula],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEulasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eulas', 'listEulasResponse_eulas' - A collection of EULA resources.
--
-- 'nextToken', 'listEulasResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listEulasResponse_httpStatus' - The response's http status code.
newListEulasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEulasResponse
newListEulasResponse pHttpStatus_ =
  ListEulasResponse'
    { eulas = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of EULA resources.
listEulasResponse_eulas :: Lens.Lens' ListEulasResponse (Prelude.Maybe [Eula])
listEulasResponse_eulas = Lens.lens (\ListEulasResponse' {eulas} -> eulas) (\s@ListEulasResponse' {} a -> s {eulas = a} :: ListEulasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listEulasResponse_nextToken :: Lens.Lens' ListEulasResponse (Prelude.Maybe Prelude.Text)
listEulasResponse_nextToken = Lens.lens (\ListEulasResponse' {nextToken} -> nextToken) (\s@ListEulasResponse' {} a -> s {nextToken = a} :: ListEulasResponse)

-- | The response's http status code.
listEulasResponse_httpStatus :: Lens.Lens' ListEulasResponse Prelude.Int
listEulasResponse_httpStatus = Lens.lens (\ListEulasResponse' {httpStatus} -> httpStatus) (\s@ListEulasResponse' {} a -> s {httpStatus = a} :: ListEulasResponse)

instance Prelude.NFData ListEulasResponse where
  rnf ListEulasResponse' {..} =
    Prelude.rnf eulas
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
