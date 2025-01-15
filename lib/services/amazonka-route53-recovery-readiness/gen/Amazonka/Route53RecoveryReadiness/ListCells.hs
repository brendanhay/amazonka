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
-- Module      : Amazonka.Route53RecoveryReadiness.ListCells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the cells for an account.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListCells
  ( -- * Creating a Request
    ListCells (..),
    newListCells,

    -- * Request Lenses
    listCells_maxResults,
    listCells_nextToken,

    -- * Destructuring the Response
    ListCellsResponse (..),
    newListCellsResponse,

    -- * Response Lenses
    listCellsResponse_cells,
    listCellsResponse_nextToken,
    listCellsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListCells' smart constructor.
data ListCells = ListCells'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCells_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listCells_nextToken' - The token that identifies which batch of results you want to see.
newListCells ::
  ListCells
newListCells =
  ListCells'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of objects that you want to return with this call.
listCells_maxResults :: Lens.Lens' ListCells (Prelude.Maybe Prelude.Natural)
listCells_maxResults = Lens.lens (\ListCells' {maxResults} -> maxResults) (\s@ListCells' {} a -> s {maxResults = a} :: ListCells)

-- | The token that identifies which batch of results you want to see.
listCells_nextToken :: Lens.Lens' ListCells (Prelude.Maybe Prelude.Text)
listCells_nextToken = Lens.lens (\ListCells' {nextToken} -> nextToken) (\s@ListCells' {} a -> s {nextToken = a} :: ListCells)

instance Core.AWSPager ListCells where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCellsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCellsResponse_cells
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listCells_nextToken
              Lens..~ rs
              Lens.^? listCellsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListCells where
  type AWSResponse ListCells = ListCellsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCellsResponse'
            Prelude.<$> (x Data..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCells where
  hashWithSalt _salt ListCells' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCells where
  rnf ListCells' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListCells where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCells where
  toPath = Prelude.const "/cells"

instance Data.ToQuery ListCells where
  toQuery ListCells' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListCellsResponse' smart constructor.
data ListCellsResponse = ListCellsResponse'
  { -- | A list of cells.
    cells :: Prelude.Maybe [CellOutput],
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCellsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cells', 'listCellsResponse_cells' - A list of cells.
--
-- 'nextToken', 'listCellsResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'httpStatus', 'listCellsResponse_httpStatus' - The response's http status code.
newListCellsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCellsResponse
newListCellsResponse pHttpStatus_ =
  ListCellsResponse'
    { cells = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of cells.
listCellsResponse_cells :: Lens.Lens' ListCellsResponse (Prelude.Maybe [CellOutput])
listCellsResponse_cells = Lens.lens (\ListCellsResponse' {cells} -> cells) (\s@ListCellsResponse' {} a -> s {cells = a} :: ListCellsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that identifies which batch of results you want to see.
listCellsResponse_nextToken :: Lens.Lens' ListCellsResponse (Prelude.Maybe Prelude.Text)
listCellsResponse_nextToken = Lens.lens (\ListCellsResponse' {nextToken} -> nextToken) (\s@ListCellsResponse' {} a -> s {nextToken = a} :: ListCellsResponse)

-- | The response's http status code.
listCellsResponse_httpStatus :: Lens.Lens' ListCellsResponse Prelude.Int
listCellsResponse_httpStatus = Lens.lens (\ListCellsResponse' {httpStatus} -> httpStatus) (\s@ListCellsResponse' {} a -> s {httpStatus = a} :: ListCellsResponse)

instance Prelude.NFData ListCellsResponse where
  rnf ListCellsResponse' {..} =
    Prelude.rnf cells `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
