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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of Cells.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListCells
  ( -- * Creating a Request
    ListCells (..),
    newListCells,

    -- * Request Lenses
    listCells_nextToken,
    listCells_maxResults,

    -- * Destructuring the Response
    ListCellsResponse (..),
    newListCellsResponse,

    -- * Response Lenses
    listCellsResponse_nextToken,
    listCellsResponse_cells,
    listCellsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListCells' smart constructor.
data ListCells = ListCells'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listCells_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'listCells_maxResults' - Upper bound on number of records to return.
newListCells ::
  ListCells
newListCells =
  ListCells'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token used to resume pagination from the end of a previous request.
listCells_nextToken :: Lens.Lens' ListCells (Prelude.Maybe Prelude.Text)
listCells_nextToken = Lens.lens (\ListCells' {nextToken} -> nextToken) (\s@ListCells' {} a -> s {nextToken = a} :: ListCells)

-- | Upper bound on number of records to return.
listCells_maxResults :: Lens.Lens' ListCells (Prelude.Maybe Prelude.Natural)
listCells_maxResults = Lens.lens (\ListCells' {maxResults} -> maxResults) (\s@ListCells' {} a -> s {maxResults = a} :: ListCells)

instance Core.AWSPager ListCells where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCellsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCellsResponse_cells Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCells_nextToken
          Lens..~ rs
          Lens.^? listCellsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListCells where
  type AWSResponse ListCells = ListCellsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCellsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCells where
  hashWithSalt _salt ListCells' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCells where
  rnf ListCells' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListCells where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListCells where
  toPath = Prelude.const "/cells"

instance Core.ToQuery ListCells where
  toQuery ListCells' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListCellsResponse' smart constructor.
data ListCellsResponse = ListCellsResponse'
  { -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of Cells
    cells :: Prelude.Maybe [CellOutput],
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
-- 'nextToken', 'listCellsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'cells', 'listCellsResponse_cells' - A list of Cells
--
-- 'httpStatus', 'listCellsResponse_httpStatus' - The response's http status code.
newListCellsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCellsResponse
newListCellsResponse pHttpStatus_ =
  ListCellsResponse'
    { nextToken = Prelude.Nothing,
      cells = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to resume pagination from the end of the
-- collection.
listCellsResponse_nextToken :: Lens.Lens' ListCellsResponse (Prelude.Maybe Prelude.Text)
listCellsResponse_nextToken = Lens.lens (\ListCellsResponse' {nextToken} -> nextToken) (\s@ListCellsResponse' {} a -> s {nextToken = a} :: ListCellsResponse)

-- | A list of Cells
listCellsResponse_cells :: Lens.Lens' ListCellsResponse (Prelude.Maybe [CellOutput])
listCellsResponse_cells = Lens.lens (\ListCellsResponse' {cells} -> cells) (\s@ListCellsResponse' {} a -> s {cells = a} :: ListCellsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCellsResponse_httpStatus :: Lens.Lens' ListCellsResponse Prelude.Int
listCellsResponse_httpStatus = Lens.lens (\ListCellsResponse' {httpStatus} -> httpStatus) (\s@ListCellsResponse' {} a -> s {httpStatus = a} :: ListCellsResponse)

instance Prelude.NFData ListCellsResponse where
  rnf ListCellsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf cells
      `Prelude.seq` Prelude.rnf httpStatus
