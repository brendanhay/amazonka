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
-- Module      : Amazonka.LakeFormation.ListDataCellsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the data cell filters on a table.
--
-- This operation returns paginated results.
module Amazonka.LakeFormation.ListDataCellsFilter
  ( -- * Creating a Request
    ListDataCellsFilter (..),
    newListDataCellsFilter,

    -- * Request Lenses
    listDataCellsFilter_maxResults,
    listDataCellsFilter_nextToken,
    listDataCellsFilter_table,

    -- * Destructuring the Response
    ListDataCellsFilterResponse (..),
    newListDataCellsFilterResponse,

    -- * Response Lenses
    listDataCellsFilterResponse_dataCellsFilters,
    listDataCellsFilterResponse_nextToken,
    listDataCellsFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataCellsFilter' smart constructor.
data ListDataCellsFilter = ListDataCellsFilter'
  { -- | The maximum size of the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A table in the Glue Data Catalog.
    table :: Prelude.Maybe TableResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataCellsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataCellsFilter_maxResults' - The maximum size of the response.
--
-- 'nextToken', 'listDataCellsFilter_nextToken' - A continuation token, if this is a continuation call.
--
-- 'table', 'listDataCellsFilter_table' - A table in the Glue Data Catalog.
newListDataCellsFilter ::
  ListDataCellsFilter
newListDataCellsFilter =
  ListDataCellsFilter'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      table = Prelude.Nothing
    }

-- | The maximum size of the response.
listDataCellsFilter_maxResults :: Lens.Lens' ListDataCellsFilter (Prelude.Maybe Prelude.Natural)
listDataCellsFilter_maxResults = Lens.lens (\ListDataCellsFilter' {maxResults} -> maxResults) (\s@ListDataCellsFilter' {} a -> s {maxResults = a} :: ListDataCellsFilter)

-- | A continuation token, if this is a continuation call.
listDataCellsFilter_nextToken :: Lens.Lens' ListDataCellsFilter (Prelude.Maybe Prelude.Text)
listDataCellsFilter_nextToken = Lens.lens (\ListDataCellsFilter' {nextToken} -> nextToken) (\s@ListDataCellsFilter' {} a -> s {nextToken = a} :: ListDataCellsFilter)

-- | A table in the Glue Data Catalog.
listDataCellsFilter_table :: Lens.Lens' ListDataCellsFilter (Prelude.Maybe TableResource)
listDataCellsFilter_table = Lens.lens (\ListDataCellsFilter' {table} -> table) (\s@ListDataCellsFilter' {} a -> s {table = a} :: ListDataCellsFilter)

instance Core.AWSPager ListDataCellsFilter where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataCellsFilterResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDataCellsFilterResponse_dataCellsFilters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDataCellsFilter_nextToken
          Lens..~ rs
          Lens.^? listDataCellsFilterResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDataCellsFilter where
  type
    AWSResponse ListDataCellsFilter =
      ListDataCellsFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataCellsFilterResponse'
            Prelude.<$> ( x
                            Data..?> "DataCellsFilters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataCellsFilter where
  hashWithSalt _salt ListDataCellsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` table

instance Prelude.NFData ListDataCellsFilter where
  rnf ListDataCellsFilter' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf table

instance Data.ToHeaders ListDataCellsFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDataCellsFilter where
  toJSON ListDataCellsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Table" Data..=) Prelude.<$> table
          ]
      )

instance Data.ToPath ListDataCellsFilter where
  toPath = Prelude.const "/ListDataCellsFilter"

instance Data.ToQuery ListDataCellsFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataCellsFilterResponse' smart constructor.
data ListDataCellsFilterResponse = ListDataCellsFilterResponse'
  { -- | A list of @DataCellFilter@ structures.
    dataCellsFilters :: Prelude.Maybe [DataCellsFilter],
    -- | A continuation token, if not all requested data cell filters have been
    -- returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataCellsFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataCellsFilters', 'listDataCellsFilterResponse_dataCellsFilters' - A list of @DataCellFilter@ structures.
--
-- 'nextToken', 'listDataCellsFilterResponse_nextToken' - A continuation token, if not all requested data cell filters have been
-- returned.
--
-- 'httpStatus', 'listDataCellsFilterResponse_httpStatus' - The response's http status code.
newListDataCellsFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataCellsFilterResponse
newListDataCellsFilterResponse pHttpStatus_ =
  ListDataCellsFilterResponse'
    { dataCellsFilters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DataCellFilter@ structures.
listDataCellsFilterResponse_dataCellsFilters :: Lens.Lens' ListDataCellsFilterResponse (Prelude.Maybe [DataCellsFilter])
listDataCellsFilterResponse_dataCellsFilters = Lens.lens (\ListDataCellsFilterResponse' {dataCellsFilters} -> dataCellsFilters) (\s@ListDataCellsFilterResponse' {} a -> s {dataCellsFilters = a} :: ListDataCellsFilterResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if not all requested data cell filters have been
-- returned.
listDataCellsFilterResponse_nextToken :: Lens.Lens' ListDataCellsFilterResponse (Prelude.Maybe Prelude.Text)
listDataCellsFilterResponse_nextToken = Lens.lens (\ListDataCellsFilterResponse' {nextToken} -> nextToken) (\s@ListDataCellsFilterResponse' {} a -> s {nextToken = a} :: ListDataCellsFilterResponse)

-- | The response's http status code.
listDataCellsFilterResponse_httpStatus :: Lens.Lens' ListDataCellsFilterResponse Prelude.Int
listDataCellsFilterResponse_httpStatus = Lens.lens (\ListDataCellsFilterResponse' {httpStatus} -> httpStatus) (\s@ListDataCellsFilterResponse' {} a -> s {httpStatus = a} :: ListDataCellsFilterResponse)

instance Prelude.NFData ListDataCellsFilterResponse where
  rnf ListDataCellsFilterResponse' {..} =
    Prelude.rnf dataCellsFilters
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
