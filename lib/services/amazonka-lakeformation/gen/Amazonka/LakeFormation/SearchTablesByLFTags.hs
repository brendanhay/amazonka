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
-- Module      : Amazonka.LakeFormation.SearchTablesByLFTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows a search on @TABLE@ resources by @LFTag@s. This
-- will be used by admins who want to grant user permissions on certain
-- LF-tags. Before making a grant, the admin can use @SearchTablesByLFTags@
-- to find all resources where the given @LFTag@s are valid to verify
-- whether the returned resources can be shared.
--
-- This operation returns paginated results.
module Amazonka.LakeFormation.SearchTablesByLFTags
  ( -- * Creating a Request
    SearchTablesByLFTags (..),
    newSearchTablesByLFTags,

    -- * Request Lenses
    searchTablesByLFTags_catalogId,
    searchTablesByLFTags_maxResults,
    searchTablesByLFTags_nextToken,
    searchTablesByLFTags_expression,

    -- * Destructuring the Response
    SearchTablesByLFTagsResponse (..),
    newSearchTablesByLFTagsResponse,

    -- * Response Lenses
    searchTablesByLFTagsResponse_nextToken,
    searchTablesByLFTagsResponse_tableList,
    searchTablesByLFTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchTablesByLFTags' smart constructor.
data SearchTablesByLFTags = SearchTablesByLFTags'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is not the first call to retrieve this
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of conditions (@LFTag@ structures) to search for in table
    -- resources.
    expression :: Prelude.NonEmpty LFTag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchTablesByLFTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'searchTablesByLFTags_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'maxResults', 'searchTablesByLFTags_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'searchTablesByLFTags_nextToken' - A continuation token, if this is not the first call to retrieve this
-- list.
--
-- 'expression', 'searchTablesByLFTags_expression' - A list of conditions (@LFTag@ structures) to search for in table
-- resources.
newSearchTablesByLFTags ::
  -- | 'expression'
  Prelude.NonEmpty LFTag ->
  SearchTablesByLFTags
newSearchTablesByLFTags pExpression_ =
  SearchTablesByLFTags'
    { catalogId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      expression = Lens.coerced Lens.# pExpression_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
searchTablesByLFTags_catalogId :: Lens.Lens' SearchTablesByLFTags (Prelude.Maybe Prelude.Text)
searchTablesByLFTags_catalogId = Lens.lens (\SearchTablesByLFTags' {catalogId} -> catalogId) (\s@SearchTablesByLFTags' {} a -> s {catalogId = a} :: SearchTablesByLFTags)

-- | The maximum number of results to return.
searchTablesByLFTags_maxResults :: Lens.Lens' SearchTablesByLFTags (Prelude.Maybe Prelude.Natural)
searchTablesByLFTags_maxResults = Lens.lens (\SearchTablesByLFTags' {maxResults} -> maxResults) (\s@SearchTablesByLFTags' {} a -> s {maxResults = a} :: SearchTablesByLFTags)

-- | A continuation token, if this is not the first call to retrieve this
-- list.
searchTablesByLFTags_nextToken :: Lens.Lens' SearchTablesByLFTags (Prelude.Maybe Prelude.Text)
searchTablesByLFTags_nextToken = Lens.lens (\SearchTablesByLFTags' {nextToken} -> nextToken) (\s@SearchTablesByLFTags' {} a -> s {nextToken = a} :: SearchTablesByLFTags)

-- | A list of conditions (@LFTag@ structures) to search for in table
-- resources.
searchTablesByLFTags_expression :: Lens.Lens' SearchTablesByLFTags (Prelude.NonEmpty LFTag)
searchTablesByLFTags_expression = Lens.lens (\SearchTablesByLFTags' {expression} -> expression) (\s@SearchTablesByLFTags' {} a -> s {expression = a} :: SearchTablesByLFTags) Prelude.. Lens.coerced

instance Core.AWSPager SearchTablesByLFTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchTablesByLFTagsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchTablesByLFTagsResponse_tableList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchTablesByLFTags_nextToken
          Lens..~ rs
          Lens.^? searchTablesByLFTagsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchTablesByLFTags where
  type
    AWSResponse SearchTablesByLFTags =
      SearchTablesByLFTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchTablesByLFTagsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TableList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchTablesByLFTags where
  hashWithSalt _salt SearchTablesByLFTags' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` expression

instance Prelude.NFData SearchTablesByLFTags where
  rnf SearchTablesByLFTags' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf expression

instance Data.ToHeaders SearchTablesByLFTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchTablesByLFTags where
  toJSON SearchTablesByLFTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Expression" Data..= expression)
          ]
      )

instance Data.ToPath SearchTablesByLFTags where
  toPath = Prelude.const "/SearchTablesByLFTags"

instance Data.ToQuery SearchTablesByLFTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchTablesByLFTagsResponse' smart constructor.
data SearchTablesByLFTagsResponse = SearchTablesByLFTagsResponse'
  { -- | A continuation token, present if the current list segment is not the
    -- last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of tables that meet the LF-tag conditions.
    tableList :: Prelude.Maybe [TaggedTable],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchTablesByLFTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchTablesByLFTagsResponse_nextToken' - A continuation token, present if the current list segment is not the
-- last.
--
-- 'tableList', 'searchTablesByLFTagsResponse_tableList' - A list of tables that meet the LF-tag conditions.
--
-- 'httpStatus', 'searchTablesByLFTagsResponse_httpStatus' - The response's http status code.
newSearchTablesByLFTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchTablesByLFTagsResponse
newSearchTablesByLFTagsResponse pHttpStatus_ =
  SearchTablesByLFTagsResponse'
    { nextToken =
        Prelude.Nothing,
      tableList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, present if the current list segment is not the
-- last.
searchTablesByLFTagsResponse_nextToken :: Lens.Lens' SearchTablesByLFTagsResponse (Prelude.Maybe Prelude.Text)
searchTablesByLFTagsResponse_nextToken = Lens.lens (\SearchTablesByLFTagsResponse' {nextToken} -> nextToken) (\s@SearchTablesByLFTagsResponse' {} a -> s {nextToken = a} :: SearchTablesByLFTagsResponse)

-- | A list of tables that meet the LF-tag conditions.
searchTablesByLFTagsResponse_tableList :: Lens.Lens' SearchTablesByLFTagsResponse (Prelude.Maybe [TaggedTable])
searchTablesByLFTagsResponse_tableList = Lens.lens (\SearchTablesByLFTagsResponse' {tableList} -> tableList) (\s@SearchTablesByLFTagsResponse' {} a -> s {tableList = a} :: SearchTablesByLFTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchTablesByLFTagsResponse_httpStatus :: Lens.Lens' SearchTablesByLFTagsResponse Prelude.Int
searchTablesByLFTagsResponse_httpStatus = Lens.lens (\SearchTablesByLFTagsResponse' {httpStatus} -> httpStatus) (\s@SearchTablesByLFTagsResponse' {} a -> s {httpStatus = a} :: SearchTablesByLFTagsResponse)

instance Prelude.NFData SearchTablesByLFTagsResponse where
  rnf SearchTablesByLFTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tableList
      `Prelude.seq` Prelude.rnf httpStatus
