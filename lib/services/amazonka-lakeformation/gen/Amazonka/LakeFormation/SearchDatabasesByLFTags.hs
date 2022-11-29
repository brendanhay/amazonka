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
-- Module      : Amazonka.LakeFormation.SearchDatabasesByLFTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows a search on @DATABASE@ resources by
-- @TagCondition@. This operation is used by admins who want to grant user
-- permissions on certain @TagConditions@. Before making a grant, the admin
-- can use @SearchDatabasesByTags@ to find all resources where the given
-- @TagConditions@ are valid to verify whether the returned resources can
-- be shared.
--
-- This operation returns paginated results.
module Amazonka.LakeFormation.SearchDatabasesByLFTags
  ( -- * Creating a Request
    SearchDatabasesByLFTags (..),
    newSearchDatabasesByLFTags,

    -- * Request Lenses
    searchDatabasesByLFTags_nextToken,
    searchDatabasesByLFTags_maxResults,
    searchDatabasesByLFTags_catalogId,
    searchDatabasesByLFTags_expression,

    -- * Destructuring the Response
    SearchDatabasesByLFTagsResponse (..),
    newSearchDatabasesByLFTagsResponse,

    -- * Response Lenses
    searchDatabasesByLFTagsResponse_nextToken,
    searchDatabasesByLFTagsResponse_databaseList,
    searchDatabasesByLFTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchDatabasesByLFTags' smart constructor.
data SearchDatabasesByLFTags = SearchDatabasesByLFTags'
  { -- | A continuation token, if this is not the first call to retrieve this
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A list of conditions (@LFTag@ structures) to search for in database
    -- resources.
    expression :: Prelude.NonEmpty LFTag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDatabasesByLFTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchDatabasesByLFTags_nextToken' - A continuation token, if this is not the first call to retrieve this
-- list.
--
-- 'maxResults', 'searchDatabasesByLFTags_maxResults' - The maximum number of results to return.
--
-- 'catalogId', 'searchDatabasesByLFTags_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'expression', 'searchDatabasesByLFTags_expression' - A list of conditions (@LFTag@ structures) to search for in database
-- resources.
newSearchDatabasesByLFTags ::
  -- | 'expression'
  Prelude.NonEmpty LFTag ->
  SearchDatabasesByLFTags
newSearchDatabasesByLFTags pExpression_ =
  SearchDatabasesByLFTags'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      expression = Lens.coerced Lens.# pExpression_
    }

-- | A continuation token, if this is not the first call to retrieve this
-- list.
searchDatabasesByLFTags_nextToken :: Lens.Lens' SearchDatabasesByLFTags (Prelude.Maybe Prelude.Text)
searchDatabasesByLFTags_nextToken = Lens.lens (\SearchDatabasesByLFTags' {nextToken} -> nextToken) (\s@SearchDatabasesByLFTags' {} a -> s {nextToken = a} :: SearchDatabasesByLFTags)

-- | The maximum number of results to return.
searchDatabasesByLFTags_maxResults :: Lens.Lens' SearchDatabasesByLFTags (Prelude.Maybe Prelude.Natural)
searchDatabasesByLFTags_maxResults = Lens.lens (\SearchDatabasesByLFTags' {maxResults} -> maxResults) (\s@SearchDatabasesByLFTags' {} a -> s {maxResults = a} :: SearchDatabasesByLFTags)

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
searchDatabasesByLFTags_catalogId :: Lens.Lens' SearchDatabasesByLFTags (Prelude.Maybe Prelude.Text)
searchDatabasesByLFTags_catalogId = Lens.lens (\SearchDatabasesByLFTags' {catalogId} -> catalogId) (\s@SearchDatabasesByLFTags' {} a -> s {catalogId = a} :: SearchDatabasesByLFTags)

-- | A list of conditions (@LFTag@ structures) to search for in database
-- resources.
searchDatabasesByLFTags_expression :: Lens.Lens' SearchDatabasesByLFTags (Prelude.NonEmpty LFTag)
searchDatabasesByLFTags_expression = Lens.lens (\SearchDatabasesByLFTags' {expression} -> expression) (\s@SearchDatabasesByLFTags' {} a -> s {expression = a} :: SearchDatabasesByLFTags) Prelude.. Lens.coerced

instance Core.AWSPager SearchDatabasesByLFTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchDatabasesByLFTagsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchDatabasesByLFTagsResponse_databaseList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchDatabasesByLFTags_nextToken
          Lens..~ rs
          Lens.^? searchDatabasesByLFTagsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchDatabasesByLFTags where
  type
    AWSResponse SearchDatabasesByLFTags =
      SearchDatabasesByLFTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchDatabasesByLFTagsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "DatabaseList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchDatabasesByLFTags where
  hashWithSalt _salt SearchDatabasesByLFTags' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` expression

instance Prelude.NFData SearchDatabasesByLFTags where
  rnf SearchDatabasesByLFTags' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf expression

instance Core.ToHeaders SearchDatabasesByLFTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchDatabasesByLFTags where
  toJSON SearchDatabasesByLFTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("Expression" Core..= expression)
          ]
      )

instance Core.ToPath SearchDatabasesByLFTags where
  toPath = Prelude.const "/SearchDatabasesByLFTags"

instance Core.ToQuery SearchDatabasesByLFTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchDatabasesByLFTagsResponse' smart constructor.
data SearchDatabasesByLFTagsResponse = SearchDatabasesByLFTagsResponse'
  { -- | A continuation token, present if the current list segment is not the
    -- last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of databases that meet the LF-tag conditions.
    databaseList :: Prelude.Maybe [TaggedDatabase],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchDatabasesByLFTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchDatabasesByLFTagsResponse_nextToken' - A continuation token, present if the current list segment is not the
-- last.
--
-- 'databaseList', 'searchDatabasesByLFTagsResponse_databaseList' - A list of databases that meet the LF-tag conditions.
--
-- 'httpStatus', 'searchDatabasesByLFTagsResponse_httpStatus' - The response's http status code.
newSearchDatabasesByLFTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchDatabasesByLFTagsResponse
newSearchDatabasesByLFTagsResponse pHttpStatus_ =
  SearchDatabasesByLFTagsResponse'
    { nextToken =
        Prelude.Nothing,
      databaseList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, present if the current list segment is not the
-- last.
searchDatabasesByLFTagsResponse_nextToken :: Lens.Lens' SearchDatabasesByLFTagsResponse (Prelude.Maybe Prelude.Text)
searchDatabasesByLFTagsResponse_nextToken = Lens.lens (\SearchDatabasesByLFTagsResponse' {nextToken} -> nextToken) (\s@SearchDatabasesByLFTagsResponse' {} a -> s {nextToken = a} :: SearchDatabasesByLFTagsResponse)

-- | A list of databases that meet the LF-tag conditions.
searchDatabasesByLFTagsResponse_databaseList :: Lens.Lens' SearchDatabasesByLFTagsResponse (Prelude.Maybe [TaggedDatabase])
searchDatabasesByLFTagsResponse_databaseList = Lens.lens (\SearchDatabasesByLFTagsResponse' {databaseList} -> databaseList) (\s@SearchDatabasesByLFTagsResponse' {} a -> s {databaseList = a} :: SearchDatabasesByLFTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchDatabasesByLFTagsResponse_httpStatus :: Lens.Lens' SearchDatabasesByLFTagsResponse Prelude.Int
searchDatabasesByLFTagsResponse_httpStatus = Lens.lens (\SearchDatabasesByLFTagsResponse' {httpStatus} -> httpStatus) (\s@SearchDatabasesByLFTagsResponse' {} a -> s {httpStatus = a} :: SearchDatabasesByLFTagsResponse)

instance
  Prelude.NFData
    SearchDatabasesByLFTagsResponse
  where
  rnf SearchDatabasesByLFTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf databaseList
      `Prelude.seq` Prelude.rnf httpStatus
