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
-- Module      : Amazonka.Glue.SearchTables
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches a set of tables based on properties in the table metadata as
-- well as on the parent database. You can search against text or filter
-- conditions.
--
-- You can only get tables that you have access to based on the security
-- policies defined in Lake Formation. You need at least a read-only access
-- to the table for it to be returned. If you do not have access to all the
-- columns in the table, these columns will not be searched against when
-- returning the list of tables back to you. If you have access to the
-- columns but not the data in the columns, those columns and the
-- associated metadata for those columns will be included in the search.
module Amazonka.Glue.SearchTables
  ( -- * Creating a Request
    SearchTables (..),
    newSearchTables,

    -- * Request Lenses
    searchTables_catalogId,
    searchTables_filters,
    searchTables_maxResults,
    searchTables_nextToken,
    searchTables_resourceShareType,
    searchTables_searchText,
    searchTables_sortCriteria,

    -- * Destructuring the Response
    SearchTablesResponse (..),
    newSearchTablesResponse,

    -- * Response Lenses
    searchTablesResponse_nextToken,
    searchTablesResponse_tableList,
    searchTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchTables' smart constructor.
data SearchTables = SearchTables'
  { -- | A unique identifier, consisting of @ account_id @.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs, and a comparator used to filter the search
    -- results. Returns all entities matching the predicate.
    --
    -- The @Comparator@ member of the @PropertyPredicate@ struct is used only
    -- for time fields, and can be omitted for other field types. Also, when
    -- comparing string values, such as when @Key=Name@, a fuzzy match
    -- algorithm is used. The @Key@ field (for example, the value of the @Name@
    -- field) is split on certain punctuation characters, for example, -, :, #,
    -- etc. into tokens. Then each token is exact-match compared with the
    -- @Value@ member of @PropertyPredicate@. For example, if @Key=Name@ and
    -- @Value=link@, tables named @customer-link@ and @xx-link-yy@ are
    -- returned, but @xxlinkyy@ is not returned.
    filters :: Prelude.Maybe [PropertyPredicate],
    -- | The maximum number of tables to return in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, included if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Allows you to specify that you want to search the tables shared with
    -- your account. The allowable values are @FOREIGN@ or @ALL@.
    --
    -- -   If set to @FOREIGN@, will search the tables shared with your
    --     account.
    --
    -- -   If set to @ALL@, will search the tables shared with your account, as
    --     well as the tables in yor local account.
    resourceShareType :: Prelude.Maybe ResourceShareType,
    -- | A string used for a text search.
    --
    -- Specifying a value in quotes filters based on an exact match to the
    -- value.
    searchText :: Prelude.Maybe Prelude.Text,
    -- | A list of criteria for sorting the results by a field name, in an
    -- ascending or descending order.
    sortCriteria :: Prelude.Maybe [SortCriterion]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'searchTables_catalogId' - A unique identifier, consisting of @ account_id @.
--
-- 'filters', 'searchTables_filters' - A list of key-value pairs, and a comparator used to filter the search
-- results. Returns all entities matching the predicate.
--
-- The @Comparator@ member of the @PropertyPredicate@ struct is used only
-- for time fields, and can be omitted for other field types. Also, when
-- comparing string values, such as when @Key=Name@, a fuzzy match
-- algorithm is used. The @Key@ field (for example, the value of the @Name@
-- field) is split on certain punctuation characters, for example, -, :, #,
-- etc. into tokens. Then each token is exact-match compared with the
-- @Value@ member of @PropertyPredicate@. For example, if @Key=Name@ and
-- @Value=link@, tables named @customer-link@ and @xx-link-yy@ are
-- returned, but @xxlinkyy@ is not returned.
--
-- 'maxResults', 'searchTables_maxResults' - The maximum number of tables to return in a single response.
--
-- 'nextToken', 'searchTables_nextToken' - A continuation token, included if this is a continuation call.
--
-- 'resourceShareType', 'searchTables_resourceShareType' - Allows you to specify that you want to search the tables shared with
-- your account. The allowable values are @FOREIGN@ or @ALL@.
--
-- -   If set to @FOREIGN@, will search the tables shared with your
--     account.
--
-- -   If set to @ALL@, will search the tables shared with your account, as
--     well as the tables in yor local account.
--
-- 'searchText', 'searchTables_searchText' - A string used for a text search.
--
-- Specifying a value in quotes filters based on an exact match to the
-- value.
--
-- 'sortCriteria', 'searchTables_sortCriteria' - A list of criteria for sorting the results by a field name, in an
-- ascending or descending order.
newSearchTables ::
  SearchTables
newSearchTables =
  SearchTables'
    { catalogId = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceShareType = Prelude.Nothing,
      searchText = Prelude.Nothing,
      sortCriteria = Prelude.Nothing
    }

-- | A unique identifier, consisting of @ account_id @.
searchTables_catalogId :: Lens.Lens' SearchTables (Prelude.Maybe Prelude.Text)
searchTables_catalogId = Lens.lens (\SearchTables' {catalogId} -> catalogId) (\s@SearchTables' {} a -> s {catalogId = a} :: SearchTables)

-- | A list of key-value pairs, and a comparator used to filter the search
-- results. Returns all entities matching the predicate.
--
-- The @Comparator@ member of the @PropertyPredicate@ struct is used only
-- for time fields, and can be omitted for other field types. Also, when
-- comparing string values, such as when @Key=Name@, a fuzzy match
-- algorithm is used. The @Key@ field (for example, the value of the @Name@
-- field) is split on certain punctuation characters, for example, -, :, #,
-- etc. into tokens. Then each token is exact-match compared with the
-- @Value@ member of @PropertyPredicate@. For example, if @Key=Name@ and
-- @Value=link@, tables named @customer-link@ and @xx-link-yy@ are
-- returned, but @xxlinkyy@ is not returned.
searchTables_filters :: Lens.Lens' SearchTables (Prelude.Maybe [PropertyPredicate])
searchTables_filters = Lens.lens (\SearchTables' {filters} -> filters) (\s@SearchTables' {} a -> s {filters = a} :: SearchTables) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of tables to return in a single response.
searchTables_maxResults :: Lens.Lens' SearchTables (Prelude.Maybe Prelude.Natural)
searchTables_maxResults = Lens.lens (\SearchTables' {maxResults} -> maxResults) (\s@SearchTables' {} a -> s {maxResults = a} :: SearchTables)

-- | A continuation token, included if this is a continuation call.
searchTables_nextToken :: Lens.Lens' SearchTables (Prelude.Maybe Prelude.Text)
searchTables_nextToken = Lens.lens (\SearchTables' {nextToken} -> nextToken) (\s@SearchTables' {} a -> s {nextToken = a} :: SearchTables)

-- | Allows you to specify that you want to search the tables shared with
-- your account. The allowable values are @FOREIGN@ or @ALL@.
--
-- -   If set to @FOREIGN@, will search the tables shared with your
--     account.
--
-- -   If set to @ALL@, will search the tables shared with your account, as
--     well as the tables in yor local account.
searchTables_resourceShareType :: Lens.Lens' SearchTables (Prelude.Maybe ResourceShareType)
searchTables_resourceShareType = Lens.lens (\SearchTables' {resourceShareType} -> resourceShareType) (\s@SearchTables' {} a -> s {resourceShareType = a} :: SearchTables)

-- | A string used for a text search.
--
-- Specifying a value in quotes filters based on an exact match to the
-- value.
searchTables_searchText :: Lens.Lens' SearchTables (Prelude.Maybe Prelude.Text)
searchTables_searchText = Lens.lens (\SearchTables' {searchText} -> searchText) (\s@SearchTables' {} a -> s {searchText = a} :: SearchTables)

-- | A list of criteria for sorting the results by a field name, in an
-- ascending or descending order.
searchTables_sortCriteria :: Lens.Lens' SearchTables (Prelude.Maybe [SortCriterion])
searchTables_sortCriteria = Lens.lens (\SearchTables' {sortCriteria} -> sortCriteria) (\s@SearchTables' {} a -> s {sortCriteria = a} :: SearchTables) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest SearchTables where
  type AWSResponse SearchTables = SearchTablesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchTablesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TableList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchTables where
  hashWithSalt _salt SearchTables' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceShareType
      `Prelude.hashWithSalt` searchText
      `Prelude.hashWithSalt` sortCriteria

instance Prelude.NFData SearchTables where
  rnf SearchTables' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceShareType
      `Prelude.seq` Prelude.rnf searchText
      `Prelude.seq` Prelude.rnf sortCriteria

instance Data.ToHeaders SearchTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.SearchTables" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchTables where
  toJSON SearchTables' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceShareType" Data..=)
              Prelude.<$> resourceShareType,
            ("SearchText" Data..=) Prelude.<$> searchText,
            ("SortCriteria" Data..=) Prelude.<$> sortCriteria
          ]
      )

instance Data.ToPath SearchTables where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchTables where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchTablesResponse' smart constructor.
data SearchTablesResponse = SearchTablesResponse'
  { -- | A continuation token, present if the current list segment is not the
    -- last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the requested @Table@ objects. The @SearchTables@ response
    -- returns only the tables that you have access to.
    tableList :: Prelude.Maybe [Table],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchTablesResponse_nextToken' - A continuation token, present if the current list segment is not the
-- last.
--
-- 'tableList', 'searchTablesResponse_tableList' - A list of the requested @Table@ objects. The @SearchTables@ response
-- returns only the tables that you have access to.
--
-- 'httpStatus', 'searchTablesResponse_httpStatus' - The response's http status code.
newSearchTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchTablesResponse
newSearchTablesResponse pHttpStatus_ =
  SearchTablesResponse'
    { nextToken = Prelude.Nothing,
      tableList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, present if the current list segment is not the
-- last.
searchTablesResponse_nextToken :: Lens.Lens' SearchTablesResponse (Prelude.Maybe Prelude.Text)
searchTablesResponse_nextToken = Lens.lens (\SearchTablesResponse' {nextToken} -> nextToken) (\s@SearchTablesResponse' {} a -> s {nextToken = a} :: SearchTablesResponse)

-- | A list of the requested @Table@ objects. The @SearchTables@ response
-- returns only the tables that you have access to.
searchTablesResponse_tableList :: Lens.Lens' SearchTablesResponse (Prelude.Maybe [Table])
searchTablesResponse_tableList = Lens.lens (\SearchTablesResponse' {tableList} -> tableList) (\s@SearchTablesResponse' {} a -> s {tableList = a} :: SearchTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchTablesResponse_httpStatus :: Lens.Lens' SearchTablesResponse Prelude.Int
searchTablesResponse_httpStatus = Lens.lens (\SearchTablesResponse' {httpStatus} -> httpStatus) (\s@SearchTablesResponse' {} a -> s {httpStatus = a} :: SearchTablesResponse)

instance Prelude.NFData SearchTablesResponse where
  rnf SearchTablesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tableList
      `Prelude.seq` Prelude.rnf httpStatus
