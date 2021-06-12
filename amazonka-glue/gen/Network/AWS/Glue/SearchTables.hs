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
-- Module      : Network.AWS.Glue.SearchTables
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glue.SearchTables
  ( -- * Creating a Request
    SearchTables (..),
    newSearchTables,

    -- * Request Lenses
    searchTables_nextToken,
    searchTables_sortCriteria,
    searchTables_catalogId,
    searchTables_maxResults,
    searchTables_searchText,
    searchTables_resourceShareType,
    searchTables_filters,

    -- * Destructuring the Response
    SearchTablesResponse (..),
    newSearchTablesResponse,

    -- * Response Lenses
    searchTablesResponse_nextToken,
    searchTablesResponse_tableList,
    searchTablesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchTables' smart constructor.
data SearchTables = SearchTables'
  { -- | A continuation token, included if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of criteria for sorting the results by a field name, in an
    -- ascending or descending order.
    sortCriteria :: Core.Maybe [SortCriterion],
    -- | A unique identifier, consisting of @ account_id @.
    catalogId :: Core.Maybe Core.Text,
    -- | The maximum number of tables to return in a single response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string used for a text search.
    --
    -- Specifying a value in quotes filters based on an exact match to the
    -- value.
    searchText :: Core.Maybe Core.Text,
    -- | Allows you to specify that you want to search the tables shared with
    -- your account. The allowable values are @FOREIGN@ or @ALL@.
    --
    -- -   If set to @FOREIGN@, will search the tables shared with your
    --     account.
    --
    -- -   If set to @ALL@, will search the tables shared with your account, as
    --     well as the tables in yor local account.
    resourceShareType :: Core.Maybe ResourceShareType,
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
    filters :: Core.Maybe [PropertyPredicate]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchTables_nextToken' - A continuation token, included if this is a continuation call.
--
-- 'sortCriteria', 'searchTables_sortCriteria' - A list of criteria for sorting the results by a field name, in an
-- ascending or descending order.
--
-- 'catalogId', 'searchTables_catalogId' - A unique identifier, consisting of @ account_id @.
--
-- 'maxResults', 'searchTables_maxResults' - The maximum number of tables to return in a single response.
--
-- 'searchText', 'searchTables_searchText' - A string used for a text search.
--
-- Specifying a value in quotes filters based on an exact match to the
-- value.
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
newSearchTables ::
  SearchTables
newSearchTables =
  SearchTables'
    { nextToken = Core.Nothing,
      sortCriteria = Core.Nothing,
      catalogId = Core.Nothing,
      maxResults = Core.Nothing,
      searchText = Core.Nothing,
      resourceShareType = Core.Nothing,
      filters = Core.Nothing
    }

-- | A continuation token, included if this is a continuation call.
searchTables_nextToken :: Lens.Lens' SearchTables (Core.Maybe Core.Text)
searchTables_nextToken = Lens.lens (\SearchTables' {nextToken} -> nextToken) (\s@SearchTables' {} a -> s {nextToken = a} :: SearchTables)

-- | A list of criteria for sorting the results by a field name, in an
-- ascending or descending order.
searchTables_sortCriteria :: Lens.Lens' SearchTables (Core.Maybe [SortCriterion])
searchTables_sortCriteria = Lens.lens (\SearchTables' {sortCriteria} -> sortCriteria) (\s@SearchTables' {} a -> s {sortCriteria = a} :: SearchTables) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier, consisting of @ account_id @.
searchTables_catalogId :: Lens.Lens' SearchTables (Core.Maybe Core.Text)
searchTables_catalogId = Lens.lens (\SearchTables' {catalogId} -> catalogId) (\s@SearchTables' {} a -> s {catalogId = a} :: SearchTables)

-- | The maximum number of tables to return in a single response.
searchTables_maxResults :: Lens.Lens' SearchTables (Core.Maybe Core.Natural)
searchTables_maxResults = Lens.lens (\SearchTables' {maxResults} -> maxResults) (\s@SearchTables' {} a -> s {maxResults = a} :: SearchTables)

-- | A string used for a text search.
--
-- Specifying a value in quotes filters based on an exact match to the
-- value.
searchTables_searchText :: Lens.Lens' SearchTables (Core.Maybe Core.Text)
searchTables_searchText = Lens.lens (\SearchTables' {searchText} -> searchText) (\s@SearchTables' {} a -> s {searchText = a} :: SearchTables)

-- | Allows you to specify that you want to search the tables shared with
-- your account. The allowable values are @FOREIGN@ or @ALL@.
--
-- -   If set to @FOREIGN@, will search the tables shared with your
--     account.
--
-- -   If set to @ALL@, will search the tables shared with your account, as
--     well as the tables in yor local account.
searchTables_resourceShareType :: Lens.Lens' SearchTables (Core.Maybe ResourceShareType)
searchTables_resourceShareType = Lens.lens (\SearchTables' {resourceShareType} -> resourceShareType) (\s@SearchTables' {} a -> s {resourceShareType = a} :: SearchTables)

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
searchTables_filters :: Lens.Lens' SearchTables (Core.Maybe [PropertyPredicate])
searchTables_filters = Lens.lens (\SearchTables' {filters} -> filters) (\s@SearchTables' {} a -> s {filters = a} :: SearchTables) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest SearchTables where
  type AWSResponse SearchTables = SearchTablesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchTablesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "TableList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchTables

instance Core.NFData SearchTables

instance Core.ToHeaders SearchTables where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.SearchTables" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchTables where
  toJSON SearchTables' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria,
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SearchText" Core..=) Core.<$> searchText,
            ("ResourceShareType" Core..=)
              Core.<$> resourceShareType,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath SearchTables where
  toPath = Core.const "/"

instance Core.ToQuery SearchTables where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchTablesResponse' smart constructor.
data SearchTablesResponse = SearchTablesResponse'
  { -- | A continuation token, present if the current list segment is not the
    -- last.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of the requested @Table@ objects. The @SearchTables@ response
    -- returns only the tables that you have access to.
    tableList :: Core.Maybe [Table],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SearchTablesResponse
newSearchTablesResponse pHttpStatus_ =
  SearchTablesResponse'
    { nextToken = Core.Nothing,
      tableList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, present if the current list segment is not the
-- last.
searchTablesResponse_nextToken :: Lens.Lens' SearchTablesResponse (Core.Maybe Core.Text)
searchTablesResponse_nextToken = Lens.lens (\SearchTablesResponse' {nextToken} -> nextToken) (\s@SearchTablesResponse' {} a -> s {nextToken = a} :: SearchTablesResponse)

-- | A list of the requested @Table@ objects. The @SearchTables@ response
-- returns only the tables that you have access to.
searchTablesResponse_tableList :: Lens.Lens' SearchTablesResponse (Core.Maybe [Table])
searchTablesResponse_tableList = Lens.lens (\SearchTablesResponse' {tableList} -> tableList) (\s@SearchTablesResponse' {} a -> s {tableList = a} :: SearchTablesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchTablesResponse_httpStatus :: Lens.Lens' SearchTablesResponse Core.Int
searchTablesResponse_httpStatus = Lens.lens (\SearchTablesResponse' {httpStatus} -> httpStatus) (\s@SearchTablesResponse' {} a -> s {httpStatus = a} :: SearchTablesResponse)

instance Core.NFData SearchTablesResponse
