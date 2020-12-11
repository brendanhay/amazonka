{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.SearchTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches a set of tables based on properties in the table metadata as well as on the parent database. You can search against text or filter conditions.
--
-- You can only get tables that you have access to based on the security policies defined in Lake Formation. You need at least a read-only access to the table for it to be returned. If you do not have access to all the columns in the table, these columns will not be searched against when returning the list of tables back to you. If you have access to the columns but not the data in the columns, those columns and the associated metadata for those columns will be included in the search.
module Network.AWS.Glue.SearchTables
  ( -- * Creating a request
    SearchTables (..),
    mkSearchTables,

    -- ** Request lenses
    stResourceShareType,
    stSearchText,
    stFilters,
    stCatalogId,
    stSortCriteria,
    stNextToken,
    stMaxResults,

    -- * Destructuring the response
    SearchTablesResponse (..),
    mkSearchTablesResponse,

    -- ** Response lenses
    stsrsTableList,
    stsrsNextToken,
    stsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchTables' smart constructor.
data SearchTables = SearchTables'
  { resourceShareType ::
      Lude.Maybe ResourceShareType,
    searchText :: Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [PropertyPredicate],
    catalogId :: Lude.Maybe Lude.Text,
    sortCriteria :: Lude.Maybe [SortCriterion],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchTables' with the minimum fields required to make a request.
--
-- * 'catalogId' - A unique identifier, consisting of @/account_id/ @ .
-- * 'filters' - A list of key-value pairs, and a comparator used to filter the search results. Returns all entities matching the predicate.
--
-- The @Comparator@ member of the @PropertyPredicate@ struct is used only for time fields, and can be omitted for other field types. Also, when comparing string values, such as when @Key=Name@ , a fuzzy match algorithm is used. The @Key@ field (for example, the value of the @Name@ field) is split on certain punctuation characters, for example, -, :, #, etc. into tokens. Then each token is exact-match compared with the @Value@ member of @PropertyPredicate@ . For example, if @Key=Name@ and @Value=link@ , tables named @customer-link@ and @xx-link-yy@ are returned, but @xxlinkyy@ is not returned.
-- * 'maxResults' - The maximum number of tables to return in a single response.
-- * 'nextToken' - A continuation token, included if this is a continuation call.
-- * 'resourceShareType' - Allows you to specify that you want to search the tables shared with your account. The allowable values are @FOREIGN@ or @ALL@ .
--
--
--     * If set to @FOREIGN@ , will search the tables shared with your account.
--
--
--     * If set to @ALL@ , will search the tables shared with your account, as well as the tables in yor local account.
--
--
-- * 'searchText' - A string used for a text search.
--
-- Specifying a value in quotes filters based on an exact match to the value.
-- * 'sortCriteria' - A list of criteria for sorting the results by a field name, in an ascending or descending order.
mkSearchTables ::
  SearchTables
mkSearchTables =
  SearchTables'
    { resourceShareType = Lude.Nothing,
      searchText = Lude.Nothing,
      filters = Lude.Nothing,
      catalogId = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Allows you to specify that you want to search the tables shared with your account. The allowable values are @FOREIGN@ or @ALL@ .
--
--
--     * If set to @FOREIGN@ , will search the tables shared with your account.
--
--
--     * If set to @ALL@ , will search the tables shared with your account, as well as the tables in yor local account.
--
--
--
-- /Note:/ Consider using 'resourceShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stResourceShareType :: Lens.Lens' SearchTables (Lude.Maybe ResourceShareType)
stResourceShareType = Lens.lens (resourceShareType :: SearchTables -> Lude.Maybe ResourceShareType) (\s a -> s {resourceShareType = a} :: SearchTables)
{-# DEPRECATED stResourceShareType "Use generic-lens or generic-optics with 'resourceShareType' instead." #-}

-- | A string used for a text search.
--
-- Specifying a value in quotes filters based on an exact match to the value.
--
-- /Note:/ Consider using 'searchText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSearchText :: Lens.Lens' SearchTables (Lude.Maybe Lude.Text)
stSearchText = Lens.lens (searchText :: SearchTables -> Lude.Maybe Lude.Text) (\s a -> s {searchText = a} :: SearchTables)
{-# DEPRECATED stSearchText "Use generic-lens or generic-optics with 'searchText' instead." #-}

-- | A list of key-value pairs, and a comparator used to filter the search results. Returns all entities matching the predicate.
--
-- The @Comparator@ member of the @PropertyPredicate@ struct is used only for time fields, and can be omitted for other field types. Also, when comparing string values, such as when @Key=Name@ , a fuzzy match algorithm is used. The @Key@ field (for example, the value of the @Name@ field) is split on certain punctuation characters, for example, -, :, #, etc. into tokens. Then each token is exact-match compared with the @Value@ member of @PropertyPredicate@ . For example, if @Key=Name@ and @Value=link@ , tables named @customer-link@ and @xx-link-yy@ are returned, but @xxlinkyy@ is not returned.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stFilters :: Lens.Lens' SearchTables (Lude.Maybe [PropertyPredicate])
stFilters = Lens.lens (filters :: SearchTables -> Lude.Maybe [PropertyPredicate]) (\s a -> s {filters = a} :: SearchTables)
{-# DEPRECATED stFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A unique identifier, consisting of @/account_id/ @ .
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCatalogId :: Lens.Lens' SearchTables (Lude.Maybe Lude.Text)
stCatalogId = Lens.lens (catalogId :: SearchTables -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: SearchTables)
{-# DEPRECATED stCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A list of criteria for sorting the results by a field name, in an ascending or descending order.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSortCriteria :: Lens.Lens' SearchTables (Lude.Maybe [SortCriterion])
stSortCriteria = Lens.lens (sortCriteria :: SearchTables -> Lude.Maybe [SortCriterion]) (\s a -> s {sortCriteria = a} :: SearchTables)
{-# DEPRECATED stSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | A continuation token, included if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stNextToken :: Lens.Lens' SearchTables (Lude.Maybe Lude.Text)
stNextToken = Lens.lens (nextToken :: SearchTables -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchTables)
{-# DEPRECATED stNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of tables to return in a single response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stMaxResults :: Lens.Lens' SearchTables (Lude.Maybe Lude.Natural)
stMaxResults = Lens.lens (maxResults :: SearchTables -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchTables)
{-# DEPRECATED stMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest SearchTables where
  type Rs SearchTables = SearchTablesResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchTablesResponse'
            Lude.<$> (x Lude..?> "TableList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchTables where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.SearchTables" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchTables where
  toJSON SearchTables' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceShareType" Lude..=) Lude.<$> resourceShareType,
            ("SearchText" Lude..=) Lude.<$> searchText,
            ("Filters" Lude..=) Lude.<$> filters,
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("SortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchTables where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchTables where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchTablesResponse' smart constructor.
data SearchTablesResponse = SearchTablesResponse'
  { tableList ::
      Lude.Maybe [Table],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchTablesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, present if the current list segment is not the last.
-- * 'responseStatus' - The response status code.
-- * 'tableList' - A list of the requested @Table@ objects. The @SearchTables@ response returns only the tables that you have access to.
mkSearchTablesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchTablesResponse
mkSearchTablesResponse pResponseStatus_ =
  SearchTablesResponse'
    { tableList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the requested @Table@ objects. The @SearchTables@ response returns only the tables that you have access to.
--
-- /Note:/ Consider using 'tableList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrsTableList :: Lens.Lens' SearchTablesResponse (Lude.Maybe [Table])
stsrsTableList = Lens.lens (tableList :: SearchTablesResponse -> Lude.Maybe [Table]) (\s a -> s {tableList = a} :: SearchTablesResponse)
{-# DEPRECATED stsrsTableList "Use generic-lens or generic-optics with 'tableList' instead." #-}

-- | A continuation token, present if the current list segment is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrsNextToken :: Lens.Lens' SearchTablesResponse (Lude.Maybe Lude.Text)
stsrsNextToken = Lens.lens (nextToken :: SearchTablesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchTablesResponse)
{-# DEPRECATED stsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrsResponseStatus :: Lens.Lens' SearchTablesResponse Lude.Int
stsrsResponseStatus = Lens.lens (responseStatus :: SearchTablesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchTablesResponse)
{-# DEPRECATED stsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
