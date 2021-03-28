{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SearchTables (..)
    , mkSearchTables
    -- ** Request lenses
    , stCatalogId
    , stFilters
    , stMaxResults
    , stNextToken
    , stResourceShareType
    , stSearchText
    , stSortCriteria

    -- * Destructuring the response
    , SearchTablesResponse (..)
    , mkSearchTablesResponse
    -- ** Response lenses
    , strfrsNextToken
    , strfrsTableList
    , strfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchTables' smart constructor.
data SearchTables = SearchTables'
  { catalogId :: Core.Maybe Types.CatalogId
    -- ^ A unique identifier, consisting of @/account_id/ @ .
  , filters :: Core.Maybe [Types.PropertyPredicate]
    -- ^ A list of key-value pairs, and a comparator used to filter the search results. Returns all entities matching the predicate.
--
-- The @Comparator@ member of the @PropertyPredicate@ struct is used only for time fields, and can be omitted for other field types. Also, when comparing string values, such as when @Key=Name@ , a fuzzy match algorithm is used. The @Key@ field (for example, the value of the @Name@ field) is split on certain punctuation characters, for example, -, :, #, etc. into tokens. Then each token is exact-match compared with the @Value@ member of @PropertyPredicate@ . For example, if @Key=Name@ and @Value=link@ , tables named @customer-link@ and @xx-link-yy@ are returned, but @xxlinkyy@ is not returned.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of tables to return in a single response.
  , nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, included if this is a continuation call.
  , resourceShareType :: Core.Maybe Types.ResourceShareType
    -- ^ Allows you to specify that you want to search the tables shared with your account. The allowable values are @FOREIGN@ or @ALL@ . 
--
--
--     * If set to @FOREIGN@ , will search the tables shared with your account. 
--
--
--     * If set to @ALL@ , will search the tables shared with your account, as well as the tables in yor local account. 
--
--
  , searchText :: Core.Maybe Types.SearchText
    -- ^ A string used for a text search.
--
-- Specifying a value in quotes filters based on an exact match to the value.
  , sortCriteria :: Core.Maybe [Types.SortCriterion]
    -- ^ A list of criteria for sorting the results by a field name, in an ascending or descending order.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchTables' value with any optional fields omitted.
mkSearchTables
    :: SearchTables
mkSearchTables
  = SearchTables'{catalogId = Core.Nothing, filters = Core.Nothing,
                  maxResults = Core.Nothing, nextToken = Core.Nothing,
                  resourceShareType = Core.Nothing, searchText = Core.Nothing,
                  sortCriteria = Core.Nothing}

-- | A unique identifier, consisting of @/account_id/ @ .
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCatalogId :: Lens.Lens' SearchTables (Core.Maybe Types.CatalogId)
stCatalogId = Lens.field @"catalogId"
{-# INLINEABLE stCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | A list of key-value pairs, and a comparator used to filter the search results. Returns all entities matching the predicate.
--
-- The @Comparator@ member of the @PropertyPredicate@ struct is used only for time fields, and can be omitted for other field types. Also, when comparing string values, such as when @Key=Name@ , a fuzzy match algorithm is used. The @Key@ field (for example, the value of the @Name@ field) is split on certain punctuation characters, for example, -, :, #, etc. into tokens. Then each token is exact-match compared with the @Value@ member of @PropertyPredicate@ . For example, if @Key=Name@ and @Value=link@ , tables named @customer-link@ and @xx-link-yy@ are returned, but @xxlinkyy@ is not returned.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stFilters :: Lens.Lens' SearchTables (Core.Maybe [Types.PropertyPredicate])
stFilters = Lens.field @"filters"
{-# INLINEABLE stFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of tables to return in a single response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stMaxResults :: Lens.Lens' SearchTables (Core.Maybe Core.Natural)
stMaxResults = Lens.field @"maxResults"
{-# INLINEABLE stMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, included if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stNextToken :: Lens.Lens' SearchTables (Core.Maybe Types.Token)
stNextToken = Lens.field @"nextToken"
{-# INLINEABLE stNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

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
stResourceShareType :: Lens.Lens' SearchTables (Core.Maybe Types.ResourceShareType)
stResourceShareType = Lens.field @"resourceShareType"
{-# INLINEABLE stResourceShareType #-}
{-# DEPRECATED resourceShareType "Use generic-lens or generic-optics with 'resourceShareType' instead"  #-}

-- | A string used for a text search.
--
-- Specifying a value in quotes filters based on an exact match to the value.
--
-- /Note:/ Consider using 'searchText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSearchText :: Lens.Lens' SearchTables (Core.Maybe Types.SearchText)
stSearchText = Lens.field @"searchText"
{-# INLINEABLE stSearchText #-}
{-# DEPRECATED searchText "Use generic-lens or generic-optics with 'searchText' instead"  #-}

-- | A list of criteria for sorting the results by a field name, in an ascending or descending order.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stSortCriteria :: Lens.Lens' SearchTables (Core.Maybe [Types.SortCriterion])
stSortCriteria = Lens.field @"sortCriteria"
{-# INLINEABLE stSortCriteria #-}
{-# DEPRECATED sortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead"  #-}

instance Core.ToQuery SearchTables where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SearchTables where
        toHeaders SearchTables{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.SearchTables") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SearchTables where
        toJSON SearchTables{..}
          = Core.object
              (Core.catMaybes
                 [("CatalogId" Core..=) Core.<$> catalogId,
                  ("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ResourceShareType" Core..=) Core.<$> resourceShareType,
                  ("SearchText" Core..=) Core.<$> searchText,
                  ("SortCriteria" Core..=) Core.<$> sortCriteria])

instance Core.AWSRequest SearchTables where
        type Rs SearchTables = SearchTablesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SearchTablesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "TableList" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSearchTablesResponse' smart constructor.
data SearchTablesResponse = SearchTablesResponse'
  { nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, present if the current list segment is not the last.
  , tableList :: Core.Maybe [Types.Table]
    -- ^ A list of the requested @Table@ objects. The @SearchTables@ response returns only the tables that you have access to.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SearchTablesResponse' value with any optional fields omitted.
mkSearchTablesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchTablesResponse
mkSearchTablesResponse responseStatus
  = SearchTablesResponse'{nextToken = Core.Nothing,
                          tableList = Core.Nothing, responseStatus}

-- | A continuation token, present if the current list segment is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strfrsNextToken :: Lens.Lens' SearchTablesResponse (Core.Maybe Types.Token)
strfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE strfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of the requested @Table@ objects. The @SearchTables@ response returns only the tables that you have access to.
--
-- /Note:/ Consider using 'tableList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strfrsTableList :: Lens.Lens' SearchTablesResponse (Core.Maybe [Types.Table])
strfrsTableList = Lens.field @"tableList"
{-# INLINEABLE strfrsTableList #-}
{-# DEPRECATED tableList "Use generic-lens or generic-optics with 'tableList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strfrsResponseStatus :: Lens.Lens' SearchTablesResponse Core.Int
strfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE strfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
