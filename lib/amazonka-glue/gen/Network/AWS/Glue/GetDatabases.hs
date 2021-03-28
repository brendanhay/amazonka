{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDatabases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all databases defined in a given Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetDatabases
    (
    -- * Creating a request
      GetDatabases (..)
    , mkGetDatabases
    -- ** Request lenses
    , gdCatalogId
    , gdMaxResults
    , gdNextToken
    , gdResourceShareType

    -- * Destructuring the response
    , GetDatabasesResponse (..)
    , mkGetDatabasesResponse
    -- ** Response lenses
    , gdrfrsDatabaseList
    , gdrfrsNextToken
    , gdrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDatabases' smart constructor.
data GetDatabases = GetDatabases'
  { catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog from which to retrieve @Databases@ . If none is provided, the AWS account ID is used by default.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of databases to return in one response.
  , nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, if this is a continuation call.
  , resourceShareType :: Core.Maybe Types.ResourceShareType
    -- ^ Allows you to specify that you want to list the databases shared with your account. The allowable values are @FOREIGN@ or @ALL@ . 
--
--
--     * If set to @FOREIGN@ , will list the databases shared with your account. 
--
--
--     * If set to @ALL@ , will list the databases shared with your account, as well as the databases in yor local account. 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDatabases' value with any optional fields omitted.
mkGetDatabases
    :: GetDatabases
mkGetDatabases
  = GetDatabases'{catalogId = Core.Nothing,
                  maxResults = Core.Nothing, nextToken = Core.Nothing,
                  resourceShareType = Core.Nothing}

-- | The ID of the Data Catalog from which to retrieve @Databases@ . If none is provided, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdCatalogId :: Lens.Lens' GetDatabases (Core.Maybe Types.CatalogId)
gdCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gdCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | The maximum number of databases to return in one response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdMaxResults :: Lens.Lens' GetDatabases (Core.Maybe Core.Natural)
gdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdNextToken :: Lens.Lens' GetDatabases (Core.Maybe Types.Token)
gdNextToken = Lens.field @"nextToken"
{-# INLINEABLE gdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Allows you to specify that you want to list the databases shared with your account. The allowable values are @FOREIGN@ or @ALL@ . 
--
--
--     * If set to @FOREIGN@ , will list the databases shared with your account. 
--
--
--     * If set to @ALL@ , will list the databases shared with your account, as well as the databases in yor local account. 
--
--
--
-- /Note:/ Consider using 'resourceShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdResourceShareType :: Lens.Lens' GetDatabases (Core.Maybe Types.ResourceShareType)
gdResourceShareType = Lens.field @"resourceShareType"
{-# INLINEABLE gdResourceShareType #-}
{-# DEPRECATED resourceShareType "Use generic-lens or generic-optics with 'resourceShareType' instead"  #-}

instance Core.ToQuery GetDatabases where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDatabases where
        toHeaders GetDatabases{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetDatabases") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDatabases where
        toJSON GetDatabases{..}
          = Core.object
              (Core.catMaybes
                 [("CatalogId" Core..=) Core.<$> catalogId,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ResourceShareType" Core..=) Core.<$> resourceShareType])

instance Core.AWSRequest GetDatabases where
        type Rs GetDatabases = GetDatabasesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDatabasesResponse' Core.<$>
                   (x Core..:? "DatabaseList" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetDatabases where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"databaseList") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetDatabasesResponse' smart constructor.
data GetDatabasesResponse = GetDatabasesResponse'
  { databaseList :: [Types.Database]
    -- ^ A list of @Database@ objects from the specified catalog.
  , nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDatabasesResponse' value with any optional fields omitted.
mkGetDatabasesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDatabasesResponse
mkGetDatabasesResponse responseStatus
  = GetDatabasesResponse'{databaseList = Core.mempty,
                          nextToken = Core.Nothing, responseStatus}

-- | A list of @Database@ objects from the specified catalog.
--
-- /Note:/ Consider using 'databaseList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrfrsDatabaseList :: Lens.Lens' GetDatabasesResponse [Types.Database]
gdrfrsDatabaseList = Lens.field @"databaseList"
{-# INLINEABLE gdrfrsDatabaseList #-}
{-# DEPRECATED databaseList "Use generic-lens or generic-optics with 'databaseList' instead"  #-}

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrfrsNextToken :: Lens.Lens' GetDatabasesResponse (Core.Maybe Types.Token)
gdrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gdrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrfrsResponseStatus :: Lens.Lens' GetDatabasesResponse Core.Int
gdrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
