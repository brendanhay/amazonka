{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ListSchemaExtensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all schema extensions applied to a Microsoft AD Directory.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListSchemaExtensions
    (
    -- * Creating a request
      ListSchemaExtensions (..)
    , mkListSchemaExtensions
    -- ** Request lenses
    , lseDirectoryId
    , lseLimit
    , lseNextToken

    -- * Destructuring the response
    , ListSchemaExtensionsResponse (..)
    , mkListSchemaExtensionsResponse
    -- ** Response lenses
    , lserrsNextToken
    , lserrsSchemaExtensionsInfo
    , lserrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSchemaExtensions' smart constructor.
data ListSchemaExtensions = ListSchemaExtensions'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory from which to retrieve the schema extension information.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @ListSchemaExtensions.NextToken@ value from a previous call to @ListSchemaExtensions@ . Pass null if this is the first call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSchemaExtensions' value with any optional fields omitted.
mkListSchemaExtensions
    :: Types.DirectoryId -- ^ 'directoryId'
    -> ListSchemaExtensions
mkListSchemaExtensions directoryId
  = ListSchemaExtensions'{directoryId, limit = Core.Nothing,
                          nextToken = Core.Nothing}

-- | The identifier of the directory from which to retrieve the schema extension information.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lseDirectoryId :: Lens.Lens' ListSchemaExtensions Types.DirectoryId
lseDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE lseDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lseLimit :: Lens.Lens' ListSchemaExtensions (Core.Maybe Core.Natural)
lseLimit = Lens.field @"limit"
{-# INLINEABLE lseLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @ListSchemaExtensions.NextToken@ value from a previous call to @ListSchemaExtensions@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lseNextToken :: Lens.Lens' ListSchemaExtensions (Core.Maybe Types.NextToken)
lseNextToken = Lens.field @"nextToken"
{-# INLINEABLE lseNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSchemaExtensions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSchemaExtensions where
        toHeaders ListSchemaExtensions{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.ListSchemaExtensions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSchemaExtensions where
        toJSON ListSchemaExtensions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListSchemaExtensions where
        type Rs ListSchemaExtensions = ListSchemaExtensionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSchemaExtensionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SchemaExtensionsInfo"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSchemaExtensions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"schemaExtensionsInfo" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSchemaExtensionsResponse' smart constructor.
data ListSchemaExtensionsResponse = ListSchemaExtensionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to retrieve the next set of items.
  , schemaExtensionsInfo :: Core.Maybe [Types.SchemaExtensionInfo]
    -- ^ Information about the schema extensions applied to the directory.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListSchemaExtensionsResponse' value with any optional fields omitted.
mkListSchemaExtensionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSchemaExtensionsResponse
mkListSchemaExtensionsResponse responseStatus
  = ListSchemaExtensionsResponse'{nextToken = Core.Nothing,
                                  schemaExtensionsInfo = Core.Nothing, responseStatus}

-- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lserrsNextToken :: Lens.Lens' ListSchemaExtensionsResponse (Core.Maybe Types.NextToken)
lserrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lserrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the schema extensions applied to the directory.
--
-- /Note:/ Consider using 'schemaExtensionsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lserrsSchemaExtensionsInfo :: Lens.Lens' ListSchemaExtensionsResponse (Core.Maybe [Types.SchemaExtensionInfo])
lserrsSchemaExtensionsInfo = Lens.field @"schemaExtensionsInfo"
{-# INLINEABLE lserrsSchemaExtensionsInfo #-}
{-# DEPRECATED schemaExtensionsInfo "Use generic-lens or generic-optics with 'schemaExtensionsInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lserrsResponseStatus :: Lens.Lens' ListSchemaExtensionsResponse Core.Int
lserrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lserrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
