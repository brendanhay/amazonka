{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetPartitionIndexes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the partition indexes associated with a table.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetPartitionIndexes
    (
    -- * Creating a request
      GetPartitionIndexes (..)
    , mkGetPartitionIndexes
    -- ** Request lenses
    , gpiDatabaseName
    , gpiTableName
    , gpiCatalogId
    , gpiNextToken

    -- * Destructuring the response
    , GetPartitionIndexesResponse (..)
    , mkGetPartitionIndexesResponse
    -- ** Response lenses
    , gpirrsNextToken
    , gpirrsPartitionIndexDescriptorList
    , gpirrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPartitionIndexes' smart constructor.
data GetPartitionIndexes = GetPartitionIndexes'
  { databaseName :: Types.DatabaseName
    -- ^ Specifies the name of a database from which you want to retrieve partition indexes.
  , tableName :: Types.TableName
    -- ^ Specifies the name of a table for which you want to retrieve the partition indexes.
  , catalogId :: Core.Maybe Types.CatalogId
    -- ^ The catalog ID where the table resides.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, included if this is a continuation call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPartitionIndexes' value with any optional fields omitted.
mkGetPartitionIndexes
    :: Types.DatabaseName -- ^ 'databaseName'
    -> Types.TableName -- ^ 'tableName'
    -> GetPartitionIndexes
mkGetPartitionIndexes databaseName tableName
  = GetPartitionIndexes'{databaseName, tableName,
                         catalogId = Core.Nothing, nextToken = Core.Nothing}

-- | Specifies the name of a database from which you want to retrieve partition indexes.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpiDatabaseName :: Lens.Lens' GetPartitionIndexes Types.DatabaseName
gpiDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gpiDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | Specifies the name of a table for which you want to retrieve the partition indexes.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpiTableName :: Lens.Lens' GetPartitionIndexes Types.TableName
gpiTableName = Lens.field @"tableName"
{-# INLINEABLE gpiTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The catalog ID where the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpiCatalogId :: Lens.Lens' GetPartitionIndexes (Core.Maybe Types.CatalogId)
gpiCatalogId = Lens.field @"catalogId"
{-# INLINEABLE gpiCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | A continuation token, included if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpiNextToken :: Lens.Lens' GetPartitionIndexes (Core.Maybe Types.NextToken)
gpiNextToken = Lens.field @"nextToken"
{-# INLINEABLE gpiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetPartitionIndexes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPartitionIndexes where
        toHeaders GetPartitionIndexes{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetPartitionIndexes") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPartitionIndexes where
        toJSON GetPartitionIndexes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("TableName" Core..= tableName),
                  ("CatalogId" Core..=) Core.<$> catalogId,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetPartitionIndexes where
        type Rs GetPartitionIndexes = GetPartitionIndexesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPartitionIndexesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "PartitionIndexDescriptorList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetPartitionIndexes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"partitionIndexDescriptorList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetPartitionIndexesResponse' smart constructor.
data GetPartitionIndexesResponse = GetPartitionIndexesResponse'
  { nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, present if the current list segment is not the last.
  , partitionIndexDescriptorList :: Core.Maybe [Types.PartitionIndexDescriptor]
    -- ^ A list of index descriptors.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPartitionIndexesResponse' value with any optional fields omitted.
mkGetPartitionIndexesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPartitionIndexesResponse
mkGetPartitionIndexesResponse responseStatus
  = GetPartitionIndexesResponse'{nextToken = Core.Nothing,
                                 partitionIndexDescriptorList = Core.Nothing, responseStatus}

-- | A continuation token, present if the current list segment is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpirrsNextToken :: Lens.Lens' GetPartitionIndexesResponse (Core.Maybe Types.Token)
gpirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gpirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of index descriptors.
--
-- /Note:/ Consider using 'partitionIndexDescriptorList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpirrsPartitionIndexDescriptorList :: Lens.Lens' GetPartitionIndexesResponse (Core.Maybe [Types.PartitionIndexDescriptor])
gpirrsPartitionIndexDescriptorList = Lens.field @"partitionIndexDescriptorList"
{-# INLINEABLE gpirrsPartitionIndexDescriptorList #-}
{-# DEPRECATED partitionIndexDescriptorList "Use generic-lens or generic-optics with 'partitionIndexDescriptorList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpirrsResponseStatus :: Lens.Lens' GetPartitionIndexesResponse Core.Int
gpirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
