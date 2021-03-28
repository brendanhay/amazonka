{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.UpdateContributorInsights
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status for contributor insights for a specific table or index.
module Network.AWS.DynamoDB.UpdateContributorInsights
    (
    -- * Creating a request
      UpdateContributorInsights (..)
    , mkUpdateContributorInsights
    -- ** Request lenses
    , uciTableName
    , uciContributorInsightsAction
    , uciIndexName

    -- * Destructuring the response
    , UpdateContributorInsightsResponse (..)
    , mkUpdateContributorInsightsResponse
    -- ** Response lenses
    , ucirrsContributorInsightsStatus
    , ucirrsIndexName
    , ucirrsTableName
    , ucirrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateContributorInsights' smart constructor.
data UpdateContributorInsights = UpdateContributorInsights'
  { tableName :: Types.TableName
    -- ^ The name of the table.
  , contributorInsightsAction :: Types.ContributorInsightsAction
    -- ^ Represents the contributor insights action.
  , indexName :: Core.Maybe Types.IndexName
    -- ^ The global secondary index name, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContributorInsights' value with any optional fields omitted.
mkUpdateContributorInsights
    :: Types.TableName -- ^ 'tableName'
    -> Types.ContributorInsightsAction -- ^ 'contributorInsightsAction'
    -> UpdateContributorInsights
mkUpdateContributorInsights tableName contributorInsightsAction
  = UpdateContributorInsights'{tableName, contributorInsightsAction,
                               indexName = Core.Nothing}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciTableName :: Lens.Lens' UpdateContributorInsights Types.TableName
uciTableName = Lens.field @"tableName"
{-# INLINEABLE uciTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Represents the contributor insights action.
--
-- /Note:/ Consider using 'contributorInsightsAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciContributorInsightsAction :: Lens.Lens' UpdateContributorInsights Types.ContributorInsightsAction
uciContributorInsightsAction = Lens.field @"contributorInsightsAction"
{-# INLINEABLE uciContributorInsightsAction #-}
{-# DEPRECATED contributorInsightsAction "Use generic-lens or generic-optics with 'contributorInsightsAction' instead"  #-}

-- | The global secondary index name, if applicable.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uciIndexName :: Lens.Lens' UpdateContributorInsights (Core.Maybe Types.IndexName)
uciIndexName = Lens.field @"indexName"
{-# INLINEABLE uciIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

instance Core.ToQuery UpdateContributorInsights where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateContributorInsights where
        toHeaders UpdateContributorInsights{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.UpdateContributorInsights")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON UpdateContributorInsights where
        toJSON UpdateContributorInsights{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TableName" Core..= tableName),
                  Core.Just
                    ("ContributorInsightsAction" Core..= contributorInsightsAction),
                  ("IndexName" Core..=) Core.<$> indexName])

instance Core.AWSRequest UpdateContributorInsights where
        type Rs UpdateContributorInsights =
             UpdateContributorInsightsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateContributorInsightsResponse' Core.<$>
                   (x Core..:? "ContributorInsightsStatus") Core.<*>
                     x Core..:? "IndexName"
                     Core.<*> x Core..:? "TableName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateContributorInsightsResponse' smart constructor.
data UpdateContributorInsightsResponse = UpdateContributorInsightsResponse'
  { contributorInsightsStatus :: Core.Maybe Types.ContributorInsightsStatus
    -- ^ The status of contributor insights
  , indexName :: Core.Maybe Types.IndexName
    -- ^ The name of the global secondary index, if applicable.
  , tableName :: Core.Maybe Types.TableName
    -- ^ The name of the table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateContributorInsightsResponse' value with any optional fields omitted.
mkUpdateContributorInsightsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateContributorInsightsResponse
mkUpdateContributorInsightsResponse responseStatus
  = UpdateContributorInsightsResponse'{contributorInsightsStatus =
                                         Core.Nothing,
                                       indexName = Core.Nothing, tableName = Core.Nothing,
                                       responseStatus}

-- | The status of contributor insights
--
-- /Note:/ Consider using 'contributorInsightsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirrsContributorInsightsStatus :: Lens.Lens' UpdateContributorInsightsResponse (Core.Maybe Types.ContributorInsightsStatus)
ucirrsContributorInsightsStatus = Lens.field @"contributorInsightsStatus"
{-# INLINEABLE ucirrsContributorInsightsStatus #-}
{-# DEPRECATED contributorInsightsStatus "Use generic-lens or generic-optics with 'contributorInsightsStatus' instead"  #-}

-- | The name of the global secondary index, if applicable.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirrsIndexName :: Lens.Lens' UpdateContributorInsightsResponse (Core.Maybe Types.IndexName)
ucirrsIndexName = Lens.field @"indexName"
{-# INLINEABLE ucirrsIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirrsTableName :: Lens.Lens' UpdateContributorInsightsResponse (Core.Maybe Types.TableName)
ucirrsTableName = Lens.field @"tableName"
{-# INLINEABLE ucirrsTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucirrsResponseStatus :: Lens.Lens' UpdateContributorInsightsResponse Core.Int
ucirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
