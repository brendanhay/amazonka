{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.SelectResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a structured query language (SQL) @SELECT@ command, performs the corresponding search, and returns resource configurations matching the properties.
--
-- For more information about query components, see the <https://docs.aws.amazon.com/config/latest/developerguide/query-components.html __Query Components__ > section in the AWS Config Developer Guide.
module Network.AWS.Config.SelectResourceConfig
    (
    -- * Creating a request
      SelectResourceConfig (..)
    , mkSelectResourceConfig
    -- ** Request lenses
    , srcExpression
    , srcLimit
    , srcNextToken

    -- * Destructuring the response
    , SelectResourceConfigResponse (..)
    , mkSelectResourceConfigResponse
    -- ** Response lenses
    , srcrrsNextToken
    , srcrrsQueryInfo
    , srcrrsResults
    , srcrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSelectResourceConfig' smart constructor.
data SelectResourceConfig = SelectResourceConfig'
  { expression :: Types.Expression
    -- ^ The SQL query @SELECT@ command.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of query results returned on each page. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectResourceConfig' value with any optional fields omitted.
mkSelectResourceConfig
    :: Types.Expression -- ^ 'expression'
    -> SelectResourceConfig
mkSelectResourceConfig expression
  = SelectResourceConfig'{expression, limit = Core.Nothing,
                          nextToken = Core.Nothing}

-- | The SQL query @SELECT@ command.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcExpression :: Lens.Lens' SelectResourceConfig Types.Expression
srcExpression = Lens.field @"expression"
{-# INLINEABLE srcExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

-- | The maximum number of query results returned on each page. 
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcLimit :: Lens.Lens' SelectResourceConfig (Core.Maybe Core.Natural)
srcLimit = Lens.field @"limit"
{-# INLINEABLE srcLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcNextToken :: Lens.Lens' SelectResourceConfig (Core.Maybe Types.NextToken)
srcNextToken = Lens.field @"nextToken"
{-# INLINEABLE srcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery SelectResourceConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SelectResourceConfig where
        toHeaders SelectResourceConfig{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.SelectResourceConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SelectResourceConfig where
        toJSON SelectResourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Expression" Core..= expression),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest SelectResourceConfig where
        type Rs SelectResourceConfig = SelectResourceConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SelectResourceConfigResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "QueryInfo" Core.<*>
                     x Core..:? "Results"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSelectResourceConfigResponse' smart constructor.
data SelectResourceConfigResponse = SelectResourceConfigResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response. 
  , queryInfo :: Core.Maybe Types.QueryInfo
    -- ^ Returns the @QueryInfo@ object.
  , results :: Core.Maybe [Core.Text]
    -- ^ Returns the results for the SQL query.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectResourceConfigResponse' value with any optional fields omitted.
mkSelectResourceConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SelectResourceConfigResponse
mkSelectResourceConfigResponse responseStatus
  = SelectResourceConfigResponse'{nextToken = Core.Nothing,
                                  queryInfo = Core.Nothing, results = Core.Nothing, responseStatus}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrrsNextToken :: Lens.Lens' SelectResourceConfigResponse (Core.Maybe Types.NextToken)
srcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE srcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Returns the @QueryInfo@ object.
--
-- /Note:/ Consider using 'queryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrrsQueryInfo :: Lens.Lens' SelectResourceConfigResponse (Core.Maybe Types.QueryInfo)
srcrrsQueryInfo = Lens.field @"queryInfo"
{-# INLINEABLE srcrrsQueryInfo #-}
{-# DEPRECATED queryInfo "Use generic-lens or generic-optics with 'queryInfo' instead"  #-}

-- | Returns the results for the SQL query.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrrsResults :: Lens.Lens' SelectResourceConfigResponse (Core.Maybe [Core.Text])
srcrrsResults = Lens.field @"results"
{-# INLINEABLE srcrrsResults #-}
{-# DEPRECATED results "Use generic-lens or generic-optics with 'results' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrrsResponseStatus :: Lens.Lens' SelectResourceConfigResponse Core.Int
srcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
