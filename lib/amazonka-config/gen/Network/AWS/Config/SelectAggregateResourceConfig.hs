{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.SelectAggregateResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a structured query language (SQL) SELECT command and an aggregator to query configuration state of AWS resources across multiple accounts and regions, performs the corresponding search, and returns resource configurations matching the properties.
--
-- For more information about query components, see the <https://docs.aws.amazon.com/config/latest/developerguide/query-components.html __Query Components__ > section in the AWS Config Developer Guide.
module Network.AWS.Config.SelectAggregateResourceConfig
    (
    -- * Creating a request
      SelectAggregateResourceConfig (..)
    , mkSelectAggregateResourceConfig
    -- ** Request lenses
    , sarcExpression
    , sarcConfigurationAggregatorName
    , sarcLimit
    , sarcMaxResults
    , sarcNextToken

    -- * Destructuring the response
    , SelectAggregateResourceConfigResponse (..)
    , mkSelectAggregateResourceConfigResponse
    -- ** Response lenses
    , sarcrrsNextToken
    , sarcrrsQueryInfo
    , sarcrrsResults
    , sarcrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSelectAggregateResourceConfig' smart constructor.
data SelectAggregateResourceConfig = SelectAggregateResourceConfig'
  { expression :: Types.Expression
    -- ^ The SQL query SELECT command. 
  , configurationAggregatorName :: Types.ConfigurationAggregatorName
    -- ^ The name of the configuration aggregator.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of query results returned on each page. 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of query results returned on each page. AWS Config also allows the Limit request parameter.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The nextToken string returned in a previous request that you use to request the next page of results in a paginated response. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectAggregateResourceConfig' value with any optional fields omitted.
mkSelectAggregateResourceConfig
    :: Types.Expression -- ^ 'expression'
    -> Types.ConfigurationAggregatorName -- ^ 'configurationAggregatorName'
    -> SelectAggregateResourceConfig
mkSelectAggregateResourceConfig expression
  configurationAggregatorName
  = SelectAggregateResourceConfig'{expression,
                                   configurationAggregatorName, limit = Core.Nothing,
                                   maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The SQL query SELECT command. 
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcExpression :: Lens.Lens' SelectAggregateResourceConfig Types.Expression
sarcExpression = Lens.field @"expression"
{-# INLINEABLE sarcExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcConfigurationAggregatorName :: Lens.Lens' SelectAggregateResourceConfig Types.ConfigurationAggregatorName
sarcConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# INLINEABLE sarcConfigurationAggregatorName #-}
{-# DEPRECATED configurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead"  #-}

-- | The maximum number of query results returned on each page. 
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcLimit :: Lens.Lens' SelectAggregateResourceConfig (Core.Maybe Core.Natural)
sarcLimit = Lens.field @"limit"
{-# INLINEABLE sarcLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The maximum number of query results returned on each page. AWS Config also allows the Limit request parameter.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcMaxResults :: Lens.Lens' SelectAggregateResourceConfig (Core.Maybe Core.Natural)
sarcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE sarcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The nextToken string returned in a previous request that you use to request the next page of results in a paginated response. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcNextToken :: Lens.Lens' SelectAggregateResourceConfig (Core.Maybe Types.NextToken)
sarcNextToken = Lens.field @"nextToken"
{-# INLINEABLE sarcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery SelectAggregateResourceConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SelectAggregateResourceConfig where
        toHeaders SelectAggregateResourceConfig{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.SelectAggregateResourceConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SelectAggregateResourceConfig where
        toJSON SelectAggregateResourceConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Expression" Core..= expression),
                  Core.Just
                    ("ConfigurationAggregatorName" Core..=
                       configurationAggregatorName),
                  ("Limit" Core..=) Core.<$> limit,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest SelectAggregateResourceConfig where
        type Rs SelectAggregateResourceConfig =
             SelectAggregateResourceConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SelectAggregateResourceConfigResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "QueryInfo" Core.<*>
                     x Core..:? "Results"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSelectAggregateResourceConfigResponse' smart constructor.
data SelectAggregateResourceConfigResponse = SelectAggregateResourceConfigResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The nextToken string returned in a previous request that you use to request the next page of results in a paginated response. 
  , queryInfo :: Core.Maybe Types.QueryInfo
  , results :: Core.Maybe [Core.Text]
    -- ^ Returns the results for the SQL query.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectAggregateResourceConfigResponse' value with any optional fields omitted.
mkSelectAggregateResourceConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SelectAggregateResourceConfigResponse
mkSelectAggregateResourceConfigResponse responseStatus
  = SelectAggregateResourceConfigResponse'{nextToken = Core.Nothing,
                                           queryInfo = Core.Nothing, results = Core.Nothing,
                                           responseStatus}

-- | The nextToken string returned in a previous request that you use to request the next page of results in a paginated response. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcrrsNextToken :: Lens.Lens' SelectAggregateResourceConfigResponse (Core.Maybe Types.NextToken)
sarcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE sarcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'queryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcrrsQueryInfo :: Lens.Lens' SelectAggregateResourceConfigResponse (Core.Maybe Types.QueryInfo)
sarcrrsQueryInfo = Lens.field @"queryInfo"
{-# INLINEABLE sarcrrsQueryInfo #-}
{-# DEPRECATED queryInfo "Use generic-lens or generic-optics with 'queryInfo' instead"  #-}

-- | Returns the results for the SQL query.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcrrsResults :: Lens.Lens' SelectAggregateResourceConfigResponse (Core.Maybe [Core.Text])
sarcrrsResults = Lens.field @"results"
{-# INLINEABLE sarcrrsResults #-}
{-# DEPRECATED results "Use generic-lens or generic-optics with 'results' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarcrrsResponseStatus :: Lens.Lens' SelectAggregateResourceConfigResponse Core.Int
sarcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sarcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
