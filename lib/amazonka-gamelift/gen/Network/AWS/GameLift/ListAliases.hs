{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all aliases for this AWS account. You can filter the result set by alias name and/or routing strategy type. Use the pagination parameters to retrieve results in sequential pages.
--
--
--     * 'CreateAlias' 
--
--
--     * 'ListAliases' 
--
--
--     * 'DescribeAlias' 
--
--
--     * 'UpdateAlias' 
--
--
--     * 'DeleteAlias' 
--
--
--     * 'ResolveAlias' 
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListAliases
    (
    -- * Creating a request
      ListAliases (..)
    , mkListAliases
    -- ** Request lenses
    , laLimit
    , laName
    , laNextToken
    , laRoutingStrategyType

    -- * Destructuring the response
    , ListAliasesResponse (..)
    , mkListAliasesResponse
    -- ** Response lenses
    , larrsAliases
    , larrsNextToken
    , larrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkListAliases' smart constructor.
data ListAliases = ListAliases'
  { limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
  , name :: Core.Maybe Types.Name
    -- ^ A descriptive label that is associated with an alias. Alias names do not need to be unique.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
  , routingStrategyType :: Core.Maybe Types.RoutingStrategyType
    -- ^ The routing type to filter results on. Use this parameter to retrieve only aliases with a certain routing type. To retrieve all aliases, leave this parameter empty.
--
-- Possible routing types include the following:
--
--     * __SIMPLE__ -- The alias resolves to one specific fleet. Use this type when routing to active fleets.
--
--
--     * __TERMINAL__ -- The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAliases' value with any optional fields omitted.
mkListAliases
    :: ListAliases
mkListAliases
  = ListAliases'{limit = Core.Nothing, name = Core.Nothing,
                 nextToken = Core.Nothing, routingStrategyType = Core.Nothing}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLimit :: Lens.Lens' ListAliases (Core.Maybe Core.Natural)
laLimit = Lens.field @"limit"
{-# INLINEABLE laLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laName :: Lens.Lens' ListAliases (Core.Maybe Types.Name)
laName = Lens.field @"name"
{-# INLINEABLE laName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAliases (Core.Maybe Types.NextToken)
laNextToken = Lens.field @"nextToken"
{-# INLINEABLE laNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The routing type to filter results on. Use this parameter to retrieve only aliases with a certain routing type. To retrieve all aliases, leave this parameter empty.
--
-- Possible routing types include the following:
--
--     * __SIMPLE__ -- The alias resolves to one specific fleet. Use this type when routing to active fleets.
--
--
--     * __TERMINAL__ -- The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
--
--
--
-- /Note:/ Consider using 'routingStrategyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laRoutingStrategyType :: Lens.Lens' ListAliases (Core.Maybe Types.RoutingStrategyType)
laRoutingStrategyType = Lens.field @"routingStrategyType"
{-# INLINEABLE laRoutingStrategyType #-}
{-# DEPRECATED routingStrategyType "Use generic-lens or generic-optics with 'routingStrategyType' instead"  #-}

instance Core.ToQuery ListAliases where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAliases where
        toHeaders ListAliases{..}
          = Core.pure ("X-Amz-Target", "GameLift.ListAliases") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAliases where
        toJSON ListAliases{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit, ("Name" Core..=) Core.<$> name,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("RoutingStrategyType" Core..=) Core.<$> routingStrategyType])

instance Core.AWSRequest ListAliases where
        type Rs ListAliases = ListAliasesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAliasesResponse' Core.<$>
                   (x Core..:? "Aliases") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAliases where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"aliases" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { aliases :: Core.Maybe [Types.Alias]
    -- ^ A collection of alias resources that match the request parameters.
  , nextToken :: Core.Maybe Types.NonEmptyString
    -- ^ A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAliasesResponse' value with any optional fields omitted.
mkListAliasesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAliasesResponse
mkListAliasesResponse responseStatus
  = ListAliasesResponse'{aliases = Core.Nothing,
                         nextToken = Core.Nothing, responseStatus}

-- | A collection of alias resources that match the request parameters.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsAliases :: Lens.Lens' ListAliasesResponse (Core.Maybe [Types.Alias])
larrsAliases = Lens.field @"aliases"
{-# INLINEABLE larrsAliases #-}
{-# DEPRECATED aliases "Use generic-lens or generic-optics with 'aliases' instead"  #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListAliasesResponse (Core.Maybe Types.NonEmptyString)
larrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE larrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAliasesResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
