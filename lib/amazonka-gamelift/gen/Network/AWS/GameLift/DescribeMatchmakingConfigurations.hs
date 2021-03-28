{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeMatchmakingConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of FlexMatch matchmaking configurations. 
--
-- This operation offers the following options: (1) retrieve all matchmaking configurations, (2) retrieve configurations for a specified list, or (3) retrieve all configurations that use a specified rule set name. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages. 
-- If successful, a configuration is returned for each requested name. When specifying a list of names, only configurations that currently exist are returned. 
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/matchmaker-build.html Setting Up FlexMatch Matchmakers> 
-- __Related operations__ 
--
--     * 'CreateMatchmakingConfiguration' 
--
--
--     * 'DescribeMatchmakingConfigurations' 
--
--
--     * 'UpdateMatchmakingConfiguration' 
--
--
--     * 'DeleteMatchmakingConfiguration' 
--
--
--     * 'CreateMatchmakingRuleSet' 
--
--
--     * 'DescribeMatchmakingRuleSets' 
--
--
--     * 'ValidateMatchmakingRuleSet' 
--
--
--     * 'DeleteMatchmakingRuleSet' 
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeMatchmakingConfigurations
    (
    -- * Creating a request
      DescribeMatchmakingConfigurations (..)
    , mkDescribeMatchmakingConfigurations
    -- ** Request lenses
    , dmcLimit
    , dmcNames
    , dmcNextToken
    , dmcRuleSetName

    -- * Destructuring the response
    , DescribeMatchmakingConfigurationsResponse (..)
    , mkDescribeMatchmakingConfigurationsResponse
    -- ** Response lenses
    , dmcrrsConfigurations
    , dmcrrsNextToken
    , dmcrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeMatchmakingConfigurations' smart constructor.
data DescribeMatchmakingConfigurations = DescribeMatchmakingConfigurations'
  { limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is limited to 10.
  , names :: Core.Maybe [Types.MatchmakingConfigurationName]
    -- ^ A unique identifier for a matchmaking configuration(s) to retrieve. You can use either the configuration name or ARN value. To request all existing configurations, leave this parameter empty.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
  , ruleSetName :: Core.Maybe Types.RuleSetName
    -- ^ A unique identifier for a matchmaking rule set. You can use either the rule set name or ARN value. Use this parameter to retrieve all matchmaking configurations that use this rule set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMatchmakingConfigurations' value with any optional fields omitted.
mkDescribeMatchmakingConfigurations
    :: DescribeMatchmakingConfigurations
mkDescribeMatchmakingConfigurations
  = DescribeMatchmakingConfigurations'{limit = Core.Nothing,
                                       names = Core.Nothing, nextToken = Core.Nothing,
                                       ruleSetName = Core.Nothing}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is limited to 10.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcLimit :: Lens.Lens' DescribeMatchmakingConfigurations (Core.Maybe Core.Natural)
dmcLimit = Lens.field @"limit"
{-# INLINEABLE dmcLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | A unique identifier for a matchmaking configuration(s) to retrieve. You can use either the configuration name or ARN value. To request all existing configurations, leave this parameter empty.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcNames :: Lens.Lens' DescribeMatchmakingConfigurations (Core.Maybe [Types.MatchmakingConfigurationName])
dmcNames = Lens.field @"names"
{-# INLINEABLE dmcNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcNextToken :: Lens.Lens' DescribeMatchmakingConfigurations (Core.Maybe Types.NextToken)
dmcNextToken = Lens.field @"nextToken"
{-# INLINEABLE dmcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A unique identifier for a matchmaking rule set. You can use either the rule set name or ARN value. Use this parameter to retrieve all matchmaking configurations that use this rule set.
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcRuleSetName :: Lens.Lens' DescribeMatchmakingConfigurations (Core.Maybe Types.RuleSetName)
dmcRuleSetName = Lens.field @"ruleSetName"
{-# INLINEABLE dmcRuleSetName #-}
{-# DEPRECATED ruleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead"  #-}

instance Core.ToQuery DescribeMatchmakingConfigurations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeMatchmakingConfigurations where
        toHeaders DescribeMatchmakingConfigurations{..}
          = Core.pure
              ("X-Amz-Target", "GameLift.DescribeMatchmakingConfigurations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeMatchmakingConfigurations where
        toJSON DescribeMatchmakingConfigurations{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("Names" Core..=) Core.<$> names,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("RuleSetName" Core..=) Core.<$> ruleSetName])

instance Core.AWSRequest DescribeMatchmakingConfigurations where
        type Rs DescribeMatchmakingConfigurations =
             DescribeMatchmakingConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeMatchmakingConfigurationsResponse' Core.<$>
                   (x Core..:? "Configurations") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeMatchmakingConfigurations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"configurations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeMatchmakingConfigurationsResponse' smart constructor.
data DescribeMatchmakingConfigurationsResponse = DescribeMatchmakingConfigurationsResponse'
  { configurations :: Core.Maybe [Types.MatchmakingConfiguration]
    -- ^ A collection of requested matchmaking configurations.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeMatchmakingConfigurationsResponse' value with any optional fields omitted.
mkDescribeMatchmakingConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeMatchmakingConfigurationsResponse
mkDescribeMatchmakingConfigurationsResponse responseStatus
  = DescribeMatchmakingConfigurationsResponse'{configurations =
                                                 Core.Nothing,
                                               nextToken = Core.Nothing, responseStatus}

-- | A collection of requested matchmaking configurations.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsConfigurations :: Lens.Lens' DescribeMatchmakingConfigurationsResponse (Core.Maybe [Types.MatchmakingConfiguration])
dmcrrsConfigurations = Lens.field @"configurations"
{-# INLINEABLE dmcrrsConfigurations #-}
{-# DEPRECATED configurations "Use generic-lens or generic-optics with 'configurations' instead"  #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsNextToken :: Lens.Lens' DescribeMatchmakingConfigurationsResponse (Core.Maybe Types.NonZeroAndMaxString)
dmcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dmcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcrrsResponseStatus :: Lens.Lens' DescribeMatchmakingConfigurationsResponse Core.Int
dmcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
