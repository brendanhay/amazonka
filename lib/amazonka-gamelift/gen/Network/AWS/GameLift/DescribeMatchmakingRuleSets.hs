{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeMatchmakingRuleSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details for FlexMatch matchmaking rule sets. You can request all existing rule sets for the Region, or provide a list of one or more rule set names. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a rule set is returned for each requested name.
--
-- __Learn more__
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
--
--
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
module Network.AWS.GameLift.DescribeMatchmakingRuleSets
  ( -- * Creating a request
    DescribeMatchmakingRuleSets (..),
    mkDescribeMatchmakingRuleSets,

    -- ** Request lenses
    dmrsLimit,
    dmrsNames,
    dmrsNextToken,

    -- * Destructuring the response
    DescribeMatchmakingRuleSetsResponse (..),
    mkDescribeMatchmakingRuleSetsResponse,

    -- ** Response lenses
    dmrsrrsRuleSets,
    dmrsrrsNextToken,
    dmrsrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeMatchmakingRuleSets' smart constructor.
data DescribeMatchmakingRuleSets = DescribeMatchmakingRuleSets'
  { -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Core.Maybe Core.Natural,
    -- | A list of one or more matchmaking rule set names to retrieve details for. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
    names :: Core.Maybe (Core.NonEmpty Types.MatchmakingRuleSetName),
    -- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMatchmakingRuleSets' value with any optional fields omitted.
mkDescribeMatchmakingRuleSets ::
  DescribeMatchmakingRuleSets
mkDescribeMatchmakingRuleSets =
  DescribeMatchmakingRuleSets'
    { limit = Core.Nothing,
      names = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsLimit :: Lens.Lens' DescribeMatchmakingRuleSets (Core.Maybe Core.Natural)
dmrsLimit = Lens.field @"limit"
{-# DEPRECATED dmrsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A list of one or more matchmaking rule set names to retrieve details for. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsNames :: Lens.Lens' DescribeMatchmakingRuleSets (Core.Maybe (Core.NonEmpty Types.MatchmakingRuleSetName))
dmrsNames = Lens.field @"names"
{-# DEPRECATED dmrsNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsNextToken :: Lens.Lens' DescribeMatchmakingRuleSets (Core.Maybe Types.NonZeroAndMaxString)
dmrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeMatchmakingRuleSets where
  toJSON DescribeMatchmakingRuleSets {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("Names" Core..=) Core.<$> names,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeMatchmakingRuleSets where
  type
    Rs DescribeMatchmakingRuleSets =
      DescribeMatchmakingRuleSetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeMatchmakingRuleSets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMatchmakingRuleSetsResponse'
            Core.<$> (x Core..:? "RuleSets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMatchmakingRuleSets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"ruleSets") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeMatchmakingRuleSetsResponse' smart constructor.
data DescribeMatchmakingRuleSetsResponse = DescribeMatchmakingRuleSetsResponse'
  { -- | A collection of requested matchmaking rule set objects.
    ruleSets :: [Types.MatchmakingRuleSet],
    -- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeMatchmakingRuleSetsResponse' value with any optional fields omitted.
mkDescribeMatchmakingRuleSetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMatchmakingRuleSetsResponse
mkDescribeMatchmakingRuleSetsResponse responseStatus =
  DescribeMatchmakingRuleSetsResponse'
    { ruleSets = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A collection of requested matchmaking rule set objects.
--
-- /Note:/ Consider using 'ruleSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsrrsRuleSets :: Lens.Lens' DescribeMatchmakingRuleSetsResponse [Types.MatchmakingRuleSet]
dmrsrrsRuleSets = Lens.field @"ruleSets"
{-# DEPRECATED dmrsrrsRuleSets "Use generic-lens or generic-optics with 'ruleSets' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsrrsNextToken :: Lens.Lens' DescribeMatchmakingRuleSetsResponse (Core.Maybe Types.NonZeroAndMaxString)
dmrsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmrsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsrrsResponseStatus :: Lens.Lens' DescribeMatchmakingRuleSetsResponse Core.Int
dmrsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmrsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
