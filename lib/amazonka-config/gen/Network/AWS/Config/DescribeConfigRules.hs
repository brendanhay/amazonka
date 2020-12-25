{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about your AWS Config rules.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigRules
  ( -- * Creating a request
    DescribeConfigRules (..),
    mkDescribeConfigRules,

    -- ** Request lenses
    dcrConfigRuleNames,
    dcrNextToken,

    -- * Destructuring the response
    DescribeConfigRulesResponse (..),
    mkDescribeConfigRulesResponse,

    -- ** Response lenses
    dcrrrsConfigRules,
    dcrrrsNextToken,
    dcrrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeConfigRules' smart constructor.
data DescribeConfigRules = DescribeConfigRules'
  { -- | The names of the AWS Config rules for which you want details. If you do not specify any names, AWS Config returns details for all your rules.
    configRuleNames :: Core.Maybe [Types.ConfigRuleName],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigRules' value with any optional fields omitted.
mkDescribeConfigRules ::
  DescribeConfigRules
mkDescribeConfigRules =
  DescribeConfigRules'
    { configRuleNames = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The names of the AWS Config rules for which you want details. If you do not specify any names, AWS Config returns details for all your rules.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigRuleNames :: Lens.Lens' DescribeConfigRules (Core.Maybe [Types.ConfigRuleName])
dcrConfigRuleNames = Lens.field @"configRuleNames"
{-# DEPRECATED dcrConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrNextToken :: Lens.Lens' DescribeConfigRules (Core.Maybe Types.String)
dcrNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeConfigRules where
  toJSON DescribeConfigRules {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConfigRuleNames" Core..=) Core.<$> configRuleNames,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeConfigRules where
  type Rs DescribeConfigRules = DescribeConfigRulesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.DescribeConfigRules")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigRulesResponse'
            Core.<$> (x Core..:? "ConfigRules")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeConfigRules where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"configRules" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- |
--
-- /See:/ 'mkDescribeConfigRulesResponse' smart constructor.
data DescribeConfigRulesResponse = DescribeConfigRulesResponse'
  { -- | The details about your AWS Config rules.
    configRules :: Core.Maybe [Types.ConfigRule],
    -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigRulesResponse' value with any optional fields omitted.
mkDescribeConfigRulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConfigRulesResponse
mkDescribeConfigRulesResponse responseStatus =
  DescribeConfigRulesResponse'
    { configRules = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The details about your AWS Config rules.
--
-- /Note:/ Consider using 'configRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsConfigRules :: Lens.Lens' DescribeConfigRulesResponse (Core.Maybe [Types.ConfigRule])
dcrrrsConfigRules = Lens.field @"configRules"
{-# DEPRECATED dcrrrsConfigRules "Use generic-lens or generic-optics with 'configRules' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsNextToken :: Lens.Lens' DescribeConfigRulesResponse (Core.Maybe Types.String)
dcrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsResponseStatus :: Lens.Lens' DescribeConfigRulesResponse Core.Int
dcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
