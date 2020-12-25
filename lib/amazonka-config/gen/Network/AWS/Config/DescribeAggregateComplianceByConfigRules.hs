{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeAggregateComplianceByConfigRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of compliant and noncompliant rules with the number of resources for compliant and noncompliant rules.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeAggregateComplianceByConfigRules
  ( -- * Creating a request
    DescribeAggregateComplianceByConfigRules (..),
    mkDescribeAggregateComplianceByConfigRules,

    -- ** Request lenses
    dacbcrConfigurationAggregatorName,
    dacbcrFilters,
    dacbcrLimit,
    dacbcrNextToken,

    -- * Destructuring the response
    DescribeAggregateComplianceByConfigRulesResponse (..),
    mkDescribeAggregateComplianceByConfigRulesResponse,

    -- ** Response lenses
    dacbcrrrsAggregateComplianceByConfigRules,
    dacbcrrrsNextToken,
    dacbcrrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAggregateComplianceByConfigRules' smart constructor.
data DescribeAggregateComplianceByConfigRules = DescribeAggregateComplianceByConfigRules'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Types.ConfigurationAggregatorName,
    -- | Filters the results by ConfigRuleComplianceFilters object.
    filters :: Core.Maybe Types.ConfigRuleComplianceFilters,
    -- | The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAggregateComplianceByConfigRules' value with any optional fields omitted.
mkDescribeAggregateComplianceByConfigRules ::
  -- | 'configurationAggregatorName'
  Types.ConfigurationAggregatorName ->
  DescribeAggregateComplianceByConfigRules
mkDescribeAggregateComplianceByConfigRules
  configurationAggregatorName =
    DescribeAggregateComplianceByConfigRules'
      { configurationAggregatorName,
        filters = Core.Nothing,
        limit = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrConfigurationAggregatorName :: Lens.Lens' DescribeAggregateComplianceByConfigRules Types.ConfigurationAggregatorName
dacbcrConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# DEPRECATED dacbcrConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | Filters the results by ConfigRuleComplianceFilters object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrFilters :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Core.Maybe Types.ConfigRuleComplianceFilters)
dacbcrFilters = Lens.field @"filters"
{-# DEPRECATED dacbcrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrLimit :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Core.Maybe Core.Natural)
dacbcrLimit = Lens.field @"limit"
{-# DEPRECATED dacbcrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrNextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRules (Core.Maybe Types.NextToken)
dacbcrNextToken = Lens.field @"nextToken"
{-# DEPRECATED dacbcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeAggregateComplianceByConfigRules where
  toJSON DescribeAggregateComplianceByConfigRules {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            ("Filters" Core..=) Core.<$> filters,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeAggregateComplianceByConfigRules where
  type
    Rs DescribeAggregateComplianceByConfigRules =
      DescribeAggregateComplianceByConfigRulesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeAggregateComplianceByConfigRules"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAggregateComplianceByConfigRulesResponse'
            Core.<$> (x Core..:? "AggregateComplianceByConfigRules")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAggregateComplianceByConfigRules where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"aggregateComplianceByConfigRules" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAggregateComplianceByConfigRulesResponse' smart constructor.
data DescribeAggregateComplianceByConfigRulesResponse = DescribeAggregateComplianceByConfigRulesResponse'
  { -- | Returns a list of AggregateComplianceByConfigRule object.
    aggregateComplianceByConfigRules :: Core.Maybe [Types.AggregateComplianceByConfigRule],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAggregateComplianceByConfigRulesResponse' value with any optional fields omitted.
mkDescribeAggregateComplianceByConfigRulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAggregateComplianceByConfigRulesResponse
mkDescribeAggregateComplianceByConfigRulesResponse responseStatus =
  DescribeAggregateComplianceByConfigRulesResponse'
    { aggregateComplianceByConfigRules =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Returns a list of AggregateComplianceByConfigRule object.
--
-- /Note:/ Consider using 'aggregateComplianceByConfigRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrrrsAggregateComplianceByConfigRules :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Core.Maybe [Types.AggregateComplianceByConfigRule])
dacbcrrrsAggregateComplianceByConfigRules = Lens.field @"aggregateComplianceByConfigRules"
{-# DEPRECATED dacbcrrrsAggregateComplianceByConfigRules "Use generic-lens or generic-optics with 'aggregateComplianceByConfigRules' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrrrsNextToken :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse (Core.Maybe Types.NextToken)
dacbcrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dacbcrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacbcrrrsResponseStatus :: Lens.Lens' DescribeAggregateComplianceByConfigRulesResponse Core.Int
dacbcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dacbcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
