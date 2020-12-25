{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for sources within an aggregator. The status includes information about the last time AWS Config verified authorization between the source account and an aggregator account. In case of a failure, the status contains the related error code or message.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigurationAggregatorSourcesStatus
  ( -- * Creating a request
    DescribeConfigurationAggregatorSourcesStatus (..),
    mkDescribeConfigurationAggregatorSourcesStatus,

    -- ** Request lenses
    dcassConfigurationAggregatorName,
    dcassLimit,
    dcassNextToken,
    dcassUpdateStatus,

    -- * Destructuring the response
    DescribeConfigurationAggregatorSourcesStatusResponse (..),
    mkDescribeConfigurationAggregatorSourcesStatusResponse,

    -- ** Response lenses
    dcassrrsAggregatedSourceStatusList,
    dcassrrsNextToken,
    dcassrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConfigurationAggregatorSourcesStatus' smart constructor.
data DescribeConfigurationAggregatorSourcesStatus = DescribeConfigurationAggregatorSourcesStatus'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Types.ConfigurationAggregatorName,
    -- | The maximum number of AggregatorSourceStatus returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | Filters the status type.
    --
    --
    --     * Valid value FAILED indicates errors while moving data.
    --
    --
    --     * Valid value SUCCEEDED indicates the data was successfully moved.
    --
    --
    --     * Valid value OUTDATED indicates the data is not the most recent.
    updateStatus :: Core.Maybe (Core.NonEmpty Types.AggregatedSourceStatusType)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConfigurationAggregatorSourcesStatus' value with any optional fields omitted.
mkDescribeConfigurationAggregatorSourcesStatus ::
  -- | 'configurationAggregatorName'
  Types.ConfigurationAggregatorName ->
  DescribeConfigurationAggregatorSourcesStatus
mkDescribeConfigurationAggregatorSourcesStatus
  configurationAggregatorName =
    DescribeConfigurationAggregatorSourcesStatus'
      { configurationAggregatorName,
        limit = Core.Nothing,
        nextToken = Core.Nothing,
        updateStatus = Core.Nothing
      }

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassConfigurationAggregatorName :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus Types.ConfigurationAggregatorName
dcassConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# DEPRECATED dcassConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | The maximum number of AggregatorSourceStatus returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassLimit :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus (Core.Maybe Core.Natural)
dcassLimit = Lens.field @"limit"
{-# DEPRECATED dcassLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassNextToken :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus (Core.Maybe Types.String)
dcassNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcassNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the status type.
--
--
--     * Valid value FAILED indicates errors while moving data.
--
--
--     * Valid value SUCCEEDED indicates the data was successfully moved.
--
--
--     * Valid value OUTDATED indicates the data is not the most recent.
--
--
--
-- /Note:/ Consider using 'updateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassUpdateStatus :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatus (Core.Maybe (Core.NonEmpty Types.AggregatedSourceStatusType))
dcassUpdateStatus = Lens.field @"updateStatus"
{-# DEPRECATED dcassUpdateStatus "Use generic-lens or generic-optics with 'updateStatus' instead." #-}

instance Core.FromJSON DescribeConfigurationAggregatorSourcesStatus where
  toJSON DescribeConfigurationAggregatorSourcesStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("UpdateStatus" Core..=) Core.<$> updateStatus
          ]
      )

instance
  Core.AWSRequest
    DescribeConfigurationAggregatorSourcesStatus
  where
  type
    Rs DescribeConfigurationAggregatorSourcesStatus =
      DescribeConfigurationAggregatorSourcesStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeConfigurationAggregatorSourcesStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationAggregatorSourcesStatusResponse'
            Core.<$> (x Core..:? "AggregatedSourceStatusList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Pager.AWSPager
    DescribeConfigurationAggregatorSourcesStatus
  where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"aggregatedSourceStatusList" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeConfigurationAggregatorSourcesStatusResponse' smart constructor.
data DescribeConfigurationAggregatorSourcesStatusResponse = DescribeConfigurationAggregatorSourcesStatusResponse'
  { -- | Returns an AggregatedSourceStatus object.
    aggregatedSourceStatusList :: Core.Maybe [Types.AggregatedSourceStatus],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeConfigurationAggregatorSourcesStatusResponse' value with any optional fields omitted.
mkDescribeConfigurationAggregatorSourcesStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeConfigurationAggregatorSourcesStatusResponse
mkDescribeConfigurationAggregatorSourcesStatusResponse
  responseStatus =
    DescribeConfigurationAggregatorSourcesStatusResponse'
      { aggregatedSourceStatusList =
          Core.Nothing,
        nextToken = Core.Nothing,
        responseStatus
      }

-- | Returns an AggregatedSourceStatus object.
--
-- /Note:/ Consider using 'aggregatedSourceStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassrrsAggregatedSourceStatusList :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatusResponse (Core.Maybe [Types.AggregatedSourceStatus])
dcassrrsAggregatedSourceStatusList = Lens.field @"aggregatedSourceStatusList"
{-# DEPRECATED dcassrrsAggregatedSourceStatusList "Use generic-lens or generic-optics with 'aggregatedSourceStatusList' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassrrsNextToken :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatusResponse (Core.Maybe Types.NextToken)
dcassrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcassrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcassrrsResponseStatus :: Lens.Lens' DescribeConfigurationAggregatorSourcesStatusResponse Core.Int
dcassrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcassrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
