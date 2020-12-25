{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeElasticGpus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Elastic Graphics accelerator associated with your instances. For more information about Elastic Graphics, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon Elastic Graphics> .
module Network.AWS.EC2.DescribeElasticGpus
  ( -- * Creating a request
    DescribeElasticGpus (..),
    mkDescribeElasticGpus,

    -- ** Request lenses
    degDryRun,
    degElasticGpuIds,
    degFilters,
    degMaxResults,
    degNextToken,

    -- * Destructuring the response
    DescribeElasticGpusResponse (..),
    mkDescribeElasticGpusResponse,

    -- ** Response lenses
    degrrsElasticGpuSet,
    degrrsMaxResults,
    degrrsNextToken,
    degrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeElasticGpus' smart constructor.
data DescribeElasticGpus = DescribeElasticGpus'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The Elastic Graphics accelerator IDs.
    elasticGpuIds :: Core.Maybe [Types.ElasticGpuId],
    -- | The filters.
    --
    --
    --     * @availability-zone@ - The Availability Zone in which the Elastic Graphics accelerator resides.
    --
    --
    --     * @elastic-gpu-health@ - The status of the Elastic Graphics accelerator (@OK@ | @IMPAIRED@ ).
    --
    --
    --     * @elastic-gpu-state@ - The state of the Elastic Graphics accelerator (@ATTACHED@ ).
    --
    --
    --     * @elastic-gpu-type@ - The type of Elastic Graphics accelerator; for example, @eg1.medium@ .
    --
    --
    --     * @instance-id@ - The ID of the instance to which the Elastic Graphics accelerator is associated.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to request the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticGpus' value with any optional fields omitted.
mkDescribeElasticGpus ::
  DescribeElasticGpus
mkDescribeElasticGpus =
  DescribeElasticGpus'
    { dryRun = Core.Nothing,
      elasticGpuIds = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degDryRun :: Lens.Lens' DescribeElasticGpus (Core.Maybe Core.Bool)
degDryRun = Lens.field @"dryRun"
{-# DEPRECATED degDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The Elastic Graphics accelerator IDs.
--
-- /Note:/ Consider using 'elasticGpuIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degElasticGpuIds :: Lens.Lens' DescribeElasticGpus (Core.Maybe [Types.ElasticGpuId])
degElasticGpuIds = Lens.field @"elasticGpuIds"
{-# DEPRECATED degElasticGpuIds "Use generic-lens or generic-optics with 'elasticGpuIds' instead." #-}

-- | The filters.
--
--
--     * @availability-zone@ - The Availability Zone in which the Elastic Graphics accelerator resides.
--
--
--     * @elastic-gpu-health@ - The status of the Elastic Graphics accelerator (@OK@ | @IMPAIRED@ ).
--
--
--     * @elastic-gpu-state@ - The state of the Elastic Graphics accelerator (@ATTACHED@ ).
--
--
--     * @elastic-gpu-type@ - The type of Elastic Graphics accelerator; for example, @eg1.medium@ .
--
--
--     * @instance-id@ - The ID of the instance to which the Elastic Graphics accelerator is associated.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degFilters :: Lens.Lens' DescribeElasticGpus (Core.Maybe [Types.Filter])
degFilters = Lens.field @"filters"
{-# DEPRECATED degFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degMaxResults :: Lens.Lens' DescribeElasticGpus (Core.Maybe Core.Natural)
degMaxResults = Lens.field @"maxResults"
{-# DEPRECATED degMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degNextToken :: Lens.Lens' DescribeElasticGpus (Core.Maybe Types.String)
degNextToken = Lens.field @"nextToken"
{-# DEPRECATED degNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeElasticGpus where
  type Rs DescribeElasticGpus = DescribeElasticGpusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeElasticGpus")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "ElasticGpuId" Core.<$> elasticGpuIds)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeElasticGpusResponse'
            Core.<$> (x Core..@? "elasticGpuSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "maxResults")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeElasticGpusResponse' smart constructor.
data DescribeElasticGpusResponse = DescribeElasticGpusResponse'
  { -- | Information about the Elastic Graphics accelerators.
    elasticGpuSet :: Core.Maybe [Types.ElasticGpus],
    -- | The total number of items to return. If the total number of items available is more than the value specified in max-items then a Next-Token will be provided in the output that you can use to resume pagination.
    maxResults :: Core.Maybe Core.Int,
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeElasticGpusResponse' value with any optional fields omitted.
mkDescribeElasticGpusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeElasticGpusResponse
mkDescribeElasticGpusResponse responseStatus =
  DescribeElasticGpusResponse'
    { elasticGpuSet = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the Elastic Graphics accelerators.
--
-- /Note:/ Consider using 'elasticGpuSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrrsElasticGpuSet :: Lens.Lens' DescribeElasticGpusResponse (Core.Maybe [Types.ElasticGpus])
degrrsElasticGpuSet = Lens.field @"elasticGpuSet"
{-# DEPRECATED degrrsElasticGpuSet "Use generic-lens or generic-optics with 'elasticGpuSet' instead." #-}

-- | The total number of items to return. If the total number of items available is more than the value specified in max-items then a Next-Token will be provided in the output that you can use to resume pagination.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrrsMaxResults :: Lens.Lens' DescribeElasticGpusResponse (Core.Maybe Core.Int)
degrrsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED degrrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrrsNextToken :: Lens.Lens' DescribeElasticGpusResponse (Core.Maybe Types.String)
degrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED degrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrrsResponseStatus :: Lens.Lens' DescribeElasticGpusResponse Core.Int
degrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED degrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
