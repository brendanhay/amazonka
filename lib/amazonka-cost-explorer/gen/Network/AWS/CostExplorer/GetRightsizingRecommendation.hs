{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetRightsizingRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates recommendations that help you save cost by identifying idle and underutilized Amazon EC2 instances.
--
-- Recommendations are generated to either downsize or terminate instances, along with providing savings detail and metrics. For details on calculation and function, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/ce-rightsizing.html Optimizing Your Cost with Rightsizing Recommendations> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.CostExplorer.GetRightsizingRecommendation
  ( -- * Creating a request
    GetRightsizingRecommendation (..),
    mkGetRightsizingRecommendation,

    -- ** Request lenses
    grrService,
    grrConfiguration,
    grrFilter,
    grrNextPageToken,
    grrPageSize,

    -- * Destructuring the response
    GetRightsizingRecommendationResponse (..),
    mkGetRightsizingRecommendationResponse,

    -- ** Response lenses
    grrrrsConfiguration,
    grrrrsMetadata,
    grrrrsNextPageToken,
    grrrrsRightsizingRecommendations,
    grrrrsSummary,
    grrrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRightsizingRecommendation' smart constructor.
data GetRightsizingRecommendation = GetRightsizingRecommendation'
  { -- | The specific service that you want recommendations for. The only valid value for @GetRightsizingRecommendation@ is "@AmazonEC2@ ".
    service :: Types.GenericString,
    -- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
    configuration :: Core.Maybe Types.RightsizingRecommendationConfiguration,
    filter :: Core.Maybe Types.Expression,
    -- | The pagination token that indicates the next set of results that you want to retrieve.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The number of recommendations that you want returned in a single response object.
    pageSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRightsizingRecommendation' value with any optional fields omitted.
mkGetRightsizingRecommendation ::
  -- | 'service'
  Types.GenericString ->
  GetRightsizingRecommendation
mkGetRightsizingRecommendation service =
  GetRightsizingRecommendation'
    { service,
      configuration = Core.Nothing,
      filter = Core.Nothing,
      nextPageToken = Core.Nothing,
      pageSize = Core.Nothing
    }

-- | The specific service that you want recommendations for. The only valid value for @GetRightsizingRecommendation@ is "@AmazonEC2@ ".
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrService :: Lens.Lens' GetRightsizingRecommendation Types.GenericString
grrService = Lens.field @"service"
{-# DEPRECATED grrService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrConfiguration :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Types.RightsizingRecommendationConfiguration)
grrConfiguration = Lens.field @"configuration"
{-# DEPRECATED grrConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrFilter :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Types.Expression)
grrFilter = Lens.field @"filter"
{-# DEPRECATED grrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrNextPageToken :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Types.NextPageToken)
grrNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED grrNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The number of recommendations that you want returned in a single response object.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrPageSize :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Core.Natural)
grrPageSize = Lens.field @"pageSize"
{-# DEPRECATED grrPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Core.FromJSON GetRightsizingRecommendation where
  toJSON GetRightsizingRecommendation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Service" Core..= service),
            ("Configuration" Core..=) Core.<$> configuration,
            ("Filter" Core..=) Core.<$> filter,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("PageSize" Core..=) Core.<$> pageSize
          ]
      )

instance Core.AWSRequest GetRightsizingRecommendation where
  type
    Rs GetRightsizingRecommendation =
      GetRightsizingRecommendationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSInsightsIndexService.GetRightsizingRecommendation"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRightsizingRecommendationResponse'
            Core.<$> (x Core..:? "Configuration")
            Core.<*> (x Core..:? "Metadata")
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "RightsizingRecommendations")
            Core.<*> (x Core..:? "Summary")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRightsizingRecommendationResponse' smart constructor.
data GetRightsizingRecommendationResponse = GetRightsizingRecommendationResponse'
  { -- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
    configuration :: Core.Maybe Types.RightsizingRecommendationConfiguration,
    -- | Information regarding this specific recommendation set.
    metadata :: Core.Maybe Types.RightsizingRecommendationMetadata,
    -- | The token to retrieve the next set of results.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | Recommendations to rightsize resources.
    rightsizingRecommendations :: Core.Maybe [Types.RightsizingRecommendation],
    -- | Summary of this recommendation set.
    summary :: Core.Maybe Types.RightsizingRecommendationSummary,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRightsizingRecommendationResponse' value with any optional fields omitted.
mkGetRightsizingRecommendationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRightsizingRecommendationResponse
mkGetRightsizingRecommendationResponse responseStatus =
  GetRightsizingRecommendationResponse'
    { configuration =
        Core.Nothing,
      metadata = Core.Nothing,
      nextPageToken = Core.Nothing,
      rightsizingRecommendations = Core.Nothing,
      summary = Core.Nothing,
      responseStatus
    }

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsConfiguration :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe Types.RightsizingRecommendationConfiguration)
grrrrsConfiguration = Lens.field @"configuration"
{-# DEPRECATED grrrrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Information regarding this specific recommendation set.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsMetadata :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe Types.RightsizingRecommendationMetadata)
grrrrsMetadata = Lens.field @"metadata"
{-# DEPRECATED grrrrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsNextPageToken :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe Types.NextPageToken)
grrrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED grrrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Recommendations to rightsize resources.
--
-- /Note:/ Consider using 'rightsizingRecommendations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsRightsizingRecommendations :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe [Types.RightsizingRecommendation])
grrrrsRightsizingRecommendations = Lens.field @"rightsizingRecommendations"
{-# DEPRECATED grrrrsRightsizingRecommendations "Use generic-lens or generic-optics with 'rightsizingRecommendations' instead." #-}

-- | Summary of this recommendation set.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsSummary :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe Types.RightsizingRecommendationSummary)
grrrrsSummary = Lens.field @"summary"
{-# DEPRECATED grrrrsSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsResponseStatus :: Lens.Lens' GetRightsizingRecommendationResponse Core.Int
grrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
