{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetConformancePackComplianceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details of a conformance pack for all AWS resources that are monitered by conformance pack.
module Network.AWS.Config.GetConformancePackComplianceDetails
  ( -- * Creating a request
    GetConformancePackComplianceDetails (..),
    mkGetConformancePackComplianceDetails,

    -- ** Request lenses
    gcpcdConformancePackName,
    gcpcdFilters,
    gcpcdLimit,
    gcpcdNextToken,

    -- * Destructuring the response
    GetConformancePackComplianceDetailsResponse (..),
    mkGetConformancePackComplianceDetailsResponse,

    -- ** Response lenses
    gcpcdrrsConformancePackName,
    gcpcdrrsConformancePackRuleEvaluationResults,
    gcpcdrrsNextToken,
    gcpcdrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConformancePackComplianceDetails' smart constructor.
data GetConformancePackComplianceDetails = GetConformancePackComplianceDetails'
  { -- | Name of the conformance pack.
    conformancePackName :: Types.ConformancePackName,
    -- | A @ConformancePackEvaluationFilters@ object.
    filters :: Core.Maybe Types.ConformancePackEvaluationFilters,
    -- | The maximum number of evaluation results returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConformancePackComplianceDetails' value with any optional fields omitted.
mkGetConformancePackComplianceDetails ::
  -- | 'conformancePackName'
  Types.ConformancePackName ->
  GetConformancePackComplianceDetails
mkGetConformancePackComplianceDetails conformancePackName =
  GetConformancePackComplianceDetails'
    { conformancePackName,
      filters = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdConformancePackName :: Lens.Lens' GetConformancePackComplianceDetails Types.ConformancePackName
gcpcdConformancePackName = Lens.field @"conformancePackName"
{-# DEPRECATED gcpcdConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | A @ConformancePackEvaluationFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdFilters :: Lens.Lens' GetConformancePackComplianceDetails (Core.Maybe Types.ConformancePackEvaluationFilters)
gcpcdFilters = Lens.field @"filters"
{-# DEPRECATED gcpcdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of evaluation results returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdLimit :: Lens.Lens' GetConformancePackComplianceDetails (Core.Maybe Core.Natural)
gcpcdLimit = Lens.field @"limit"
{-# DEPRECATED gcpcdLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdNextToken :: Lens.Lens' GetConformancePackComplianceDetails (Core.Maybe Types.NextToken)
gcpcdNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcpcdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetConformancePackComplianceDetails where
  toJSON GetConformancePackComplianceDetails {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConformancePackName" Core..= conformancePackName),
            ("Filters" Core..=) Core.<$> filters,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetConformancePackComplianceDetails where
  type
    Rs GetConformancePackComplianceDetails =
      GetConformancePackComplianceDetailsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.GetConformancePackComplianceDetails"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConformancePackComplianceDetailsResponse'
            Core.<$> (x Core..: "ConformancePackName")
            Core.<*> (x Core..:? "ConformancePackRuleEvaluationResults")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetConformancePackComplianceDetailsResponse' smart constructor.
data GetConformancePackComplianceDetailsResponse = GetConformancePackComplianceDetailsResponse'
  { -- | Name of the conformance pack.
    conformancePackName :: Types.ConformancePackName,
    -- | Returns a list of @ConformancePackEvaluationResult@ objects.
    conformancePackRuleEvaluationResults :: Core.Maybe [Types.ConformancePackEvaluationResult],
    -- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetConformancePackComplianceDetailsResponse' value with any optional fields omitted.
mkGetConformancePackComplianceDetailsResponse ::
  -- | 'conformancePackName'
  Types.ConformancePackName ->
  -- | 'responseStatus'
  Core.Int ->
  GetConformancePackComplianceDetailsResponse
mkGetConformancePackComplianceDetailsResponse
  conformancePackName
  responseStatus =
    GetConformancePackComplianceDetailsResponse'
      { conformancePackName,
        conformancePackRuleEvaluationResults =
          Core.Nothing,
        nextToken = Core.Nothing,
        responseStatus
      }

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrrsConformancePackName :: Lens.Lens' GetConformancePackComplianceDetailsResponse Types.ConformancePackName
gcpcdrrsConformancePackName = Lens.field @"conformancePackName"
{-# DEPRECATED gcpcdrrsConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | Returns a list of @ConformancePackEvaluationResult@ objects.
--
-- /Note:/ Consider using 'conformancePackRuleEvaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrrsConformancePackRuleEvaluationResults :: Lens.Lens' GetConformancePackComplianceDetailsResponse (Core.Maybe [Types.ConformancePackEvaluationResult])
gcpcdrrsConformancePackRuleEvaluationResults = Lens.field @"conformancePackRuleEvaluationResults"
{-# DEPRECATED gcpcdrrsConformancePackRuleEvaluationResults "Use generic-lens or generic-optics with 'conformancePackRuleEvaluationResults' instead." #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrrsNextToken :: Lens.Lens' GetConformancePackComplianceDetailsResponse (Core.Maybe Types.NextToken)
gcpcdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcpcdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrrsResponseStatus :: Lens.Lens' GetConformancePackComplianceDetailsResponse Core.Int
gcpcdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcpcdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
