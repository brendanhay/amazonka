{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetComplianceDetailsByResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS resource. The results indicate which AWS Config rules were used to evaluate the resource, when each rule was last used, and whether the resource complies with each rule.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetComplianceDetailsByResource
  ( -- * Creating a request
    GetComplianceDetailsByResource (..),
    mkGetComplianceDetailsByResource,

    -- ** Request lenses
    gcdbrResourceType,
    gcdbrResourceId,
    gcdbrComplianceTypes,
    gcdbrNextToken,

    -- * Destructuring the response
    GetComplianceDetailsByResourceResponse (..),
    mkGetComplianceDetailsByResourceResponse,

    -- ** Response lenses
    gcdbrrrsEvaluationResults,
    gcdbrrrsNextToken,
    gcdbrrrsResponseStatus,
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
-- /See:/ 'mkGetComplianceDetailsByResource' smart constructor.
data GetComplianceDetailsByResource = GetComplianceDetailsByResource'
  { -- | The type of the AWS resource for which you want compliance information.
    resourceType :: Types.StringWithCharLimit256,
    -- | The ID of the AWS resource for which you want compliance information.
    resourceId :: Types.BaseResourceId,
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
    complianceTypes :: Core.Maybe [Types.ComplianceType],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetComplianceDetailsByResource' value with any optional fields omitted.
mkGetComplianceDetailsByResource ::
  -- | 'resourceType'
  Types.StringWithCharLimit256 ->
  -- | 'resourceId'
  Types.BaseResourceId ->
  GetComplianceDetailsByResource
mkGetComplianceDetailsByResource resourceType resourceId =
  GetComplianceDetailsByResource'
    { resourceType,
      resourceId,
      complianceTypes = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The type of the AWS resource for which you want compliance information.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrResourceType :: Lens.Lens' GetComplianceDetailsByResource Types.StringWithCharLimit256
gcdbrResourceType = Lens.field @"resourceType"
{-# DEPRECATED gcdbrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the AWS resource for which you want compliance information.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrResourceId :: Lens.Lens' GetComplianceDetailsByResource Types.BaseResourceId
gcdbrResourceId = Lens.field @"resourceId"
{-# DEPRECATED gcdbrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
--
-- /Note:/ Consider using 'complianceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrComplianceTypes :: Lens.Lens' GetComplianceDetailsByResource (Core.Maybe [Types.ComplianceType])
gcdbrComplianceTypes = Lens.field @"complianceTypes"
{-# DEPRECATED gcdbrComplianceTypes "Use generic-lens or generic-optics with 'complianceTypes' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrNextToken :: Lens.Lens' GetComplianceDetailsByResource (Core.Maybe Types.String)
gcdbrNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcdbrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetComplianceDetailsByResource where
  toJSON GetComplianceDetailsByResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceType" Core..= resourceType),
            Core.Just ("ResourceId" Core..= resourceId),
            ("ComplianceTypes" Core..=) Core.<$> complianceTypes,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetComplianceDetailsByResource where
  type
    Rs GetComplianceDetailsByResource =
      GetComplianceDetailsByResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.GetComplianceDetailsByResource"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComplianceDetailsByResourceResponse'
            Core.<$> (x Core..:? "EvaluationResults")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetComplianceDetailsByResource where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"evaluationResults" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- |
--
-- /See:/ 'mkGetComplianceDetailsByResourceResponse' smart constructor.
data GetComplianceDetailsByResourceResponse = GetComplianceDetailsByResourceResponse'
  { -- | Indicates whether the specified AWS resource complies each AWS Config rule.
    evaluationResults :: Core.Maybe [Types.EvaluationResult],
    -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetComplianceDetailsByResourceResponse' value with any optional fields omitted.
mkGetComplianceDetailsByResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetComplianceDetailsByResourceResponse
mkGetComplianceDetailsByResourceResponse responseStatus =
  GetComplianceDetailsByResourceResponse'
    { evaluationResults =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Indicates whether the specified AWS resource complies each AWS Config rule.
--
-- /Note:/ Consider using 'evaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrrrsEvaluationResults :: Lens.Lens' GetComplianceDetailsByResourceResponse (Core.Maybe [Types.EvaluationResult])
gcdbrrrsEvaluationResults = Lens.field @"evaluationResults"
{-# DEPRECATED gcdbrrrsEvaluationResults "Use generic-lens or generic-optics with 'evaluationResults' instead." #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrrrsNextToken :: Lens.Lens' GetComplianceDetailsByResourceResponse (Core.Maybe Types.String)
gcdbrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcdbrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrrrsResponseStatus :: Lens.Lens' GetComplianceDetailsByResourceResponse Core.Int
gcdbrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcdbrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
