{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetConformancePackComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details for the conformance pack based on the cumulative compliance results of all the rules in that conformance pack.
module Network.AWS.Config.GetConformancePackComplianceSummary
  ( -- * Creating a request
    GetConformancePackComplianceSummary (..),
    mkGetConformancePackComplianceSummary,

    -- ** Request lenses
    gcpcsConformancePackNames,
    gcpcsLimit,
    gcpcsNextToken,

    -- * Destructuring the response
    GetConformancePackComplianceSummaryResponse (..),
    mkGetConformancePackComplianceSummaryResponse,

    -- ** Response lenses
    gcpcsrrsConformancePackComplianceSummaryList,
    gcpcsrrsNextToken,
    gcpcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConformancePackComplianceSummary' smart constructor.
data GetConformancePackComplianceSummary = GetConformancePackComplianceSummary'
  { -- | Names of conformance packs.
    conformancePackNames :: Core.NonEmpty Types.ConformancePackName,
    -- | The maximum number of conformance packs returned on each page.
    limit :: Core.Maybe Core.Natural,
    -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConformancePackComplianceSummary' value with any optional fields omitted.
mkGetConformancePackComplianceSummary ::
  -- | 'conformancePackNames'
  Core.NonEmpty Types.ConformancePackName ->
  GetConformancePackComplianceSummary
mkGetConformancePackComplianceSummary conformancePackNames =
  GetConformancePackComplianceSummary'
    { conformancePackNames,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Names of conformance packs.
--
-- /Note:/ Consider using 'conformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsConformancePackNames :: Lens.Lens' GetConformancePackComplianceSummary (Core.NonEmpty Types.ConformancePackName)
gcpcsConformancePackNames = Lens.field @"conformancePackNames"
{-# DEPRECATED gcpcsConformancePackNames "Use generic-lens or generic-optics with 'conformancePackNames' instead." #-}

-- | The maximum number of conformance packs returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsLimit :: Lens.Lens' GetConformancePackComplianceSummary (Core.Maybe Core.Natural)
gcpcsLimit = Lens.field @"limit"
{-# DEPRECATED gcpcsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsNextToken :: Lens.Lens' GetConformancePackComplianceSummary (Core.Maybe Types.NextToken)
gcpcsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcpcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetConformancePackComplianceSummary where
  toJSON GetConformancePackComplianceSummary {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConformancePackNames" Core..= conformancePackNames),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetConformancePackComplianceSummary where
  type
    Rs GetConformancePackComplianceSummary =
      GetConformancePackComplianceSummaryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.GetConformancePackComplianceSummary"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConformancePackComplianceSummaryResponse'
            Core.<$> (x Core..:? "ConformancePackComplianceSummaryList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetConformancePackComplianceSummaryResponse' smart constructor.
data GetConformancePackComplianceSummaryResponse = GetConformancePackComplianceSummaryResponse'
  { -- | A list of @ConformancePackComplianceSummary@ objects.
    conformancePackComplianceSummaryList :: Core.Maybe (Core.NonEmpty Types.ConformancePackComplianceSummary),
    -- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConformancePackComplianceSummaryResponse' value with any optional fields omitted.
mkGetConformancePackComplianceSummaryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetConformancePackComplianceSummaryResponse
mkGetConformancePackComplianceSummaryResponse responseStatus =
  GetConformancePackComplianceSummaryResponse'
    { conformancePackComplianceSummaryList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of @ConformancePackComplianceSummary@ objects.
--
-- /Note:/ Consider using 'conformancePackComplianceSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsrrsConformancePackComplianceSummaryList :: Lens.Lens' GetConformancePackComplianceSummaryResponse (Core.Maybe (Core.NonEmpty Types.ConformancePackComplianceSummary))
gcpcsrrsConformancePackComplianceSummaryList = Lens.field @"conformancePackComplianceSummaryList"
{-# DEPRECATED gcpcsrrsConformancePackComplianceSummaryList "Use generic-lens or generic-optics with 'conformancePackComplianceSummaryList' instead." #-}

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsrrsNextToken :: Lens.Lens' GetConformancePackComplianceSummaryResponse (Core.Maybe Types.NextToken)
gcpcsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcpcsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcsrrsResponseStatus :: Lens.Lens' GetConformancePackComplianceSummaryResponse Core.Int
gcpcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcpcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
