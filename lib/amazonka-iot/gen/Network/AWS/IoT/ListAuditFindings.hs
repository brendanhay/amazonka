{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAuditFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the findings (results) of a Device Defender audit or of the audits performed during a specified time period. (Findings are retained for 90 days.)
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditFindings
    (
    -- * Creating a request
      ListAuditFindings (..)
    , mkListAuditFindings
    -- ** Request lenses
    , lafCheckName
    , lafEndTime
    , lafListSuppressedFindings
    , lafMaxResults
    , lafNextToken
    , lafResourceIdentifier
    , lafStartTime
    , lafTaskId

    -- * Destructuring the response
    , ListAuditFindingsResponse (..)
    , mkListAuditFindingsResponse
    -- ** Response lenses
    , lafrrsFindings
    , lafrrsNextToken
    , lafrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAuditFindings' smart constructor.
data ListAuditFindings = ListAuditFindings'
  { checkName :: Core.Maybe Types.CheckName
    -- ^ A filter to limit results to the findings for the specified audit check.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter to limit results to those found before the specified time. You must specify either the startTime and endTime or the taskId, but not both.
  , listSuppressedFindings :: Core.Maybe Core.Bool
    -- ^ Boolean flag indicating whether only the suppressed findings or the unsuppressed findings should be listed. If this parameter isn't provided, the response will list both suppressed and unsuppressed findings. 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time. The default is 25.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  , resourceIdentifier :: Core.Maybe Types.ResourceIdentifier
    -- ^ Information identifying the noncompliant resource.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter to limit results to those found after the specified time. You must specify either the startTime and endTime or the taskId, but not both.
  , taskId :: Core.Maybe Types.AuditTaskId
    -- ^ A filter to limit results to the audit with the specified ID. You must specify either the taskId or the startTime and endTime, but not both.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAuditFindings' value with any optional fields omitted.
mkListAuditFindings
    :: ListAuditFindings
mkListAuditFindings
  = ListAuditFindings'{checkName = Core.Nothing,
                       endTime = Core.Nothing, listSuppressedFindings = Core.Nothing,
                       maxResults = Core.Nothing, nextToken = Core.Nothing,
                       resourceIdentifier = Core.Nothing, startTime = Core.Nothing,
                       taskId = Core.Nothing}

-- | A filter to limit results to the findings for the specified audit check.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafCheckName :: Lens.Lens' ListAuditFindings (Core.Maybe Types.CheckName)
lafCheckName = Lens.field @"checkName"
{-# INLINEABLE lafCheckName #-}
{-# DEPRECATED checkName "Use generic-lens or generic-optics with 'checkName' instead"  #-}

-- | A filter to limit results to those found before the specified time. You must specify either the startTime and endTime or the taskId, but not both.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafEndTime :: Lens.Lens' ListAuditFindings (Core.Maybe Core.NominalDiffTime)
lafEndTime = Lens.field @"endTime"
{-# INLINEABLE lafEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Boolean flag indicating whether only the suppressed findings or the unsuppressed findings should be listed. If this parameter isn't provided, the response will list both suppressed and unsuppressed findings. 
--
-- /Note:/ Consider using 'listSuppressedFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafListSuppressedFindings :: Lens.Lens' ListAuditFindings (Core.Maybe Core.Bool)
lafListSuppressedFindings = Lens.field @"listSuppressedFindings"
{-# INLINEABLE lafListSuppressedFindings #-}
{-# DEPRECATED listSuppressedFindings "Use generic-lens or generic-optics with 'listSuppressedFindings' instead"  #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafMaxResults :: Lens.Lens' ListAuditFindings (Core.Maybe Core.Natural)
lafMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lafMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafNextToken :: Lens.Lens' ListAuditFindings (Core.Maybe Types.NextToken)
lafNextToken = Lens.field @"nextToken"
{-# INLINEABLE lafNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information identifying the noncompliant resource.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafResourceIdentifier :: Lens.Lens' ListAuditFindings (Core.Maybe Types.ResourceIdentifier)
lafResourceIdentifier = Lens.field @"resourceIdentifier"
{-# INLINEABLE lafResourceIdentifier #-}
{-# DEPRECATED resourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead"  #-}

-- | A filter to limit results to those found after the specified time. You must specify either the startTime and endTime or the taskId, but not both.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafStartTime :: Lens.Lens' ListAuditFindings (Core.Maybe Core.NominalDiffTime)
lafStartTime = Lens.field @"startTime"
{-# INLINEABLE lafStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | A filter to limit results to the audit with the specified ID. You must specify either the taskId or the startTime and endTime, but not both.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafTaskId :: Lens.Lens' ListAuditFindings (Core.Maybe Types.AuditTaskId)
lafTaskId = Lens.field @"taskId"
{-# INLINEABLE lafTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

instance Core.ToQuery ListAuditFindings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAuditFindings where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ListAuditFindings where
        toJSON ListAuditFindings{..}
          = Core.object
              (Core.catMaybes
                 [("checkName" Core..=) Core.<$> checkName,
                  ("endTime" Core..=) Core.<$> endTime,
                  ("listSuppressedFindings" Core..=) Core.<$> listSuppressedFindings,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("resourceIdentifier" Core..=) Core.<$> resourceIdentifier,
                  ("startTime" Core..=) Core.<$> startTime,
                  ("taskId" Core..=) Core.<$> taskId])

instance Core.AWSRequest ListAuditFindings where
        type Rs ListAuditFindings = ListAuditFindingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/audit/findings",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAuditFindingsResponse' Core.<$>
                   (x Core..:? "findings") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAuditFindings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"findings" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAuditFindingsResponse' smart constructor.
data ListAuditFindingsResponse = ListAuditFindingsResponse'
  { findings :: Core.Maybe [Types.AuditFinding]
    -- ^ The findings (results) of the audit.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAuditFindingsResponse' value with any optional fields omitted.
mkListAuditFindingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAuditFindingsResponse
mkListAuditFindingsResponse responseStatus
  = ListAuditFindingsResponse'{findings = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | The findings (results) of the audit.
--
-- /Note:/ Consider using 'findings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrrsFindings :: Lens.Lens' ListAuditFindingsResponse (Core.Maybe [Types.AuditFinding])
lafrrsFindings = Lens.field @"findings"
{-# INLINEABLE lafrrsFindings #-}
{-# DEPRECATED findings "Use generic-lens or generic-optics with 'findings' instead"  #-}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrrsNextToken :: Lens.Lens' ListAuditFindingsResponse (Core.Maybe Types.NextToken)
lafrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lafrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrrsResponseStatus :: Lens.Lens' ListAuditFindingsResponse Core.Int
lafrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lafrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
