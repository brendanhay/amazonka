{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListMailboxExportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox export jobs started for the specified organization within the last seven days.
module Network.AWS.WorkMail.ListMailboxExportJobs
    (
    -- * Creating a request
      ListMailboxExportJobs (..)
    , mkListMailboxExportJobs
    -- ** Request lenses
    , lmejOrganizationId
    , lmejMaxResults
    , lmejNextToken

    -- * Destructuring the response
    , ListMailboxExportJobsResponse (..)
    , mkListMailboxExportJobsResponse
    -- ** Response lenses
    , lmejrrsJobs
    , lmejrrsNextToken
    , lmejrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkListMailboxExportJobs' smart constructor.
data ListMailboxExportJobs = ListMailboxExportJobs'
  { organizationId :: Types.OrganizationId
    -- ^ The organization ID.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMailboxExportJobs' value with any optional fields omitted.
mkListMailboxExportJobs
    :: Types.OrganizationId -- ^ 'organizationId'
    -> ListMailboxExportJobs
mkListMailboxExportJobs organizationId
  = ListMailboxExportJobs'{organizationId, maxResults = Core.Nothing,
                           nextToken = Core.Nothing}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejOrganizationId :: Lens.Lens' ListMailboxExportJobs Types.OrganizationId
lmejOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE lmejOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejMaxResults :: Lens.Lens' ListMailboxExportJobs (Core.Maybe Core.Natural)
lmejMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lmejMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejNextToken :: Lens.Lens' ListMailboxExportJobs (Core.Maybe Types.NextToken)
lmejNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmejNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListMailboxExportJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListMailboxExportJobs where
        toHeaders ListMailboxExportJobs{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.ListMailboxExportJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListMailboxExportJobs where
        toJSON ListMailboxExportJobs{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListMailboxExportJobs where
        type Rs ListMailboxExportJobs = ListMailboxExportJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListMailboxExportJobsResponse' Core.<$>
                   (x Core..:? "Jobs") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListMailboxExportJobsResponse' smart constructor.
data ListMailboxExportJobsResponse = ListMailboxExportJobsResponse'
  { jobs :: Core.Maybe [Types.MailboxExportJob]
    -- ^ The mailbox export job details.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListMailboxExportJobsResponse' value with any optional fields omitted.
mkListMailboxExportJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListMailboxExportJobsResponse
mkListMailboxExportJobsResponse responseStatus
  = ListMailboxExportJobsResponse'{jobs = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | The mailbox export job details.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejrrsJobs :: Lens.Lens' ListMailboxExportJobsResponse (Core.Maybe [Types.MailboxExportJob])
lmejrrsJobs = Lens.field @"jobs"
{-# INLINEABLE lmejrrsJobs #-}
{-# DEPRECATED jobs "Use generic-lens or generic-optics with 'jobs' instead"  #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejrrsNextToken :: Lens.Lens' ListMailboxExportJobsResponse (Core.Maybe Types.NextToken)
lmejrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmejrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejrrsResponseStatus :: Lens.Lens' ListMailboxExportJobsResponse Core.Int
lmejrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmejrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
