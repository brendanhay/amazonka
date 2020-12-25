{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListMailboxExportJobs (..),
    mkListMailboxExportJobs,

    -- ** Request lenses
    lmejOrganizationId,
    lmejMaxResults,
    lmejNextToken,

    -- * Destructuring the response
    ListMailboxExportJobsResponse (..),
    mkListMailboxExportJobsResponse,

    -- ** Response lenses
    lmejrrsJobs,
    lmejrrsNextToken,
    lmejrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkListMailboxExportJobs' smart constructor.
data ListMailboxExportJobs = ListMailboxExportJobs'
  { -- | The organization ID.
    organizationId :: Types.OrganizationId,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMailboxExportJobs' value with any optional fields omitted.
mkListMailboxExportJobs ::
  -- | 'organizationId'
  Types.OrganizationId ->
  ListMailboxExportJobs
mkListMailboxExportJobs organizationId =
  ListMailboxExportJobs'
    { organizationId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejOrganizationId :: Lens.Lens' ListMailboxExportJobs Types.OrganizationId
lmejOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED lmejOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejMaxResults :: Lens.Lens' ListMailboxExportJobs (Core.Maybe Core.Natural)
lmejMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lmejMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejNextToken :: Lens.Lens' ListMailboxExportJobs (Core.Maybe Types.NextToken)
lmejNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmejNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListMailboxExportJobs where
  toJSON ListMailboxExportJobs {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListMailboxExportJobs where
  type Rs ListMailboxExportJobs = ListMailboxExportJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.ListMailboxExportJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMailboxExportJobsResponse'
            Core.<$> (x Core..:? "Jobs")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListMailboxExportJobsResponse' smart constructor.
data ListMailboxExportJobsResponse = ListMailboxExportJobsResponse'
  { -- | The mailbox export job details.
    jobs :: Core.Maybe [Types.MailboxExportJob],
    -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListMailboxExportJobsResponse' value with any optional fields omitted.
mkListMailboxExportJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListMailboxExportJobsResponse
mkListMailboxExportJobsResponse responseStatus =
  ListMailboxExportJobsResponse'
    { jobs = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The mailbox export job details.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejrrsJobs :: Lens.Lens' ListMailboxExportJobsResponse (Core.Maybe [Types.MailboxExportJob])
lmejrrsJobs = Lens.field @"jobs"
{-# DEPRECATED lmejrrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejrrsNextToken :: Lens.Lens' ListMailboxExportJobsResponse (Core.Maybe Types.NextToken)
lmejrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmejrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejrrsResponseStatus :: Lens.Lens' ListMailboxExportJobsResponse Core.Int
lmejrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lmejrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
