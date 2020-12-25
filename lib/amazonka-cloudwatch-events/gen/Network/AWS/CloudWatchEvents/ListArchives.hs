{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListArchives
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your archives. You can either list all the archives or you can provide a prefix to match to the archive names. Filter parameters are exclusive.
module Network.AWS.CloudWatchEvents.ListArchives
  ( -- * Creating a request
    ListArchives (..),
    mkListArchives,

    -- ** Request lenses
    laEventSourceArn,
    laLimit,
    laNamePrefix,
    laNextToken,
    laState,

    -- * Destructuring the response
    ListArchivesResponse (..),
    mkListArchivesResponse,

    -- ** Response lenses
    larrsArchives,
    larrsNextToken,
    larrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListArchives' smart constructor.
data ListArchives = ListArchives'
  { -- | The ARN of the event source associated with the archive.
    eventSourceArn :: Core.Maybe Types.EventSourceArn,
    -- | The maximum number of results to return.
    limit :: Core.Maybe Core.Natural,
    -- | A name prefix to filter the archives returned. Only archives with name that match the prefix are returned.
    namePrefix :: Core.Maybe Types.NamePrefix,
    -- | The token returned by a previous call to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The state of the archive.
    state :: Core.Maybe Types.ArchiveState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListArchives' value with any optional fields omitted.
mkListArchives ::
  ListArchives
mkListArchives =
  ListArchives'
    { eventSourceArn = Core.Nothing,
      limit = Core.Nothing,
      namePrefix = Core.Nothing,
      nextToken = Core.Nothing,
      state = Core.Nothing
    }

-- | The ARN of the event source associated with the archive.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laEventSourceArn :: Lens.Lens' ListArchives (Core.Maybe Types.EventSourceArn)
laEventSourceArn = Lens.field @"eventSourceArn"
{-# DEPRECATED laEventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLimit :: Lens.Lens' ListArchives (Core.Maybe Core.Natural)
laLimit = Lens.field @"limit"
{-# DEPRECATED laLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A name prefix to filter the archives returned. Only archives with name that match the prefix are returned.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNamePrefix :: Lens.Lens' ListArchives (Core.Maybe Types.NamePrefix)
laNamePrefix = Lens.field @"namePrefix"
{-# DEPRECATED laNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListArchives (Core.Maybe Types.NextToken)
laNextToken = Lens.field @"nextToken"
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The state of the archive.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laState :: Lens.Lens' ListArchives (Core.Maybe Types.ArchiveState)
laState = Lens.field @"state"
{-# DEPRECATED laState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON ListArchives where
  toJSON ListArchives {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventSourceArn" Core..=) Core.<$> eventSourceArn,
            ("Limit" Core..=) Core.<$> limit,
            ("NamePrefix" Core..=) Core.<$> namePrefix,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("State" Core..=) Core.<$> state
          ]
      )

instance Core.AWSRequest ListArchives where
  type Rs ListArchives = ListArchivesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.ListArchives")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListArchivesResponse'
            Core.<$> (x Core..:? "Archives")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListArchivesResponse' smart constructor.
data ListArchivesResponse = ListArchivesResponse'
  { -- | An array of @Archive@ objects that include details about an archive.
    archives :: Core.Maybe [Types.Archive],
    -- | The token returned by a previous call to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListArchivesResponse' value with any optional fields omitted.
mkListArchivesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListArchivesResponse
mkListArchivesResponse responseStatus =
  ListArchivesResponse'
    { archives = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @Archive@ objects that include details about an archive.
--
-- /Note:/ Consider using 'archives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsArchives :: Lens.Lens' ListArchivesResponse (Core.Maybe [Types.Archive])
larrsArchives = Lens.field @"archives"
{-# DEPRECATED larrsArchives "Use generic-lens or generic-optics with 'archives' instead." #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListArchivesResponse (Core.Maybe Types.NextToken)
larrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED larrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListArchivesResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED larrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
