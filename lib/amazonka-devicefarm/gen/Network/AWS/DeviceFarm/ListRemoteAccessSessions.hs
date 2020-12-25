{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListRemoteAccessSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all currently running remote access sessions.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListRemoteAccessSessions
  ( -- * Creating a request
    ListRemoteAccessSessions (..),
    mkListRemoteAccessSessions,

    -- ** Request lenses
    lrasArn,
    lrasNextToken,

    -- * Destructuring the response
    ListRemoteAccessSessionsResponse (..),
    mkListRemoteAccessSessionsResponse,

    -- ** Response lenses
    lrasrrsNextToken,
    lrasrrsRemoteAccessSessions,
    lrasrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to return information about the remote access session.
--
-- /See:/ 'mkListRemoteAccessSessions' smart constructor.
data ListRemoteAccessSessions = ListRemoteAccessSessions'
  { -- | The Amazon Resource Name (ARN) of the project about which you are requesting information.
    arn :: Types.Arn,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRemoteAccessSessions' value with any optional fields omitted.
mkListRemoteAccessSessions ::
  -- | 'arn'
  Types.Arn ->
  ListRemoteAccessSessions
mkListRemoteAccessSessions arn =
  ListRemoteAccessSessions' {arn, nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the project about which you are requesting information.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasArn :: Lens.Lens' ListRemoteAccessSessions Types.Arn
lrasArn = Lens.field @"arn"
{-# DEPRECATED lrasArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasNextToken :: Lens.Lens' ListRemoteAccessSessions (Core.Maybe Types.PaginationToken)
lrasNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListRemoteAccessSessions where
  toJSON ListRemoteAccessSessions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("arn" Core..= arn),
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListRemoteAccessSessions where
  type Rs ListRemoteAccessSessions = ListRemoteAccessSessionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.ListRemoteAccessSessions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRemoteAccessSessionsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "remoteAccessSessions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListRemoteAccessSessions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"remoteAccessSessions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the response from the server after AWS Device Farm makes a request to return information about the remote access session.
--
-- /See:/ 'mkListRemoteAccessSessionsResponse' smart constructor.
data ListRemoteAccessSessionsResponse = ListRemoteAccessSessionsResponse'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | A container that represents the metadata from the service about each remote access session you are requesting.
    remoteAccessSessions :: Core.Maybe [Types.RemoteAccessSession],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListRemoteAccessSessionsResponse' value with any optional fields omitted.
mkListRemoteAccessSessionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRemoteAccessSessionsResponse
mkListRemoteAccessSessionsResponse responseStatus =
  ListRemoteAccessSessionsResponse'
    { nextToken = Core.Nothing,
      remoteAccessSessions = Core.Nothing,
      responseStatus
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasrrsNextToken :: Lens.Lens' ListRemoteAccessSessionsResponse (Core.Maybe Types.PaginationToken)
lrasrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrasrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A container that represents the metadata from the service about each remote access session you are requesting.
--
-- /Note:/ Consider using 'remoteAccessSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasrrsRemoteAccessSessions :: Lens.Lens' ListRemoteAccessSessionsResponse (Core.Maybe [Types.RemoteAccessSession])
lrasrrsRemoteAccessSessions = Lens.field @"remoteAccessSessions"
{-# DEPRECATED lrasrrsRemoteAccessSessions "Use generic-lens or generic-optics with 'remoteAccessSessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasrrsResponseStatus :: Lens.Lens' ListRemoteAccessSessionsResponse Core.Int
lrasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
