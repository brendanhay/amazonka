{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the streaming sessions for a specified stack and fleet. If a UserId is provided for the stack and fleet, only streaming sessions for that user are described. If an authentication type is not provided, the default is to authenticate users using a streaming URL.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeSessions
  ( -- * Creating a request
    DescribeSessions (..),
    mkDescribeSessions,

    -- ** Request lenses
    dsStackName,
    dsFleetName,
    dsAuthenticationType,
    dsLimit,
    dsNextToken,
    dsUserId,

    -- * Destructuring the response
    DescribeSessionsResponse (..),
    mkDescribeSessionsResponse,

    -- ** Response lenses
    dsrfrsNextToken,
    dsrfrsSessions,
    dsrfrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
  { -- | The name of the stack. This value is case-sensitive.
    stackName :: Types.String,
    -- | The name of the fleet. This value is case-sensitive.
    fleetName :: Types.String,
    -- | The authentication method. Specify @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user. The default is to authenticate users using a streaming URL.
    authenticationType :: Core.Maybe Types.AuthenticationType,
    -- | The size of each page of results. The default value is 20 and the maximum value is 50.
    limit :: Core.Maybe Core.Int,
    -- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Types.String,
    -- | The user identifier (ID). If you specify a user ID, you must also specify the authentication type.
    userId :: Core.Maybe Types.UserId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSessions' value with any optional fields omitted.
mkDescribeSessions ::
  -- | 'stackName'
  Types.String ->
  -- | 'fleetName'
  Types.String ->
  DescribeSessions
mkDescribeSessions stackName fleetName =
  DescribeSessions'
    { stackName,
      fleetName,
      authenticationType = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing,
      userId = Core.Nothing
    }

-- | The name of the stack. This value is case-sensitive.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStackName :: Lens.Lens' DescribeSessions Types.String
dsStackName = Lens.field @"stackName"
{-# DEPRECATED dsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name of the fleet. This value is case-sensitive.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFleetName :: Lens.Lens' DescribeSessions Types.String
dsFleetName = Lens.field @"fleetName"
{-# DEPRECATED dsFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The authentication method. Specify @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user. The default is to authenticate users using a streaming URL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAuthenticationType :: Lens.Lens' DescribeSessions (Core.Maybe Types.AuthenticationType)
dsAuthenticationType = Lens.field @"authenticationType"
{-# DEPRECATED dsAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

-- | The size of each page of results. The default value is 20 and the maximum value is 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimit :: Lens.Lens' DescribeSessions (Core.Maybe Core.Int)
dsLimit = Lens.field @"limit"
{-# DEPRECATED dsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeSessions (Core.Maybe Types.String)
dsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The user identifier (ID). If you specify a user ID, you must also specify the authentication type.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsUserId :: Lens.Lens' DescribeSessions (Core.Maybe Types.UserId)
dsUserId = Lens.field @"userId"
{-# DEPRECATED dsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.FromJSON DescribeSessions where
  toJSON DescribeSessions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackName" Core..= stackName),
            Core.Just ("FleetName" Core..= fleetName),
            ("AuthenticationType" Core..=) Core.<$> authenticationType,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("UserId" Core..=) Core.<$> userId
          ]
      )

instance Core.AWSRequest DescribeSessions where
  type Rs DescribeSessions = DescribeSessionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.DescribeSessions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSessionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Sessions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSessions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"sessions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
  { -- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the streaming sessions.
    sessions :: Core.Maybe [Types.Session],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSessionsResponse' value with any optional fields omitted.
mkDescribeSessionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSessionsResponse
mkDescribeSessionsResponse responseStatus =
  DescribeSessionsResponse'
    { nextToken = Core.Nothing,
      sessions = Core.Nothing,
      responseStatus
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsNextToken :: Lens.Lens' DescribeSessionsResponse (Core.Maybe Types.String)
dsrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the streaming sessions.
--
-- /Note:/ Consider using 'sessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSessions :: Lens.Lens' DescribeSessionsResponse (Core.Maybe [Types.Session])
dsrfrsSessions = Lens.field @"sessions"
{-# DEPRECATED dsrfrsSessions "Use generic-lens or generic-optics with 'sessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeSessionsResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
