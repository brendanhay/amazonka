{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeSessions (..)
    , mkDescribeSessions
    -- ** Request lenses
    , dsStackName
    , dsFleetName
    , dsAuthenticationType
    , dsLimit
    , dsNextToken
    , dsUserId

    -- * Destructuring the response
    , DescribeSessionsResponse (..)
    , mkDescribeSessionsResponse
    -- ** Response lenses
    , dsrfrsNextToken
    , dsrfrsSessions
    , dsrfrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
  { stackName :: Core.Text
    -- ^ The name of the stack. This value is case-sensitive.
  , fleetName :: Core.Text
    -- ^ The name of the fleet. This value is case-sensitive.
  , authenticationType :: Core.Maybe Types.AuthenticationType
    -- ^ The authentication method. Specify @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user. The default is to authenticate users using a streaming URL.
  , limit :: Core.Maybe Core.Int
    -- ^ The size of each page of results. The default value is 20 and the maximum value is 50.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
  , userId :: Core.Maybe Types.UserId
    -- ^ The user identifier (ID). If you specify a user ID, you must also specify the authentication type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSessions' value with any optional fields omitted.
mkDescribeSessions
    :: Core.Text -- ^ 'stackName'
    -> Core.Text -- ^ 'fleetName'
    -> DescribeSessions
mkDescribeSessions stackName fleetName
  = DescribeSessions'{stackName, fleetName,
                      authenticationType = Core.Nothing, limit = Core.Nothing,
                      nextToken = Core.Nothing, userId = Core.Nothing}

-- | The name of the stack. This value is case-sensitive.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStackName :: Lens.Lens' DescribeSessions Core.Text
dsStackName = Lens.field @"stackName"
{-# INLINEABLE dsStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

-- | The name of the fleet. This value is case-sensitive.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFleetName :: Lens.Lens' DescribeSessions Core.Text
dsFleetName = Lens.field @"fleetName"
{-# INLINEABLE dsFleetName #-}
{-# DEPRECATED fleetName "Use generic-lens or generic-optics with 'fleetName' instead"  #-}

-- | The authentication method. Specify @API@ for a user authenticated using a streaming URL or @SAML@ for a SAML federated user. The default is to authenticate users using a streaming URL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAuthenticationType :: Lens.Lens' DescribeSessions (Core.Maybe Types.AuthenticationType)
dsAuthenticationType = Lens.field @"authenticationType"
{-# INLINEABLE dsAuthenticationType #-}
{-# DEPRECATED authenticationType "Use generic-lens or generic-optics with 'authenticationType' instead"  #-}

-- | The size of each page of results. The default value is 20 and the maximum value is 50.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimit :: Lens.Lens' DescribeSessions (Core.Maybe Core.Int)
dsLimit = Lens.field @"limit"
{-# INLINEABLE dsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeSessions (Core.Maybe Core.Text)
dsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The user identifier (ID). If you specify a user ID, you must also specify the authentication type.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsUserId :: Lens.Lens' DescribeSessions (Core.Maybe Types.UserId)
dsUserId = Lens.field @"userId"
{-# INLINEABLE dsUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery DescribeSessions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSessions where
        toHeaders DescribeSessions{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.DescribeSessions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSessions where
        toJSON DescribeSessions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StackName" Core..= stackName),
                  Core.Just ("FleetName" Core..= fleetName),
                  ("AuthenticationType" Core..=) Core.<$> authenticationType,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("UserId" Core..=) Core.<$> userId])

instance Core.AWSRequest DescribeSessions where
        type Rs DescribeSessions = DescribeSessionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSessionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Sessions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSessions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"sessions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
  , sessions :: Core.Maybe [Types.Session]
    -- ^ Information about the streaming sessions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSessionsResponse' value with any optional fields omitted.
mkDescribeSessionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSessionsResponse
mkDescribeSessionsResponse responseStatus
  = DescribeSessionsResponse'{nextToken = Core.Nothing,
                              sessions = Core.Nothing, responseStatus}

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsNextToken :: Lens.Lens' DescribeSessionsResponse (Core.Maybe Core.Text)
dsrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the streaming sessions.
--
-- /Note:/ Consider using 'sessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSessions :: Lens.Lens' DescribeSessionsResponse (Core.Maybe [Types.Session])
dsrfrsSessions = Lens.field @"sessions"
{-# INLINEABLE dsrfrsSessions #-}
{-# DEPRECATED sessions "Use generic-lens or generic-optics with 'sessions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeSessionsResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
