{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists a history of user activity and any risks detected as part of Amazon Cognito advanced security.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents
    (
    -- * Creating a request
      AdminListUserAuthEvents (..)
    , mkAdminListUserAuthEvents
    -- ** Request lenses
    , aluaeUserPoolId
    , aluaeUsername
    , aluaeMaxResults
    , aluaeNextToken

    -- * Destructuring the response
    , AdminListUserAuthEventsResponse (..)
    , mkAdminListUserAuthEventsResponse
    -- ** Response lenses
    , aluaerrsAuthEvents
    , aluaerrsNextToken
    , aluaerrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminListUserAuthEvents' smart constructor.
data AdminListUserAuthEvents = AdminListUserAuthEvents'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , username :: Types.Username
    -- ^ The user pool username or an alias.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of authentication events to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminListUserAuthEvents' value with any optional fields omitted.
mkAdminListUserAuthEvents
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> AdminListUserAuthEvents
mkAdminListUserAuthEvents userPoolId username
  = AdminListUserAuthEvents'{userPoolId, username,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeUserPoolId :: Lens.Lens' AdminListUserAuthEvents Types.UserPoolId
aluaeUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE aluaeUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The user pool username or an alias.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeUsername :: Lens.Lens' AdminListUserAuthEvents Types.Username
aluaeUsername = Lens.field @"username"
{-# INLINEABLE aluaeUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The maximum number of authentication events to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeMaxResults :: Lens.Lens' AdminListUserAuthEvents (Core.Maybe Core.Natural)
aluaeMaxResults = Lens.field @"maxResults"
{-# INLINEABLE aluaeMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeNextToken :: Lens.Lens' AdminListUserAuthEvents (Core.Maybe Types.NextToken)
aluaeNextToken = Lens.field @"nextToken"
{-# INLINEABLE aluaeNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery AdminListUserAuthEvents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminListUserAuthEvents where
        toHeaders AdminListUserAuthEvents{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminListUserAuthEvents")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminListUserAuthEvents where
        toJSON AdminListUserAuthEvents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest AdminListUserAuthEvents where
        type Rs AdminListUserAuthEvents = AdminListUserAuthEventsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AdminListUserAuthEventsResponse' Core.<$>
                   (x Core..:? "AuthEvents") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager AdminListUserAuthEvents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"authEvents" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkAdminListUserAuthEventsResponse' smart constructor.
data AdminListUserAuthEventsResponse = AdminListUserAuthEventsResponse'
  { authEvents :: Core.Maybe [Types.AuthEventType]
    -- ^ The response object. It includes the @EventID@ , @EventType@ , @CreationDate@ , @EventRisk@ , and @EventResponse@ .
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AdminListUserAuthEventsResponse' value with any optional fields omitted.
mkAdminListUserAuthEventsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminListUserAuthEventsResponse
mkAdminListUserAuthEventsResponse responseStatus
  = AdminListUserAuthEventsResponse'{authEvents = Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | The response object. It includes the @EventID@ , @EventType@ , @CreationDate@ , @EventRisk@ , and @EventResponse@ .
--
-- /Note:/ Consider using 'authEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaerrsAuthEvents :: Lens.Lens' AdminListUserAuthEventsResponse (Core.Maybe [Types.AuthEventType])
aluaerrsAuthEvents = Lens.field @"authEvents"
{-# INLINEABLE aluaerrsAuthEvents #-}
{-# DEPRECATED authEvents "Use generic-lens or generic-optics with 'authEvents' instead"  #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaerrsNextToken :: Lens.Lens' AdminListUserAuthEventsResponse (Core.Maybe Types.NextToken)
aluaerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE aluaerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaerrsResponseStatus :: Lens.Lens' AdminListUserAuthEventsResponse Core.Int
aluaerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aluaerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
