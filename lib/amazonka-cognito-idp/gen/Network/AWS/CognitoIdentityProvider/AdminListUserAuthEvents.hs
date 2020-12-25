{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AdminListUserAuthEvents (..),
    mkAdminListUserAuthEvents,

    -- ** Request lenses
    aluaeUserPoolId,
    aluaeUsername,
    aluaeMaxResults,
    aluaeNextToken,

    -- * Destructuring the response
    AdminListUserAuthEventsResponse (..),
    mkAdminListUserAuthEventsResponse,

    -- ** Response lenses
    aluaerrsAuthEvents,
    aluaerrsNextToken,
    aluaerrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminListUserAuthEvents' smart constructor.
data AdminListUserAuthEvents = AdminListUserAuthEvents'
  { -- | The user pool ID.
    userPoolId :: Types.UserPoolId,
    -- | The user pool username or an alias.
    username :: Types.Username,
    -- | The maximum number of authentication events to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminListUserAuthEvents' value with any optional fields omitted.
mkAdminListUserAuthEvents ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'username'
  Types.Username ->
  AdminListUserAuthEvents
mkAdminListUserAuthEvents userPoolId username =
  AdminListUserAuthEvents'
    { userPoolId,
      username,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeUserPoolId :: Lens.Lens' AdminListUserAuthEvents Types.UserPoolId
aluaeUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED aluaeUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user pool username or an alias.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeUsername :: Lens.Lens' AdminListUserAuthEvents Types.Username
aluaeUsername = Lens.field @"username"
{-# DEPRECATED aluaeUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The maximum number of authentication events to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeMaxResults :: Lens.Lens' AdminListUserAuthEvents (Core.Maybe Core.Natural)
aluaeMaxResults = Lens.field @"maxResults"
{-# DEPRECATED aluaeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaeNextToken :: Lens.Lens' AdminListUserAuthEvents (Core.Maybe Types.NextToken)
aluaeNextToken = Lens.field @"nextToken"
{-# DEPRECATED aluaeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON AdminListUserAuthEvents where
  toJSON AdminListUserAuthEvents {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest AdminListUserAuthEvents where
  type Rs AdminListUserAuthEvents = AdminListUserAuthEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminListUserAuthEvents"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminListUserAuthEventsResponse'
            Core.<$> (x Core..:? "AuthEvents")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager AdminListUserAuthEvents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"authEvents" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkAdminListUserAuthEventsResponse' smart constructor.
data AdminListUserAuthEventsResponse = AdminListUserAuthEventsResponse'
  { -- | The response object. It includes the @EventID@ , @EventType@ , @CreationDate@ , @EventRisk@ , and @EventResponse@ .
    authEvents :: Core.Maybe [Types.AuthEventType],
    -- | A pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AdminListUserAuthEventsResponse' value with any optional fields omitted.
mkAdminListUserAuthEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AdminListUserAuthEventsResponse
mkAdminListUserAuthEventsResponse responseStatus =
  AdminListUserAuthEventsResponse'
    { authEvents = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The response object. It includes the @EventID@ , @EventType@ , @CreationDate@ , @EventRisk@ , and @EventResponse@ .
--
-- /Note:/ Consider using 'authEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaerrsAuthEvents :: Lens.Lens' AdminListUserAuthEventsResponse (Core.Maybe [Types.AuthEventType])
aluaerrsAuthEvents = Lens.field @"authEvents"
{-# DEPRECATED aluaerrsAuthEvents "Use generic-lens or generic-optics with 'authEvents' instead." #-}

-- | A pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaerrsNextToken :: Lens.Lens' AdminListUserAuthEventsResponse (Core.Maybe Types.NextToken)
aluaerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED aluaerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aluaerrsResponseStatus :: Lens.Lens' AdminListUserAuthEventsResponse Core.Int
aluaerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aluaerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
