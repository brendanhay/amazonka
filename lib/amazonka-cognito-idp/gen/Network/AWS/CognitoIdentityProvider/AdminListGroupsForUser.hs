{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the groups that the user belongs to.
--
-- Calling this action requires developer credentials.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser
  ( -- * Creating a request
    AdminListGroupsForUser (..),
    mkAdminListGroupsForUser,

    -- ** Request lenses
    algfuUsername,
    algfuUserPoolId,
    algfuLimit,
    algfuNextToken,

    -- * Destructuring the response
    AdminListGroupsForUserResponse (..),
    mkAdminListGroupsForUserResponse,

    -- ** Response lenses
    algfurrsGroups,
    algfurrsNextToken,
    algfurrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminListGroupsForUser' smart constructor.
data AdminListGroupsForUser = AdminListGroupsForUser'
  { -- | The username for the user.
    username :: Types.Username,
    -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolId,
    -- | The limit of the request to list groups.
    limit :: Core.Maybe Core.Natural,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminListGroupsForUser' value with any optional fields omitted.
mkAdminListGroupsForUser ::
  -- | 'username'
  Types.Username ->
  -- | 'userPoolId'
  Types.UserPoolId ->
  AdminListGroupsForUser
mkAdminListGroupsForUser username userPoolId =
  AdminListGroupsForUser'
    { username,
      userPoolId,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The username for the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfuUsername :: Lens.Lens' AdminListGroupsForUser Types.Username
algfuUsername = Lens.field @"username"
{-# DEPRECATED algfuUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfuUserPoolId :: Lens.Lens' AdminListGroupsForUser Types.UserPoolId
algfuUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED algfuUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The limit of the request to list groups.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfuLimit :: Lens.Lens' AdminListGroupsForUser (Core.Maybe Core.Natural)
algfuLimit = Lens.field @"limit"
{-# DEPRECATED algfuLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfuNextToken :: Lens.Lens' AdminListGroupsForUser (Core.Maybe Types.NextToken)
algfuNextToken = Lens.field @"nextToken"
{-# DEPRECATED algfuNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON AdminListGroupsForUser where
  toJSON AdminListGroupsForUser {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Username" Core..= username),
            Core.Just ("UserPoolId" Core..= userPoolId),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest AdminListGroupsForUser where
  type Rs AdminListGroupsForUser = AdminListGroupsForUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AdminListGroupsForUser"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminListGroupsForUserResponse'
            Core.<$> (x Core..:? "Groups")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager AdminListGroupsForUser where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"groups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkAdminListGroupsForUserResponse' smart constructor.
data AdminListGroupsForUserResponse = AdminListGroupsForUserResponse'
  { -- | The groups that the user belongs to.
    groups :: Core.Maybe [Types.GroupType],
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AdminListGroupsForUserResponse' value with any optional fields omitted.
mkAdminListGroupsForUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AdminListGroupsForUserResponse
mkAdminListGroupsForUserResponse responseStatus =
  AdminListGroupsForUserResponse'
    { groups = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The groups that the user belongs to.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfurrsGroups :: Lens.Lens' AdminListGroupsForUserResponse (Core.Maybe [Types.GroupType])
algfurrsGroups = Lens.field @"groups"
{-# DEPRECATED algfurrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfurrsNextToken :: Lens.Lens' AdminListGroupsForUserResponse (Core.Maybe Types.NextToken)
algfurrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED algfurrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
algfurrsResponseStatus :: Lens.Lens' AdminListGroupsForUserResponse Core.Int
algfurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED algfurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
