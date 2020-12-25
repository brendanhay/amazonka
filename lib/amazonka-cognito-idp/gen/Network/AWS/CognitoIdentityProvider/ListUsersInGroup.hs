{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.ListUsersInGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users in the specified group.
--
-- Calling this action requires developer credentials.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUsersInGroup
  ( -- * Creating a request
    ListUsersInGroup (..),
    mkListUsersInGroup,

    -- ** Request lenses
    luigUserPoolId,
    luigGroupName,
    luigLimit,
    luigNextToken,

    -- * Destructuring the response
    ListUsersInGroupResponse (..),
    mkListUsersInGroupResponse,

    -- ** Response lenses
    luigrrsNextToken,
    luigrrsUsers,
    luigrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListUsersInGroup' smart constructor.
data ListUsersInGroup = ListUsersInGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolIdType,
    -- | The name of the group.
    groupName :: Types.GroupNameType,
    -- | The limit of the request to list users.
    limit :: Core.Maybe Core.Natural,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUsersInGroup' value with any optional fields omitted.
mkListUsersInGroup ::
  -- | 'userPoolId'
  Types.UserPoolIdType ->
  -- | 'groupName'
  Types.GroupNameType ->
  ListUsersInGroup
mkListUsersInGroup userPoolId groupName =
  ListUsersInGroup'
    { userPoolId,
      groupName,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigUserPoolId :: Lens.Lens' ListUsersInGroup Types.UserPoolIdType
luigUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED luigUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigGroupName :: Lens.Lens' ListUsersInGroup Types.GroupNameType
luigGroupName = Lens.field @"groupName"
{-# DEPRECATED luigGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The limit of the request to list users.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigLimit :: Lens.Lens' ListUsersInGroup (Core.Maybe Core.Natural)
luigLimit = Lens.field @"limit"
{-# DEPRECATED luigLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigNextToken :: Lens.Lens' ListUsersInGroup (Core.Maybe Types.PaginationKey)
luigNextToken = Lens.field @"nextToken"
{-# DEPRECATED luigNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListUsersInGroup where
  toJSON ListUsersInGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("GroupName" Core..= groupName),
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListUsersInGroup where
  type Rs ListUsersInGroup = ListUsersInGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.ListUsersInGroup"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersInGroupResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Users")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListUsersInGroup where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"users" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListUsersInGroupResponse' smart constructor.
data ListUsersInGroupResponse = ListUsersInGroupResponse'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The users returned in the request to list users.
    users :: Core.Maybe [Types.UserType],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListUsersInGroupResponse' value with any optional fields omitted.
mkListUsersInGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListUsersInGroupResponse
mkListUsersInGroupResponse responseStatus =
  ListUsersInGroupResponse'
    { nextToken = Core.Nothing,
      users = Core.Nothing,
      responseStatus
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigrrsNextToken :: Lens.Lens' ListUsersInGroupResponse (Core.Maybe Types.NextToken)
luigrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED luigrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The users returned in the request to list users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigrrsUsers :: Lens.Lens' ListUsersInGroupResponse (Core.Maybe [Types.UserType])
luigrrsUsers = Lens.field @"users"
{-# DEPRECATED luigrrsUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luigrrsResponseStatus :: Lens.Lens' ListUsersInGroupResponse Core.Int
luigrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED luigrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
