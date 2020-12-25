{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.GetGroup
  ( -- * Creating a request
    GetGroup (..),
    mkGetGroup,

    -- ** Request lenses
    ggGroupName,
    ggUserPoolId,

    -- * Destructuring the response
    GetGroupResponse (..),
    mkGetGroupResponse,

    -- ** Response lenses
    ggrrsGroup,
    ggrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { -- | The name of the group.
    groupName :: Types.GroupNameType,
    -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroup' value with any optional fields omitted.
mkGetGroup ::
  -- | 'groupName'
  Types.GroupNameType ->
  -- | 'userPoolId'
  Types.UserPoolIdType ->
  GetGroup
mkGetGroup groupName userPoolId = GetGroup' {groupName, userPoolId}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupName :: Lens.Lens' GetGroup Types.GroupNameType
ggGroupName = Lens.field @"groupName"
{-# DEPRECATED ggGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggUserPoolId :: Lens.Lens' GetGroup Types.UserPoolIdType
ggUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED ggUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Core.FromJSON GetGroup where
  toJSON GetGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GroupName" Core..= groupName),
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.AWSRequest GetGroup where
  type Rs GetGroup = GetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityProviderService.GetGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupResponse'
            Core.<$> (x Core..:? "Group") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { -- | The group object for the group.
    group :: Core.Maybe Types.GroupType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetGroupResponse' value with any optional fields omitted.
mkGetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetGroupResponse
mkGetGroupResponse responseStatus =
  GetGroupResponse' {group = Core.Nothing, responseStatus}

-- | The group object for the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsGroup :: Lens.Lens' GetGroupResponse (Core.Maybe Types.GroupType)
ggrrsGroup = Lens.field @"group"
{-# DEPRECATED ggrrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsResponseStatus :: Lens.Lens' GetGroupResponse Core.Int
ggrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
