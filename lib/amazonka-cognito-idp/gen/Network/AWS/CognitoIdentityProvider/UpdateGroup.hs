{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified group with the specified attributes.
--
-- Calling this action requires developer credentials.
-- /Important:/ If you don't provide a value for an attribute, it will be set to the default value.
module Network.AWS.CognitoIdentityProvider.UpdateGroup
  ( -- * Creating a request
    UpdateGroup (..),
    mkUpdateGroup,

    -- ** Request lenses
    ugGroupName,
    ugUserPoolId,
    ugDescription,
    ugPrecedence,
    ugRoleArn,

    -- * Destructuring the response
    UpdateGroupResponse (..),
    mkUpdateGroupResponse,

    -- ** Response lenses
    ugrrsGroup,
    ugrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The name of the group.
    groupName :: Types.GroupNameType,
    -- | The user pool ID for the user pool.
    userPoolId :: Types.UserPoolId,
    -- | A string containing the new description of the group.
    description :: Core.Maybe Types.DescriptionType,
    -- | The new precedence value for the group. For more information about this parameter, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup> .
    precedence :: Core.Maybe Core.Natural,
    -- | The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
    roleArn :: Core.Maybe Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroup' value with any optional fields omitted.
mkUpdateGroup ::
  -- | 'groupName'
  Types.GroupNameType ->
  -- | 'userPoolId'
  Types.UserPoolId ->
  UpdateGroup
mkUpdateGroup groupName userPoolId =
  UpdateGroup'
    { groupName,
      userPoolId,
      description = Core.Nothing,
      precedence = Core.Nothing,
      roleArn = Core.Nothing
    }

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup Types.GroupNameType
ugGroupName = Lens.field @"groupName"
{-# DEPRECATED ugGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserPoolId :: Lens.Lens' UpdateGroup Types.UserPoolId
ugUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED ugUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | A string containing the new description of the group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugDescription :: Lens.Lens' UpdateGroup (Core.Maybe Types.DescriptionType)
ugDescription = Lens.field @"description"
{-# DEPRECATED ugDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new precedence value for the group. For more information about this parameter, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup> .
--
-- /Note:/ Consider using 'precedence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugPrecedence :: Lens.Lens' UpdateGroup (Core.Maybe Core.Natural)
ugPrecedence = Lens.field @"precedence"
{-# DEPRECATED ugPrecedence "Use generic-lens or generic-optics with 'precedence' instead." #-}

-- | The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugRoleArn :: Lens.Lens' UpdateGroup (Core.Maybe Types.RoleArn)
ugRoleArn = Lens.field @"roleArn"
{-# DEPRECATED ugRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON UpdateGroup where
  toJSON UpdateGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GroupName" Core..= groupName),
            Core.Just ("UserPoolId" Core..= userPoolId),
            ("Description" Core..=) Core.<$> description,
            ("Precedence" Core..=) Core.<$> precedence,
            ("RoleArn" Core..=) Core.<$> roleArn
          ]
      )

instance Core.AWSRequest UpdateGroup where
  type Rs UpdateGroup = UpdateGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityProviderService.UpdateGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Core.<$> (x Core..:? "Group") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The group object for the group.
    group :: Core.Maybe Types.GroupType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateGroupResponse' value with any optional fields omitted.
mkUpdateGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGroupResponse
mkUpdateGroupResponse responseStatus =
  UpdateGroupResponse' {group = Core.Nothing, responseStatus}

-- | The group object for the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrrsGroup :: Lens.Lens' UpdateGroupResponse (Core.Maybe Types.GroupType)
ugrrsGroup = Lens.field @"group"
{-# DEPRECATED ugrrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrrsResponseStatus :: Lens.Lens' UpdateGroupResponse Core.Int
ugrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ugrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
