{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateGroup (..)
    , mkUpdateGroup
    -- ** Request lenses
    , ugGroupName
    , ugUserPoolId
    , ugDescription
    , ugPrecedence
    , ugRoleArn

    -- * Destructuring the response
    , UpdateGroupResponse (..)
    , mkUpdateGroupResponse
    -- ** Response lenses
    , ugrrsGroup
    , ugrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { groupName :: Types.GroupNameType
    -- ^ The name of the group.
  , userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool.
  , description :: Core.Maybe Types.DescriptionType
    -- ^ A string containing the new description of the group.
  , precedence :: Core.Maybe Core.Natural
    -- ^ The new precedence value for the group. For more information about this parameter, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup> .
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroup' value with any optional fields omitted.
mkUpdateGroup
    :: Types.GroupNameType -- ^ 'groupName'
    -> Types.UserPoolId -- ^ 'userPoolId'
    -> UpdateGroup
mkUpdateGroup groupName userPoolId
  = UpdateGroup'{groupName, userPoolId, description = Core.Nothing,
                 precedence = Core.Nothing, roleArn = Core.Nothing}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup Types.GroupNameType
ugGroupName = Lens.field @"groupName"
{-# INLINEABLE ugGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserPoolId :: Lens.Lens' UpdateGroup Types.UserPoolId
ugUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE ugUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | A string containing the new description of the group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugDescription :: Lens.Lens' UpdateGroup (Core.Maybe Types.DescriptionType)
ugDescription = Lens.field @"description"
{-# INLINEABLE ugDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The new precedence value for the group. For more information about this parameter, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup> .
--
-- /Note:/ Consider using 'precedence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugPrecedence :: Lens.Lens' UpdateGroup (Core.Maybe Core.Natural)
ugPrecedence = Lens.field @"precedence"
{-# INLINEABLE ugPrecedence #-}
{-# DEPRECATED precedence "Use generic-lens or generic-optics with 'precedence' instead"  #-}

-- | The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugRoleArn :: Lens.Lens' UpdateGroup (Core.Maybe Types.RoleArn)
ugRoleArn = Lens.field @"roleArn"
{-# INLINEABLE ugRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery UpdateGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGroup where
        toHeaders UpdateGroup{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.UpdateGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateGroup where
        toJSON UpdateGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GroupName" Core..= groupName),
                  Core.Just ("UserPoolId" Core..= userPoolId),
                  ("Description" Core..=) Core.<$> description,
                  ("Precedence" Core..=) Core.<$> precedence,
                  ("RoleArn" Core..=) Core.<$> roleArn])

instance Core.AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateGroupResponse' Core.<$>
                   (x Core..:? "Group") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { group :: Core.Maybe Types.GroupType
    -- ^ The group object for the group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateGroupResponse' value with any optional fields omitted.
mkUpdateGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGroupResponse
mkUpdateGroupResponse responseStatus
  = UpdateGroupResponse'{group = Core.Nothing, responseStatus}

-- | The group object for the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrrsGroup :: Lens.Lens' UpdateGroupResponse (Core.Maybe Types.GroupType)
ugrrsGroup = Lens.field @"group"
{-# INLINEABLE ugrrsGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrrsResponseStatus :: Lens.Lens' UpdateGroupResponse Core.Int
ugrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
