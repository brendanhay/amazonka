{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new group in the specified user pool.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.CreateGroup
    (
    -- * Creating a request
      CreateGroup (..)
    , mkCreateGroup
    -- ** Request lenses
    , cgGroupName
    , cgUserPoolId
    , cgDescription
    , cgPrecedence
    , cgRoleArn

    -- * Destructuring the response
    , CreateGroupResponse (..)
    , mkCreateGroupResponse
    -- ** Response lenses
    , cgrrsGroup
    , cgrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { groupName :: Types.GroupNameType
    -- ^ The name of the group. Must be unique.
  , userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool.
  , description :: Core.Maybe Types.DescriptionType
    -- ^ A string containing the description of the group.
  , precedence :: Core.Maybe Core.Natural
    -- ^ A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. Zero is the highest precedence value. Groups with lower @Precedence@ values take precedence over groups with higher or null @Precedence@ values. If a user belongs to two or more groups, it is the group with the lowest precedence value whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens.
--
-- Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens.
-- The default @Precedence@ value is null.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The role ARN for the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGroup' value with any optional fields omitted.
mkCreateGroup
    :: Types.GroupNameType -- ^ 'groupName'
    -> Types.UserPoolId -- ^ 'userPoolId'
    -> CreateGroup
mkCreateGroup groupName userPoolId
  = CreateGroup'{groupName, userPoolId, description = Core.Nothing,
                 precedence = Core.Nothing, roleArn = Core.Nothing}

-- | The name of the group. Must be unique.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgGroupName :: Lens.Lens' CreateGroup Types.GroupNameType
cgGroupName = Lens.field @"groupName"
{-# INLINEABLE cgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgUserPoolId :: Lens.Lens' CreateGroup Types.UserPoolId
cgUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE cgUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | A string containing the description of the group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgDescription :: Lens.Lens' CreateGroup (Core.Maybe Types.DescriptionType)
cgDescription = Lens.field @"description"
{-# INLINEABLE cgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. Zero is the highest precedence value. Groups with lower @Precedence@ values take precedence over groups with higher or null @Precedence@ values. If a user belongs to two or more groups, it is the group with the lowest precedence value whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens.
--
-- Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens.
-- The default @Precedence@ value is null.
--
-- /Note:/ Consider using 'precedence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgPrecedence :: Lens.Lens' CreateGroup (Core.Maybe Core.Natural)
cgPrecedence = Lens.field @"precedence"
{-# INLINEABLE cgPrecedence #-}
{-# DEPRECATED precedence "Use generic-lens or generic-optics with 'precedence' instead"  #-}

-- | The role ARN for the group.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgRoleArn :: Lens.Lens' CreateGroup (Core.Maybe Types.RoleArn)
cgRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cgRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery CreateGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateGroup where
        toHeaders CreateGroup{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityProviderService.CreateGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateGroup where
        toJSON CreateGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GroupName" Core..= groupName),
                  Core.Just ("UserPoolId" Core..= userPoolId),
                  ("Description" Core..=) Core.<$> description,
                  ("Precedence" Core..=) Core.<$> precedence,
                  ("RoleArn" Core..=) Core.<$> roleArn])

instance Core.AWSRequest CreateGroup where
        type Rs CreateGroup = CreateGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateGroupResponse' Core.<$>
                   (x Core..:? "Group") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { group :: Core.Maybe Types.GroupType
    -- ^ The group object for the group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateGroupResponse' value with any optional fields omitted.
mkCreateGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateGroupResponse
mkCreateGroupResponse responseStatus
  = CreateGroupResponse'{group = Core.Nothing, responseStatus}

-- | The group object for the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsGroup :: Lens.Lens' CreateGroupResponse (Core.Maybe Types.GroupType)
cgrrsGroup = Lens.field @"group"
{-# INLINEABLE cgrrsGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrrsResponseStatus :: Lens.Lens' CreateGroupResponse Core.Int
cgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
