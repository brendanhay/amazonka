{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
    (
    -- * Creating a request
      AdminRemoveUserFromGroup (..)
    , mkAdminRemoveUserFromGroup
    -- ** Request lenses
    , arufgUserPoolId
    , arufgUsername
    , arufgGroupName

    -- * Destructuring the response
    , AdminRemoveUserFromGroupResponse (..)
    , mkAdminRemoveUserFromGroupResponse
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminRemoveUserFromGroup' smart constructor.
data AdminRemoveUserFromGroup = AdminRemoveUserFromGroup'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool.
  , username :: Types.Username
    -- ^ The username for the user.
  , groupName :: Types.GroupNameType
    -- ^ The group name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminRemoveUserFromGroup' value with any optional fields omitted.
mkAdminRemoveUserFromGroup
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Username -- ^ 'username'
    -> Types.GroupNameType -- ^ 'groupName'
    -> AdminRemoveUserFromGroup
mkAdminRemoveUserFromGroup userPoolId username groupName
  = AdminRemoveUserFromGroup'{userPoolId, username, groupName}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arufgUserPoolId :: Lens.Lens' AdminRemoveUserFromGroup Types.UserPoolId
arufgUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE arufgUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The username for the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arufgUsername :: Lens.Lens' AdminRemoveUserFromGroup Types.Username
arufgUsername = Lens.field @"username"
{-# INLINEABLE arufgUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The group name.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arufgGroupName :: Lens.Lens' AdminRemoveUserFromGroup Types.GroupNameType
arufgGroupName = Lens.field @"groupName"
{-# INLINEABLE arufgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery AdminRemoveUserFromGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminRemoveUserFromGroup where
        toHeaders AdminRemoveUserFromGroup{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminRemoveUserFromGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminRemoveUserFromGroup where
        toJSON AdminRemoveUserFromGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Username" Core..= username),
                  Core.Just ("GroupName" Core..= groupName)])

instance Core.AWSRequest AdminRemoveUserFromGroup where
        type Rs AdminRemoveUserFromGroup = AdminRemoveUserFromGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull AdminRemoveUserFromGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAdminRemoveUserFromGroupResponse' smart constructor.
data AdminRemoveUserFromGroupResponse = AdminRemoveUserFromGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminRemoveUserFromGroupResponse' value with any optional fields omitted.
mkAdminRemoveUserFromGroupResponse
    :: AdminRemoveUserFromGroupResponse
mkAdminRemoveUserFromGroupResponse
  = AdminRemoveUserFromGroupResponse'
