{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.UpdateEnvironmentMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing environment member for an AWS Cloud9 development environment.
module Network.AWS.Cloud9.UpdateEnvironmentMembership
    (
    -- * Creating a request
      UpdateEnvironmentMembership (..)
    , mkUpdateEnvironmentMembership
    -- ** Request lenses
    , uemEnvironmentId
    , uemUserArn
    , uemPermissions

    -- * Destructuring the response
    , UpdateEnvironmentMembershipResponse (..)
    , mkUpdateEnvironmentMembershipResponse
    -- ** Response lenses
    , uemrrsMembership
    , uemrrsResponseStatus
    ) where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEnvironmentMembership' smart constructor.
data UpdateEnvironmentMembership = UpdateEnvironmentMembership'
  { environmentId :: Types.EnvironmentId
    -- ^ The ID of the environment for the environment member whose settings you want to change.
  , userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the environment member whose settings you want to change.
  , permissions :: Types.MemberPermissions
    -- ^ The replacement type of environment member permissions you want to associate with this environment member. Available values include:
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEnvironmentMembership' value with any optional fields omitted.
mkUpdateEnvironmentMembership
    :: Types.EnvironmentId -- ^ 'environmentId'
    -> Types.UserArn -- ^ 'userArn'
    -> Types.MemberPermissions -- ^ 'permissions'
    -> UpdateEnvironmentMembership
mkUpdateEnvironmentMembership environmentId userArn permissions
  = UpdateEnvironmentMembership'{environmentId, userArn, permissions}

-- | The ID of the environment for the environment member whose settings you want to change.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemEnvironmentId :: Lens.Lens' UpdateEnvironmentMembership Types.EnvironmentId
uemEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE uemEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the environment member whose settings you want to change.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemUserArn :: Lens.Lens' UpdateEnvironmentMembership Types.UserArn
uemUserArn = Lens.field @"userArn"
{-# INLINEABLE uemUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The replacement type of environment member permissions you want to associate with this environment member. Available values include:
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemPermissions :: Lens.Lens' UpdateEnvironmentMembership Types.MemberPermissions
uemPermissions = Lens.field @"permissions"
{-# INLINEABLE uemPermissions #-}
{-# DEPRECATED permissions "Use generic-lens or generic-optics with 'permissions' instead"  #-}

instance Core.ToQuery UpdateEnvironmentMembership where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateEnvironmentMembership where
        toHeaders UpdateEnvironmentMembership{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCloud9WorkspaceManagementService.UpdateEnvironmentMembership")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateEnvironmentMembership where
        toJSON UpdateEnvironmentMembership{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("environmentId" Core..= environmentId),
                  Core.Just ("userArn" Core..= userArn),
                  Core.Just ("permissions" Core..= permissions)])

instance Core.AWSRequest UpdateEnvironmentMembership where
        type Rs UpdateEnvironmentMembership =
             UpdateEnvironmentMembershipResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateEnvironmentMembershipResponse' Core.<$>
                   (x Core..:? "membership") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateEnvironmentMembershipResponse' smart constructor.
data UpdateEnvironmentMembershipResponse = UpdateEnvironmentMembershipResponse'
  { membership :: Core.Maybe Types.EnvironmentMember
    -- ^ Information about the environment member whose settings were changed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateEnvironmentMembershipResponse' value with any optional fields omitted.
mkUpdateEnvironmentMembershipResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateEnvironmentMembershipResponse
mkUpdateEnvironmentMembershipResponse responseStatus
  = UpdateEnvironmentMembershipResponse'{membership = Core.Nothing,
                                         responseStatus}

-- | Information about the environment member whose settings were changed.
--
-- /Note:/ Consider using 'membership' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemrrsMembership :: Lens.Lens' UpdateEnvironmentMembershipResponse (Core.Maybe Types.EnvironmentMember)
uemrrsMembership = Lens.field @"membership"
{-# INLINEABLE uemrrsMembership #-}
{-# DEPRECATED membership "Use generic-lens or generic-optics with 'membership' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemrrsResponseStatus :: Lens.Lens' UpdateEnvironmentMembershipResponse Core.Int
uemrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uemrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
