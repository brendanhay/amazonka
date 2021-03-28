{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.CreateEnvironmentMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an environment member to an AWS Cloud9 development environment.
module Network.AWS.Cloud9.CreateEnvironmentMembership
    (
    -- * Creating a request
      CreateEnvironmentMembership (..)
    , mkCreateEnvironmentMembership
    -- ** Request lenses
    , cemEnvironmentId
    , cemUserArn
    , cemPermissions

    -- * Destructuring the response
    , CreateEnvironmentMembershipResponse (..)
    , mkCreateEnvironmentMembershipResponse
    -- ** Response lenses
    , cemrrsMembership
    , cemrrsResponseStatus
    ) where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEnvironmentMembership' smart constructor.
data CreateEnvironmentMembership = CreateEnvironmentMembership'
  { environmentId :: Types.EnvironmentId
    -- ^ The ID of the environment that contains the environment member you want to add.
  , userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the environment member you want to add.
  , permissions :: Types.MemberPermissions
    -- ^ The type of environment member permissions you want to associate with this environment member. Available values include:
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

-- | Creates a 'CreateEnvironmentMembership' value with any optional fields omitted.
mkCreateEnvironmentMembership
    :: Types.EnvironmentId -- ^ 'environmentId'
    -> Types.UserArn -- ^ 'userArn'
    -> Types.MemberPermissions -- ^ 'permissions'
    -> CreateEnvironmentMembership
mkCreateEnvironmentMembership environmentId userArn permissions
  = CreateEnvironmentMembership'{environmentId, userArn, permissions}

-- | The ID of the environment that contains the environment member you want to add.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemEnvironmentId :: Lens.Lens' CreateEnvironmentMembership Types.EnvironmentId
cemEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE cemEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the environment member you want to add.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemUserArn :: Lens.Lens' CreateEnvironmentMembership Types.UserArn
cemUserArn = Lens.field @"userArn"
{-# INLINEABLE cemUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The type of environment member permissions you want to associate with this environment member. Available values include:
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
cemPermissions :: Lens.Lens' CreateEnvironmentMembership Types.MemberPermissions
cemPermissions = Lens.field @"permissions"
{-# INLINEABLE cemPermissions #-}
{-# DEPRECATED permissions "Use generic-lens or generic-optics with 'permissions' instead"  #-}

instance Core.ToQuery CreateEnvironmentMembership where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateEnvironmentMembership where
        toHeaders CreateEnvironmentMembership{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCloud9WorkspaceManagementService.CreateEnvironmentMembership")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateEnvironmentMembership where
        toJSON CreateEnvironmentMembership{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("environmentId" Core..= environmentId),
                  Core.Just ("userArn" Core..= userArn),
                  Core.Just ("permissions" Core..= permissions)])

instance Core.AWSRequest CreateEnvironmentMembership where
        type Rs CreateEnvironmentMembership =
             CreateEnvironmentMembershipResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateEnvironmentMembershipResponse' Core.<$>
                   (x Core..:? "membership") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateEnvironmentMembershipResponse' smart constructor.
data CreateEnvironmentMembershipResponse = CreateEnvironmentMembershipResponse'
  { membership :: Core.Maybe Types.EnvironmentMember
    -- ^ Information about the environment member that was added.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateEnvironmentMembershipResponse' value with any optional fields omitted.
mkCreateEnvironmentMembershipResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateEnvironmentMembershipResponse
mkCreateEnvironmentMembershipResponse responseStatus
  = CreateEnvironmentMembershipResponse'{membership = Core.Nothing,
                                         responseStatus}

-- | Information about the environment member that was added.
--
-- /Note:/ Consider using 'membership' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemrrsMembership :: Lens.Lens' CreateEnvironmentMembershipResponse (Core.Maybe Types.EnvironmentMember)
cemrrsMembership = Lens.field @"membership"
{-# INLINEABLE cemrrsMembership #-}
{-# DEPRECATED membership "Use generic-lens or generic-optics with 'membership' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemrrsResponseStatus :: Lens.Lens' CreateEnvironmentMembershipResponse Core.Int
cemrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cemrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
