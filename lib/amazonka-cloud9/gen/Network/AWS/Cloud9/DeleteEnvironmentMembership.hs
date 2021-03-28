{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DeleteEnvironmentMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an environment member from an AWS Cloud9 development environment.
module Network.AWS.Cloud9.DeleteEnvironmentMembership
    (
    -- * Creating a request
      DeleteEnvironmentMembership (..)
    , mkDeleteEnvironmentMembership
    -- ** Request lenses
    , demEnvironmentId
    , demUserArn

    -- * Destructuring the response
    , DeleteEnvironmentMembershipResponse (..)
    , mkDeleteEnvironmentMembershipResponse
    -- ** Response lenses
    , demrrsResponseStatus
    ) where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEnvironmentMembership' smart constructor.
data DeleteEnvironmentMembership = DeleteEnvironmentMembership'
  { environmentId :: Types.EnvironmentId
    -- ^ The ID of the environment to delete the environment member from.
  , userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the environment member to delete from the environment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEnvironmentMembership' value with any optional fields omitted.
mkDeleteEnvironmentMembership
    :: Types.EnvironmentId -- ^ 'environmentId'
    -> Types.UserArn -- ^ 'userArn'
    -> DeleteEnvironmentMembership
mkDeleteEnvironmentMembership environmentId userArn
  = DeleteEnvironmentMembership'{environmentId, userArn}

-- | The ID of the environment to delete the environment member from.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demEnvironmentId :: Lens.Lens' DeleteEnvironmentMembership Types.EnvironmentId
demEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE demEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the environment member to delete from the environment.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demUserArn :: Lens.Lens' DeleteEnvironmentMembership Types.UserArn
demUserArn = Lens.field @"userArn"
{-# INLINEABLE demUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.ToQuery DeleteEnvironmentMembership where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteEnvironmentMembership where
        toHeaders DeleteEnvironmentMembership{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCloud9WorkspaceManagementService.DeleteEnvironmentMembership")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteEnvironmentMembership where
        toJSON DeleteEnvironmentMembership{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("environmentId" Core..= environmentId),
                  Core.Just ("userArn" Core..= userArn)])

instance Core.AWSRequest DeleteEnvironmentMembership where
        type Rs DeleteEnvironmentMembership =
             DeleteEnvironmentMembershipResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteEnvironmentMembershipResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteEnvironmentMembershipResponse' smart constructor.
newtype DeleteEnvironmentMembershipResponse = DeleteEnvironmentMembershipResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEnvironmentMembershipResponse' value with any optional fields omitted.
mkDeleteEnvironmentMembershipResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteEnvironmentMembershipResponse
mkDeleteEnvironmentMembershipResponse responseStatus
  = DeleteEnvironmentMembershipResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
demrrsResponseStatus :: Lens.Lens' DeleteEnvironmentMembershipResponse Core.Int
demrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE demrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
