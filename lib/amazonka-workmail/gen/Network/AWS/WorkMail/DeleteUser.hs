{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user from Amazon WorkMail and all subsequent systems. Before you can delete a user, the user state must be @DISABLED@ . Use the 'DescribeUser' action to confirm the user state.
--
-- Deleting a user is permanent and cannot be undone. WorkMail archives user mailboxes for 30 days before they are permanently removed.
module Network.AWS.WorkMail.DeleteUser
    (
    -- * Creating a request
      DeleteUser (..)
    , mkDeleteUser
    -- ** Request lenses
    , dufOrganizationId
    , dufUserId

    -- * Destructuring the response
    , DeleteUserResponse (..)
    , mkDeleteUserResponse
    -- ** Response lenses
    , durfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { organizationId :: Types.OrganizationId
    -- ^ The organization that contains the user to be deleted.
  , userId :: Types.WorkMailIdentifier
    -- ^ The identifier of the user to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUser' value with any optional fields omitted.
mkDeleteUser
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.WorkMailIdentifier -- ^ 'userId'
    -> DeleteUser
mkDeleteUser organizationId userId
  = DeleteUser'{organizationId, userId}

-- | The organization that contains the user to be deleted.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufOrganizationId :: Lens.Lens' DeleteUser Types.OrganizationId
dufOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dufOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier of the user to be deleted.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufUserId :: Lens.Lens' DeleteUser Types.WorkMailIdentifier
dufUserId = Lens.field @"userId"
{-# INLINEABLE dufUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery DeleteUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUser where
        toHeaders DeleteUser{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.DeleteUser") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteUser where
        toJSON DeleteUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("UserId" Core..= userId)])

instance Core.AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteUserResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
newtype DeleteUserResponse = DeleteUserResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserResponse' value with any optional fields omitted.
mkDeleteUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteUserResponse
mkDeleteUserResponse responseStatus
  = DeleteUserResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durfrsResponseStatus :: Lens.Lens' DeleteUserResponse Core.Int
durfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE durfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
