{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified user by user ARN and enrollment ARN.
module Network.AWS.AlexaBusiness.DeleteUser
    (
    -- * Creating a request
      DeleteUser (..)
    , mkDeleteUser
    -- ** Request lenses
    , duEnrollmentId
    , duUserArn

    -- * Destructuring the response
    , DeleteUserResponse (..)
    , mkDeleteUserResponse
    -- ** Response lenses
    , durrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { enrollmentId :: Types.EnrollmentId
    -- ^ The ARN of the user's enrollment in the organization. Required.
  , userArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the user to delete in the organization. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUser' value with any optional fields omitted.
mkDeleteUser
    :: Types.EnrollmentId -- ^ 'enrollmentId'
    -> DeleteUser
mkDeleteUser enrollmentId
  = DeleteUser'{enrollmentId, userArn = Core.Nothing}

-- | The ARN of the user's enrollment in the organization. Required.
--
-- /Note:/ Consider using 'enrollmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duEnrollmentId :: Lens.Lens' DeleteUser Types.EnrollmentId
duEnrollmentId = Lens.field @"enrollmentId"
{-# INLINEABLE duEnrollmentId #-}
{-# DEPRECATED enrollmentId "Use generic-lens or generic-optics with 'enrollmentId' instead"  #-}

-- | The ARN of the user to delete in the organization. Required.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserArn :: Lens.Lens' DeleteUser (Core.Maybe Types.Arn)
duUserArn = Lens.field @"userArn"
{-# INLINEABLE duUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.ToQuery DeleteUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUser where
        toHeaders DeleteUser{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteUser") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteUser where
        toJSON DeleteUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EnrollmentId" Core..= enrollmentId),
                  ("UserArn" Core..=) Core.<$> userArn])

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
durrsResponseStatus :: Lens.Lens' DeleteUserResponse Core.Int
durrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE durrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
