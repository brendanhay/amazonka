{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.RevokeInvitation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes an invitation and invalidates the enrollment URL.
module Network.AWS.AlexaBusiness.RevokeInvitation
    (
    -- * Creating a request
      RevokeInvitation (..)
    , mkRevokeInvitation
    -- ** Request lenses
    , riEnrollmentId
    , riUserArn

    -- * Destructuring the response
    , RevokeInvitationResponse (..)
    , mkRevokeInvitationResponse
    -- ** Response lenses
    , rirrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRevokeInvitation' smart constructor.
data RevokeInvitation = RevokeInvitation'
  { enrollmentId :: Core.Maybe Types.EnrollmentId
    -- ^ The ARN of the enrollment invitation to revoke. Required.
  , userArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the user for whom to revoke an enrollment invitation. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeInvitation' value with any optional fields omitted.
mkRevokeInvitation
    :: RevokeInvitation
mkRevokeInvitation
  = RevokeInvitation'{enrollmentId = Core.Nothing,
                      userArn = Core.Nothing}

-- | The ARN of the enrollment invitation to revoke. Required.
--
-- /Note:/ Consider using 'enrollmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEnrollmentId :: Lens.Lens' RevokeInvitation (Core.Maybe Types.EnrollmentId)
riEnrollmentId = Lens.field @"enrollmentId"
{-# INLINEABLE riEnrollmentId #-}
{-# DEPRECATED enrollmentId "Use generic-lens or generic-optics with 'enrollmentId' instead"  #-}

-- | The ARN of the user for whom to revoke an enrollment invitation. Required.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riUserArn :: Lens.Lens' RevokeInvitation (Core.Maybe Types.Arn)
riUserArn = Lens.field @"userArn"
{-# INLINEABLE riUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.ToQuery RevokeInvitation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RevokeInvitation where
        toHeaders RevokeInvitation{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.RevokeInvitation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RevokeInvitation where
        toJSON RevokeInvitation{..}
          = Core.object
              (Core.catMaybes
                 [("EnrollmentId" Core..=) Core.<$> enrollmentId,
                  ("UserArn" Core..=) Core.<$> userArn])

instance Core.AWSRequest RevokeInvitation where
        type Rs RevokeInvitation = RevokeInvitationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RevokeInvitationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRevokeInvitationResponse' smart constructor.
newtype RevokeInvitationResponse = RevokeInvitationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeInvitationResponse' value with any optional fields omitted.
mkRevokeInvitationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RevokeInvitationResponse
mkRevokeInvitationResponse responseStatus
  = RevokeInvitationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsResponseStatus :: Lens.Lens' RevokeInvitationResponse Core.Int
rirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
