{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SendInvitation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an enrollment invitation email with a URL to a user. The URL is valid for 30 days or until you call this operation again, whichever comes first. 
module Network.AWS.AlexaBusiness.SendInvitation
    (
    -- * Creating a request
      SendInvitation (..)
    , mkSendInvitation
    -- ** Request lenses
    , siUserArn

    -- * Destructuring the response
    , SendInvitationResponse (..)
    , mkSendInvitationResponse
    -- ** Response lenses
    , sirrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendInvitation' smart constructor.
newtype SendInvitation = SendInvitation'
  { userArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the user to whom to send an invitation. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SendInvitation' value with any optional fields omitted.
mkSendInvitation
    :: SendInvitation
mkSendInvitation = SendInvitation'{userArn = Core.Nothing}

-- | The ARN of the user to whom to send an invitation. Required.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siUserArn :: Lens.Lens' SendInvitation (Core.Maybe Types.Arn)
siUserArn = Lens.field @"userArn"
{-# INLINEABLE siUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.ToQuery SendInvitation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SendInvitation where
        toHeaders SendInvitation{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.SendInvitation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SendInvitation where
        toJSON SendInvitation{..}
          = Core.object
              (Core.catMaybes [("UserArn" Core..=) Core.<$> userArn])

instance Core.AWSRequest SendInvitation where
        type Rs SendInvitation = SendInvitationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 SendInvitationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSendInvitationResponse' smart constructor.
newtype SendInvitationResponse = SendInvitationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SendInvitationResponse' value with any optional fields omitted.
mkSendInvitationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SendInvitationResponse
mkSendInvitationResponse responseStatus
  = SendInvitationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsResponseStatus :: Lens.Lens' SendInvitationResponse Core.Int
sirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
