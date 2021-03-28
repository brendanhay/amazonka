{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.AcceptInvitation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the invitation to be monitored by a master GuardDuty account.
module Network.AWS.GuardDuty.AcceptInvitation
    (
    -- * Creating a request
      AcceptInvitation (..)
    , mkAcceptInvitation
    -- ** Request lenses
    , aiDetectorId
    , aiMasterId
    , aiInvitationId

    -- * Destructuring the response
    , AcceptInvitationResponse (..)
    , mkAcceptInvitationResponse
    -- ** Response lenses
    , airrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptInvitation' smart constructor.
data AcceptInvitation = AcceptInvitation'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector of the GuardDuty member account.
  , masterId :: Core.Text
    -- ^ The account ID of the master GuardDuty account whose invitation you're accepting.
  , invitationId :: Core.Text
    -- ^ The value that is used to validate the master account to the member account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptInvitation' value with any optional fields omitted.
mkAcceptInvitation
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.Text -- ^ 'masterId'
    -> Core.Text -- ^ 'invitationId'
    -> AcceptInvitation
mkAcceptInvitation detectorId masterId invitationId
  = AcceptInvitation'{detectorId, masterId, invitationId}

-- | The unique ID of the detector of the GuardDuty member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiDetectorId :: Lens.Lens' AcceptInvitation Types.DetectorId
aiDetectorId = Lens.field @"detectorId"
{-# INLINEABLE aiDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The account ID of the master GuardDuty account whose invitation you're accepting.
--
-- /Note:/ Consider using 'masterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiMasterId :: Lens.Lens' AcceptInvitation Core.Text
aiMasterId = Lens.field @"masterId"
{-# INLINEABLE aiMasterId #-}
{-# DEPRECATED masterId "Use generic-lens or generic-optics with 'masterId' instead"  #-}

-- | The value that is used to validate the master account to the member account.
--
-- /Note:/ Consider using 'invitationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInvitationId :: Lens.Lens' AcceptInvitation Core.Text
aiInvitationId = Lens.field @"invitationId"
{-# INLINEABLE aiInvitationId #-}
{-# DEPRECATED invitationId "Use generic-lens or generic-optics with 'invitationId' instead"  #-}

instance Core.ToQuery AcceptInvitation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AcceptInvitation where
        toHeaders AcceptInvitation{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AcceptInvitation where
        toJSON AcceptInvitation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("masterId" Core..= masterId),
                  Core.Just ("invitationId" Core..= invitationId)])

instance Core.AWSRequest AcceptInvitation where
        type Rs AcceptInvitation = AcceptInvitationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/master",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AcceptInvitationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptInvitationResponse' smart constructor.
newtype AcceptInvitationResponse = AcceptInvitationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptInvitationResponse' value with any optional fields omitted.
mkAcceptInvitationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptInvitationResponse
mkAcceptInvitationResponse responseStatus
  = AcceptInvitationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airrsResponseStatus :: Lens.Lens' AcceptInvitationResponse Core.Int
airrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE airrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
