{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DisassociateQualificationFromWorker@ revokes a previously granted Qualification from a user. 
--
-- You can provide a text message explaining why the Qualification was revoked. The user who had the Qualification can see this message. 
module Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker
    (
    -- * Creating a request
      DisassociateQualificationFromWorker (..)
    , mkDisassociateQualificationFromWorker
    -- ** Request lenses
    , dqfwWorkerId
    , dqfwQualificationTypeId
    , dqfwReason

    -- * Destructuring the response
    , DisassociateQualificationFromWorkerResponse (..)
    , mkDisassociateQualificationFromWorkerResponse
    -- ** Response lenses
    , dqfwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateQualificationFromWorker' smart constructor.
data DisassociateQualificationFromWorker = DisassociateQualificationFromWorker'
  { workerId :: Types.WorkerId
    -- ^ The ID of the Worker who possesses the Qualification to be revoked.
  , qualificationTypeId :: Types.QualificationTypeId
    -- ^ The ID of the Qualification type of the Qualification to be revoked.
  , reason :: Core.Maybe Core.Text
    -- ^ A text message that explains why the Qualification was revoked. The user who had the Qualification sees this message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateQualificationFromWorker' value with any optional fields omitted.
mkDisassociateQualificationFromWorker
    :: Types.WorkerId -- ^ 'workerId'
    -> Types.QualificationTypeId -- ^ 'qualificationTypeId'
    -> DisassociateQualificationFromWorker
mkDisassociateQualificationFromWorker workerId qualificationTypeId
  = DisassociateQualificationFromWorker'{workerId,
                                         qualificationTypeId, reason = Core.Nothing}

-- | The ID of the Worker who possesses the Qualification to be revoked.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwWorkerId :: Lens.Lens' DisassociateQualificationFromWorker Types.WorkerId
dqfwWorkerId = Lens.field @"workerId"
{-# INLINEABLE dqfwWorkerId #-}
{-# DEPRECATED workerId "Use generic-lens or generic-optics with 'workerId' instead"  #-}

-- | The ID of the Qualification type of the Qualification to be revoked.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwQualificationTypeId :: Lens.Lens' DisassociateQualificationFromWorker Types.QualificationTypeId
dqfwQualificationTypeId = Lens.field @"qualificationTypeId"
{-# INLINEABLE dqfwQualificationTypeId #-}
{-# DEPRECATED qualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead"  #-}

-- | A text message that explains why the Qualification was revoked. The user who had the Qualification sees this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwReason :: Lens.Lens' DisassociateQualificationFromWorker (Core.Maybe Core.Text)
dqfwReason = Lens.field @"reason"
{-# INLINEABLE dqfwReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.ToQuery DisassociateQualificationFromWorker where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateQualificationFromWorker where
        toHeaders DisassociateQualificationFromWorker{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.DisassociateQualificationFromWorker")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateQualificationFromWorker where
        toJSON DisassociateQualificationFromWorker{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkerId" Core..= workerId),
                  Core.Just ("QualificationTypeId" Core..= qualificationTypeId),
                  ("Reason" Core..=) Core.<$> reason])

instance Core.AWSRequest DisassociateQualificationFromWorker where
        type Rs DisassociateQualificationFromWorker =
             DisassociateQualificationFromWorkerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateQualificationFromWorkerResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateQualificationFromWorkerResponse' smart constructor.
newtype DisassociateQualificationFromWorkerResponse = DisassociateQualificationFromWorkerResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateQualificationFromWorkerResponse' value with any optional fields omitted.
mkDisassociateQualificationFromWorkerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateQualificationFromWorkerResponse
mkDisassociateQualificationFromWorkerResponse responseStatus
  = DisassociateQualificationFromWorkerResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwrrsResponseStatus :: Lens.Lens' DisassociateQualificationFromWorkerResponse Core.Int
dqfwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dqfwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
