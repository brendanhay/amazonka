{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DisassociateQualificationFromWorker (..),
    mkDisassociateQualificationFromWorker,

    -- ** Request lenses
    dqfwWorkerId,
    dqfwQualificationTypeId,
    dqfwReason,

    -- * Destructuring the response
    DisassociateQualificationFromWorkerResponse (..),
    mkDisassociateQualificationFromWorkerResponse,

    -- ** Response lenses
    dqfwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateQualificationFromWorker' smart constructor.
data DisassociateQualificationFromWorker = DisassociateQualificationFromWorker'
  { -- | The ID of the Worker who possesses the Qualification to be revoked.
    workerId :: Types.WorkerId,
    -- | The ID of the Qualification type of the Qualification to be revoked.
    qualificationTypeId :: Types.QualificationTypeId,
    -- | A text message that explains why the Qualification was revoked. The user who had the Qualification sees this message.
    reason :: Core.Maybe Types.Reason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateQualificationFromWorker' value with any optional fields omitted.
mkDisassociateQualificationFromWorker ::
  -- | 'workerId'
  Types.WorkerId ->
  -- | 'qualificationTypeId'
  Types.QualificationTypeId ->
  DisassociateQualificationFromWorker
mkDisassociateQualificationFromWorker workerId qualificationTypeId =
  DisassociateQualificationFromWorker'
    { workerId,
      qualificationTypeId,
      reason = Core.Nothing
    }

-- | The ID of the Worker who possesses the Qualification to be revoked.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwWorkerId :: Lens.Lens' DisassociateQualificationFromWorker Types.WorkerId
dqfwWorkerId = Lens.field @"workerId"
{-# DEPRECATED dqfwWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

-- | The ID of the Qualification type of the Qualification to be revoked.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwQualificationTypeId :: Lens.Lens' DisassociateQualificationFromWorker Types.QualificationTypeId
dqfwQualificationTypeId = Lens.field @"qualificationTypeId"
{-# DEPRECATED dqfwQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | A text message that explains why the Qualification was revoked. The user who had the Qualification sees this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwReason :: Lens.Lens' DisassociateQualificationFromWorker (Core.Maybe Types.Reason)
dqfwReason = Lens.field @"reason"
{-# DEPRECATED dqfwReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON DisassociateQualificationFromWorker where
  toJSON DisassociateQualificationFromWorker {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WorkerId" Core..= workerId),
            Core.Just ("QualificationTypeId" Core..= qualificationTypeId),
            ("Reason" Core..=) Core.<$> reason
          ]
      )

instance Core.AWSRequest DisassociateQualificationFromWorker where
  type
    Rs DisassociateQualificationFromWorker =
      DisassociateQualificationFromWorkerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.DisassociateQualificationFromWorker"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateQualificationFromWorkerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateQualificationFromWorkerResponse' smart constructor.
newtype DisassociateQualificationFromWorkerResponse = DisassociateQualificationFromWorkerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateQualificationFromWorkerResponse' value with any optional fields omitted.
mkDisassociateQualificationFromWorkerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateQualificationFromWorkerResponse
mkDisassociateQualificationFromWorkerResponse responseStatus =
  DisassociateQualificationFromWorkerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwrrsResponseStatus :: Lens.Lens' DisassociateQualificationFromWorkerResponse Core.Int
dqfwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dqfwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
