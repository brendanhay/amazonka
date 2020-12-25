{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.AssociateQualificationWithWorker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AssociateQualificationWithWorker@ operation gives a Worker a Qualification. @AssociateQualificationWithWorker@ does not require that the Worker submit a Qualification request. It gives the Qualification directly to the Worker.
--
-- You can only assign a Qualification of a Qualification type that you created (using the @CreateQualificationType@ operation).
module Network.AWS.MechanicalTurk.AssociateQualificationWithWorker
  ( -- * Creating a request
    AssociateQualificationWithWorker (..),
    mkAssociateQualificationWithWorker,

    -- ** Request lenses
    aqwwQualificationTypeId,
    aqwwWorkerId,
    aqwwIntegerValue,
    aqwwSendNotification,

    -- * Destructuring the response
    AssociateQualificationWithWorkerResponse (..),
    mkAssociateQualificationWithWorkerResponse,

    -- ** Response lenses
    aqwwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateQualificationWithWorker' smart constructor.
data AssociateQualificationWithWorker = AssociateQualificationWithWorker'
  { -- | The ID of the Qualification type to use for the assigned Qualification.
    qualificationTypeId :: Types.QualificationTypeId,
    -- | The ID of the Worker to whom the Qualification is being assigned. Worker IDs are included with submitted HIT assignments and Qualification requests.
    workerId :: Types.CustomerId,
    -- | The value of the Qualification to assign.
    integerValue :: Core.Maybe Core.Int,
    -- | Specifies whether to send a notification email message to the Worker saying that the qualification was assigned to the Worker. Note: this is true by default.
    sendNotification :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateQualificationWithWorker' value with any optional fields omitted.
mkAssociateQualificationWithWorker ::
  -- | 'qualificationTypeId'
  Types.QualificationTypeId ->
  -- | 'workerId'
  Types.CustomerId ->
  AssociateQualificationWithWorker
mkAssociateQualificationWithWorker qualificationTypeId workerId =
  AssociateQualificationWithWorker'
    { qualificationTypeId,
      workerId,
      integerValue = Core.Nothing,
      sendNotification = Core.Nothing
    }

-- | The ID of the Qualification type to use for the assigned Qualification.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwQualificationTypeId :: Lens.Lens' AssociateQualificationWithWorker Types.QualificationTypeId
aqwwQualificationTypeId = Lens.field @"qualificationTypeId"
{-# DEPRECATED aqwwQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The ID of the Worker to whom the Qualification is being assigned. Worker IDs are included with submitted HIT assignments and Qualification requests.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwWorkerId :: Lens.Lens' AssociateQualificationWithWorker Types.CustomerId
aqwwWorkerId = Lens.field @"workerId"
{-# DEPRECATED aqwwWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

-- | The value of the Qualification to assign.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwIntegerValue :: Lens.Lens' AssociateQualificationWithWorker (Core.Maybe Core.Int)
aqwwIntegerValue = Lens.field @"integerValue"
{-# DEPRECATED aqwwIntegerValue "Use generic-lens or generic-optics with 'integerValue' instead." #-}

-- | Specifies whether to send a notification email message to the Worker saying that the qualification was assigned to the Worker. Note: this is true by default.
--
-- /Note:/ Consider using 'sendNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwSendNotification :: Lens.Lens' AssociateQualificationWithWorker (Core.Maybe Core.Bool)
aqwwSendNotification = Lens.field @"sendNotification"
{-# DEPRECATED aqwwSendNotification "Use generic-lens or generic-optics with 'sendNotification' instead." #-}

instance Core.FromJSON AssociateQualificationWithWorker where
  toJSON AssociateQualificationWithWorker {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("QualificationTypeId" Core..= qualificationTypeId),
            Core.Just ("WorkerId" Core..= workerId),
            ("IntegerValue" Core..=) Core.<$> integerValue,
            ("SendNotification" Core..=) Core.<$> sendNotification
          ]
      )

instance Core.AWSRequest AssociateQualificationWithWorker where
  type
    Rs AssociateQualificationWithWorker =
      AssociateQualificationWithWorkerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.AssociateQualificationWithWorker"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateQualificationWithWorkerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateQualificationWithWorkerResponse' smart constructor.
newtype AssociateQualificationWithWorkerResponse = AssociateQualificationWithWorkerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateQualificationWithWorkerResponse' value with any optional fields omitted.
mkAssociateQualificationWithWorkerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateQualificationWithWorkerResponse
mkAssociateQualificationWithWorkerResponse responseStatus =
  AssociateQualificationWithWorkerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwrrsResponseStatus :: Lens.Lens' AssociateQualificationWithWorkerResponse Core.Int
aqwwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aqwwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
