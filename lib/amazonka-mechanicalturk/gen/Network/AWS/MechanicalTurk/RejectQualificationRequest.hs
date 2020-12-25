{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.RejectQualificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @RejectQualificationRequest@ operation rejects a user's request for a Qualification.
--
-- You can provide a text message explaining why the request was rejected. The Worker who made the request can see this message.
module Network.AWS.MechanicalTurk.RejectQualificationRequest
  ( -- * Creating a request
    RejectQualificationRequest (..),
    mkRejectQualificationRequest,

    -- ** Request lenses
    rqrQualificationRequestId,
    rqrReason,

    -- * Destructuring the response
    RejectQualificationRequestResponse (..),
    mkRejectQualificationRequestResponse,

    -- ** Response lenses
    rqrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectQualificationRequest' smart constructor.
data RejectQualificationRequest = RejectQualificationRequest'
  { -- | The ID of the Qualification request, as returned by the @ListQualificationRequests@ operation.
    qualificationRequestId :: Types.String,
    -- | A text message explaining why the request was rejected, to be shown to the Worker who made the request.
    reason :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectQualificationRequest' value with any optional fields omitted.
mkRejectQualificationRequest ::
  -- | 'qualificationRequestId'
  Types.String ->
  RejectQualificationRequest
mkRejectQualificationRequest qualificationRequestId =
  RejectQualificationRequest'
    { qualificationRequestId,
      reason = Core.Nothing
    }

-- | The ID of the Qualification request, as returned by the @ListQualificationRequests@ operation.
--
-- /Note:/ Consider using 'qualificationRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqrQualificationRequestId :: Lens.Lens' RejectQualificationRequest Types.String
rqrQualificationRequestId = Lens.field @"qualificationRequestId"
{-# DEPRECATED rqrQualificationRequestId "Use generic-lens or generic-optics with 'qualificationRequestId' instead." #-}

-- | A text message explaining why the request was rejected, to be shown to the Worker who made the request.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqrReason :: Lens.Lens' RejectQualificationRequest (Core.Maybe Types.String)
rqrReason = Lens.field @"reason"
{-# DEPRECATED rqrReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON RejectQualificationRequest where
  toJSON RejectQualificationRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QualificationRequestId" Core..= qualificationRequestId),
            ("Reason" Core..=) Core.<$> reason
          ]
      )

instance Core.AWSRequest RejectQualificationRequest where
  type
    Rs RejectQualificationRequest =
      RejectQualificationRequestResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.RejectQualificationRequest"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectQualificationRequestResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRejectQualificationRequestResponse' smart constructor.
newtype RejectQualificationRequestResponse = RejectQualificationRequestResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectQualificationRequestResponse' value with any optional fields omitted.
mkRejectQualificationRequestResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RejectQualificationRequestResponse
mkRejectQualificationRequestResponse responseStatus =
  RejectQualificationRequestResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqrrrsResponseStatus :: Lens.Lens' RejectQualificationRequestResponse Core.Int
rqrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rqrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
