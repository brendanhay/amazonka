{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.AcceptQualificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AcceptQualificationRequest@ operation approves a Worker's request for a Qualification.
--
-- Only the owner of the Qualification type can grant a Qualification request for that type.
-- A successful request for the @AcceptQualificationRequest@ operation returns with no errors and an empty body.
module Network.AWS.MechanicalTurk.AcceptQualificationRequest
  ( -- * Creating a request
    AcceptQualificationRequest (..),
    mkAcceptQualificationRequest,

    -- ** Request lenses
    aqrQualificationRequestId,
    aqrIntegerValue,

    -- * Destructuring the response
    AcceptQualificationRequestResponse (..),
    mkAcceptQualificationRequestResponse,

    -- ** Response lenses
    aqrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptQualificationRequest' smart constructor.
data AcceptQualificationRequest = AcceptQualificationRequest'
  { -- | The ID of the Qualification request, as returned by the @GetQualificationRequests@ operation.
    qualificationRequestId :: Types.String,
    -- | The value of the Qualification. You can omit this value if you are using the presence or absence of the Qualification as the basis for a HIT requirement.
    integerValue :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptQualificationRequest' value with any optional fields omitted.
mkAcceptQualificationRequest ::
  -- | 'qualificationRequestId'
  Types.String ->
  AcceptQualificationRequest
mkAcceptQualificationRequest qualificationRequestId =
  AcceptQualificationRequest'
    { qualificationRequestId,
      integerValue = Core.Nothing
    }

-- | The ID of the Qualification request, as returned by the @GetQualificationRequests@ operation.
--
-- /Note:/ Consider using 'qualificationRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqrQualificationRequestId :: Lens.Lens' AcceptQualificationRequest Types.String
aqrQualificationRequestId = Lens.field @"qualificationRequestId"
{-# DEPRECATED aqrQualificationRequestId "Use generic-lens or generic-optics with 'qualificationRequestId' instead." #-}

-- | The value of the Qualification. You can omit this value if you are using the presence or absence of the Qualification as the basis for a HIT requirement.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqrIntegerValue :: Lens.Lens' AcceptQualificationRequest (Core.Maybe Core.Int)
aqrIntegerValue = Lens.field @"integerValue"
{-# DEPRECATED aqrIntegerValue "Use generic-lens or generic-optics with 'integerValue' instead." #-}

instance Core.FromJSON AcceptQualificationRequest where
  toJSON AcceptQualificationRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QualificationRequestId" Core..= qualificationRequestId),
            ("IntegerValue" Core..=) Core.<$> integerValue
          ]
      )

instance Core.AWSRequest AcceptQualificationRequest where
  type
    Rs AcceptQualificationRequest =
      AcceptQualificationRequestResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.AcceptQualificationRequest"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptQualificationRequestResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAcceptQualificationRequestResponse' smart constructor.
newtype AcceptQualificationRequestResponse = AcceptQualificationRequestResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptQualificationRequestResponse' value with any optional fields omitted.
mkAcceptQualificationRequestResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcceptQualificationRequestResponse
mkAcceptQualificationRequestResponse responseStatus =
  AcceptQualificationRequestResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqrrrsResponseStatus :: Lens.Lens' AcceptQualificationRequestResponse Core.Int
aqrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aqrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
