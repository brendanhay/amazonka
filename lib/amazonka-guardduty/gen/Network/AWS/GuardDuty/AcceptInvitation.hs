{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AcceptInvitation (..),
    mkAcceptInvitation,

    -- ** Request lenses
    aiDetectorId,
    aiMasterId,
    aiInvitationId,

    -- * Destructuring the response
    AcceptInvitationResponse (..),
    mkAcceptInvitationResponse,

    -- ** Response lenses
    airrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptInvitation' smart constructor.
data AcceptInvitation = AcceptInvitation'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Types.DetectorId,
    -- | The account ID of the master GuardDuty account whose invitation you're accepting.
    masterId :: Types.String,
    -- | The value that is used to validate the master account to the member account.
    invitationId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptInvitation' value with any optional fields omitted.
mkAcceptInvitation ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'masterId'
  Types.String ->
  -- | 'invitationId'
  Types.String ->
  AcceptInvitation
mkAcceptInvitation detectorId masterId invitationId =
  AcceptInvitation' {detectorId, masterId, invitationId}

-- | The unique ID of the detector of the GuardDuty member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiDetectorId :: Lens.Lens' AcceptInvitation Types.DetectorId
aiDetectorId = Lens.field @"detectorId"
{-# DEPRECATED aiDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The account ID of the master GuardDuty account whose invitation you're accepting.
--
-- /Note:/ Consider using 'masterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiMasterId :: Lens.Lens' AcceptInvitation Types.String
aiMasterId = Lens.field @"masterId"
{-# DEPRECATED aiMasterId "Use generic-lens or generic-optics with 'masterId' instead." #-}

-- | The value that is used to validate the master account to the member account.
--
-- /Note:/ Consider using 'invitationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInvitationId :: Lens.Lens' AcceptInvitation Types.String
aiInvitationId = Lens.field @"invitationId"
{-# DEPRECATED aiInvitationId "Use generic-lens or generic-optics with 'invitationId' instead." #-}

instance Core.FromJSON AcceptInvitation where
  toJSON AcceptInvitation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("masterId" Core..= masterId),
            Core.Just ("invitationId" Core..= invitationId)
          ]
      )

instance Core.AWSRequest AcceptInvitation where
  type Rs AcceptInvitation = AcceptInvitationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/master")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptInvitationResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAcceptInvitationResponse' smart constructor.
newtype AcceptInvitationResponse = AcceptInvitationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptInvitationResponse' value with any optional fields omitted.
mkAcceptInvitationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcceptInvitationResponse
mkAcceptInvitationResponse responseStatus =
  AcceptInvitationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
airrsResponseStatus :: Lens.Lens' AcceptInvitationResponse Core.Int
airrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED airrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
