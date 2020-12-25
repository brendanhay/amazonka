{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.AssociateProactiveEngagementDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initializes proactive engagement and sets the list of contacts for the DDoS Response Team (DRT) to use. You must provide at least one phone number in the emergency contact list.
--
-- After you have initialized proactive engagement using this call, to disable or enable proactive engagement, use the calls @DisableProactiveEngagement@ and @EnableProactiveEngagement@ .
module Network.AWS.Shield.AssociateProactiveEngagementDetails
  ( -- * Creating a request
    AssociateProactiveEngagementDetails (..),
    mkAssociateProactiveEngagementDetails,

    -- ** Request lenses
    apedEmergencyContactList,

    -- * Destructuring the response
    AssociateProactiveEngagementDetailsResponse (..),
    mkAssociateProactiveEngagementDetailsResponse,

    -- ** Response lenses
    apedrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkAssociateProactiveEngagementDetails' smart constructor.
newtype AssociateProactiveEngagementDetails = AssociateProactiveEngagementDetails'
  { -- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you for escalations to the DRT and to initiate proactive customer support.
    --
    -- To enable proactive engagement, the contact list must include at least one phone number.
    emergencyContactList :: [Types.EmergencyContact]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateProactiveEngagementDetails' value with any optional fields omitted.
mkAssociateProactiveEngagementDetails ::
  AssociateProactiveEngagementDetails
mkAssociateProactiveEngagementDetails =
  AssociateProactiveEngagementDetails'
    { emergencyContactList =
        Core.mempty
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you for escalations to the DRT and to initiate proactive customer support.
--
-- To enable proactive engagement, the contact list must include at least one phone number.
--
-- /Note:/ Consider using 'emergencyContactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apedEmergencyContactList :: Lens.Lens' AssociateProactiveEngagementDetails [Types.EmergencyContact]
apedEmergencyContactList = Lens.field @"emergencyContactList"
{-# DEPRECATED apedEmergencyContactList "Use generic-lens or generic-optics with 'emergencyContactList' instead." #-}

instance Core.FromJSON AssociateProactiveEngagementDetails where
  toJSON AssociateProactiveEngagementDetails {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EmergencyContactList" Core..= emergencyContactList)]
      )

instance Core.AWSRequest AssociateProactiveEngagementDetails where
  type
    Rs AssociateProactiveEngagementDetails =
      AssociateProactiveEngagementDetailsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSShield_20160616.AssociateProactiveEngagementDetails"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateProactiveEngagementDetailsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateProactiveEngagementDetailsResponse' smart constructor.
newtype AssociateProactiveEngagementDetailsResponse = AssociateProactiveEngagementDetailsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateProactiveEngagementDetailsResponse' value with any optional fields omitted.
mkAssociateProactiveEngagementDetailsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateProactiveEngagementDetailsResponse
mkAssociateProactiveEngagementDetailsResponse responseStatus =
  AssociateProactiveEngagementDetailsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apedrrsResponseStatus :: Lens.Lens' AssociateProactiveEngagementDetailsResponse Core.Int
apedrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED apedrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
