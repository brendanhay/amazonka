{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.UpdateEmergencyContactSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of the list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.UpdateEmergencyContactSettings
  ( -- * Creating a request
    UpdateEmergencyContactSettings (..),
    mkUpdateEmergencyContactSettings,

    -- ** Request lenses
    uecsEmergencyContactList,

    -- * Destructuring the response
    UpdateEmergencyContactSettingsResponse (..),
    mkUpdateEmergencyContactSettingsResponse,

    -- ** Response lenses
    uecsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkUpdateEmergencyContactSettings' smart constructor.
newtype UpdateEmergencyContactSettings = UpdateEmergencyContactSettings'
  { -- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
    --
    -- If you have proactive engagement enabled, the contact list must include at least one phone number.
    emergencyContactList :: Core.Maybe [Types.EmergencyContact]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEmergencyContactSettings' value with any optional fields omitted.
mkUpdateEmergencyContactSettings ::
  UpdateEmergencyContactSettings
mkUpdateEmergencyContactSettings =
  UpdateEmergencyContactSettings'
    { emergencyContactList =
        Core.Nothing
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- If you have proactive engagement enabled, the contact list must include at least one phone number.
--
-- /Note:/ Consider using 'emergencyContactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecsEmergencyContactList :: Lens.Lens' UpdateEmergencyContactSettings (Core.Maybe [Types.EmergencyContact])
uecsEmergencyContactList = Lens.field @"emergencyContactList"
{-# DEPRECATED uecsEmergencyContactList "Use generic-lens or generic-optics with 'emergencyContactList' instead." #-}

instance Core.FromJSON UpdateEmergencyContactSettings where
  toJSON UpdateEmergencyContactSettings {..} =
    Core.object
      ( Core.catMaybes
          [("EmergencyContactList" Core..=) Core.<$> emergencyContactList]
      )

instance Core.AWSRequest UpdateEmergencyContactSettings where
  type
    Rs UpdateEmergencyContactSettings =
      UpdateEmergencyContactSettingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSShield_20160616.UpdateEmergencyContactSettings"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEmergencyContactSettingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateEmergencyContactSettingsResponse' smart constructor.
newtype UpdateEmergencyContactSettingsResponse = UpdateEmergencyContactSettingsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEmergencyContactSettingsResponse' value with any optional fields omitted.
mkUpdateEmergencyContactSettingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateEmergencyContactSettingsResponse
mkUpdateEmergencyContactSettingsResponse responseStatus =
  UpdateEmergencyContactSettingsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecsrrsResponseStatus :: Lens.Lens' UpdateEmergencyContactSettingsResponse Core.Int
uecsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uecsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
