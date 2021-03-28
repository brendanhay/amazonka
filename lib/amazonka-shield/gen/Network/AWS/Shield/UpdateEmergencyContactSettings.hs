{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateEmergencyContactSettings (..)
    , mkUpdateEmergencyContactSettings
    -- ** Request lenses
    , uecsEmergencyContactList

    -- * Destructuring the response
    , UpdateEmergencyContactSettingsResponse (..)
    , mkUpdateEmergencyContactSettingsResponse
    -- ** Response lenses
    , uecsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkUpdateEmergencyContactSettings' smart constructor.
newtype UpdateEmergencyContactSettings = UpdateEmergencyContactSettings'
  { emergencyContactList :: Core.Maybe [Types.EmergencyContact]
    -- ^ A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- If you have proactive engagement enabled, the contact list must include at least one phone number.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEmergencyContactSettings' value with any optional fields omitted.
mkUpdateEmergencyContactSettings
    :: UpdateEmergencyContactSettings
mkUpdateEmergencyContactSettings
  = UpdateEmergencyContactSettings'{emergencyContactList =
                                      Core.Nothing}

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- If you have proactive engagement enabled, the contact list must include at least one phone number.
--
-- /Note:/ Consider using 'emergencyContactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecsEmergencyContactList :: Lens.Lens' UpdateEmergencyContactSettings (Core.Maybe [Types.EmergencyContact])
uecsEmergencyContactList = Lens.field @"emergencyContactList"
{-# INLINEABLE uecsEmergencyContactList #-}
{-# DEPRECATED emergencyContactList "Use generic-lens or generic-optics with 'emergencyContactList' instead"  #-}

instance Core.ToQuery UpdateEmergencyContactSettings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateEmergencyContactSettings where
        toHeaders UpdateEmergencyContactSettings{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSShield_20160616.UpdateEmergencyContactSettings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateEmergencyContactSettings where
        toJSON UpdateEmergencyContactSettings{..}
          = Core.object
              (Core.catMaybes
                 [("EmergencyContactList" Core..=) Core.<$> emergencyContactList])

instance Core.AWSRequest UpdateEmergencyContactSettings where
        type Rs UpdateEmergencyContactSettings =
             UpdateEmergencyContactSettingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateEmergencyContactSettingsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateEmergencyContactSettingsResponse' smart constructor.
newtype UpdateEmergencyContactSettingsResponse = UpdateEmergencyContactSettingsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEmergencyContactSettingsResponse' value with any optional fields omitted.
mkUpdateEmergencyContactSettingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateEmergencyContactSettingsResponse
mkUpdateEmergencyContactSettingsResponse responseStatus
  = UpdateEmergencyContactSettingsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecsrrsResponseStatus :: Lens.Lens' UpdateEmergencyContactSettingsResponse Core.Int
uecsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uecsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
