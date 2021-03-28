{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateNotificationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateNotificationSettings@ operation creates, updates, disables or re-enables notifications for a HIT type. If you call the UpdateNotificationSettings operation for a HIT type that already has a notification specification, the operation replaces the old specification with a new one. You can call the UpdateNotificationSettings operation to enable or disable notifications for the HIT type, without having to modify the notification specification itself by providing updates to the Active status without specifying a new notification specification. To change the Active status of a HIT type's notifications, the HIT type must already have a notification specification, or one must be provided in the same call to @UpdateNotificationSettings@ . 
module Network.AWS.MechanicalTurk.UpdateNotificationSettings
    (
    -- * Creating a request
      UpdateNotificationSettings (..)
    , mkUpdateNotificationSettings
    -- ** Request lenses
    , unsHITTypeId
    , unsActive
    , unsNotification

    -- * Destructuring the response
    , UpdateNotificationSettingsResponse (..)
    , mkUpdateNotificationSettingsResponse
    -- ** Response lenses
    , unsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateNotificationSettings' smart constructor.
data UpdateNotificationSettings = UpdateNotificationSettings'
  { hITTypeId :: Types.HITTypeId
    -- ^ The ID of the HIT type whose notification specification is being updated. 
  , active :: Core.Maybe Core.Bool
    -- ^ Specifies whether notifications are sent for HITs of this HIT type, according to the notification specification. You must specify either the Notification parameter or the Active parameter for the call to UpdateNotificationSettings to succeed. 
  , notification :: Core.Maybe Types.NotificationSpecification
    -- ^ The notification specification for the HIT type. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotificationSettings' value with any optional fields omitted.
mkUpdateNotificationSettings
    :: Types.HITTypeId -- ^ 'hITTypeId'
    -> UpdateNotificationSettings
mkUpdateNotificationSettings hITTypeId
  = UpdateNotificationSettings'{hITTypeId, active = Core.Nothing,
                                notification = Core.Nothing}

-- | The ID of the HIT type whose notification specification is being updated. 
--
-- /Note:/ Consider using 'hITTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unsHITTypeId :: Lens.Lens' UpdateNotificationSettings Types.HITTypeId
unsHITTypeId = Lens.field @"hITTypeId"
{-# INLINEABLE unsHITTypeId #-}
{-# DEPRECATED hITTypeId "Use generic-lens or generic-optics with 'hITTypeId' instead"  #-}

-- | Specifies whether notifications are sent for HITs of this HIT type, according to the notification specification. You must specify either the Notification parameter or the Active parameter for the call to UpdateNotificationSettings to succeed. 
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unsActive :: Lens.Lens' UpdateNotificationSettings (Core.Maybe Core.Bool)
unsActive = Lens.field @"active"
{-# INLINEABLE unsActive #-}
{-# DEPRECATED active "Use generic-lens or generic-optics with 'active' instead"  #-}

-- | The notification specification for the HIT type. 
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unsNotification :: Lens.Lens' UpdateNotificationSettings (Core.Maybe Types.NotificationSpecification)
unsNotification = Lens.field @"notification"
{-# INLINEABLE unsNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

instance Core.ToQuery UpdateNotificationSettings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateNotificationSettings where
        toHeaders UpdateNotificationSettings{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.UpdateNotificationSettings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateNotificationSettings where
        toJSON UpdateNotificationSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("HITTypeId" Core..= hITTypeId),
                  ("Active" Core..=) Core.<$> active,
                  ("Notification" Core..=) Core.<$> notification])

instance Core.AWSRequest UpdateNotificationSettings where
        type Rs UpdateNotificationSettings =
             UpdateNotificationSettingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateNotificationSettingsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateNotificationSettingsResponse' smart constructor.
newtype UpdateNotificationSettingsResponse = UpdateNotificationSettingsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNotificationSettingsResponse' value with any optional fields omitted.
mkUpdateNotificationSettingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateNotificationSettingsResponse
mkUpdateNotificationSettingsResponse responseStatus
  = UpdateNotificationSettingsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unsrrsResponseStatus :: Lens.Lens' UpdateNotificationSettingsResponse Core.Int
unsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE unsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
