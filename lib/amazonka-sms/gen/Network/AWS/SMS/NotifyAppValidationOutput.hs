{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.NotifyAppValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS SMS about whether application validation is successful.
module Network.AWS.SMS.NotifyAppValidationOutput
    (
    -- * Creating a request
      NotifyAppValidationOutput (..)
    , mkNotifyAppValidationOutput
    -- ** Request lenses
    , navoAppId
    , navoNotificationContext

    -- * Destructuring the response
    , NotifyAppValidationOutputResponse (..)
    , mkNotifyAppValidationOutputResponse
    -- ** Response lenses
    , navorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkNotifyAppValidationOutput' smart constructor.
data NotifyAppValidationOutput = NotifyAppValidationOutput'
  { appId :: Types.AppIdWithValidation
    -- ^ The ID of the application.
  , notificationContext :: Core.Maybe Types.NotificationContext
    -- ^ The notification information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyAppValidationOutput' value with any optional fields omitted.
mkNotifyAppValidationOutput
    :: Types.AppIdWithValidation -- ^ 'appId'
    -> NotifyAppValidationOutput
mkNotifyAppValidationOutput appId
  = NotifyAppValidationOutput'{appId,
                               notificationContext = Core.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
navoAppId :: Lens.Lens' NotifyAppValidationOutput Types.AppIdWithValidation
navoAppId = Lens.field @"appId"
{-# INLINEABLE navoAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | The notification information.
--
-- /Note:/ Consider using 'notificationContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
navoNotificationContext :: Lens.Lens' NotifyAppValidationOutput (Core.Maybe Types.NotificationContext)
navoNotificationContext = Lens.field @"notificationContext"
{-# INLINEABLE navoNotificationContext #-}
{-# DEPRECATED notificationContext "Use generic-lens or generic-optics with 'notificationContext' instead"  #-}

instance Core.ToQuery NotifyAppValidationOutput where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders NotifyAppValidationOutput where
        toHeaders NotifyAppValidationOutput{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.NotifyAppValidationOutput")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON NotifyAppValidationOutput where
        toJSON NotifyAppValidationOutput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("appId" Core..= appId),
                  ("notificationContext" Core..=) Core.<$> notificationContext])

instance Core.AWSRequest NotifyAppValidationOutput where
        type Rs NotifyAppValidationOutput =
             NotifyAppValidationOutputResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 NotifyAppValidationOutputResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkNotifyAppValidationOutputResponse' smart constructor.
newtype NotifyAppValidationOutputResponse = NotifyAppValidationOutputResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyAppValidationOutputResponse' value with any optional fields omitted.
mkNotifyAppValidationOutputResponse
    :: Core.Int -- ^ 'responseStatus'
    -> NotifyAppValidationOutputResponse
mkNotifyAppValidationOutputResponse responseStatus
  = NotifyAppValidationOutputResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
navorrsResponseStatus :: Lens.Lens' NotifyAppValidationOutputResponse Core.Int
navorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE navorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
