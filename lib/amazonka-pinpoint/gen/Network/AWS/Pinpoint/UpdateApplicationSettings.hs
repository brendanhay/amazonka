{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateApplicationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for an application.
module Network.AWS.Pinpoint.UpdateApplicationSettings
    (
    -- * Creating a request
      UpdateApplicationSettings (..)
    , mkUpdateApplicationSettings
    -- ** Request lenses
    , uasApplicationId
    , uasWriteApplicationSettingsRequest

    -- * Destructuring the response
    , UpdateApplicationSettingsResponse (..)
    , mkUpdateApplicationSettingsResponse
    -- ** Response lenses
    , uasrrsApplicationSettingsResource
    , uasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApplicationSettings' smart constructor.
data UpdateApplicationSettings = UpdateApplicationSettings'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , writeApplicationSettingsRequest :: Types.WriteApplicationSettingsRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationSettings' value with any optional fields omitted.
mkUpdateApplicationSettings
    :: Core.Text -- ^ 'applicationId'
    -> Types.WriteApplicationSettingsRequest -- ^ 'writeApplicationSettingsRequest'
    -> UpdateApplicationSettings
mkUpdateApplicationSettings applicationId
  writeApplicationSettingsRequest
  = UpdateApplicationSettings'{applicationId,
                               writeApplicationSettingsRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasApplicationId :: Lens.Lens' UpdateApplicationSettings Core.Text
uasApplicationId = Lens.field @"applicationId"
{-# INLINEABLE uasApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeApplicationSettingsRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasWriteApplicationSettingsRequest :: Lens.Lens' UpdateApplicationSettings Types.WriteApplicationSettingsRequest
uasWriteApplicationSettingsRequest = Lens.field @"writeApplicationSettingsRequest"
{-# INLINEABLE uasWriteApplicationSettingsRequest #-}
{-# DEPRECATED writeApplicationSettingsRequest "Use generic-lens or generic-optics with 'writeApplicationSettingsRequest' instead"  #-}

instance Core.ToQuery UpdateApplicationSettings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApplicationSettings where
        toHeaders UpdateApplicationSettings{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApplicationSettings where
        toJSON UpdateApplicationSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("WriteApplicationSettingsRequest" Core..=
                       writeApplicationSettingsRequest)])

instance Core.AWSRequest UpdateApplicationSettings where
        type Rs UpdateApplicationSettings =
             UpdateApplicationSettingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/settings",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateApplicationSettingsResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApplicationSettingsResponse' smart constructor.
data UpdateApplicationSettingsResponse = UpdateApplicationSettingsResponse'
  { applicationSettingsResource :: Types.ApplicationSettingsResource
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationSettingsResponse' value with any optional fields omitted.
mkUpdateApplicationSettingsResponse
    :: Types.ApplicationSettingsResource -- ^ 'applicationSettingsResource'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateApplicationSettingsResponse
mkUpdateApplicationSettingsResponse applicationSettingsResource
  responseStatus
  = UpdateApplicationSettingsResponse'{applicationSettingsResource,
                                       responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationSettingsResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsApplicationSettingsResource :: Lens.Lens' UpdateApplicationSettingsResponse Types.ApplicationSettingsResource
uasrrsApplicationSettingsResource = Lens.field @"applicationSettingsResource"
{-# INLINEABLE uasrrsApplicationSettingsResource #-}
{-# DEPRECATED applicationSettingsResource "Use generic-lens or generic-optics with 'applicationSettingsResource' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsResponseStatus :: Lens.Lens' UpdateApplicationSettingsResponse Core.Int
uasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
