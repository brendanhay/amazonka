{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetAppValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves output from validating an application.
module Network.AWS.SMS.GetAppValidationOutput
    (
    -- * Creating a request
      GetAppValidationOutput (..)
    , mkGetAppValidationOutput
    -- ** Request lenses
    , gavoAppId

    -- * Destructuring the response
    , GetAppValidationOutputResponse (..)
    , mkGetAppValidationOutputResponse
    -- ** Response lenses
    , gavorrsValidationOutputList
    , gavorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetAppValidationOutput' smart constructor.
newtype GetAppValidationOutput = GetAppValidationOutput'
  { appId :: Types.AppIdWithValidation
    -- ^ The ID of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAppValidationOutput' value with any optional fields omitted.
mkGetAppValidationOutput
    :: Types.AppIdWithValidation -- ^ 'appId'
    -> GetAppValidationOutput
mkGetAppValidationOutput appId = GetAppValidationOutput'{appId}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavoAppId :: Lens.Lens' GetAppValidationOutput Types.AppIdWithValidation
gavoAppId = Lens.field @"appId"
{-# INLINEABLE gavoAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

instance Core.ToQuery GetAppValidationOutput where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAppValidationOutput where
        toHeaders GetAppValidationOutput{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.GetAppValidationOutput")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAppValidationOutput where
        toJSON GetAppValidationOutput{..}
          = Core.object (Core.catMaybes [Core.Just ("appId" Core..= appId)])

instance Core.AWSRequest GetAppValidationOutput where
        type Rs GetAppValidationOutput = GetAppValidationOutputResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAppValidationOutputResponse' Core.<$>
                   (x Core..:? "validationOutputList") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAppValidationOutputResponse' smart constructor.
data GetAppValidationOutputResponse = GetAppValidationOutputResponse'
  { validationOutputList :: Core.Maybe [Types.ValidationOutput]
    -- ^ The validation output.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAppValidationOutputResponse' value with any optional fields omitted.
mkGetAppValidationOutputResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAppValidationOutputResponse
mkGetAppValidationOutputResponse responseStatus
  = GetAppValidationOutputResponse'{validationOutputList =
                                      Core.Nothing,
                                    responseStatus}

-- | The validation output.
--
-- /Note:/ Consider using 'validationOutputList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavorrsValidationOutputList :: Lens.Lens' GetAppValidationOutputResponse (Core.Maybe [Types.ValidationOutput])
gavorrsValidationOutputList = Lens.field @"validationOutputList"
{-# INLINEABLE gavorrsValidationOutputList #-}
{-# DEPRECATED validationOutputList "Use generic-lens or generic-optics with 'validationOutputList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavorrsResponseStatus :: Lens.Lens' GetAppValidationOutputResponse Core.Int
gavorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gavorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
