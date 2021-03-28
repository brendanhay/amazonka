{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteAppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the validation configuration for the specified application.
module Network.AWS.SMS.DeleteAppValidationConfiguration
    (
    -- * Creating a request
      DeleteAppValidationConfiguration (..)
    , mkDeleteAppValidationConfiguration
    -- ** Request lenses
    , davcAppId

    -- * Destructuring the response
    , DeleteAppValidationConfigurationResponse (..)
    , mkDeleteAppValidationConfigurationResponse
    -- ** Response lenses
    , davcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkDeleteAppValidationConfiguration' smart constructor.
newtype DeleteAppValidationConfiguration = DeleteAppValidationConfiguration'
  { appId :: Types.AppIdWithValidation
    -- ^ The ID of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppValidationConfiguration' value with any optional fields omitted.
mkDeleteAppValidationConfiguration
    :: Types.AppIdWithValidation -- ^ 'appId'
    -> DeleteAppValidationConfiguration
mkDeleteAppValidationConfiguration appId
  = DeleteAppValidationConfiguration'{appId}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davcAppId :: Lens.Lens' DeleteAppValidationConfiguration Types.AppIdWithValidation
davcAppId = Lens.field @"appId"
{-# INLINEABLE davcAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

instance Core.ToQuery DeleteAppValidationConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAppValidationConfiguration where
        toHeaders DeleteAppValidationConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.DeleteAppValidationConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAppValidationConfiguration where
        toJSON DeleteAppValidationConfiguration{..}
          = Core.object (Core.catMaybes [Core.Just ("appId" Core..= appId)])

instance Core.AWSRequest DeleteAppValidationConfiguration where
        type Rs DeleteAppValidationConfiguration =
             DeleteAppValidationConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteAppValidationConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAppValidationConfigurationResponse' smart constructor.
newtype DeleteAppValidationConfigurationResponse = DeleteAppValidationConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppValidationConfigurationResponse' value with any optional fields omitted.
mkDeleteAppValidationConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAppValidationConfigurationResponse
mkDeleteAppValidationConfigurationResponse responseStatus
  = DeleteAppValidationConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
davcrrsResponseStatus :: Lens.Lens' DeleteAppValidationConfigurationResponse Core.Int
davcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE davcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
