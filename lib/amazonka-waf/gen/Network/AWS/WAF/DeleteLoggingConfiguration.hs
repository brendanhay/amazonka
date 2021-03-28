{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteLoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the 'LoggingConfiguration' from the specified web ACL.
module Network.AWS.WAF.DeleteLoggingConfiguration
    (
    -- * Creating a request
      DeleteLoggingConfiguration (..)
    , mkDeleteLoggingConfiguration
    -- ** Request lenses
    , dlcResourceArn

    -- * Destructuring the response
    , DeleteLoggingConfigurationResponse (..)
    , mkDeleteLoggingConfigurationResponse
    -- ** Response lenses
    , dlcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkDeleteLoggingConfiguration' smart constructor.
newtype DeleteLoggingConfiguration = DeleteLoggingConfiguration'
  { resourceArn :: Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the web ACL from which you want to delete the 'LoggingConfiguration' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoggingConfiguration' value with any optional fields omitted.
mkDeleteLoggingConfiguration
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> DeleteLoggingConfiguration
mkDeleteLoggingConfiguration resourceArn
  = DeleteLoggingConfiguration'{resourceArn}

-- | The Amazon Resource Name (ARN) of the web ACL from which you want to delete the 'LoggingConfiguration' .
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcResourceArn :: Lens.Lens' DeleteLoggingConfiguration Types.ResourceArn
dlcResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE dlcResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery DeleteLoggingConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteLoggingConfiguration where
        toHeaders DeleteLoggingConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.DeleteLoggingConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteLoggingConfiguration where
        toJSON DeleteLoggingConfiguration{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest DeleteLoggingConfiguration where
        type Rs DeleteLoggingConfiguration =
             DeleteLoggingConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteLoggingConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLoggingConfigurationResponse' smart constructor.
newtype DeleteLoggingConfigurationResponse = DeleteLoggingConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoggingConfigurationResponse' value with any optional fields omitted.
mkDeleteLoggingConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLoggingConfigurationResponse
mkDeleteLoggingConfigurationResponse responseStatus
  = DeleteLoggingConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsResponseStatus :: Lens.Lens' DeleteLoggingConfigurationResponse Core.Int
dlcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
