{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified security configuration.
module Network.AWS.Glue.DeleteSecurityConfiguration
    (
    -- * Creating a request
      DeleteSecurityConfiguration (..)
    , mkDeleteSecurityConfiguration
    -- ** Request lenses
    , dscName

    -- * Destructuring the response
    , DeleteSecurityConfigurationResponse (..)
    , mkDeleteSecurityConfigurationResponse
    -- ** Response lenses
    , dscrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSecurityConfiguration' smart constructor.
newtype DeleteSecurityConfiguration = DeleteSecurityConfiguration'
  { name :: Types.Name
    -- ^ The name of the security configuration to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSecurityConfiguration' value with any optional fields omitted.
mkDeleteSecurityConfiguration
    :: Types.Name -- ^ 'name'
    -> DeleteSecurityConfiguration
mkDeleteSecurityConfiguration name
  = DeleteSecurityConfiguration'{name}

-- | The name of the security configuration to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscName :: Lens.Lens' DeleteSecurityConfiguration Types.Name
dscName = Lens.field @"name"
{-# INLINEABLE dscName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteSecurityConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSecurityConfiguration where
        toHeaders DeleteSecurityConfiguration{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteSecurityConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSecurityConfiguration where
        toJSON DeleteSecurityConfiguration{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteSecurityConfiguration where
        type Rs DeleteSecurityConfiguration =
             DeleteSecurityConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteSecurityConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSecurityConfigurationResponse' smart constructor.
newtype DeleteSecurityConfigurationResponse = DeleteSecurityConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSecurityConfigurationResponse' value with any optional fields omitted.
mkDeleteSecurityConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSecurityConfigurationResponse
mkDeleteSecurityConfigurationResponse responseStatus
  = DeleteSecurityConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsResponseStatus :: Lens.Lens' DeleteSecurityConfigurationResponse Core.Int
dscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
