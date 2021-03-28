{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain configuration.
module Network.AWS.IoT.DeleteDomainConfiguration
    (
    -- * Creating a request
      DeleteDomainConfiguration (..)
    , mkDeleteDomainConfiguration
    -- ** Request lenses
    , dDomainConfigurationName

    -- * Destructuring the response
    , DeleteDomainConfigurationResponse (..)
    , mkDeleteDomainConfigurationResponse
    -- ** Response lenses
    , ddcrfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDomainConfiguration' smart constructor.
newtype DeleteDomainConfiguration = DeleteDomainConfiguration'
  { domainConfigurationName :: Types.DomainConfigurationName
    -- ^ The name of the domain configuration to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainConfiguration' value with any optional fields omitted.
mkDeleteDomainConfiguration
    :: Types.DomainConfigurationName -- ^ 'domainConfigurationName'
    -> DeleteDomainConfiguration
mkDeleteDomainConfiguration domainConfigurationName
  = DeleteDomainConfiguration'{domainConfigurationName}

-- | The name of the domain configuration to be deleted.
--
-- /Note:/ Consider using 'domainConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainConfigurationName :: Lens.Lens' DeleteDomainConfiguration Types.DomainConfigurationName
dDomainConfigurationName = Lens.field @"domainConfigurationName"
{-# INLINEABLE dDomainConfigurationName #-}
{-# DEPRECATED domainConfigurationName "Use generic-lens or generic-optics with 'domainConfigurationName' instead"  #-}

instance Core.ToQuery DeleteDomainConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDomainConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDomainConfiguration where
        type Rs DeleteDomainConfiguration =
             DeleteDomainConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/domainConfigurations/" Core.<>
                             Core.toText domainConfigurationName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteDomainConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDomainConfigurationResponse' smart constructor.
newtype DeleteDomainConfigurationResponse = DeleteDomainConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainConfigurationResponse' value with any optional fields omitted.
mkDeleteDomainConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDomainConfigurationResponse
mkDeleteDomainConfigurationResponse responseStatus
  = DeleteDomainConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrfrsResponseStatus :: Lens.Lens' DeleteDomainConfigurationResponse Core.Int
ddcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
