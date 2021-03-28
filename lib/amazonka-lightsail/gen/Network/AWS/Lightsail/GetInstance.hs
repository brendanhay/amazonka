{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific Amazon Lightsail instance, which is a virtual private server.
module Network.AWS.Lightsail.GetInstance
    (
    -- * Creating a request
      GetInstance (..)
    , mkGetInstance
    -- ** Request lenses
    , giInstanceName

    -- * Destructuring the response
    , GetInstanceResponse (..)
    , mkGetInstanceResponse
    -- ** Response lenses
    , grsInstance
    , grsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstance' smart constructor.
newtype GetInstance = GetInstance'
  { instanceName :: Types.ResourceName
    -- ^ The name of the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstance' value with any optional fields omitted.
mkGetInstance
    :: Types.ResourceName -- ^ 'instanceName'
    -> GetInstance
mkGetInstance instanceName = GetInstance'{instanceName}

-- | The name of the instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giInstanceName :: Lens.Lens' GetInstance Types.ResourceName
giInstanceName = Lens.field @"instanceName"
{-# INLINEABLE giInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

instance Core.ToQuery GetInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetInstance where
        toHeaders GetInstance{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetInstance where
        toJSON GetInstance{..}
          = Core.object
              (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest GetInstance where
        type Rs GetInstance = GetInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetInstanceResponse' Core.<$>
                   (x Core..:? "instance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetInstanceResponse' smart constructor.
data GetInstanceResponse = GetInstanceResponse'
  { instance' :: Core.Maybe Types.Instance
    -- ^ An array of key-value pairs containing information about the specified instance.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetInstanceResponse' value with any optional fields omitted.
mkGetInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInstanceResponse
mkGetInstanceResponse responseStatus
  = GetInstanceResponse'{instance' = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about the specified instance.
--
-- /Note:/ Consider using 'instance'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsInstance :: Lens.Lens' GetInstanceResponse (Core.Maybe Types.Instance)
grsInstance = Lens.field @"instance'"
{-# INLINEABLE grsInstance #-}
{-# DEPRECATED instance' "Use generic-lens or generic-optics with 'instance'' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetInstanceResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
