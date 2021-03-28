{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.StopInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific Amazon Lightsail instance that is currently running.
--
-- The @stop instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.StopInstance
    (
    -- * Creating a request
      StopInstance (..)
    , mkStopInstance
    -- ** Request lenses
    , siInstanceName
    , siForce

    -- * Destructuring the response
    , StopInstanceResponse (..)
    , mkStopInstanceResponse
    -- ** Response lenses
    , sirrsOperations
    , sirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopInstance' smart constructor.
data StopInstance = StopInstance'
  { instanceName :: Types.ResourceName
    -- ^ The name of the instance (a virtual private server) to stop.
  , force :: Core.Maybe Core.Bool
    -- ^ When set to @True@ , forces a Lightsail instance that is stuck in a @stopping@ state to stop.
--
-- /Important:/ Only use the @force@ parameter if your instance is stuck in the @stopping@ state. In any other state, your instance should stop normally without adding this parameter to your API request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopInstance' value with any optional fields omitted.
mkStopInstance
    :: Types.ResourceName -- ^ 'instanceName'
    -> StopInstance
mkStopInstance instanceName
  = StopInstance'{instanceName, force = Core.Nothing}

-- | The name of the instance (a virtual private server) to stop.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceName :: Lens.Lens' StopInstance Types.ResourceName
siInstanceName = Lens.field @"instanceName"
{-# INLINEABLE siInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

-- | When set to @True@ , forces a Lightsail instance that is stuck in a @stopping@ state to stop.
--
-- /Important:/ Only use the @force@ parameter if your instance is stuck in the @stopping@ state. In any other state, your instance should stop normally without adding this parameter to your API request.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siForce :: Lens.Lens' StopInstance (Core.Maybe Core.Bool)
siForce = Lens.field @"force"
{-# INLINEABLE siForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

instance Core.ToQuery StopInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopInstance where
        toHeaders StopInstance{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.StopInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopInstance where
        toJSON StopInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("instanceName" Core..= instanceName),
                  ("force" Core..=) Core.<$> force])

instance Core.AWSRequest StopInstance where
        type Rs StopInstance = StopInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopInstanceResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopInstanceResponse' smart constructor.
data StopInstanceResponse = StopInstanceResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopInstanceResponse' value with any optional fields omitted.
mkStopInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopInstanceResponse
mkStopInstanceResponse responseStatus
  = StopInstanceResponse'{operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsOperations :: Lens.Lens' StopInstanceResponse (Core.Maybe [Types.Operation])
sirrsOperations = Lens.field @"operations"
{-# INLINEABLE sirrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsResponseStatus :: Lens.Lens' StopInstanceResponse Core.Int
sirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
