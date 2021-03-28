{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Lightsail instance.
--
-- The @delete instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteInstance
    (
    -- * Creating a request
      DeleteInstance (..)
    , mkDeleteInstance
    -- ** Request lenses
    , diInstanceName
    , diForceDeleteAddOns

    -- * Destructuring the response
    , DeleteInstanceResponse (..)
    , mkDeleteInstanceResponse
    -- ** Response lenses
    , dirrsOperations
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInstance' smart constructor.
data DeleteInstance = DeleteInstance'
  { instanceName :: Types.ResourceName
    -- ^ The name of the instance to delete.
  , forceDeleteAddOns :: Core.Maybe Core.Bool
    -- ^ A Boolean value to indicate whether to delete the enabled add-ons for the disk.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstance' value with any optional fields omitted.
mkDeleteInstance
    :: Types.ResourceName -- ^ 'instanceName'
    -> DeleteInstance
mkDeleteInstance instanceName
  = DeleteInstance'{instanceName, forceDeleteAddOns = Core.Nothing}

-- | The name of the instance to delete.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceName :: Lens.Lens' DeleteInstance Types.ResourceName
diInstanceName = Lens.field @"instanceName"
{-# INLINEABLE diInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

-- | A Boolean value to indicate whether to delete the enabled add-ons for the disk.
--
-- /Note:/ Consider using 'forceDeleteAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diForceDeleteAddOns :: Lens.Lens' DeleteInstance (Core.Maybe Core.Bool)
diForceDeleteAddOns = Lens.field @"forceDeleteAddOns"
{-# INLINEABLE diForceDeleteAddOns #-}
{-# DEPRECATED forceDeleteAddOns "Use generic-lens or generic-optics with 'forceDeleteAddOns' instead"  #-}

instance Core.ToQuery DeleteInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteInstance where
        toHeaders DeleteInstance{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteInstance where
        toJSON DeleteInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("instanceName" Core..= instanceName),
                  ("forceDeleteAddOns" Core..=) Core.<$> forceDeleteAddOns])

instance Core.AWSRequest DeleteInstance where
        type Rs DeleteInstance = DeleteInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteInstanceResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteInstanceResponse' smart constructor.
data DeleteInstanceResponse = DeleteInstanceResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteInstanceResponse' value with any optional fields omitted.
mkDeleteInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteInstanceResponse
mkDeleteInstanceResponse responseStatus
  = DeleteInstanceResponse'{operations = Core.Nothing,
                            responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsOperations :: Lens.Lens' DeleteInstanceResponse (Core.Maybe [Types.Operation])
dirrsOperations = Lens.field @"operations"
{-# INLINEABLE dirrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteInstanceResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
