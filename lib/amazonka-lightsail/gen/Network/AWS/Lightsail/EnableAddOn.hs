{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.EnableAddOn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or modifies an add-on for an Amazon Lightsail resource. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.EnableAddOn
    (
    -- * Creating a request
      EnableAddOn (..)
    , mkEnableAddOn
    -- ** Request lenses
    , eaoResourceName
    , eaoAddOnRequest

    -- * Destructuring the response
    , EnableAddOnResponse (..)
    , mkEnableAddOnResponse
    -- ** Response lenses
    , eaorrsOperations
    , eaorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableAddOn' smart constructor.
data EnableAddOn = EnableAddOn'
  { resourceName :: Types.ResourceName
    -- ^ The name of the source resource for which to enable or modify the add-on.
  , addOnRequest :: Types.AddOnRequest
    -- ^ An array of strings representing the add-on to enable or modify.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAddOn' value with any optional fields omitted.
mkEnableAddOn
    :: Types.ResourceName -- ^ 'resourceName'
    -> Types.AddOnRequest -- ^ 'addOnRequest'
    -> EnableAddOn
mkEnableAddOn resourceName addOnRequest
  = EnableAddOn'{resourceName, addOnRequest}

-- | The name of the source resource for which to enable or modify the add-on.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaoResourceName :: Lens.Lens' EnableAddOn Types.ResourceName
eaoResourceName = Lens.field @"resourceName"
{-# INLINEABLE eaoResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | An array of strings representing the add-on to enable or modify.
--
-- /Note:/ Consider using 'addOnRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaoAddOnRequest :: Lens.Lens' EnableAddOn Types.AddOnRequest
eaoAddOnRequest = Lens.field @"addOnRequest"
{-# INLINEABLE eaoAddOnRequest #-}
{-# DEPRECATED addOnRequest "Use generic-lens or generic-optics with 'addOnRequest' instead"  #-}

instance Core.ToQuery EnableAddOn where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableAddOn where
        toHeaders EnableAddOn{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.EnableAddOn")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableAddOn where
        toJSON EnableAddOn{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("resourceName" Core..= resourceName),
                  Core.Just ("addOnRequest" Core..= addOnRequest)])

instance Core.AWSRequest EnableAddOn where
        type Rs EnableAddOn = EnableAddOnResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 EnableAddOnResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableAddOnResponse' smart constructor.
data EnableAddOnResponse = EnableAddOnResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EnableAddOnResponse' value with any optional fields omitted.
mkEnableAddOnResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableAddOnResponse
mkEnableAddOnResponse responseStatus
  = EnableAddOnResponse'{operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaorrsOperations :: Lens.Lens' EnableAddOnResponse (Core.Maybe [Types.Operation])
eaorrsOperations = Lens.field @"operations"
{-# INLINEABLE eaorrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaorrsResponseStatus :: Lens.Lens' EnableAddOnResponse Core.Int
eaorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eaorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
