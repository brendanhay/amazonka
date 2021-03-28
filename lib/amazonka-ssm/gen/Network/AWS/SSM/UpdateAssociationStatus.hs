{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateAssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the Systems Manager document associated with the specified instance.
module Network.AWS.SSM.UpdateAssociationStatus
    (
    -- * Creating a request
      UpdateAssociationStatus (..)
    , mkUpdateAssociationStatus
    -- ** Request lenses
    , uasName
    , uasInstanceId
    , uasAssociationStatus

    -- * Destructuring the response
    , UpdateAssociationStatusResponse (..)
    , mkUpdateAssociationStatusResponse
    -- ** Response lenses
    , uasrrsAssociationDescription
    , uasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateAssociationStatus' smart constructor.
data UpdateAssociationStatus = UpdateAssociationStatus'
  { name :: Types.DocumentARN
    -- ^ The name of the Systems Manager document.
  , instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , associationStatus :: Types.AssociationStatus
    -- ^ The association status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateAssociationStatus' value with any optional fields omitted.
mkUpdateAssociationStatus
    :: Types.DocumentARN -- ^ 'name'
    -> Types.InstanceId -- ^ 'instanceId'
    -> Types.AssociationStatus -- ^ 'associationStatus'
    -> UpdateAssociationStatus
mkUpdateAssociationStatus name instanceId associationStatus
  = UpdateAssociationStatus'{name, instanceId, associationStatus}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasName :: Lens.Lens' UpdateAssociationStatus Types.DocumentARN
uasName = Lens.field @"name"
{-# INLINEABLE uasName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasInstanceId :: Lens.Lens' UpdateAssociationStatus Types.InstanceId
uasInstanceId = Lens.field @"instanceId"
{-# INLINEABLE uasInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The association status.
--
-- /Note:/ Consider using 'associationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasAssociationStatus :: Lens.Lens' UpdateAssociationStatus Types.AssociationStatus
uasAssociationStatus = Lens.field @"associationStatus"
{-# INLINEABLE uasAssociationStatus #-}
{-# DEPRECATED associationStatus "Use generic-lens or generic-optics with 'associationStatus' instead"  #-}

instance Core.ToQuery UpdateAssociationStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAssociationStatus where
        toHeaders UpdateAssociationStatus{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.UpdateAssociationStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateAssociationStatus where
        toJSON UpdateAssociationStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("InstanceId" Core..= instanceId),
                  Core.Just ("AssociationStatus" Core..= associationStatus)])

instance Core.AWSRequest UpdateAssociationStatus where
        type Rs UpdateAssociationStatus = UpdateAssociationStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateAssociationStatusResponse' Core.<$>
                   (x Core..:? "AssociationDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAssociationStatusResponse' smart constructor.
data UpdateAssociationStatusResponse = UpdateAssociationStatusResponse'
  { associationDescription :: Core.Maybe Types.AssociationDescription
    -- ^ Information about the association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateAssociationStatusResponse' value with any optional fields omitted.
mkUpdateAssociationStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateAssociationStatusResponse
mkUpdateAssociationStatusResponse responseStatus
  = UpdateAssociationStatusResponse'{associationDescription =
                                       Core.Nothing,
                                     responseStatus}

-- | Information about the association.
--
-- /Note:/ Consider using 'associationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsAssociationDescription :: Lens.Lens' UpdateAssociationStatusResponse (Core.Maybe Types.AssociationDescription)
uasrrsAssociationDescription = Lens.field @"associationDescription"
{-# INLINEABLE uasrrsAssociationDescription #-}
{-# DEPRECATED associationDescription "Use generic-lens or generic-optics with 'associationDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrrsResponseStatus :: Lens.Lens' UpdateAssociationStatusResponse Core.Int
uasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
