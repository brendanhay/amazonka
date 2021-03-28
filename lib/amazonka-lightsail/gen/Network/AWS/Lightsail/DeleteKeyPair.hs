{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific SSH key pair.
--
-- The @delete key pair@ operation supports tag-based access control via resource tags applied to the resource identified by @key pair name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteKeyPair
    (
    -- * Creating a request
      DeleteKeyPair (..)
    , mkDeleteKeyPair
    -- ** Request lenses
    , dkpKeyPairName

    -- * Destructuring the response
    , DeleteKeyPairResponse (..)
    , mkDeleteKeyPairResponse
    -- ** Response lenses
    , dkprrsOperation
    , dkprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteKeyPair' smart constructor.
newtype DeleteKeyPair = DeleteKeyPair'
  { keyPairName :: Types.ResourceName
    -- ^ The name of the key pair to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteKeyPair' value with any optional fields omitted.
mkDeleteKeyPair
    :: Types.ResourceName -- ^ 'keyPairName'
    -> DeleteKeyPair
mkDeleteKeyPair keyPairName = DeleteKeyPair'{keyPairName}

-- | The name of the key pair to delete.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyPairName :: Lens.Lens' DeleteKeyPair Types.ResourceName
dkpKeyPairName = Lens.field @"keyPairName"
{-# INLINEABLE dkpKeyPairName #-}
{-# DEPRECATED keyPairName "Use generic-lens or generic-optics with 'keyPairName' instead"  #-}

instance Core.ToQuery DeleteKeyPair where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteKeyPair where
        toHeaders DeleteKeyPair{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteKeyPair")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteKeyPair where
        toJSON DeleteKeyPair{..}
          = Core.object
              (Core.catMaybes [Core.Just ("keyPairName" Core..= keyPairName)])

instance Core.AWSRequest DeleteKeyPair where
        type Rs DeleteKeyPair = DeleteKeyPairResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteKeyPairResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteKeyPairResponse' value with any optional fields omitted.
mkDeleteKeyPairResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteKeyPairResponse
mkDeleteKeyPairResponse responseStatus
  = DeleteKeyPairResponse'{operation = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprrsOperation :: Lens.Lens' DeleteKeyPairResponse (Core.Maybe Types.Operation)
dkprrsOperation = Lens.field @"operation"
{-# INLINEABLE dkprrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprrsResponseStatus :: Lens.Lens' DeleteKeyPairResponse Core.Int
dkprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dkprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
