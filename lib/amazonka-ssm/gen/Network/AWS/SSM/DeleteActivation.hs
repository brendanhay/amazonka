{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteActivation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activation. You are not required to delete an activation. If you delete an activation, you can no longer use it to register additional managed instances. Deleting an activation does not de-register managed instances. You must manually de-register managed instances.
module Network.AWS.SSM.DeleteActivation
    (
    -- * Creating a request
      DeleteActivation (..)
    , mkDeleteActivation
    -- ** Request lenses
    , daActivationId

    -- * Destructuring the response
    , DeleteActivationResponse (..)
    , mkDeleteActivationResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteActivation' smart constructor.
newtype DeleteActivation = DeleteActivation'
  { activationId :: Types.ActivationId
    -- ^ The ID of the activation that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteActivation' value with any optional fields omitted.
mkDeleteActivation
    :: Types.ActivationId -- ^ 'activationId'
    -> DeleteActivation
mkDeleteActivation activationId = DeleteActivation'{activationId}

-- | The ID of the activation that you want to delete.
--
-- /Note:/ Consider using 'activationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daActivationId :: Lens.Lens' DeleteActivation Types.ActivationId
daActivationId = Lens.field @"activationId"
{-# INLINEABLE daActivationId #-}
{-# DEPRECATED activationId "Use generic-lens or generic-optics with 'activationId' instead"  #-}

instance Core.ToQuery DeleteActivation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteActivation where
        toHeaders DeleteActivation{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DeleteActivation") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteActivation where
        toJSON DeleteActivation{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ActivationId" Core..= activationId)])

instance Core.AWSRequest DeleteActivation where
        type Rs DeleteActivation = DeleteActivationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteActivationResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteActivationResponse' smart constructor.
newtype DeleteActivationResponse = DeleteActivationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteActivationResponse' value with any optional fields omitted.
mkDeleteActivationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteActivationResponse
mkDeleteActivationResponse responseStatus
  = DeleteActivationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteActivationResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
