{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DeleteLunaClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Deletes a client.
module Network.AWS.CloudHSM.DeleteLunaClient
    (
    -- * Creating a request
      DeleteLunaClient (..)
    , mkDeleteLunaClient
    -- ** Request lenses
    , dClientArn

    -- * Destructuring the response
    , DeleteLunaClientResponse (..)
    , mkDeleteLunaClientResponse
    -- ** Response lenses
    , dlcrrsStatus
    , dlcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLunaClient' smart constructor.
newtype DeleteLunaClient = DeleteLunaClient'
  { clientArn :: Types.ClientArn
    -- ^ The ARN of the client to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLunaClient' value with any optional fields omitted.
mkDeleteLunaClient
    :: Types.ClientArn -- ^ 'clientArn'
    -> DeleteLunaClient
mkDeleteLunaClient clientArn = DeleteLunaClient'{clientArn}

-- | The ARN of the client to delete.
--
-- /Note:/ Consider using 'clientArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dClientArn :: Lens.Lens' DeleteLunaClient Types.ClientArn
dClientArn = Lens.field @"clientArn"
{-# INLINEABLE dClientArn #-}
{-# DEPRECATED clientArn "Use generic-lens or generic-optics with 'clientArn' instead"  #-}

instance Core.ToQuery DeleteLunaClient where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteLunaClient where
        toHeaders DeleteLunaClient{..}
          = Core.pure
              ("X-Amz-Target", "CloudHsmFrontendService.DeleteLunaClient")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteLunaClient where
        toJSON DeleteLunaClient{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ClientArn" Core..= clientArn)])

instance Core.AWSRequest DeleteLunaClient where
        type Rs DeleteLunaClient = DeleteLunaClientResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteLunaClientResponse' Core.<$>
                   (x Core..: "Status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLunaClientResponse' smart constructor.
data DeleteLunaClientResponse = DeleteLunaClientResponse'
  { status :: Core.Text
    -- ^ The status of the action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLunaClientResponse' value with any optional fields omitted.
mkDeleteLunaClientResponse
    :: Core.Text -- ^ 'status'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteLunaClientResponse
mkDeleteLunaClientResponse status responseStatus
  = DeleteLunaClientResponse'{status, responseStatus}

-- | The status of the action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsStatus :: Lens.Lens' DeleteLunaClientResponse Core.Text
dlcrrsStatus = Lens.field @"status"
{-# INLINEABLE dlcrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsResponseStatus :: Lens.Lens' DeleteLunaClientResponse Core.Int
dlcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
