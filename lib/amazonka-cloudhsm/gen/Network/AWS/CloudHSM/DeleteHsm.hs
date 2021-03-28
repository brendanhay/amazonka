{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DeleteHsm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Deletes an HSM. After completion, this operation cannot be undone and your key material cannot be recovered.
module Network.AWS.CloudHSM.DeleteHsm
    (
    -- * Creating a request
      DeleteHsm (..)
    , mkDeleteHsm
    -- ** Request lenses
    , dhHsmArn

    -- * Destructuring the response
    , DeleteHsmResponse (..)
    , mkDeleteHsmResponse
    -- ** Response lenses
    , dhrfrsStatus
    , dhrfrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DeleteHsm' operation.
--
-- /See:/ 'mkDeleteHsm' smart constructor.
newtype DeleteHsm = DeleteHsm'
  { hsmArn :: Types.HsmArn
    -- ^ The ARN of the HSM to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHsm' value with any optional fields omitted.
mkDeleteHsm
    :: Types.HsmArn -- ^ 'hsmArn'
    -> DeleteHsm
mkDeleteHsm hsmArn = DeleteHsm'{hsmArn}

-- | The ARN of the HSM to delete.
--
-- /Note:/ Consider using 'hsmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHsmArn :: Lens.Lens' DeleteHsm Types.HsmArn
dhHsmArn = Lens.field @"hsmArn"
{-# INLINEABLE dhHsmArn #-}
{-# DEPRECATED hsmArn "Use generic-lens or generic-optics with 'hsmArn' instead"  #-}

instance Core.ToQuery DeleteHsm where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteHsm where
        toHeaders DeleteHsm{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.DeleteHsm")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteHsm where
        toJSON DeleteHsm{..}
          = Core.object
              (Core.catMaybes [Core.Just ("HsmArn" Core..= hsmArn)])

instance Core.AWSRequest DeleteHsm where
        type Rs DeleteHsm = DeleteHsmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteHsmResponse' Core.<$>
                   (x Core..: "Status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of the 'DeleteHsm' operation.
--
-- /See:/ 'mkDeleteHsmResponse' smart constructor.
data DeleteHsmResponse = DeleteHsmResponse'
  { status :: Core.Text
    -- ^ The status of the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHsmResponse' value with any optional fields omitted.
mkDeleteHsmResponse
    :: Core.Text -- ^ 'status'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteHsmResponse
mkDeleteHsmResponse status responseStatus
  = DeleteHsmResponse'{status, responseStatus}

-- | The status of the operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrfrsStatus :: Lens.Lens' DeleteHsmResponse Core.Text
dhrfrsStatus = Lens.field @"status"
{-# INLINEABLE dhrfrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrfrsResponseStatus :: Lens.Lens' DeleteHsmResponse Core.Int
dhrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
