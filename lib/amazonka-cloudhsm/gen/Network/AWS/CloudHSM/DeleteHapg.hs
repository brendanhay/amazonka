{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DeleteHapg
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Deletes a high-availability partition group.
module Network.AWS.CloudHSM.DeleteHapg
    (
    -- * Creating a request
      DeleteHapg (..)
    , mkDeleteHapg
    -- ** Request lenses
    , dhHapgArn

    -- * Destructuring the response
    , DeleteHapgResponse (..)
    , mkDeleteHapgResponse
    -- ** Response lenses
    , dhrrsStatus
    , dhrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DeleteHapg' action.
--
-- /See:/ 'mkDeleteHapg' smart constructor.
newtype DeleteHapg = DeleteHapg'
  { hapgArn :: Types.HapgArn
    -- ^ The ARN of the high-availability partition group to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHapg' value with any optional fields omitted.
mkDeleteHapg
    :: Types.HapgArn -- ^ 'hapgArn'
    -> DeleteHapg
mkDeleteHapg hapgArn = DeleteHapg'{hapgArn}

-- | The ARN of the high-availability partition group to delete.
--
-- /Note:/ Consider using 'hapgArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHapgArn :: Lens.Lens' DeleteHapg Types.HapgArn
dhHapgArn = Lens.field @"hapgArn"
{-# INLINEABLE dhHapgArn #-}
{-# DEPRECATED hapgArn "Use generic-lens or generic-optics with 'hapgArn' instead"  #-}

instance Core.ToQuery DeleteHapg where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteHapg where
        toHeaders DeleteHapg{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.DeleteHapg")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteHapg where
        toJSON DeleteHapg{..}
          = Core.object
              (Core.catMaybes [Core.Just ("HapgArn" Core..= hapgArn)])

instance Core.AWSRequest DeleteHapg where
        type Rs DeleteHapg = DeleteHapgResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteHapgResponse' Core.<$>
                   (x Core..: "Status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of the 'DeleteHapg' action.
--
-- /See:/ 'mkDeleteHapgResponse' smart constructor.
data DeleteHapgResponse = DeleteHapgResponse'
  { status :: Core.Text
    -- ^ The status of the action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteHapgResponse' value with any optional fields omitted.
mkDeleteHapgResponse
    :: Core.Text -- ^ 'status'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteHapgResponse
mkDeleteHapgResponse status responseStatus
  = DeleteHapgResponse'{status, responseStatus}

-- | The status of the action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsStatus :: Lens.Lens' DeleteHapgResponse Core.Text
dhrrsStatus = Lens.field @"status"
{-# INLINEABLE dhrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsResponseStatus :: Lens.Lens' DeleteHapgResponse Core.Int
dhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
