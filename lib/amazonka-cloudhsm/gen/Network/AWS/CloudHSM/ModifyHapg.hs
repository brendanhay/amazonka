{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyHapg
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Modifies an existing high-availability partition group.
module Network.AWS.CloudHSM.ModifyHapg
    (
    -- * Creating a request
      ModifyHapg (..)
    , mkModifyHapg
    -- ** Request lenses
    , mhHapgArn
    , mhLabel
    , mhPartitionSerialList

    -- * Destructuring the response
    , ModifyHapgResponse (..)
    , mkModifyHapgResponse
    -- ** Response lenses
    , mrsHapgArn
    , mrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyHapg' smart constructor.
data ModifyHapg = ModifyHapg'
  { hapgArn :: Types.HapgArn
    -- ^ The ARN of the high-availability partition group to modify.
  , label :: Core.Maybe Types.Label
    -- ^ The new label for the high-availability partition group.
  , partitionSerialList :: Core.Maybe [Types.PartitionSerial]
    -- ^ The list of partition serial numbers to make members of the high-availability partition group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyHapg' value with any optional fields omitted.
mkModifyHapg
    :: Types.HapgArn -- ^ 'hapgArn'
    -> ModifyHapg
mkModifyHapg hapgArn
  = ModifyHapg'{hapgArn, label = Core.Nothing,
                partitionSerialList = Core.Nothing}

-- | The ARN of the high-availability partition group to modify.
--
-- /Note:/ Consider using 'hapgArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhHapgArn :: Lens.Lens' ModifyHapg Types.HapgArn
mhHapgArn = Lens.field @"hapgArn"
{-# INLINEABLE mhHapgArn #-}
{-# DEPRECATED hapgArn "Use generic-lens or generic-optics with 'hapgArn' instead"  #-}

-- | The new label for the high-availability partition group.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhLabel :: Lens.Lens' ModifyHapg (Core.Maybe Types.Label)
mhLabel = Lens.field @"label"
{-# INLINEABLE mhLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

-- | The list of partition serial numbers to make members of the high-availability partition group.
--
-- /Note:/ Consider using 'partitionSerialList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhPartitionSerialList :: Lens.Lens' ModifyHapg (Core.Maybe [Types.PartitionSerial])
mhPartitionSerialList = Lens.field @"partitionSerialList"
{-# INLINEABLE mhPartitionSerialList #-}
{-# DEPRECATED partitionSerialList "Use generic-lens or generic-optics with 'partitionSerialList' instead"  #-}

instance Core.ToQuery ModifyHapg where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyHapg where
        toHeaders ModifyHapg{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.ModifyHapg")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyHapg where
        toJSON ModifyHapg{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("HapgArn" Core..= hapgArn),
                  ("Label" Core..=) Core.<$> label,
                  ("PartitionSerialList" Core..=) Core.<$> partitionSerialList])

instance Core.AWSRequest ModifyHapg where
        type Rs ModifyHapg = ModifyHapgResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ModifyHapgResponse' Core.<$>
                   (x Core..:? "HapgArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyHapgResponse' smart constructor.
data ModifyHapgResponse = ModifyHapgResponse'
  { hapgArn :: Core.Maybe Types.HapgArn
    -- ^ The ARN of the high-availability partition group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyHapgResponse' value with any optional fields omitted.
mkModifyHapgResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyHapgResponse
mkModifyHapgResponse responseStatus
  = ModifyHapgResponse'{hapgArn = Core.Nothing, responseStatus}

-- | The ARN of the high-availability partition group.
--
-- /Note:/ Consider using 'hapgArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsHapgArn :: Lens.Lens' ModifyHapgResponse (Core.Maybe Types.HapgArn)
mrsHapgArn = Lens.field @"hapgArn"
{-# INLINEABLE mrsHapgArn #-}
{-# DEPRECATED hapgArn "Use generic-lens or generic-optics with 'hapgArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsResponseStatus :: Lens.Lens' ModifyHapgResponse Core.Int
mrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
