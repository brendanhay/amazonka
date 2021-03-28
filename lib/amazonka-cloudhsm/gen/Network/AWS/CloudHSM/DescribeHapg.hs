{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeHapg
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Retrieves information about a high-availability partition group.
module Network.AWS.CloudHSM.DescribeHapg
    (
    -- * Creating a request
      DescribeHapg (..)
    , mkDescribeHapg
    -- ** Request lenses
    , dHapgArn

    -- * Destructuring the response
    , DescribeHapgResponse (..)
    , mkDescribeHapgResponse
    -- ** Response lenses
    , drsHapgArn
    , drsHapgSerial
    , drsHsmsLastActionFailed
    , drsHsmsPendingDeletion
    , drsHsmsPendingRegistration
    , drsLabel
    , drsLastModifiedTimestamp
    , drsPartitionSerialList
    , drsState
    , drsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DescribeHapg' action.
--
-- /See:/ 'mkDescribeHapg' smart constructor.
newtype DescribeHapg = DescribeHapg'
  { hapgArn :: Types.HapgArn
    -- ^ The ARN of the high-availability partition group to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHapg' value with any optional fields omitted.
mkDescribeHapg
    :: Types.HapgArn -- ^ 'hapgArn'
    -> DescribeHapg
mkDescribeHapg hapgArn = DescribeHapg'{hapgArn}

-- | The ARN of the high-availability partition group to describe.
--
-- /Note:/ Consider using 'hapgArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHapgArn :: Lens.Lens' DescribeHapg Types.HapgArn
dHapgArn = Lens.field @"hapgArn"
{-# INLINEABLE dHapgArn #-}
{-# DEPRECATED hapgArn "Use generic-lens or generic-optics with 'hapgArn' instead"  #-}

instance Core.ToQuery DescribeHapg where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeHapg where
        toHeaders DescribeHapg{..}
          = Core.pure
              ("X-Amz-Target", "CloudHsmFrontendService.DescribeHapg")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeHapg where
        toJSON DescribeHapg{..}
          = Core.object
              (Core.catMaybes [Core.Just ("HapgArn" Core..= hapgArn)])

instance Core.AWSRequest DescribeHapg where
        type Rs DescribeHapg = DescribeHapgResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeHapgResponse' Core.<$>
                   (x Core..:? "HapgArn") Core.<*> x Core..:? "HapgSerial" Core.<*>
                     x Core..:? "HsmsLastActionFailed"
                     Core.<*> x Core..:? "HsmsPendingDeletion"
                     Core.<*> x Core..:? "HsmsPendingRegistration"
                     Core.<*> x Core..:? "Label"
                     Core.<*> x Core..:? "LastModifiedTimestamp"
                     Core.<*> x Core..:? "PartitionSerialList"
                     Core.<*> x Core..:? "State"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of the 'DescribeHapg' action.
--
-- /See:/ 'mkDescribeHapgResponse' smart constructor.
data DescribeHapgResponse = DescribeHapgResponse'
  { hapgArn :: Core.Maybe Types.HapgArn
    -- ^ The ARN of the high-availability partition group.
  , hapgSerial :: Core.Maybe Core.Text
    -- ^ The serial number of the high-availability partition group.
  , hsmsLastActionFailed :: Core.Maybe [Types.HsmArn]
    -- ^ 
  , hsmsPendingDeletion :: Core.Maybe [Types.HsmArn]
    -- ^ 
  , hsmsPendingRegistration :: Core.Maybe [Types.HsmArn]
    -- ^ 
  , label :: Core.Maybe Types.Label
    -- ^ The label for the high-availability partition group.
  , lastModifiedTimestamp :: Core.Maybe Types.LastModifiedTimestamp
    -- ^ The date and time the high-availability partition group was last modified.
  , partitionSerialList :: Core.Maybe [Types.PartitionSerial]
    -- ^ The list of partition serial numbers that belong to the high-availability partition group.
  , state :: Core.Maybe Types.CloudHsmObjectState
    -- ^ The state of the high-availability partition group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHapgResponse' value with any optional fields omitted.
mkDescribeHapgResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeHapgResponse
mkDescribeHapgResponse responseStatus
  = DescribeHapgResponse'{hapgArn = Core.Nothing,
                          hapgSerial = Core.Nothing, hsmsLastActionFailed = Core.Nothing,
                          hsmsPendingDeletion = Core.Nothing,
                          hsmsPendingRegistration = Core.Nothing, label = Core.Nothing,
                          lastModifiedTimestamp = Core.Nothing,
                          partitionSerialList = Core.Nothing, state = Core.Nothing,
                          responseStatus}

-- | The ARN of the high-availability partition group.
--
-- /Note:/ Consider using 'hapgArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsHapgArn :: Lens.Lens' DescribeHapgResponse (Core.Maybe Types.HapgArn)
drsHapgArn = Lens.field @"hapgArn"
{-# INLINEABLE drsHapgArn #-}
{-# DEPRECATED hapgArn "Use generic-lens or generic-optics with 'hapgArn' instead"  #-}

-- | The serial number of the high-availability partition group.
--
-- /Note:/ Consider using 'hapgSerial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsHapgSerial :: Lens.Lens' DescribeHapgResponse (Core.Maybe Core.Text)
drsHapgSerial = Lens.field @"hapgSerial"
{-# INLINEABLE drsHapgSerial #-}
{-# DEPRECATED hapgSerial "Use generic-lens or generic-optics with 'hapgSerial' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'hsmsLastActionFailed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsHsmsLastActionFailed :: Lens.Lens' DescribeHapgResponse (Core.Maybe [Types.HsmArn])
drsHsmsLastActionFailed = Lens.field @"hsmsLastActionFailed"
{-# INLINEABLE drsHsmsLastActionFailed #-}
{-# DEPRECATED hsmsLastActionFailed "Use generic-lens or generic-optics with 'hsmsLastActionFailed' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'hsmsPendingDeletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsHsmsPendingDeletion :: Lens.Lens' DescribeHapgResponse (Core.Maybe [Types.HsmArn])
drsHsmsPendingDeletion = Lens.field @"hsmsPendingDeletion"
{-# INLINEABLE drsHsmsPendingDeletion #-}
{-# DEPRECATED hsmsPendingDeletion "Use generic-lens or generic-optics with 'hsmsPendingDeletion' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'hsmsPendingRegistration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsHsmsPendingRegistration :: Lens.Lens' DescribeHapgResponse (Core.Maybe [Types.HsmArn])
drsHsmsPendingRegistration = Lens.field @"hsmsPendingRegistration"
{-# INLINEABLE drsHsmsPendingRegistration #-}
{-# DEPRECATED hsmsPendingRegistration "Use generic-lens or generic-optics with 'hsmsPendingRegistration' instead"  #-}

-- | The label for the high-availability partition group.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLabel :: Lens.Lens' DescribeHapgResponse (Core.Maybe Types.Label)
drsLabel = Lens.field @"label"
{-# INLINEABLE drsLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

-- | The date and time the high-availability partition group was last modified.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLastModifiedTimestamp :: Lens.Lens' DescribeHapgResponse (Core.Maybe Types.LastModifiedTimestamp)
drsLastModifiedTimestamp = Lens.field @"lastModifiedTimestamp"
{-# INLINEABLE drsLastModifiedTimestamp #-}
{-# DEPRECATED lastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead"  #-}

-- | The list of partition serial numbers that belong to the high-availability partition group.
--
-- /Note:/ Consider using 'partitionSerialList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPartitionSerialList :: Lens.Lens' DescribeHapgResponse (Core.Maybe [Types.PartitionSerial])
drsPartitionSerialList = Lens.field @"partitionSerialList"
{-# INLINEABLE drsPartitionSerialList #-}
{-# DEPRECATED partitionSerialList "Use generic-lens or generic-optics with 'partitionSerialList' instead"  #-}

-- | The state of the high-availability partition group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsState :: Lens.Lens' DescribeHapgResponse (Core.Maybe Types.CloudHsmObjectState)
drsState = Lens.field @"state"
{-# INLINEABLE drsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeHapgResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
