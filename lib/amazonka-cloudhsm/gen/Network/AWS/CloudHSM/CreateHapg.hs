{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.CreateHapg
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Creates a high-availability partition group. A high-availability partition group is a group of partitions that spans multiple physical HSMs.
module Network.AWS.CloudHSM.CreateHapg
    (
    -- * Creating a request
      CreateHapg (..)
    , mkCreateHapg
    -- ** Request lenses
    , chLabel

    -- * Destructuring the response
    , CreateHapgResponse (..)
    , mkCreateHapgResponse
    -- ** Response lenses
    , chrrsHapgArn
    , chrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'CreateHapgRequest' action.
--
-- /See:/ 'mkCreateHapg' smart constructor.
newtype CreateHapg = CreateHapg'
  { label :: Types.Label
    -- ^ The label of the new high-availability partition group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHapg' value with any optional fields omitted.
mkCreateHapg
    :: Types.Label -- ^ 'label'
    -> CreateHapg
mkCreateHapg label = CreateHapg'{label}

-- | The label of the new high-availability partition group.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chLabel :: Lens.Lens' CreateHapg Types.Label
chLabel = Lens.field @"label"
{-# INLINEABLE chLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

instance Core.ToQuery CreateHapg where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateHapg where
        toHeaders CreateHapg{..}
          = Core.pure ("X-Amz-Target", "CloudHsmFrontendService.CreateHapg")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateHapg where
        toJSON CreateHapg{..}
          = Core.object (Core.catMaybes [Core.Just ("Label" Core..= label)])

instance Core.AWSRequest CreateHapg where
        type Rs CreateHapg = CreateHapgResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateHapgResponse' Core.<$>
                   (x Core..:? "HapgArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of the 'CreateHAPartitionGroup' action.
--
-- /See:/ 'mkCreateHapgResponse' smart constructor.
data CreateHapgResponse = CreateHapgResponse'
  { hapgArn :: Core.Maybe Types.HapgArn
    -- ^ The ARN of the high-availability partition group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHapgResponse' value with any optional fields omitted.
mkCreateHapgResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateHapgResponse
mkCreateHapgResponse responseStatus
  = CreateHapgResponse'{hapgArn = Core.Nothing, responseStatus}

-- | The ARN of the high-availability partition group.
--
-- /Note:/ Consider using 'hapgArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrrsHapgArn :: Lens.Lens' CreateHapgResponse (Core.Maybe Types.HapgArn)
chrrsHapgArn = Lens.field @"hapgArn"
{-# INLINEABLE chrrsHapgArn #-}
{-# DEPRECATED hapgArn "Use generic-lens or generic-optics with 'hapgArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chrrsResponseStatus :: Lens.Lens' CreateHapgResponse Core.Int
chrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE chrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
