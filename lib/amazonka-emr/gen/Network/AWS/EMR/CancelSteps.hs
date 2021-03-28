{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.CancelSteps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending step or steps in a running cluster. Available only in Amazon EMR versions 4.8.0 and later, excluding version 5.0.0. A maximum of 256 steps are allowed in each CancelSteps request. CancelSteps is idempotent but asynchronous; it does not guarantee that a step will be canceled, even if the request is successfully submitted. You can only cancel steps that are in a @PENDING@ state.
module Network.AWS.EMR.CancelSteps
    (
    -- * Creating a request
      CancelSteps (..)
    , mkCancelSteps
    -- ** Request lenses
    , csClusterId
    , csStepIds
    , csStepCancellationOption

    -- * Destructuring the response
    , CancelStepsResponse (..)
    , mkCancelStepsResponse
    -- ** Response lenses
    , crsCancelStepsInfoList
    , crsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input argument to the 'CancelSteps' operation.
--
-- /See:/ 'mkCancelSteps' smart constructor.
data CancelSteps = CancelSteps'
  { clusterId :: Types.XmlStringMaxLen256
    -- ^ The @ClusterID@ for the specified steps that will be canceled. Use 'RunJobFlow' and 'ListClusters' to get ClusterIDs. 
  , stepIds :: [Types.XmlStringMaxLen256]
    -- ^ The list of @StepIDs@ to cancel. Use 'ListSteps' to get steps and their states for the specified cluster.
  , stepCancellationOption :: Core.Maybe Types.StepCancellationOption
    -- ^ The option to choose to cancel @RUNNING@ steps. By default, the value is @SEND_INTERRUPT@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSteps' value with any optional fields omitted.
mkCancelSteps
    :: Types.XmlStringMaxLen256 -- ^ 'clusterId'
    -> CancelSteps
mkCancelSteps clusterId
  = CancelSteps'{clusterId, stepIds = Core.mempty,
                 stepCancellationOption = Core.Nothing}

-- | The @ClusterID@ for the specified steps that will be canceled. Use 'RunJobFlow' and 'ListClusters' to get ClusterIDs. 
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csClusterId :: Lens.Lens' CancelSteps Types.XmlStringMaxLen256
csClusterId = Lens.field @"clusterId"
{-# INLINEABLE csClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The list of @StepIDs@ to cancel. Use 'ListSteps' to get steps and their states for the specified cluster.
--
-- /Note:/ Consider using 'stepIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStepIds :: Lens.Lens' CancelSteps [Types.XmlStringMaxLen256]
csStepIds = Lens.field @"stepIds"
{-# INLINEABLE csStepIds #-}
{-# DEPRECATED stepIds "Use generic-lens or generic-optics with 'stepIds' instead"  #-}

-- | The option to choose to cancel @RUNNING@ steps. By default, the value is @SEND_INTERRUPT@ .
--
-- /Note:/ Consider using 'stepCancellationOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStepCancellationOption :: Lens.Lens' CancelSteps (Core.Maybe Types.StepCancellationOption)
csStepCancellationOption = Lens.field @"stepCancellationOption"
{-# INLINEABLE csStepCancellationOption #-}
{-# DEPRECATED stepCancellationOption "Use generic-lens or generic-optics with 'stepCancellationOption' instead"  #-}

instance Core.ToQuery CancelSteps where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelSteps where
        toHeaders CancelSteps{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.CancelSteps")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelSteps where
        toJSON CancelSteps{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  Core.Just ("StepIds" Core..= stepIds),
                  ("StepCancellationOption" Core..=) Core.<$>
                    stepCancellationOption])

instance Core.AWSRequest CancelSteps where
        type Rs CancelSteps = CancelStepsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelStepsResponse' Core.<$>
                   (x Core..:? "CancelStepsInfoList") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the 'CancelSteps' operation. 
--
-- /See:/ 'mkCancelStepsResponse' smart constructor.
data CancelStepsResponse = CancelStepsResponse'
  { cancelStepsInfoList :: Core.Maybe [Types.CancelStepsInfo]
    -- ^ A list of 'CancelStepsInfo' , which shows the status of specified cancel requests for each @StepID@ specified.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelStepsResponse' value with any optional fields omitted.
mkCancelStepsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelStepsResponse
mkCancelStepsResponse responseStatus
  = CancelStepsResponse'{cancelStepsInfoList = Core.Nothing,
                         responseStatus}

-- | A list of 'CancelStepsInfo' , which shows the status of specified cancel requests for each @StepID@ specified.
--
-- /Note:/ Consider using 'cancelStepsInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCancelStepsInfoList :: Lens.Lens' CancelStepsResponse (Core.Maybe [Types.CancelStepsInfo])
crsCancelStepsInfoList = Lens.field @"cancelStepsInfoList"
{-# INLINEABLE crsCancelStepsInfoList #-}
{-# DEPRECATED cancelStepsInfoList "Use generic-lens or generic-optics with 'cancelStepsInfoList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CancelStepsResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
