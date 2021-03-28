{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddJobFlowSteps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AddJobFlowSteps adds new steps to a running cluster. A maximum of 256 steps are allowed in each job flow.
--
-- If your cluster is long-running (such as a Hive data warehouse) or complex, you may require more than 256 steps to process your data. You can bypass the 256-step limitation in various ways, including using SSH to connect to the master node and submitting queries directly to the software running on the master node, such as Hive and Hadoop. For more information on how to do this, see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html Add More than 256 Steps to a Cluster> in the /Amazon EMR Management Guide/ .
-- A step specifies the location of a JAR file stored either on the master node of the cluster or in Amazon S3. Each step is performed by the main function of the main class of the JAR file. The main class can be specified either in the manifest of the JAR or by using the MainFunction parameter of the step.
-- Amazon EMR executes each step in the order listed. For a step to be considered complete, the main function must exit with a zero exit code and all Hadoop jobs started while the step was running must have completed and run successfully.
-- You can only add steps to a cluster that is in one of the following states: STARTING, BOOTSTRAPPING, RUNNING, or WAITING.
module Network.AWS.EMR.AddJobFlowSteps
    (
    -- * Creating a request
      AddJobFlowSteps (..)
    , mkAddJobFlowSteps
    -- ** Request lenses
    , ajfsJobFlowId
    , ajfsSteps

    -- * Destructuring the response
    , AddJobFlowStepsResponse (..)
    , mkAddJobFlowStepsResponse
    -- ** Response lenses
    , ajfsrrsStepIds
    , ajfsrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input argument to the 'AddJobFlowSteps' operation. 
--
-- /See:/ 'mkAddJobFlowSteps' smart constructor.
data AddJobFlowSteps = AddJobFlowSteps'
  { jobFlowId :: Types.JobFlowId
    -- ^ A string that uniquely identifies the job flow. This identifier is returned by 'RunJobFlow' and can also be obtained from 'ListClusters' . 
  , steps :: [Types.StepConfig]
    -- ^ A list of 'StepConfig' to be executed by the job flow. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddJobFlowSteps' value with any optional fields omitted.
mkAddJobFlowSteps
    :: Types.JobFlowId -- ^ 'jobFlowId'
    -> AddJobFlowSteps
mkAddJobFlowSteps jobFlowId
  = AddJobFlowSteps'{jobFlowId, steps = Core.mempty}

-- | A string that uniquely identifies the job flow. This identifier is returned by 'RunJobFlow' and can also be obtained from 'ListClusters' . 
--
-- /Note:/ Consider using 'jobFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajfsJobFlowId :: Lens.Lens' AddJobFlowSteps Types.JobFlowId
ajfsJobFlowId = Lens.field @"jobFlowId"
{-# INLINEABLE ajfsJobFlowId #-}
{-# DEPRECATED jobFlowId "Use generic-lens or generic-optics with 'jobFlowId' instead"  #-}

-- | A list of 'StepConfig' to be executed by the job flow. 
--
-- /Note:/ Consider using 'steps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajfsSteps :: Lens.Lens' AddJobFlowSteps [Types.StepConfig]
ajfsSteps = Lens.field @"steps"
{-# INLINEABLE ajfsSteps #-}
{-# DEPRECATED steps "Use generic-lens or generic-optics with 'steps' instead"  #-}

instance Core.ToQuery AddJobFlowSteps where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddJobFlowSteps where
        toHeaders AddJobFlowSteps{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.AddJobFlowSteps")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddJobFlowSteps where
        toJSON AddJobFlowSteps{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobFlowId" Core..= jobFlowId),
                  Core.Just ("Steps" Core..= steps)])

instance Core.AWSRequest AddJobFlowSteps where
        type Rs AddJobFlowSteps = AddJobFlowStepsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddJobFlowStepsResponse' Core.<$>
                   (x Core..:? "StepIds") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the 'AddJobFlowSteps' operation. 
--
-- /See:/ 'mkAddJobFlowStepsResponse' smart constructor.
data AddJobFlowStepsResponse = AddJobFlowStepsResponse'
  { stepIds :: Core.Maybe [Types.XmlStringMaxLen256]
    -- ^ The identifiers of the list of steps added to the job flow.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddJobFlowStepsResponse' value with any optional fields omitted.
mkAddJobFlowStepsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddJobFlowStepsResponse
mkAddJobFlowStepsResponse responseStatus
  = AddJobFlowStepsResponse'{stepIds = Core.Nothing, responseStatus}

-- | The identifiers of the list of steps added to the job flow.
--
-- /Note:/ Consider using 'stepIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajfsrrsStepIds :: Lens.Lens' AddJobFlowStepsResponse (Core.Maybe [Types.XmlStringMaxLen256])
ajfsrrsStepIds = Lens.field @"stepIds"
{-# INLINEABLE ajfsrrsStepIds #-}
{-# DEPRECATED stepIds "Use generic-lens or generic-optics with 'stepIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajfsrrsResponseStatus :: Lens.Lens' AddJobFlowStepsResponse Core.Int
ajfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ajfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
