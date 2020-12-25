{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CancelSteps (..),
    mkCancelSteps,

    -- ** Request lenses
    csClusterId,
    csStepIds,
    csStepCancellationOption,

    -- * Destructuring the response
    CancelStepsResponse (..),
    mkCancelStepsResponse,

    -- ** Response lenses
    crsCancelStepsInfoList,
    crsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input argument to the 'CancelSteps' operation.
--
-- /See:/ 'mkCancelSteps' smart constructor.
data CancelSteps = CancelSteps'
  { -- | The @ClusterID@ for the specified steps that will be canceled. Use 'RunJobFlow' and 'ListClusters' to get ClusterIDs.
    clusterId :: Types.XmlStringMaxLen256,
    -- | The list of @StepIDs@ to cancel. Use 'ListSteps' to get steps and their states for the specified cluster.
    stepIds :: [Types.XmlStringMaxLen256],
    -- | The option to choose to cancel @RUNNING@ steps. By default, the value is @SEND_INTERRUPT@ .
    stepCancellationOption :: Core.Maybe Types.StepCancellationOption
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSteps' value with any optional fields omitted.
mkCancelSteps ::
  -- | 'clusterId'
  Types.XmlStringMaxLen256 ->
  CancelSteps
mkCancelSteps clusterId =
  CancelSteps'
    { clusterId,
      stepIds = Core.mempty,
      stepCancellationOption = Core.Nothing
    }

-- | The @ClusterID@ for the specified steps that will be canceled. Use 'RunJobFlow' and 'ListClusters' to get ClusterIDs.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csClusterId :: Lens.Lens' CancelSteps Types.XmlStringMaxLen256
csClusterId = Lens.field @"clusterId"
{-# DEPRECATED csClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The list of @StepIDs@ to cancel. Use 'ListSteps' to get steps and their states for the specified cluster.
--
-- /Note:/ Consider using 'stepIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStepIds :: Lens.Lens' CancelSteps [Types.XmlStringMaxLen256]
csStepIds = Lens.field @"stepIds"
{-# DEPRECATED csStepIds "Use generic-lens or generic-optics with 'stepIds' instead." #-}

-- | The option to choose to cancel @RUNNING@ steps. By default, the value is @SEND_INTERRUPT@ .
--
-- /Note:/ Consider using 'stepCancellationOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStepCancellationOption :: Lens.Lens' CancelSteps (Core.Maybe Types.StepCancellationOption)
csStepCancellationOption = Lens.field @"stepCancellationOption"
{-# DEPRECATED csStepCancellationOption "Use generic-lens or generic-optics with 'stepCancellationOption' instead." #-}

instance Core.FromJSON CancelSteps where
  toJSON CancelSteps {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("StepIds" Core..= stepIds),
            ("StepCancellationOption" Core..=)
              Core.<$> stepCancellationOption
          ]
      )

instance Core.AWSRequest CancelSteps where
  type Rs CancelSteps = CancelStepsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.CancelSteps")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelStepsResponse'
            Core.<$> (x Core..:? "CancelStepsInfoList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for the 'CancelSteps' operation.
--
-- /See:/ 'mkCancelStepsResponse' smart constructor.
data CancelStepsResponse = CancelStepsResponse'
  { -- | A list of 'CancelStepsInfo' , which shows the status of specified cancel requests for each @StepID@ specified.
    cancelStepsInfoList :: Core.Maybe [Types.CancelStepsInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelStepsResponse' value with any optional fields omitted.
mkCancelStepsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelStepsResponse
mkCancelStepsResponse responseStatus =
  CancelStepsResponse'
    { cancelStepsInfoList = Core.Nothing,
      responseStatus
    }

-- | A list of 'CancelStepsInfo' , which shows the status of specified cancel requests for each @StepID@ specified.
--
-- /Note:/ Consider using 'cancelStepsInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCancelStepsInfoList :: Lens.Lens' CancelStepsResponse (Core.Maybe [Types.CancelStepsInfo])
crsCancelStepsInfoList = Lens.field @"cancelStepsInfoList"
{-# DEPRECATED crsCancelStepsInfoList "Use generic-lens or generic-optics with 'cancelStepsInfoList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CancelStepsResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
