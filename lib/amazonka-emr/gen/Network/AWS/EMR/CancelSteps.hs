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
    csStepIds,
    csStepCancellationOption,
    csClusterId,

    -- * Destructuring the response
    CancelStepsResponse (..),
    mkCancelStepsResponse,

    -- ** Response lenses
    csrsCancelStepsInfoList,
    csrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input argument to the 'CancelSteps' operation.
--
-- /See:/ 'mkCancelSteps' smart constructor.
data CancelSteps = CancelSteps'
  { -- | The list of @StepIDs@ to cancel. Use 'ListSteps' to get steps and their states for the specified cluster.
    stepIds :: [Lude.Text],
    -- | The option to choose to cancel @RUNNING@ steps. By default, the value is @SEND_INTERRUPT@ .
    stepCancellationOption :: Lude.Maybe StepCancellationOption,
    -- | The @ClusterID@ for the specified steps that will be canceled. Use 'RunJobFlow' and 'ListClusters' to get ClusterIDs.
    clusterId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelSteps' with the minimum fields required to make a request.
--
-- * 'stepIds' - The list of @StepIDs@ to cancel. Use 'ListSteps' to get steps and their states for the specified cluster.
-- * 'stepCancellationOption' - The option to choose to cancel @RUNNING@ steps. By default, the value is @SEND_INTERRUPT@ .
-- * 'clusterId' - The @ClusterID@ for the specified steps that will be canceled. Use 'RunJobFlow' and 'ListClusters' to get ClusterIDs.
mkCancelSteps ::
  -- | 'clusterId'
  Lude.Text ->
  CancelSteps
mkCancelSteps pClusterId_ =
  CancelSteps'
    { stepIds = Lude.mempty,
      stepCancellationOption = Lude.Nothing,
      clusterId = pClusterId_
    }

-- | The list of @StepIDs@ to cancel. Use 'ListSteps' to get steps and their states for the specified cluster.
--
-- /Note:/ Consider using 'stepIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStepIds :: Lens.Lens' CancelSteps [Lude.Text]
csStepIds = Lens.lens (stepIds :: CancelSteps -> [Lude.Text]) (\s a -> s {stepIds = a} :: CancelSteps)
{-# DEPRECATED csStepIds "Use generic-lens or generic-optics with 'stepIds' instead." #-}

-- | The option to choose to cancel @RUNNING@ steps. By default, the value is @SEND_INTERRUPT@ .
--
-- /Note:/ Consider using 'stepCancellationOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStepCancellationOption :: Lens.Lens' CancelSteps (Lude.Maybe StepCancellationOption)
csStepCancellationOption = Lens.lens (stepCancellationOption :: CancelSteps -> Lude.Maybe StepCancellationOption) (\s a -> s {stepCancellationOption = a} :: CancelSteps)
{-# DEPRECATED csStepCancellationOption "Use generic-lens or generic-optics with 'stepCancellationOption' instead." #-}

-- | The @ClusterID@ for the specified steps that will be canceled. Use 'RunJobFlow' and 'ListClusters' to get ClusterIDs.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csClusterId :: Lens.Lens' CancelSteps Lude.Text
csClusterId = Lens.lens (clusterId :: CancelSteps -> Lude.Text) (\s a -> s {clusterId = a} :: CancelSteps)
{-# DEPRECATED csClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest CancelSteps where
  type Rs CancelSteps = CancelStepsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelStepsResponse'
            Lude.<$> (x Lude..?> "CancelStepsInfoList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelSteps where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.CancelSteps" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelSteps where
  toJSON CancelSteps' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StepIds" Lude..= stepIds),
            ("StepCancellationOption" Lude..=) Lude.<$> stepCancellationOption,
            Lude.Just ("ClusterId" Lude..= clusterId)
          ]
      )

instance Lude.ToPath CancelSteps where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelSteps where
  toQuery = Lude.const Lude.mempty

-- | The output for the 'CancelSteps' operation.
--
-- /See:/ 'mkCancelStepsResponse' smart constructor.
data CancelStepsResponse = CancelStepsResponse'
  { -- | A list of 'CancelStepsInfo' , which shows the status of specified cancel requests for each @StepID@ specified.
    cancelStepsInfoList :: Lude.Maybe [CancelStepsInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelStepsResponse' with the minimum fields required to make a request.
--
-- * 'cancelStepsInfoList' - A list of 'CancelStepsInfo' , which shows the status of specified cancel requests for each @StepID@ specified.
-- * 'responseStatus' - The response status code.
mkCancelStepsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelStepsResponse
mkCancelStepsResponse pResponseStatus_ =
  CancelStepsResponse'
    { cancelStepsInfoList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of 'CancelStepsInfo' , which shows the status of specified cancel requests for each @StepID@ specified.
--
-- /Note:/ Consider using 'cancelStepsInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsCancelStepsInfoList :: Lens.Lens' CancelStepsResponse (Lude.Maybe [CancelStepsInfo])
csrsCancelStepsInfoList = Lens.lens (cancelStepsInfoList :: CancelStepsResponse -> Lude.Maybe [CancelStepsInfo]) (\s a -> s {cancelStepsInfoList = a} :: CancelStepsResponse)
{-# DEPRECATED csrsCancelStepsInfoList "Use generic-lens or generic-optics with 'cancelStepsInfoList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CancelStepsResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CancelStepsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelStepsResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
