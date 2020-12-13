{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AddJobFlowSteps (..),
    mkAddJobFlowSteps,

    -- ** Request lenses
    ajfsJobFlowId,
    ajfsSteps,

    -- * Destructuring the response
    AddJobFlowStepsResponse (..),
    mkAddJobFlowStepsResponse,

    -- ** Response lenses
    ajfsrsStepIds,
    ajfsrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input argument to the 'AddJobFlowSteps' operation.
--
-- /See:/ 'mkAddJobFlowSteps' smart constructor.
data AddJobFlowSteps = AddJobFlowSteps'
  { -- | A string that uniquely identifies the job flow. This identifier is returned by 'RunJobFlow' and can also be obtained from 'ListClusters' .
    jobFlowId :: Lude.Text,
    -- | A list of 'StepConfig' to be executed by the job flow.
    steps :: [StepConfig]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddJobFlowSteps' with the minimum fields required to make a request.
--
-- * 'jobFlowId' - A string that uniquely identifies the job flow. This identifier is returned by 'RunJobFlow' and can also be obtained from 'ListClusters' .
-- * 'steps' - A list of 'StepConfig' to be executed by the job flow.
mkAddJobFlowSteps ::
  -- | 'jobFlowId'
  Lude.Text ->
  AddJobFlowSteps
mkAddJobFlowSteps pJobFlowId_ =
  AddJobFlowSteps' {jobFlowId = pJobFlowId_, steps = Lude.mempty}

-- | A string that uniquely identifies the job flow. This identifier is returned by 'RunJobFlow' and can also be obtained from 'ListClusters' .
--
-- /Note:/ Consider using 'jobFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajfsJobFlowId :: Lens.Lens' AddJobFlowSteps Lude.Text
ajfsJobFlowId = Lens.lens (jobFlowId :: AddJobFlowSteps -> Lude.Text) (\s a -> s {jobFlowId = a} :: AddJobFlowSteps)
{-# DEPRECATED ajfsJobFlowId "Use generic-lens or generic-optics with 'jobFlowId' instead." #-}

-- | A list of 'StepConfig' to be executed by the job flow.
--
-- /Note:/ Consider using 'steps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajfsSteps :: Lens.Lens' AddJobFlowSteps [StepConfig]
ajfsSteps = Lens.lens (steps :: AddJobFlowSteps -> [StepConfig]) (\s a -> s {steps = a} :: AddJobFlowSteps)
{-# DEPRECATED ajfsSteps "Use generic-lens or generic-optics with 'steps' instead." #-}

instance Lude.AWSRequest AddJobFlowSteps where
  type Rs AddJobFlowSteps = AddJobFlowStepsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddJobFlowStepsResponse'
            Lude.<$> (x Lude..?> "StepIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddJobFlowSteps where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.AddJobFlowSteps" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddJobFlowSteps where
  toJSON AddJobFlowSteps' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobFlowId" Lude..= jobFlowId),
            Lude.Just ("Steps" Lude..= steps)
          ]
      )

instance Lude.ToPath AddJobFlowSteps where
  toPath = Lude.const "/"

instance Lude.ToQuery AddJobFlowSteps where
  toQuery = Lude.const Lude.mempty

-- | The output for the 'AddJobFlowSteps' operation.
--
-- /See:/ 'mkAddJobFlowStepsResponse' smart constructor.
data AddJobFlowStepsResponse = AddJobFlowStepsResponse'
  { -- | The identifiers of the list of steps added to the job flow.
    stepIds :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddJobFlowStepsResponse' with the minimum fields required to make a request.
--
-- * 'stepIds' - The identifiers of the list of steps added to the job flow.
-- * 'responseStatus' - The response status code.
mkAddJobFlowStepsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddJobFlowStepsResponse
mkAddJobFlowStepsResponse pResponseStatus_ =
  AddJobFlowStepsResponse'
    { stepIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifiers of the list of steps added to the job flow.
--
-- /Note:/ Consider using 'stepIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajfsrsStepIds :: Lens.Lens' AddJobFlowStepsResponse (Lude.Maybe [Lude.Text])
ajfsrsStepIds = Lens.lens (stepIds :: AddJobFlowStepsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {stepIds = a} :: AddJobFlowStepsResponse)
{-# DEPRECATED ajfsrsStepIds "Use generic-lens or generic-optics with 'stepIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajfsrsResponseStatus :: Lens.Lens' AddJobFlowStepsResponse Lude.Int
ajfsrsResponseStatus = Lens.lens (responseStatus :: AddJobFlowStepsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddJobFlowStepsResponse)
{-# DEPRECATED ajfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
