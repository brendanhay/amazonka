{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more detail about the cluster step.
module Network.AWS.EMR.DescribeStep
  ( -- * Creating a request
    DescribeStep (..),
    mkDescribeStep,

    -- ** Request lenses
    dsStepId,
    dsClusterId,

    -- * Destructuring the response
    DescribeStepResponse (..),
    mkDescribeStepResponse,

    -- ** Response lenses
    dsfrsStep,
    dsfrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | This input determines which step to describe.
--
-- /See:/ 'mkDescribeStep' smart constructor.
data DescribeStep = DescribeStep'
  { -- | The identifier of the step to describe.
    stepId :: Lude.Text,
    -- | The identifier of the cluster with steps to describe.
    clusterId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStep' with the minimum fields required to make a request.
--
-- * 'stepId' - The identifier of the step to describe.
-- * 'clusterId' - The identifier of the cluster with steps to describe.
mkDescribeStep ::
  -- | 'stepId'
  Lude.Text ->
  -- | 'clusterId'
  Lude.Text ->
  DescribeStep
mkDescribeStep pStepId_ pClusterId_ =
  DescribeStep' {stepId = pStepId_, clusterId = pClusterId_}

-- | The identifier of the step to describe.
--
-- /Note:/ Consider using 'stepId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStepId :: Lens.Lens' DescribeStep Lude.Text
dsStepId = Lens.lens (stepId :: DescribeStep -> Lude.Text) (\s a -> s {stepId = a} :: DescribeStep)
{-# DEPRECATED dsStepId "Use generic-lens or generic-optics with 'stepId' instead." #-}

-- | The identifier of the cluster with steps to describe.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsClusterId :: Lens.Lens' DescribeStep Lude.Text
dsClusterId = Lens.lens (clusterId :: DescribeStep -> Lude.Text) (\s a -> s {clusterId = a} :: DescribeStep)
{-# DEPRECATED dsClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest DescribeStep where
  type Rs DescribeStep = DescribeStepResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStepResponse'
            Lude.<$> (x Lude..?> "Step") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStep where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.DescribeStep" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStep where
  toJSON DescribeStep' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StepId" Lude..= stepId),
            Lude.Just ("ClusterId" Lude..= clusterId)
          ]
      )

instance Lude.ToPath DescribeStep where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStep where
  toQuery = Lude.const Lude.mempty

-- | This output contains the description of the cluster step.
--
-- /See:/ 'mkDescribeStepResponse' smart constructor.
data DescribeStepResponse = DescribeStepResponse'
  { -- | The step details for the requested step identifier.
    step :: Lude.Maybe Step,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStepResponse' with the minimum fields required to make a request.
--
-- * 'step' - The step details for the requested step identifier.
-- * 'responseStatus' - The response status code.
mkDescribeStepResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStepResponse
mkDescribeStepResponse pResponseStatus_ =
  DescribeStepResponse'
    { step = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The step details for the requested step identifier.
--
-- /Note:/ Consider using 'step' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsStep :: Lens.Lens' DescribeStepResponse (Lude.Maybe Step)
dsfrsStep = Lens.lens (step :: DescribeStepResponse -> Lude.Maybe Step) (\s a -> s {step = a} :: DescribeStepResponse)
{-# DEPRECATED dsfrsStep "Use generic-lens or generic-optics with 'step' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsResponseStatus :: Lens.Lens' DescribeStepResponse Lude.Int
dsfrsResponseStatus = Lens.lens (responseStatus :: DescribeStepResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStepResponse)
{-# DEPRECATED dsfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
