{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.UpdateComputeEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an AWS Batch compute environment.
module Network.AWS.Batch.UpdateComputeEnvironment
  ( -- * Creating a request
    UpdateComputeEnvironment (..),
    mkUpdateComputeEnvironment,

    -- ** Request lenses
    uceState,
    uceComputeResources,
    uceServiceRole,
    uceComputeEnvironment,

    -- * Destructuring the response
    UpdateComputeEnvironmentResponse (..),
    mkUpdateComputeEnvironmentResponse,

    -- ** Response lenses
    ucersComputeEnvironmentName,
    ucersComputeEnvironmentARN,
    ucersResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateComputeEnvironment' smart constructor.
data UpdateComputeEnvironment = UpdateComputeEnvironment'
  { state ::
      Lude.Maybe CEState,
    computeResources ::
      Lude.Maybe ComputeResourceUpdate,
    serviceRole :: Lude.Maybe Lude.Text,
    computeEnvironment :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateComputeEnvironment' with the minimum fields required to make a request.
--
-- * 'computeEnvironment' - The name or full Amazon Resource Name (ARN) of the compute environment to update.
-- * 'computeResources' - Details of the compute resources managed by the compute environment. Required for a managed compute environment.
-- * 'serviceRole' - The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf.
--
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
-- * 'state' - The state of the compute environment. Compute environments in the @ENABLED@ state can accept jobs from a queue and scale in or out automatically based on the workload demand of its associated queues.
mkUpdateComputeEnvironment ::
  -- | 'computeEnvironment'
  Lude.Text ->
  UpdateComputeEnvironment
mkUpdateComputeEnvironment pComputeEnvironment_ =
  UpdateComputeEnvironment'
    { state = Lude.Nothing,
      computeResources = Lude.Nothing,
      serviceRole = Lude.Nothing,
      computeEnvironment = pComputeEnvironment_
    }

-- | The state of the compute environment. Compute environments in the @ENABLED@ state can accept jobs from a queue and scale in or out automatically based on the workload demand of its associated queues.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uceState :: Lens.Lens' UpdateComputeEnvironment (Lude.Maybe CEState)
uceState = Lens.lens (state :: UpdateComputeEnvironment -> Lude.Maybe CEState) (\s a -> s {state = a} :: UpdateComputeEnvironment)
{-# DEPRECATED uceState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Details of the compute resources managed by the compute environment. Required for a managed compute environment.
--
-- /Note:/ Consider using 'computeResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uceComputeResources :: Lens.Lens' UpdateComputeEnvironment (Lude.Maybe ComputeResourceUpdate)
uceComputeResources = Lens.lens (computeResources :: UpdateComputeEnvironment -> Lude.Maybe ComputeResourceUpdate) (\s a -> s {computeResources = a} :: UpdateComputeEnvironment)
{-# DEPRECATED uceComputeResources "Use generic-lens or generic-optics with 'computeResources' instead." #-}

-- | The full Amazon Resource Name (ARN) of the IAM role that allows AWS Batch to make calls to other AWS services on your behalf.
--
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uceServiceRole :: Lens.Lens' UpdateComputeEnvironment (Lude.Maybe Lude.Text)
uceServiceRole = Lens.lens (serviceRole :: UpdateComputeEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: UpdateComputeEnvironment)
{-# DEPRECATED uceServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The name or full Amazon Resource Name (ARN) of the compute environment to update.
--
-- /Note:/ Consider using 'computeEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uceComputeEnvironment :: Lens.Lens' UpdateComputeEnvironment Lude.Text
uceComputeEnvironment = Lens.lens (computeEnvironment :: UpdateComputeEnvironment -> Lude.Text) (\s a -> s {computeEnvironment = a} :: UpdateComputeEnvironment)
{-# DEPRECATED uceComputeEnvironment "Use generic-lens or generic-optics with 'computeEnvironment' instead." #-}

instance Lude.AWSRequest UpdateComputeEnvironment where
  type Rs UpdateComputeEnvironment = UpdateComputeEnvironmentResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateComputeEnvironmentResponse'
            Lude.<$> (x Lude..?> "computeEnvironmentName")
            Lude.<*> (x Lude..?> "computeEnvironmentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateComputeEnvironment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateComputeEnvironment where
  toJSON UpdateComputeEnvironment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("state" Lude..=) Lude.<$> state,
            ("computeResources" Lude..=) Lude.<$> computeResources,
            ("serviceRole" Lude..=) Lude.<$> serviceRole,
            Lude.Just ("computeEnvironment" Lude..= computeEnvironment)
          ]
      )

instance Lude.ToPath UpdateComputeEnvironment where
  toPath = Lude.const "/v1/updatecomputeenvironment"

instance Lude.ToQuery UpdateComputeEnvironment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateComputeEnvironmentResponse' smart constructor.
data UpdateComputeEnvironmentResponse = UpdateComputeEnvironmentResponse'
  { computeEnvironmentName ::
      Lude.Maybe Lude.Text,
    computeEnvironmentARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateComputeEnvironmentResponse' with the minimum fields required to make a request.
--
-- * 'computeEnvironmentARN' - The Amazon Resource Name (ARN) of the compute environment.
-- * 'computeEnvironmentName' - The name of the compute environment.
-- * 'responseStatus' - The response status code.
mkUpdateComputeEnvironmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateComputeEnvironmentResponse
mkUpdateComputeEnvironmentResponse pResponseStatus_ =
  UpdateComputeEnvironmentResponse'
    { computeEnvironmentName =
        Lude.Nothing,
      computeEnvironmentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucersComputeEnvironmentName :: Lens.Lens' UpdateComputeEnvironmentResponse (Lude.Maybe Lude.Text)
ucersComputeEnvironmentName = Lens.lens (computeEnvironmentName :: UpdateComputeEnvironmentResponse -> Lude.Maybe Lude.Text) (\s a -> s {computeEnvironmentName = a} :: UpdateComputeEnvironmentResponse)
{-# DEPRECATED ucersComputeEnvironmentName "Use generic-lens or generic-optics with 'computeEnvironmentName' instead." #-}

-- | The Amazon Resource Name (ARN) of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironmentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucersComputeEnvironmentARN :: Lens.Lens' UpdateComputeEnvironmentResponse (Lude.Maybe Lude.Text)
ucersComputeEnvironmentARN = Lens.lens (computeEnvironmentARN :: UpdateComputeEnvironmentResponse -> Lude.Maybe Lude.Text) (\s a -> s {computeEnvironmentARN = a} :: UpdateComputeEnvironmentResponse)
{-# DEPRECATED ucersComputeEnvironmentARN "Use generic-lens or generic-optics with 'computeEnvironmentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucersResponseStatus :: Lens.Lens' UpdateComputeEnvironmentResponse Lude.Int
ucersResponseStatus = Lens.lens (responseStatus :: UpdateComputeEnvironmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateComputeEnvironmentResponse)
{-# DEPRECATED ucersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
