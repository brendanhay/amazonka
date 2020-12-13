{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateMLTransform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing machine learning transform. Call this operation to tune the algorithm parameters to achieve better results.
--
-- After calling this operation, you can call the @StartMLEvaluationTaskRun@ operation to assess how well your new parameters achieved your goals (such as improving the quality of your machine learning transform, or making it more cost-effective).
module Network.AWS.Glue.UpdateMLTransform
  ( -- * Creating a request
    UpdateMLTransform (..),
    mkUpdateMLTransform,

    -- ** Request lenses
    umltNumberOfWorkers,
    umltWorkerType,
    umltGlueVersion,
    umltRole,
    umltName,
    umltParameters,
    umltMaxRetries,
    umltMaxCapacity,
    umltTimeout,
    umltDescription,
    umltTransformId,

    -- * Destructuring the response
    UpdateMLTransformResponse (..),
    mkUpdateMLTransformResponse,

    -- ** Response lenses
    umltrsTransformId,
    umltrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateMLTransform' smart constructor.
data UpdateMLTransform = UpdateMLTransform'
  { -- | The number of workers of a defined @workerType@ that are allocated when this task runs.
    numberOfWorkers :: Lude.Maybe Lude.Int,
    -- | The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.
    --
    --
    --     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
    --
    --
    --     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.
    --
    --
    --     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
    workerType :: Lude.Maybe WorkerType,
    -- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
    glueVersion :: Lude.Maybe Lude.Text,
    -- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
    role' :: Lude.Maybe Lude.Text,
    -- | The unique name that you gave the transform when you created it.
    name :: Lude.Maybe Lude.Text,
    -- | The configuration parameters that are specific to the transform type (algorithm) used. Conditionally dependent on the transform type.
    parameters :: Lude.Maybe TransformParameters,
    -- | The maximum number of times to retry a task for this transform after a task run fails.
    maxRetries :: Lude.Maybe Lude.Int,
    -- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
    --
    -- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
    maxCapacity :: Lude.Maybe Lude.Double,
    -- | The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
    timeout :: Lude.Maybe Lude.Natural,
    -- | A description of the transform. The default is an empty string.
    description :: Lude.Maybe Lude.Text,
    -- | A unique identifier that was generated when the transform was created.
    transformId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMLTransform' with the minimum fields required to make a request.
--
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when this task runs.
-- * 'workerType' - The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.
--
--
--     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
--
--
-- * 'glueVersion' - This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
-- * 'role'' - The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
-- * 'name' - The unique name that you gave the transform when you created it.
-- * 'parameters' - The configuration parameters that are specific to the transform type (algorithm) used. Conditionally dependent on the transform type.
-- * 'maxRetries' - The maximum number of times to retry a task for this transform after a task run fails.
-- * 'maxCapacity' - The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
-- * 'timeout' - The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
-- * 'description' - A description of the transform. The default is an empty string.
-- * 'transformId' - A unique identifier that was generated when the transform was created.
mkUpdateMLTransform ::
  -- | 'transformId'
  Lude.Text ->
  UpdateMLTransform
mkUpdateMLTransform pTransformId_ =
  UpdateMLTransform'
    { numberOfWorkers = Lude.Nothing,
      workerType = Lude.Nothing,
      glueVersion = Lude.Nothing,
      role' = Lude.Nothing,
      name = Lude.Nothing,
      parameters = Lude.Nothing,
      maxRetries = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      timeout = Lude.Nothing,
      description = Lude.Nothing,
      transformId = pTransformId_
    }

-- | The number of workers of a defined @workerType@ that are allocated when this task runs.
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltNumberOfWorkers :: Lens.Lens' UpdateMLTransform (Lude.Maybe Lude.Int)
umltNumberOfWorkers = Lens.lens (numberOfWorkers :: UpdateMLTransform -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: UpdateMLTransform)
{-# DEPRECATED umltNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.
--
--
--     * For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 50GB disk, and 2 executors per worker.
--
--
--     * For the @G.1X@ worker type, each worker provides 4 vCPU, 16 GB of memory and a 64GB disk, and 1 executor per worker.
--
--
--     * For the @G.2X@ worker type, each worker provides 8 vCPU, 32 GB of memory and a 128GB disk, and 1 executor per worker.
--
--
--
-- /Note:/ Consider using 'workerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltWorkerType :: Lens.Lens' UpdateMLTransform (Lude.Maybe WorkerType)
umltWorkerType = Lens.lens (workerType :: UpdateMLTransform -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: UpdateMLTransform)
{-# DEPRECATED umltWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltGlueVersion :: Lens.Lens' UpdateMLTransform (Lude.Maybe Lude.Text)
umltGlueVersion = Lens.lens (glueVersion :: UpdateMLTransform -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: UpdateMLTransform)
{-# DEPRECATED umltGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltRole :: Lens.Lens' UpdateMLTransform (Lude.Maybe Lude.Text)
umltRole = Lens.lens (role' :: UpdateMLTransform -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: UpdateMLTransform)
{-# DEPRECATED umltRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The unique name that you gave the transform when you created it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltName :: Lens.Lens' UpdateMLTransform (Lude.Maybe Lude.Text)
umltName = Lens.lens (name :: UpdateMLTransform -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateMLTransform)
{-# DEPRECATED umltName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The configuration parameters that are specific to the transform type (algorithm) used. Conditionally dependent on the transform type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltParameters :: Lens.Lens' UpdateMLTransform (Lude.Maybe TransformParameters)
umltParameters = Lens.lens (parameters :: UpdateMLTransform -> Lude.Maybe TransformParameters) (\s a -> s {parameters = a} :: UpdateMLTransform)
{-# DEPRECATED umltParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The maximum number of times to retry a task for this transform after a task run fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltMaxRetries :: Lens.Lens' UpdateMLTransform (Lude.Maybe Lude.Int)
umltMaxRetries = Lens.lens (maxRetries :: UpdateMLTransform -> Lude.Maybe Lude.Int) (\s a -> s {maxRetries = a} :: UpdateMLTransform)
{-# DEPRECATED umltMaxRetries "Use generic-lens or generic-optics with 'maxRetries' instead." #-}

-- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltMaxCapacity :: Lens.Lens' UpdateMLTransform (Lude.Maybe Lude.Double)
umltMaxCapacity = Lens.lens (maxCapacity :: UpdateMLTransform -> Lude.Maybe Lude.Double) (\s a -> s {maxCapacity = a} :: UpdateMLTransform)
{-# DEPRECATED umltMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltTimeout :: Lens.Lens' UpdateMLTransform (Lude.Maybe Lude.Natural)
umltTimeout = Lens.lens (timeout :: UpdateMLTransform -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: UpdateMLTransform)
{-# DEPRECATED umltTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | A description of the transform. The default is an empty string.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltDescription :: Lens.Lens' UpdateMLTransform (Lude.Maybe Lude.Text)
umltDescription = Lens.lens (description :: UpdateMLTransform -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateMLTransform)
{-# DEPRECATED umltDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A unique identifier that was generated when the transform was created.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltTransformId :: Lens.Lens' UpdateMLTransform Lude.Text
umltTransformId = Lens.lens (transformId :: UpdateMLTransform -> Lude.Text) (\s a -> s {transformId = a} :: UpdateMLTransform)
{-# DEPRECATED umltTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

instance Lude.AWSRequest UpdateMLTransform where
  type Rs UpdateMLTransform = UpdateMLTransformResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMLTransformResponse'
            Lude.<$> (x Lude..?> "TransformId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMLTransform where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateMLTransform" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMLTransform where
  toJSON UpdateMLTransform' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NumberOfWorkers" Lude..=) Lude.<$> numberOfWorkers,
            ("WorkerType" Lude..=) Lude.<$> workerType,
            ("GlueVersion" Lude..=) Lude.<$> glueVersion,
            ("Role" Lude..=) Lude.<$> role',
            ("Name" Lude..=) Lude.<$> name,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("MaxRetries" Lude..=) Lude.<$> maxRetries,
            ("MaxCapacity" Lude..=) Lude.<$> maxCapacity,
            ("Timeout" Lude..=) Lude.<$> timeout,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("TransformId" Lude..= transformId)
          ]
      )

instance Lude.ToPath UpdateMLTransform where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMLTransform where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMLTransformResponse' smart constructor.
data UpdateMLTransformResponse = UpdateMLTransformResponse'
  { -- | The unique identifier for the transform that was updated.
    transformId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMLTransformResponse' with the minimum fields required to make a request.
--
-- * 'transformId' - The unique identifier for the transform that was updated.
-- * 'responseStatus' - The response status code.
mkUpdateMLTransformResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMLTransformResponse
mkUpdateMLTransformResponse pResponseStatus_ =
  UpdateMLTransformResponse'
    { transformId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for the transform that was updated.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltrsTransformId :: Lens.Lens' UpdateMLTransformResponse (Lude.Maybe Lude.Text)
umltrsTransformId = Lens.lens (transformId :: UpdateMLTransformResponse -> Lude.Maybe Lude.Text) (\s a -> s {transformId = a} :: UpdateMLTransformResponse)
{-# DEPRECATED umltrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umltrsResponseStatus :: Lens.Lens' UpdateMLTransformResponse Lude.Int
umltrsResponseStatus = Lens.lens (responseStatus :: UpdateMLTransformResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMLTransformResponse)
{-# DEPRECATED umltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
