{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMLTransform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an AWS Glue machine learning transform artifact and all its corresponding metadata. Machine learning transforms are a special type of transform that use machine learning to learn the details of the transformation to be performed by learning from examples provided by humans. These transformations are then saved by AWS Glue. You can retrieve their metadata by calling @GetMLTransform@ .
module Network.AWS.Glue.GetMLTransform
  ( -- * Creating a request
    GetMLTransform (..),
    mkGetMLTransform,

    -- ** Request lenses
    gmltTransformId,

    -- * Destructuring the response
    GetMLTransformResponse (..),
    mkGetMLTransformResponse,

    -- ** Response lenses
    gmltrsStatus,
    gmltrsNumberOfWorkers,
    gmltrsLastModifiedOn,
    gmltrsLabelCount,
    gmltrsWorkerType,
    gmltrsInputRecordTables,
    gmltrsGlueVersion,
    gmltrsEvaluationMetrics,
    gmltrsSchema,
    gmltrsRole,
    gmltrsName,
    gmltrsParameters,
    gmltrsMaxRetries,
    gmltrsMaxCapacity,
    gmltrsTimeout,
    gmltrsTransformEncryption,
    gmltrsDescription,
    gmltrsCreatedOn,
    gmltrsTransformId,
    gmltrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMLTransform' smart constructor.
newtype GetMLTransform = GetMLTransform' {transformId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMLTransform' with the minimum fields required to make a request.
--
-- * 'transformId' - The unique identifier of the transform, generated at the time that the transform was created.
mkGetMLTransform ::
  -- | 'transformId'
  Lude.Text ->
  GetMLTransform
mkGetMLTransform pTransformId_ =
  GetMLTransform' {transformId = pTransformId_}

-- | The unique identifier of the transform, generated at the time that the transform was created.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltTransformId :: Lens.Lens' GetMLTransform Lude.Text
gmltTransformId = Lens.lens (transformId :: GetMLTransform -> Lude.Text) (\s a -> s {transformId = a} :: GetMLTransform)
{-# DEPRECATED gmltTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

instance Lude.AWSRequest GetMLTransform where
  type Rs GetMLTransform = GetMLTransformResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMLTransformResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "NumberOfWorkers")
            Lude.<*> (x Lude..?> "LastModifiedOn")
            Lude.<*> (x Lude..?> "LabelCount")
            Lude.<*> (x Lude..?> "WorkerType")
            Lude.<*> (x Lude..?> "InputRecordTables" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "GlueVersion")
            Lude.<*> (x Lude..?> "EvaluationMetrics")
            Lude.<*> (x Lude..?> "Schema" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Role")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "Parameters")
            Lude.<*> (x Lude..?> "MaxRetries")
            Lude.<*> (x Lude..?> "MaxCapacity")
            Lude.<*> (x Lude..?> "Timeout")
            Lude.<*> (x Lude..?> "TransformEncryption")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "CreatedOn")
            Lude.<*> (x Lude..?> "TransformId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMLTransform where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetMLTransform" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMLTransform where
  toJSON GetMLTransform' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TransformId" Lude..= transformId)])

instance Lude.ToPath GetMLTransform where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMLTransform where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMLTransformResponse' smart constructor.
data GetMLTransformResponse = GetMLTransformResponse'
  { status ::
      Lude.Maybe TransformStatusType,
    numberOfWorkers :: Lude.Maybe Lude.Int,
    lastModifiedOn :: Lude.Maybe Lude.Timestamp,
    labelCount :: Lude.Maybe Lude.Int,
    workerType :: Lude.Maybe WorkerType,
    inputRecordTables :: Lude.Maybe [GlueTable],
    glueVersion :: Lude.Maybe Lude.Text,
    evaluationMetrics ::
      Lude.Maybe EvaluationMetrics,
    schema :: Lude.Maybe [SchemaColumn],
    role' :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    parameters :: Lude.Maybe TransformParameters,
    maxRetries :: Lude.Maybe Lude.Int,
    maxCapacity :: Lude.Maybe Lude.Double,
    timeout :: Lude.Maybe Lude.Natural,
    transformEncryption ::
      Lude.Maybe TransformEncryption,
    description :: Lude.Maybe Lude.Text,
    createdOn :: Lude.Maybe Lude.Timestamp,
    transformId :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMLTransformResponse' with the minimum fields required to make a request.
--
-- * 'createdOn' - The date and time when the transform was created.
-- * 'description' - A description of the transform.
-- * 'evaluationMetrics' - The latest evaluation metrics.
-- * 'glueVersion' - This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
-- * 'inputRecordTables' - A list of AWS Glue table definitions used by the transform.
-- * 'labelCount' - The number of labels available for this transform.
-- * 'lastModifiedOn' - The date and time when the transform was last modified.
-- * 'maxCapacity' - The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
-- * 'maxRetries' - The maximum number of times to retry a task for this transform after a task run fails.
-- * 'name' - The unique name given to the transform when it was created.
-- * 'numberOfWorkers' - The number of workers of a defined @workerType@ that are allocated when this task runs.
-- * 'parameters' - The configuration parameters that are specific to the algorithm used.
-- * 'responseStatus' - The response status code.
-- * 'role'' - The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
-- * 'schema' - The @Map<Column, Type>@ object that represents the schema that this transform accepts. Has an upper bound of 100 columns.
-- * 'status' - The last known status of the transform (to indicate whether it can be used or not). One of "NOT_READY", "READY", or "DELETING".
-- * 'timeout' - The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
-- * 'transformEncryption' - The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
-- * 'transformId' - The unique identifier of the transform, generated at the time that the transform was created.
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
mkGetMLTransformResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMLTransformResponse
mkGetMLTransformResponse pResponseStatus_ =
  GetMLTransformResponse'
    { status = Lude.Nothing,
      numberOfWorkers = Lude.Nothing,
      lastModifiedOn = Lude.Nothing,
      labelCount = Lude.Nothing,
      workerType = Lude.Nothing,
      inputRecordTables = Lude.Nothing,
      glueVersion = Lude.Nothing,
      evaluationMetrics = Lude.Nothing,
      schema = Lude.Nothing,
      role' = Lude.Nothing,
      name = Lude.Nothing,
      parameters = Lude.Nothing,
      maxRetries = Lude.Nothing,
      maxCapacity = Lude.Nothing,
      timeout = Lude.Nothing,
      transformEncryption = Lude.Nothing,
      description = Lude.Nothing,
      createdOn = Lude.Nothing,
      transformId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The last known status of the transform (to indicate whether it can be used or not). One of "NOT_READY", "READY", or "DELETING".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsStatus :: Lens.Lens' GetMLTransformResponse (Lude.Maybe TransformStatusType)
gmltrsStatus = Lens.lens (status :: GetMLTransformResponse -> Lude.Maybe TransformStatusType) (\s a -> s {status = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The number of workers of a defined @workerType@ that are allocated when this task runs.
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsNumberOfWorkers :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Int)
gmltrsNumberOfWorkers = Lens.lens (numberOfWorkers :: GetMLTransformResponse -> Lude.Maybe Lude.Int) (\s a -> s {numberOfWorkers = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsNumberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead." #-}

-- | The date and time when the transform was last modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsLastModifiedOn :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Timestamp)
gmltrsLastModifiedOn = Lens.lens (lastModifiedOn :: GetMLTransformResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedOn = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsLastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead." #-}

-- | The number of labels available for this transform.
--
-- /Note:/ Consider using 'labelCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsLabelCount :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Int)
gmltrsLabelCount = Lens.lens (labelCount :: GetMLTransformResponse -> Lude.Maybe Lude.Int) (\s a -> s {labelCount = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsLabelCount "Use generic-lens or generic-optics with 'labelCount' instead." #-}

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
gmltrsWorkerType :: Lens.Lens' GetMLTransformResponse (Lude.Maybe WorkerType)
gmltrsWorkerType = Lens.lens (workerType :: GetMLTransformResponse -> Lude.Maybe WorkerType) (\s a -> s {workerType = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsWorkerType "Use generic-lens or generic-optics with 'workerType' instead." #-}

-- | A list of AWS Glue table definitions used by the transform.
--
-- /Note:/ Consider using 'inputRecordTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsInputRecordTables :: Lens.Lens' GetMLTransformResponse (Lude.Maybe [GlueTable])
gmltrsInputRecordTables = Lens.lens (inputRecordTables :: GetMLTransformResponse -> Lude.Maybe [GlueTable]) (\s a -> s {inputRecordTables = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsInputRecordTables "Use generic-lens or generic-optics with 'inputRecordTables' instead." #-}

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsGlueVersion :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Text)
gmltrsGlueVersion = Lens.lens (glueVersion :: GetMLTransformResponse -> Lude.Maybe Lude.Text) (\s a -> s {glueVersion = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsGlueVersion "Use generic-lens or generic-optics with 'glueVersion' instead." #-}

-- | The latest evaluation metrics.
--
-- /Note:/ Consider using 'evaluationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsEvaluationMetrics :: Lens.Lens' GetMLTransformResponse (Lude.Maybe EvaluationMetrics)
gmltrsEvaluationMetrics = Lens.lens (evaluationMetrics :: GetMLTransformResponse -> Lude.Maybe EvaluationMetrics) (\s a -> s {evaluationMetrics = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsEvaluationMetrics "Use generic-lens or generic-optics with 'evaluationMetrics' instead." #-}

-- | The @Map<Column, Type>@ object that represents the schema that this transform accepts. Has an upper bound of 100 columns.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsSchema :: Lens.Lens' GetMLTransformResponse (Lude.Maybe [SchemaColumn])
gmltrsSchema = Lens.lens (schema :: GetMLTransformResponse -> Lude.Maybe [SchemaColumn]) (\s a -> s {schema = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsRole :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Text)
gmltrsRole = Lens.lens (role' :: GetMLTransformResponse -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The unique name given to the transform when it was created.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsName :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Text)
gmltrsName = Lens.lens (name :: GetMLTransformResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The configuration parameters that are specific to the algorithm used.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsParameters :: Lens.Lens' GetMLTransformResponse (Lude.Maybe TransformParameters)
gmltrsParameters = Lens.lens (parameters :: GetMLTransformResponse -> Lude.Maybe TransformParameters) (\s a -> s {parameters = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The maximum number of times to retry a task for this transform after a task run fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsMaxRetries :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Int)
gmltrsMaxRetries = Lens.lens (maxRetries :: GetMLTransformResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxRetries = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsMaxRetries "Use generic-lens or generic-optics with 'maxRetries' instead." #-}

-- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsMaxCapacity :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Double)
gmltrsMaxCapacity = Lens.lens (maxCapacity :: GetMLTransformResponse -> Lude.Maybe Lude.Double) (\s a -> s {maxCapacity = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsTimeout :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Natural)
gmltrsTimeout = Lens.lens (timeout :: GetMLTransformResponse -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
--
-- /Note:/ Consider using 'transformEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsTransformEncryption :: Lens.Lens' GetMLTransformResponse (Lude.Maybe TransformEncryption)
gmltrsTransformEncryption = Lens.lens (transformEncryption :: GetMLTransformResponse -> Lude.Maybe TransformEncryption) (\s a -> s {transformEncryption = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsTransformEncryption "Use generic-lens or generic-optics with 'transformEncryption' instead." #-}

-- | A description of the transform.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsDescription :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Text)
gmltrsDescription = Lens.lens (description :: GetMLTransformResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date and time when the transform was created.
--
-- /Note:/ Consider using 'createdOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsCreatedOn :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Timestamp)
gmltrsCreatedOn = Lens.lens (createdOn :: GetMLTransformResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdOn = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsCreatedOn "Use generic-lens or generic-optics with 'createdOn' instead." #-}

-- | The unique identifier of the transform, generated at the time that the transform was created.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsTransformId :: Lens.Lens' GetMLTransformResponse (Lude.Maybe Lude.Text)
gmltrsTransformId = Lens.lens (transformId :: GetMLTransformResponse -> Lude.Maybe Lude.Text) (\s a -> s {transformId = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsResponseStatus :: Lens.Lens' GetMLTransformResponse Lude.Int
gmltrsResponseStatus = Lens.lens (responseStatus :: GetMLTransformResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMLTransformResponse)
{-# DEPRECATED gmltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
