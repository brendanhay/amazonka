{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetMLTransform (..)
    , mkGetMLTransform
    -- ** Request lenses
    , gmltTransformId

    -- * Destructuring the response
    , GetMLTransformResponse (..)
    , mkGetMLTransformResponse
    -- ** Response lenses
    , gmltrrsCreatedOn
    , gmltrrsDescription
    , gmltrrsEvaluationMetrics
    , gmltrrsGlueVersion
    , gmltrrsInputRecordTables
    , gmltrrsLabelCount
    , gmltrrsLastModifiedOn
    , gmltrrsMaxCapacity
    , gmltrrsMaxRetries
    , gmltrrsName
    , gmltrrsNumberOfWorkers
    , gmltrrsParameters
    , gmltrrsRole
    , gmltrrsSchema
    , gmltrrsStatus
    , gmltrrsTimeout
    , gmltrrsTransformEncryption
    , gmltrrsTransformId
    , gmltrrsWorkerType
    , gmltrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMLTransform' smart constructor.
newtype GetMLTransform = GetMLTransform'
  { transformId :: Types.HashString
    -- ^ The unique identifier of the transform, generated at the time that the transform was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetMLTransform' value with any optional fields omitted.
mkGetMLTransform
    :: Types.HashString -- ^ 'transformId'
    -> GetMLTransform
mkGetMLTransform transformId = GetMLTransform'{transformId}

-- | The unique identifier of the transform, generated at the time that the transform was created.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltTransformId :: Lens.Lens' GetMLTransform Types.HashString
gmltTransformId = Lens.field @"transformId"
{-# INLINEABLE gmltTransformId #-}
{-# DEPRECATED transformId "Use generic-lens or generic-optics with 'transformId' instead"  #-}

instance Core.ToQuery GetMLTransform where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMLTransform where
        toHeaders GetMLTransform{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetMLTransform") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMLTransform where
        toJSON GetMLTransform{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TransformId" Core..= transformId)])

instance Core.AWSRequest GetMLTransform where
        type Rs GetMLTransform = GetMLTransformResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMLTransformResponse' Core.<$>
                   (x Core..:? "CreatedOn") Core.<*> x Core..:? "Description" Core.<*>
                     x Core..:? "EvaluationMetrics"
                     Core.<*> x Core..:? "GlueVersion"
                     Core.<*> x Core..:? "InputRecordTables"
                     Core.<*> x Core..:? "LabelCount"
                     Core.<*> x Core..:? "LastModifiedOn"
                     Core.<*> x Core..:? "MaxCapacity"
                     Core.<*> x Core..:? "MaxRetries"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "NumberOfWorkers"
                     Core.<*> x Core..:? "Parameters"
                     Core.<*> x Core..:? "Role"
                     Core.<*> x Core..:? "Schema"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "Timeout"
                     Core.<*> x Core..:? "TransformEncryption"
                     Core.<*> x Core..:? "TransformId"
                     Core.<*> x Core..:? "WorkerType"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMLTransformResponse' smart constructor.
data GetMLTransformResponse = GetMLTransformResponse'
  { createdOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the transform was created.
  , description :: Core.Maybe Types.DescriptionString
    -- ^ A description of the transform.
  , evaluationMetrics :: Core.Maybe Types.EvaluationMetrics
    -- ^ The latest evaluation metrics.
  , glueVersion :: Core.Maybe Types.GlueVersionString
    -- ^ This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
  , inputRecordTables :: Core.Maybe [Types.GlueTable]
    -- ^ A list of AWS Glue table definitions used by the transform.
  , labelCount :: Core.Maybe Core.Int
    -- ^ The number of labels available for this transform.
  , lastModifiedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the transform was last modified.
  , maxCapacity :: Core.Maybe Core.Double
    -- ^ The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> . 
--
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
  , maxRetries :: Core.Maybe Core.Int
    -- ^ The maximum number of times to retry a task for this transform after a task run fails.
  , name :: Core.Maybe Types.NameString
    -- ^ The unique name given to the transform when it was created.
  , numberOfWorkers :: Core.Maybe Core.Int
    -- ^ The number of workers of a defined @workerType@ that are allocated when this task runs.
  , parameters :: Core.Maybe Types.TransformParameters
    -- ^ The configuration parameters that are specific to the algorithm used.
  , role' :: Core.Maybe Types.Role
    -- ^ The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
  , schema :: Core.Maybe [Types.SchemaColumn]
    -- ^ The @Map<Column, Type>@ object that represents the schema that this transform accepts. Has an upper bound of 100 columns.
  , status :: Core.Maybe Types.TransformStatusType
    -- ^ The last known status of the transform (to indicate whether it can be used or not). One of "NOT_READY", "READY", or "DELETING".
  , timeout :: Core.Maybe Core.Natural
    -- ^ The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
  , transformEncryption :: Core.Maybe Types.TransformEncryption
    -- ^ The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
  , transformId :: Core.Maybe Types.HashString
    -- ^ The unique identifier of the transform, generated at the time that the transform was created.
  , workerType :: Core.Maybe Types.WorkerType
    -- ^ The type of predefined worker that is allocated when this task runs. Accepts a value of Standard, G.1X, or G.2X.
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
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetMLTransformResponse' value with any optional fields omitted.
mkGetMLTransformResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMLTransformResponse
mkGetMLTransformResponse responseStatus
  = GetMLTransformResponse'{createdOn = Core.Nothing,
                            description = Core.Nothing, evaluationMetrics = Core.Nothing,
                            glueVersion = Core.Nothing, inputRecordTables = Core.Nothing,
                            labelCount = Core.Nothing, lastModifiedOn = Core.Nothing,
                            maxCapacity = Core.Nothing, maxRetries = Core.Nothing,
                            name = Core.Nothing, numberOfWorkers = Core.Nothing,
                            parameters = Core.Nothing, role' = Core.Nothing,
                            schema = Core.Nothing, status = Core.Nothing,
                            timeout = Core.Nothing, transformEncryption = Core.Nothing,
                            transformId = Core.Nothing, workerType = Core.Nothing,
                            responseStatus}

-- | The date and time when the transform was created.
--
-- /Note:/ Consider using 'createdOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsCreatedOn :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.NominalDiffTime)
gmltrrsCreatedOn = Lens.field @"createdOn"
{-# INLINEABLE gmltrrsCreatedOn #-}
{-# DEPRECATED createdOn "Use generic-lens or generic-optics with 'createdOn' instead"  #-}

-- | A description of the transform.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsDescription :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.DescriptionString)
gmltrrsDescription = Lens.field @"description"
{-# INLINEABLE gmltrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The latest evaluation metrics.
--
-- /Note:/ Consider using 'evaluationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsEvaluationMetrics :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.EvaluationMetrics)
gmltrrsEvaluationMetrics = Lens.field @"evaluationMetrics"
{-# INLINEABLE gmltrrsEvaluationMetrics #-}
{-# DEPRECATED evaluationMetrics "Use generic-lens or generic-optics with 'evaluationMetrics' instead"  #-}

-- | This value determines which version of AWS Glue this machine learning transform is compatible with. Glue 1.0 is recommended for most customers. If the value is not set, the Glue compatibility defaults to Glue 0.9. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/release-notes.html#release-notes-versions AWS Glue Versions> in the developer guide.
--
-- /Note:/ Consider using 'glueVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsGlueVersion :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.GlueVersionString)
gmltrrsGlueVersion = Lens.field @"glueVersion"
{-# INLINEABLE gmltrrsGlueVersion #-}
{-# DEPRECATED glueVersion "Use generic-lens or generic-optics with 'glueVersion' instead"  #-}

-- | A list of AWS Glue table definitions used by the transform.
--
-- /Note:/ Consider using 'inputRecordTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsInputRecordTables :: Lens.Lens' GetMLTransformResponse (Core.Maybe [Types.GlueTable])
gmltrrsInputRecordTables = Lens.field @"inputRecordTables"
{-# INLINEABLE gmltrrsInputRecordTables #-}
{-# DEPRECATED inputRecordTables "Use generic-lens or generic-optics with 'inputRecordTables' instead"  #-}

-- | The number of labels available for this transform.
--
-- /Note:/ Consider using 'labelCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsLabelCount :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Int)
gmltrrsLabelCount = Lens.field @"labelCount"
{-# INLINEABLE gmltrrsLabelCount #-}
{-# DEPRECATED labelCount "Use generic-lens or generic-optics with 'labelCount' instead"  #-}

-- | The date and time when the transform was last modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsLastModifiedOn :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.NominalDiffTime)
gmltrrsLastModifiedOn = Lens.field @"lastModifiedOn"
{-# INLINEABLE gmltrrsLastModifiedOn #-}
{-# DEPRECATED lastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead"  #-}

-- | The number of AWS Glue data processing units (DPUs) that are allocated to task runs for this transform. You can allocate from 2 to 100 DPUs; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> . 
--
-- When the @WorkerType@ field is set to a value other than @Standard@ , the @MaxCapacity@ field is set automatically and becomes read-only.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsMaxCapacity :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Double)
gmltrrsMaxCapacity = Lens.field @"maxCapacity"
{-# INLINEABLE gmltrrsMaxCapacity #-}
{-# DEPRECATED maxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead"  #-}

-- | The maximum number of times to retry a task for this transform after a task run fails.
--
-- /Note:/ Consider using 'maxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsMaxRetries :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Int)
gmltrrsMaxRetries = Lens.field @"maxRetries"
{-# INLINEABLE gmltrrsMaxRetries #-}
{-# DEPRECATED maxRetries "Use generic-lens or generic-optics with 'maxRetries' instead"  #-}

-- | The unique name given to the transform when it was created.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsName :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.NameString)
gmltrrsName = Lens.field @"name"
{-# INLINEABLE gmltrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of workers of a defined @workerType@ that are allocated when this task runs.
--
-- /Note:/ Consider using 'numberOfWorkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsNumberOfWorkers :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Int)
gmltrrsNumberOfWorkers = Lens.field @"numberOfWorkers"
{-# INLINEABLE gmltrrsNumberOfWorkers #-}
{-# DEPRECATED numberOfWorkers "Use generic-lens or generic-optics with 'numberOfWorkers' instead"  #-}

-- | The configuration parameters that are specific to the algorithm used.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsParameters :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.TransformParameters)
gmltrrsParameters = Lens.field @"parameters"
{-# INLINEABLE gmltrrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The name or Amazon Resource Name (ARN) of the IAM role with the required permissions.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsRole :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.Role)
gmltrrsRole = Lens.field @"role'"
{-# INLINEABLE gmltrrsRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | The @Map<Column, Type>@ object that represents the schema that this transform accepts. Has an upper bound of 100 columns.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsSchema :: Lens.Lens' GetMLTransformResponse (Core.Maybe [Types.SchemaColumn])
gmltrrsSchema = Lens.field @"schema"
{-# INLINEABLE gmltrrsSchema #-}
{-# DEPRECATED schema "Use generic-lens or generic-optics with 'schema' instead"  #-}

-- | The last known status of the transform (to indicate whether it can be used or not). One of "NOT_READY", "READY", or "DELETING".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsStatus :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.TransformStatusType)
gmltrrsStatus = Lens.field @"status"
{-# INLINEABLE gmltrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The timeout for a task run for this transform in minutes. This is the maximum time that a task run for this transform can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours).
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsTimeout :: Lens.Lens' GetMLTransformResponse (Core.Maybe Core.Natural)
gmltrrsTimeout = Lens.field @"timeout"
{-# INLINEABLE gmltrrsTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

-- | The encryption-at-rest settings of the transform that apply to accessing user data. Machine learning transforms can access user data encrypted in Amazon S3 using KMS.
--
-- /Note:/ Consider using 'transformEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsTransformEncryption :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.TransformEncryption)
gmltrrsTransformEncryption = Lens.field @"transformEncryption"
{-# INLINEABLE gmltrrsTransformEncryption #-}
{-# DEPRECATED transformEncryption "Use generic-lens or generic-optics with 'transformEncryption' instead"  #-}

-- | The unique identifier of the transform, generated at the time that the transform was created.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsTransformId :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.HashString)
gmltrrsTransformId = Lens.field @"transformId"
{-# INLINEABLE gmltrrsTransformId #-}
{-# DEPRECATED transformId "Use generic-lens or generic-optics with 'transformId' instead"  #-}

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
gmltrrsWorkerType :: Lens.Lens' GetMLTransformResponse (Core.Maybe Types.WorkerType)
gmltrrsWorkerType = Lens.field @"workerType"
{-# INLINEABLE gmltrrsWorkerType #-}
{-# DEPRECATED workerType "Use generic-lens or generic-optics with 'workerType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsResponseStatus :: Lens.Lens' GetMLTransformResponse Core.Int
gmltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
