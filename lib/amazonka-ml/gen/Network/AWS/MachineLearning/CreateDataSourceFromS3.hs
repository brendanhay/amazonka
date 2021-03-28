{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromS3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ object. A @DataSource@ references data that can be used to perform @CreateMLModel@ , @CreateEvaluation@ , or @CreateBatchPrediction@ operations.
--
-- @CreateDataSourceFromS3@ is an asynchronous operation. In response to @CreateDataSourceFromS3@ , Amazon Machine Learning (Amazon ML) immediately returns and sets the @DataSource@ status to @PENDING@ . After the @DataSource@ has been created and is ready for use, Amazon ML sets the @Status@ parameter to @COMPLETED@ . @DataSource@ in the @COMPLETED@ or @PENDING@ state can be used to perform only @CreateMLModel@ , @CreateEvaluation@ or @CreateBatchPrediction@ operations. 
-- If Amazon ML can't accept the input source, it sets the @Status@ parameter to @FAILED@ and includes an error message in the @Message@ attribute of the @GetDataSource@ operation response. 
-- The observation data used in a @DataSource@ should be ready to use; that is, it should have a consistent structure, and missing data values should be kept to a minimum. The observation data must reside in one or more .csv files in an Amazon Simple Storage Service (Amazon S3) location, along with a schema that describes the data items by name and type. The same schema must be used for all of the data files referenced by the @DataSource@ . 
-- After the @DataSource@ has been created, it's ready to use in evaluations and batch predictions. If you plan to use the @DataSource@ to train an @MLModel@ , the @DataSource@ also needs a recipe. A recipe describes how each input variable will be used in training an @MLModel@ . Will the variable be included or excluded from training? Will the variable be manipulated; for example, will it be combined with another variable or will it be split apart into word combinations? The recipe provides answers to these questions.
module Network.AWS.MachineLearning.CreateDataSourceFromS3
    (
    -- * Creating a request
      CreateDataSourceFromS3 (..)
    , mkCreateDataSourceFromS3
    -- ** Request lenses
    , cdsfsDataSourceId
    , cdsfsDataSpec
    , cdsfsComputeStatistics
    , cdsfsDataSourceName

    -- * Destructuring the response
    , CreateDataSourceFromS3Response (..)
    , mkCreateDataSourceFromS3Response
    -- ** Response lenses
    , cdsfsrrsDataSourceId
    , cdsfsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDataSourceFromS3' smart constructor.
data CreateDataSourceFromS3 = CreateDataSourceFromS3'
  { dataSourceId :: Types.DataSourceId
    -- ^ A user-supplied identifier that uniquely identifies the @DataSource@ . 
  , dataSpec :: Types.S3DataSpec
    -- ^ The data specification of a @DataSource@ :
--
--
--     * DataLocationS3 - The Amazon S3 location of the observation data.
--
--
--     * DataSchemaLocationS3 - The Amazon S3 location of the @DataSchema@ .
--
--
--     * DataSchema - A JSON string representing the schema. This is not required if @DataSchemaUri@ is specified. 
--
--
--     * DataRearrangement - A JSON string that represents the splitting and rearrangement requirements for the @Datasource@ . 
-- Sample - @"{\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"@ 
--
--
  , computeStatistics :: Core.Maybe Core.Bool
    -- ^ The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training.
  , dataSourceName :: Core.Maybe Types.EntityName
    -- ^ A user-supplied name or description of the @DataSource@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataSourceFromS3' value with any optional fields omitted.
mkCreateDataSourceFromS3
    :: Types.DataSourceId -- ^ 'dataSourceId'
    -> Types.S3DataSpec -- ^ 'dataSpec'
    -> CreateDataSourceFromS3
mkCreateDataSourceFromS3 dataSourceId dataSpec
  = CreateDataSourceFromS3'{dataSourceId, dataSpec,
                            computeStatistics = Core.Nothing, dataSourceName = Core.Nothing}

-- | A user-supplied identifier that uniquely identifies the @DataSource@ . 
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsDataSourceId :: Lens.Lens' CreateDataSourceFromS3 Types.DataSourceId
cdsfsDataSourceId = Lens.field @"dataSourceId"
{-# INLINEABLE cdsfsDataSourceId #-}
{-# DEPRECATED dataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead"  #-}

-- | The data specification of a @DataSource@ :
--
--
--     * DataLocationS3 - The Amazon S3 location of the observation data.
--
--
--     * DataSchemaLocationS3 - The Amazon S3 location of the @DataSchema@ .
--
--
--     * DataSchema - A JSON string representing the schema. This is not required if @DataSchemaUri@ is specified. 
--
--
--     * DataRearrangement - A JSON string that represents the splitting and rearrangement requirements for the @Datasource@ . 
-- Sample - @"{\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"@ 
--
--
--
-- /Note:/ Consider using 'dataSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsDataSpec :: Lens.Lens' CreateDataSourceFromS3 Types.S3DataSpec
cdsfsDataSpec = Lens.field @"dataSpec"
{-# INLINEABLE cdsfsDataSpec #-}
{-# DEPRECATED dataSpec "Use generic-lens or generic-optics with 'dataSpec' instead"  #-}

-- | The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training.
--
-- /Note:/ Consider using 'computeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsComputeStatistics :: Lens.Lens' CreateDataSourceFromS3 (Core.Maybe Core.Bool)
cdsfsComputeStatistics = Lens.field @"computeStatistics"
{-# INLINEABLE cdsfsComputeStatistics #-}
{-# DEPRECATED computeStatistics "Use generic-lens or generic-optics with 'computeStatistics' instead"  #-}

-- | A user-supplied name or description of the @DataSource@ . 
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsDataSourceName :: Lens.Lens' CreateDataSourceFromS3 (Core.Maybe Types.EntityName)
cdsfsDataSourceName = Lens.field @"dataSourceName"
{-# INLINEABLE cdsfsDataSourceName #-}
{-# DEPRECATED dataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead"  #-}

instance Core.ToQuery CreateDataSourceFromS3 where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDataSourceFromS3 where
        toHeaders CreateDataSourceFromS3{..}
          = Core.pure
              ("X-Amz-Target", "AmazonML_20141212.CreateDataSourceFromS3")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDataSourceFromS3 where
        toJSON CreateDataSourceFromS3{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DataSourceId" Core..= dataSourceId),
                  Core.Just ("DataSpec" Core..= dataSpec),
                  ("ComputeStatistics" Core..=) Core.<$> computeStatistics,
                  ("DataSourceName" Core..=) Core.<$> dataSourceName])

instance Core.AWSRequest CreateDataSourceFromS3 where
        type Rs CreateDataSourceFromS3 = CreateDataSourceFromS3Response
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDataSourceFromS3Response' Core.<$>
                   (x Core..:? "DataSourceId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @CreateDataSourceFromS3@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- The @CreateDataSourceFromS3@ operation is asynchronous. You can poll for updates by using the @GetBatchPrediction@ operation and checking the @Status@ parameter. 
--
-- /See:/ 'mkCreateDataSourceFromS3Response' smart constructor.
data CreateDataSourceFromS3Response = CreateDataSourceFromS3Response'
  { dataSourceId :: Core.Maybe Types.DataSourceId
    -- ^ A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataSourceFromS3Response' value with any optional fields omitted.
mkCreateDataSourceFromS3Response
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDataSourceFromS3Response
mkCreateDataSourceFromS3Response responseStatus
  = CreateDataSourceFromS3Response'{dataSourceId = Core.Nothing,
                                    responseStatus}

-- | A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request. 
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsrrsDataSourceId :: Lens.Lens' CreateDataSourceFromS3Response (Core.Maybe Types.DataSourceId)
cdsfsrrsDataSourceId = Lens.field @"dataSourceId"
{-# INLINEABLE cdsfsrrsDataSourceId #-}
{-# DEPRECATED dataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsrrsResponseStatus :: Lens.Lens' CreateDataSourceFromS3Response Core.Int
cdsfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdsfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
