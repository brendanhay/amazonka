{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDataSourceFromS3 (..),
    mkCreateDataSourceFromS3,

    -- ** Request lenses
    cdsfsDataSourceName,
    cdsfsDataSourceId,
    cdsfsDataSpec,
    cdsfsComputeStatistics,

    -- * Destructuring the response
    CreateDataSourceFromS3Response (..),
    mkCreateDataSourceFromS3Response,

    -- ** Response lenses
    cdsfsrsDataSourceId,
    cdsfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDataSourceFromS3' smart constructor.
data CreateDataSourceFromS3 = CreateDataSourceFromS3'
  { -- | A user-supplied name or description of the @DataSource@ .
    dataSourceName :: Lude.Maybe Lude.Text,
    -- | A user-supplied identifier that uniquely identifies the @DataSource@ .
    dataSourceId :: Lude.Text,
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
    dataSpec :: S3DataSpec,
    -- | The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training.
    computeStatistics :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataSourceFromS3' with the minimum fields required to make a request.
--
-- * 'dataSourceName' - A user-supplied name or description of the @DataSource@ .
-- * 'dataSourceId' - A user-supplied identifier that uniquely identifies the @DataSource@ .
-- * 'dataSpec' - The data specification of a @DataSource@ :
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
-- * 'computeStatistics' - The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training.
mkCreateDataSourceFromS3 ::
  -- | 'dataSourceId'
  Lude.Text ->
  -- | 'dataSpec'
  S3DataSpec ->
  CreateDataSourceFromS3
mkCreateDataSourceFromS3 pDataSourceId_ pDataSpec_ =
  CreateDataSourceFromS3'
    { dataSourceName = Lude.Nothing,
      dataSourceId = pDataSourceId_,
      dataSpec = pDataSpec_,
      computeStatistics = Lude.Nothing
    }

-- | A user-supplied name or description of the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsDataSourceName :: Lens.Lens' CreateDataSourceFromS3 (Lude.Maybe Lude.Text)
cdsfsDataSourceName = Lens.lens (dataSourceName :: CreateDataSourceFromS3 -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceName = a} :: CreateDataSourceFromS3)
{-# DEPRECATED cdsfsDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | A user-supplied identifier that uniquely identifies the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsDataSourceId :: Lens.Lens' CreateDataSourceFromS3 Lude.Text
cdsfsDataSourceId = Lens.lens (dataSourceId :: CreateDataSourceFromS3 -> Lude.Text) (\s a -> s {dataSourceId = a} :: CreateDataSourceFromS3)
{-# DEPRECATED cdsfsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

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
cdsfsDataSpec :: Lens.Lens' CreateDataSourceFromS3 S3DataSpec
cdsfsDataSpec = Lens.lens (dataSpec :: CreateDataSourceFromS3 -> S3DataSpec) (\s a -> s {dataSpec = a} :: CreateDataSourceFromS3)
{-# DEPRECATED cdsfsDataSpec "Use generic-lens or generic-optics with 'dataSpec' instead." #-}

-- | The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the DataSourceneeds to be used for @MLModel@ training.
--
-- /Note:/ Consider using 'computeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsComputeStatistics :: Lens.Lens' CreateDataSourceFromS3 (Lude.Maybe Lude.Bool)
cdsfsComputeStatistics = Lens.lens (computeStatistics :: CreateDataSourceFromS3 -> Lude.Maybe Lude.Bool) (\s a -> s {computeStatistics = a} :: CreateDataSourceFromS3)
{-# DEPRECATED cdsfsComputeStatistics "Use generic-lens or generic-optics with 'computeStatistics' instead." #-}

instance Lude.AWSRequest CreateDataSourceFromS3 where
  type Rs CreateDataSourceFromS3 = CreateDataSourceFromS3Response
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDataSourceFromS3Response'
            Lude.<$> (x Lude..?> "DataSourceId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDataSourceFromS3 where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.CreateDataSourceFromS3" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDataSourceFromS3 where
  toJSON CreateDataSourceFromS3' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DataSourceName" Lude..=) Lude.<$> dataSourceName,
            Lude.Just ("DataSourceId" Lude..= dataSourceId),
            Lude.Just ("DataSpec" Lude..= dataSpec),
            ("ComputeStatistics" Lude..=) Lude.<$> computeStatistics
          ]
      )

instance Lude.ToPath CreateDataSourceFromS3 where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDataSourceFromS3 where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateDataSourceFromS3@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- The @CreateDataSourceFromS3@ operation is asynchronous. You can poll for updates by using the @GetBatchPrediction@ operation and checking the @Status@ parameter.
--
-- /See:/ 'mkCreateDataSourceFromS3Response' smart constructor.
data CreateDataSourceFromS3Response = CreateDataSourceFromS3Response'
  { -- | A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request.
    dataSourceId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataSourceFromS3Response' with the minimum fields required to make a request.
--
-- * 'dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request.
-- * 'responseStatus' - The response status code.
mkCreateDataSourceFromS3Response ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDataSourceFromS3Response
mkCreateDataSourceFromS3Response pResponseStatus_ =
  CreateDataSourceFromS3Response'
    { dataSourceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsrsDataSourceId :: Lens.Lens' CreateDataSourceFromS3Response (Lude.Maybe Lude.Text)
cdsfsrsDataSourceId = Lens.lens (dataSourceId :: CreateDataSourceFromS3Response -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceId = a} :: CreateDataSourceFromS3Response)
{-# DEPRECATED cdsfsrsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfsrsResponseStatus :: Lens.Lens' CreateDataSourceFromS3Response Lude.Int
cdsfsrsResponseStatus = Lens.lens (responseStatus :: CreateDataSourceFromS3Response -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDataSourceFromS3Response)
{-# DEPRECATED cdsfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
