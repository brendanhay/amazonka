{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateDataSourceFromRedshift
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataSource@ from a database hosted on an Amazon Redshift cluster. A @DataSource@ references data that can be used to perform either @CreateMLModel@ , @CreateEvaluation@ , or @CreateBatchPrediction@ operations.
--
-- @CreateDataSourceFromRedshift@ is an asynchronous operation. In response to @CreateDataSourceFromRedshift@ , Amazon Machine Learning (Amazon ML) immediately returns and sets the @DataSource@ status to @PENDING@ . After the @DataSource@ is created and ready for use, Amazon ML sets the @Status@ parameter to @COMPLETED@ . @DataSource@ in @COMPLETED@ or @PENDING@ states can be used to perform only @CreateMLModel@ , @CreateEvaluation@ , or @CreateBatchPrediction@ operations.
-- If Amazon ML can't accept the input source, it sets the @Status@ parameter to @FAILED@ and includes an error message in the @Message@ attribute of the @GetDataSource@ operation response.
-- The observations should be contained in the database hosted on an Amazon Redshift cluster and should be specified by a @SelectSqlQuery@ query. Amazon ML executes an @Unload@ command in Amazon Redshift to transfer the result set of the @SelectSqlQuery@ query to @S3StagingLocation@ .
-- After the @DataSource@ has been created, it's ready for use in evaluations and batch predictions. If you plan to use the @DataSource@ to train an @MLModel@ , the @DataSource@ also requires a recipe. A recipe describes how each input variable will be used in training an @MLModel@ . Will the variable be included or excluded from training? Will the variable be manipulated; for example, will it be combined with another variable or will it be split apart into word combinations? The recipe provides answers to these questions.
-- You can't change an existing datasource, but you can copy and modify the settings from an existing Amazon Redshift datasource to create a new datasource. To do so, call @GetDataSource@ for an existing datasource and copy the values to a @CreateDataSource@ call. Change the settings that you want to change and make sure that all required fields have the appropriate values.
module Network.AWS.MachineLearning.CreateDataSourceFromRedshift
  ( -- * Creating a request
    CreateDataSourceFromRedshift (..),
    mkCreateDataSourceFromRedshift,

    -- ** Request lenses
    cdsfrDataSourceName,
    cdsfrComputeStatistics,
    cdsfrDataSourceId,
    cdsfrDataSpec,
    cdsfrRoleARN,

    -- * Destructuring the response
    CreateDataSourceFromRedshiftResponse (..),
    mkCreateDataSourceFromRedshiftResponse,

    -- ** Response lenses
    cdsfrrsDataSourceId,
    cdsfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDataSourceFromRedshift' smart constructor.
data CreateDataSourceFromRedshift = CreateDataSourceFromRedshift'
  { dataSourceName ::
      Lude.Maybe Lude.Text,
    computeStatistics ::
      Lude.Maybe Lude.Bool,
    dataSourceId :: Lude.Text,
    dataSpec :: RedshiftDataSpec,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataSourceFromRedshift' with the minimum fields required to make a request.
--
-- * 'computeStatistics' - The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the @DataSource@ needs to be used for @MLModel@ training.
-- * 'dataSourceId' - A user-supplied ID that uniquely identifies the @DataSource@ .
-- * 'dataSourceName' - A user-supplied name or description of the @DataSource@ .
-- * 'dataSpec' - The data specification of an Amazon Redshift @DataSource@ :
--
--
--     * DatabaseInformation -
--     * @DatabaseName@ - The name of the Amazon Redshift database.
--
--     * @ClusterIdentifier@ - The unique ID for the Amazon Redshift cluster.
--
--
--
--
--     * DatabaseCredentials - The AWS Identity and Access Management (IAM) credentials that are used to connect to the Amazon Redshift database.
--
--
--     * SelectSqlQuery - The query that is used to retrieve the observation data for the @Datasource@ .
--
--
--     * S3StagingLocation - The Amazon Simple Storage Service (Amazon S3) location for staging Amazon Redshift data. The data retrieved from Amazon Redshift using the @SelectSqlQuery@ query is stored in this location.
--
--
--     * DataSchemaUri - The Amazon S3 location of the @DataSchema@ .
--
--
--     * DataSchema - A JSON string representing the schema. This is not required if @DataSchemaUri@ is specified.
--
--
--     * DataRearrangement - A JSON string that represents the splitting and rearrangement requirements for the @DataSource@ .
-- Sample - @"{\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"@
--
--
-- * 'roleARN' - A fully specified role Amazon Resource Name (ARN). Amazon ML assumes the role on behalf of the user to create the following:
--
--
--     * A security group to allow Amazon ML to execute the @SelectSqlQuery@ query on an Amazon Redshift cluster
--
--
--     * An Amazon S3 bucket policy to grant Amazon ML read/write permissions on the @S3StagingLocation@
mkCreateDataSourceFromRedshift ::
  -- | 'dataSourceId'
  Lude.Text ->
  -- | 'dataSpec'
  RedshiftDataSpec ->
  -- | 'roleARN'
  Lude.Text ->
  CreateDataSourceFromRedshift
mkCreateDataSourceFromRedshift pDataSourceId_ pDataSpec_ pRoleARN_ =
  CreateDataSourceFromRedshift'
    { dataSourceName = Lude.Nothing,
      computeStatistics = Lude.Nothing,
      dataSourceId = pDataSourceId_,
      dataSpec = pDataSpec_,
      roleARN = pRoleARN_
    }

-- | A user-supplied name or description of the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrDataSourceName :: Lens.Lens' CreateDataSourceFromRedshift (Lude.Maybe Lude.Text)
cdsfrDataSourceName = Lens.lens (dataSourceName :: CreateDataSourceFromRedshift -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceName = a} :: CreateDataSourceFromRedshift)
{-# DEPRECATED cdsfrDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The compute statistics for a @DataSource@ . The statistics are generated from the observation data referenced by a @DataSource@ . Amazon ML uses the statistics internally during @MLModel@ training. This parameter must be set to @true@ if the @DataSource@ needs to be used for @MLModel@ training.
--
-- /Note:/ Consider using 'computeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrComputeStatistics :: Lens.Lens' CreateDataSourceFromRedshift (Lude.Maybe Lude.Bool)
cdsfrComputeStatistics = Lens.lens (computeStatistics :: CreateDataSourceFromRedshift -> Lude.Maybe Lude.Bool) (\s a -> s {computeStatistics = a} :: CreateDataSourceFromRedshift)
{-# DEPRECATED cdsfrComputeStatistics "Use generic-lens or generic-optics with 'computeStatistics' instead." #-}

-- | A user-supplied ID that uniquely identifies the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrDataSourceId :: Lens.Lens' CreateDataSourceFromRedshift Lude.Text
cdsfrDataSourceId = Lens.lens (dataSourceId :: CreateDataSourceFromRedshift -> Lude.Text) (\s a -> s {dataSourceId = a} :: CreateDataSourceFromRedshift)
{-# DEPRECATED cdsfrDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

-- | The data specification of an Amazon Redshift @DataSource@ :
--
--
--     * DatabaseInformation -
--     * @DatabaseName@ - The name of the Amazon Redshift database.
--
--     * @ClusterIdentifier@ - The unique ID for the Amazon Redshift cluster.
--
--
--
--
--     * DatabaseCredentials - The AWS Identity and Access Management (IAM) credentials that are used to connect to the Amazon Redshift database.
--
--
--     * SelectSqlQuery - The query that is used to retrieve the observation data for the @Datasource@ .
--
--
--     * S3StagingLocation - The Amazon Simple Storage Service (Amazon S3) location for staging Amazon Redshift data. The data retrieved from Amazon Redshift using the @SelectSqlQuery@ query is stored in this location.
--
--
--     * DataSchemaUri - The Amazon S3 location of the @DataSchema@ .
--
--
--     * DataSchema - A JSON string representing the schema. This is not required if @DataSchemaUri@ is specified.
--
--
--     * DataRearrangement - A JSON string that represents the splitting and rearrangement requirements for the @DataSource@ .
-- Sample - @"{\"splitting\":{\"percentBegin\":10,\"percentEnd\":60}}"@
--
--
--
-- /Note:/ Consider using 'dataSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrDataSpec :: Lens.Lens' CreateDataSourceFromRedshift RedshiftDataSpec
cdsfrDataSpec = Lens.lens (dataSpec :: CreateDataSourceFromRedshift -> RedshiftDataSpec) (\s a -> s {dataSpec = a} :: CreateDataSourceFromRedshift)
{-# DEPRECATED cdsfrDataSpec "Use generic-lens or generic-optics with 'dataSpec' instead." #-}

-- | A fully specified role Amazon Resource Name (ARN). Amazon ML assumes the role on behalf of the user to create the following:
--
--
--     * A security group to allow Amazon ML to execute the @SelectSqlQuery@ query on an Amazon Redshift cluster
--
--
--     * An Amazon S3 bucket policy to grant Amazon ML read/write permissions on the @S3StagingLocation@
--
--
--
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrRoleARN :: Lens.Lens' CreateDataSourceFromRedshift Lude.Text
cdsfrRoleARN = Lens.lens (roleARN :: CreateDataSourceFromRedshift -> Lude.Text) (\s a -> s {roleARN = a} :: CreateDataSourceFromRedshift)
{-# DEPRECATED cdsfrRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateDataSourceFromRedshift where
  type
    Rs CreateDataSourceFromRedshift =
      CreateDataSourceFromRedshiftResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDataSourceFromRedshiftResponse'
            Lude.<$> (x Lude..?> "DataSourceId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDataSourceFromRedshift where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonML_20141212.CreateDataSourceFromRedshift" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDataSourceFromRedshift where
  toJSON CreateDataSourceFromRedshift' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DataSourceName" Lude..=) Lude.<$> dataSourceName,
            ("ComputeStatistics" Lude..=) Lude.<$> computeStatistics,
            Lude.Just ("DataSourceId" Lude..= dataSourceId),
            Lude.Just ("DataSpec" Lude..= dataSpec),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateDataSourceFromRedshift where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDataSourceFromRedshift where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateDataSourceFromRedshift@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- The @CreateDataSourceFromRedshift@ operation is asynchronous. You can poll for updates by using the @GetBatchPrediction@ operation and checking the @Status@ parameter.
--
-- /See:/ 'mkCreateDataSourceFromRedshiftResponse' smart constructor.
data CreateDataSourceFromRedshiftResponse = CreateDataSourceFromRedshiftResponse'
  { dataSourceId ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'CreateDataSourceFromRedshiftResponse' with the minimum fields required to make a request.
--
-- * 'dataSourceId' - A user-supplied ID that uniquely identifies the datasource. This value should be identical to the value of the @DataSourceID@ in the request.
-- * 'responseStatus' - The response status code.
mkCreateDataSourceFromRedshiftResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDataSourceFromRedshiftResponse
mkCreateDataSourceFromRedshiftResponse pResponseStatus_ =
  CreateDataSourceFromRedshiftResponse'
    { dataSourceId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the datasource. This value should be identical to the value of the @DataSourceID@ in the request.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrrsDataSourceId :: Lens.Lens' CreateDataSourceFromRedshiftResponse (Lude.Maybe Lude.Text)
cdsfrrsDataSourceId = Lens.lens (dataSourceId :: CreateDataSourceFromRedshiftResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceId = a} :: CreateDataSourceFromRedshiftResponse)
{-# DEPRECATED cdsfrrsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsfrrsResponseStatus :: Lens.Lens' CreateDataSourceFromRedshiftResponse Lude.Int
cdsfrrsResponseStatus = Lens.lens (responseStatus :: CreateDataSourceFromRedshiftResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDataSourceFromRedshiftResponse)
{-# DEPRECATED cdsfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
