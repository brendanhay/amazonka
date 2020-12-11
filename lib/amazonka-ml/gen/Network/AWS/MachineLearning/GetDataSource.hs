{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.GetDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @DataSource@ that includes metadata and data file information, as well as the current status of the @DataSource@ .
--
-- @GetDataSource@ provides results in normal or verbose format. The verbose format adds the schema description and the list of files pointed to by the DataSource to the normal format.
module Network.AWS.MachineLearning.GetDataSource
  ( -- * Creating a request
    GetDataSource (..),
    mkGetDataSource,

    -- ** Request lenses
    gdsVerbose,
    gdsDataSourceId,

    -- * Destructuring the response
    GetDataSourceResponse (..),
    mkGetDataSourceResponse,

    -- ** Response lenses
    gdsrsStatus,
    gdsrsNumberOfFiles,
    gdsrsLastUpdatedAt,
    gdsrsCreatedAt,
    gdsrsComputeTime,
    gdsrsDataSourceId,
    gdsrsRDSMetadata,
    gdsrsDataSizeInBytes,
    gdsrsDataSourceSchema,
    gdsrsStartedAt,
    gdsrsFinishedAt,
    gdsrsCreatedByIAMUser,
    gdsrsName,
    gdsrsLogURI,
    gdsrsDataLocationS3,
    gdsrsComputeStatistics,
    gdsrsMessage,
    gdsrsRedshiftMetadata,
    gdsrsDataRearrangement,
    gdsrsRoleARN,
    gdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDataSource' smart constructor.
data GetDataSource = GetDataSource'
  { verbose ::
      Lude.Maybe Lude.Bool,
    dataSourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataSource' with the minimum fields required to make a request.
--
-- * 'dataSourceId' - The ID assigned to the @DataSource@ at creation.
-- * 'verbose' - Specifies whether the @GetDataSource@ operation should return @DataSourceSchema@ .
--
-- If true, @DataSourceSchema@ is returned.
-- If false, @DataSourceSchema@ is not returned.
mkGetDataSource ::
  -- | 'dataSourceId'
  Lude.Text ->
  GetDataSource
mkGetDataSource pDataSourceId_ =
  GetDataSource'
    { verbose = Lude.Nothing,
      dataSourceId = pDataSourceId_
    }

-- | Specifies whether the @GetDataSource@ operation should return @DataSourceSchema@ .
--
-- If true, @DataSourceSchema@ is returned.
-- If false, @DataSourceSchema@ is not returned.
--
-- /Note:/ Consider using 'verbose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsVerbose :: Lens.Lens' GetDataSource (Lude.Maybe Lude.Bool)
gdsVerbose = Lens.lens (verbose :: GetDataSource -> Lude.Maybe Lude.Bool) (\s a -> s {verbose = a} :: GetDataSource)
{-# DEPRECATED gdsVerbose "Use generic-lens or generic-optics with 'verbose' instead." #-}

-- | The ID assigned to the @DataSource@ at creation.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDataSourceId :: Lens.Lens' GetDataSource Lude.Text
gdsDataSourceId = Lens.lens (dataSourceId :: GetDataSource -> Lude.Text) (\s a -> s {dataSourceId = a} :: GetDataSource)
{-# DEPRECATED gdsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

instance Lude.AWSRequest GetDataSource where
  type Rs GetDataSource = GetDataSourceResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDataSourceResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "NumberOfFiles")
            Lude.<*> (x Lude..?> "LastUpdatedAt")
            Lude.<*> (x Lude..?> "CreatedAt")
            Lude.<*> (x Lude..?> "ComputeTime")
            Lude.<*> (x Lude..?> "DataSourceId")
            Lude.<*> (x Lude..?> "RDSMetadata")
            Lude.<*> (x Lude..?> "DataSizeInBytes")
            Lude.<*> (x Lude..?> "DataSourceSchema")
            Lude.<*> (x Lude..?> "StartedAt")
            Lude.<*> (x Lude..?> "FinishedAt")
            Lude.<*> (x Lude..?> "CreatedByIamUser")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "LogUri")
            Lude.<*> (x Lude..?> "DataLocationS3")
            Lude.<*> (x Lude..?> "ComputeStatistics")
            Lude.<*> (x Lude..?> "Message")
            Lude.<*> (x Lude..?> "RedshiftMetadata")
            Lude.<*> (x Lude..?> "DataRearrangement")
            Lude.<*> (x Lude..?> "RoleARN")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDataSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.GetDataSource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDataSource where
  toJSON GetDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Verbose" Lude..=) Lude.<$> verbose,
            Lude.Just ("DataSourceId" Lude..= dataSourceId)
          ]
      )

instance Lude.ToPath GetDataSource where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDataSource where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetDataSource@ operation and describes a @DataSource@ .
--
-- /See:/ 'mkGetDataSourceResponse' smart constructor.
data GetDataSourceResponse = GetDataSourceResponse'
  { status ::
      Lude.Maybe EntityStatus,
    numberOfFiles :: Lude.Maybe Lude.Integer,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    createdAt :: Lude.Maybe Lude.Timestamp,
    computeTime :: Lude.Maybe Lude.Integer,
    dataSourceId :: Lude.Maybe Lude.Text,
    rdsMetadata :: Lude.Maybe RDSMetadata,
    dataSizeInBytes :: Lude.Maybe Lude.Integer,
    dataSourceSchema :: Lude.Maybe Lude.Text,
    startedAt :: Lude.Maybe Lude.Timestamp,
    finishedAt :: Lude.Maybe Lude.Timestamp,
    createdByIAMUser :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    logURI :: Lude.Maybe Lude.Text,
    dataLocationS3 :: Lude.Maybe Lude.Text,
    computeStatistics :: Lude.Maybe Lude.Bool,
    message :: Lude.Maybe Lude.Text,
    redshiftMetadata :: Lude.Maybe RedshiftMetadata,
    dataRearrangement :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetDataSourceResponse' with the minimum fields required to make a request.
--
-- * 'computeStatistics' - The parameter is @true@ if statistics need to be generated from the observation data.
-- * 'computeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @DataSource@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @DataSource@ is in the @COMPLETED@ state and the @ComputeStatistics@ is set to true.
-- * 'createdAt' - The time that the @DataSource@ was created. The time is expressed in epoch time.
-- * 'createdByIAMUser' - The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
-- * 'dataLocationS3' - The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
-- * 'dataRearrangement' - A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
-- * 'dataSizeInBytes' - The total size of observations in the data files.
-- * 'dataSourceId' - The ID assigned to the @DataSource@ at creation. This value should be identical to the value of the @DataSourceId@ in the request.
-- * 'dataSourceSchema' - The schema used by all of the data files of this @DataSource@ .
-- * 'finishedAt' - The epoch time when Amazon Machine Learning marked the @DataSource@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @DataSource@ is in the @COMPLETED@ or @FAILED@ state.
-- * 'lastUpdatedAt' - The time of the most recent edit to the @DataSource@ . The time is expressed in epoch time.
-- * 'logURI' - A link to the file containing logs of @CreateDataSourceFrom*@ operations.
-- * 'message' - The user-supplied description of the most recent details about creating the @DataSource@ .
-- * 'name' - A user-supplied name or description of the @DataSource@ .
-- * 'numberOfFiles' - The number of data files referenced by the @DataSource@ .
-- * 'rdsMetadata' - Undocumented field.
-- * 'redshiftMetadata' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - Undocumented field.
-- * 'startedAt' - The epoch time when Amazon Machine Learning marked the @DataSource@ as @INPROGRESS@ . @StartedAt@ isn't available if the @DataSource@ is in the @PENDING@ state.
-- * 'status' - The current status of the @DataSource@ . This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon ML submitted a request to create a @DataSource@ .
--
--     * @INPROGRESS@ - The creation process is underway.
--
--     * @FAILED@ - The request to create a @DataSource@ did not run to completion. It is not usable.
--
--     * @COMPLETED@ - The creation process completed successfully.
--
--     * @DELETED@ - The @DataSource@ is marked as deleted. It is not usable.
mkGetDataSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDataSourceResponse
mkGetDataSourceResponse pResponseStatus_ =
  GetDataSourceResponse'
    { status = Lude.Nothing,
      numberOfFiles = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      createdAt = Lude.Nothing,
      computeTime = Lude.Nothing,
      dataSourceId = Lude.Nothing,
      rdsMetadata = Lude.Nothing,
      dataSizeInBytes = Lude.Nothing,
      dataSourceSchema = Lude.Nothing,
      startedAt = Lude.Nothing,
      finishedAt = Lude.Nothing,
      createdByIAMUser = Lude.Nothing,
      name = Lude.Nothing,
      logURI = Lude.Nothing,
      dataLocationS3 = Lude.Nothing,
      computeStatistics = Lude.Nothing,
      message = Lude.Nothing,
      redshiftMetadata = Lude.Nothing,
      dataRearrangement = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current status of the @DataSource@ . This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon ML submitted a request to create a @DataSource@ .
--
--     * @INPROGRESS@ - The creation process is underway.
--
--     * @FAILED@ - The request to create a @DataSource@ did not run to completion. It is not usable.
--
--     * @COMPLETED@ - The creation process completed successfully.
--
--     * @DELETED@ - The @DataSource@ is marked as deleted. It is not usable.
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsStatus :: Lens.Lens' GetDataSourceResponse (Lude.Maybe EntityStatus)
gdsrsStatus = Lens.lens (status :: GetDataSourceResponse -> Lude.Maybe EntityStatus) (\s a -> s {status = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The number of data files referenced by the @DataSource@ .
--
-- /Note:/ Consider using 'numberOfFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsNumberOfFiles :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Integer)
gdsrsNumberOfFiles = Lens.lens (numberOfFiles :: GetDataSourceResponse -> Lude.Maybe Lude.Integer) (\s a -> s {numberOfFiles = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsNumberOfFiles "Use generic-lens or generic-optics with 'numberOfFiles' instead." #-}

-- | The time of the most recent edit to the @DataSource@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsLastUpdatedAt :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Timestamp)
gdsrsLastUpdatedAt = Lens.lens (lastUpdatedAt :: GetDataSourceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The time that the @DataSource@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsCreatedAt :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Timestamp)
gdsrsCreatedAt = Lens.lens (createdAt :: GetDataSourceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @DataSource@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @DataSource@ is in the @COMPLETED@ state and the @ComputeStatistics@ is set to true.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsComputeTime :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Integer)
gdsrsComputeTime = Lens.lens (computeTime :: GetDataSourceResponse -> Lude.Maybe Lude.Integer) (\s a -> s {computeTime = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The ID assigned to the @DataSource@ at creation. This value should be identical to the value of the @DataSourceId@ in the request.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDataSourceId :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Text)
gdsrsDataSourceId = Lens.lens (dataSourceId :: GetDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceId = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rdsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsRDSMetadata :: Lens.Lens' GetDataSourceResponse (Lude.Maybe RDSMetadata)
gdsrsRDSMetadata = Lens.lens (rdsMetadata :: GetDataSourceResponse -> Lude.Maybe RDSMetadata) (\s a -> s {rdsMetadata = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsRDSMetadata "Use generic-lens or generic-optics with 'rdsMetadata' instead." #-}

-- | The total size of observations in the data files.
--
-- /Note:/ Consider using 'dataSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDataSizeInBytes :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Integer)
gdsrsDataSizeInBytes = Lens.lens (dataSizeInBytes :: GetDataSourceResponse -> Lude.Maybe Lude.Integer) (\s a -> s {dataSizeInBytes = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsDataSizeInBytes "Use generic-lens or generic-optics with 'dataSizeInBytes' instead." #-}

-- | The schema used by all of the data files of this @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDataSourceSchema :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Text)
gdsrsDataSourceSchema = Lens.lens (dataSourceSchema :: GetDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceSchema = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsDataSourceSchema "Use generic-lens or generic-optics with 'dataSourceSchema' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @DataSource@ as @INPROGRESS@ . @StartedAt@ isn't available if the @DataSource@ is in the @PENDING@ state.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsStartedAt :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Timestamp)
gdsrsStartedAt = Lens.lens (startedAt :: GetDataSourceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @DataSource@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @DataSource@ is in the @COMPLETED@ or @FAILED@ state.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsFinishedAt :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Timestamp)
gdsrsFinishedAt = Lens.lens (finishedAt :: GetDataSourceResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {finishedAt = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIAMUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsCreatedByIAMUser :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Text)
gdsrsCreatedByIAMUser = Lens.lens (createdByIAMUser :: GetDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdByIAMUser = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsCreatedByIAMUser "Use generic-lens or generic-optics with 'createdByIAMUser' instead." #-}

-- | A user-supplied name or description of the @DataSource@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsName :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Text)
gdsrsName = Lens.lens (name :: GetDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A link to the file containing logs of @CreateDataSourceFrom*@ operations.
--
-- /Note:/ Consider using 'logURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsLogURI :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Text)
gdsrsLogURI = Lens.lens (logURI :: GetDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {logURI = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsLogURI "Use generic-lens or generic-optics with 'logURI' instead." #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'dataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDataLocationS3 :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Text)
gdsrsDataLocationS3 = Lens.lens (dataLocationS3 :: GetDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataLocationS3 = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsDataLocationS3 "Use generic-lens or generic-optics with 'dataLocationS3' instead." #-}

-- | The parameter is @true@ if statistics need to be generated from the observation data.
--
-- /Note:/ Consider using 'computeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsComputeStatistics :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Bool)
gdsrsComputeStatistics = Lens.lens (computeStatistics :: GetDataSourceResponse -> Lude.Maybe Lude.Bool) (\s a -> s {computeStatistics = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsComputeStatistics "Use generic-lens or generic-optics with 'computeStatistics' instead." #-}

-- | The user-supplied description of the most recent details about creating the @DataSource@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsMessage :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Text)
gdsrsMessage = Lens.lens (message :: GetDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsRedshiftMetadata :: Lens.Lens' GetDataSourceResponse (Lude.Maybe RedshiftMetadata)
gdsrsRedshiftMetadata = Lens.lens (redshiftMetadata :: GetDataSourceResponse -> Lude.Maybe RedshiftMetadata) (\s a -> s {redshiftMetadata = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsRedshiftMetadata "Use generic-lens or generic-optics with 'redshiftMetadata' instead." #-}

-- | A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
--
-- /Note:/ Consider using 'dataRearrangement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsDataRearrangement :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Text)
gdsrsDataRearrangement = Lens.lens (dataRearrangement :: GetDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataRearrangement = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsDataRearrangement "Use generic-lens or generic-optics with 'dataRearrangement' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsRoleARN :: Lens.Lens' GetDataSourceResponse (Lude.Maybe Lude.Text)
gdsrsRoleARN = Lens.lens (roleARN :: GetDataSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsResponseStatus :: Lens.Lens' GetDataSourceResponse Lude.Int
gdsrsResponseStatus = Lens.lens (responseStatus :: GetDataSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDataSourceResponse)
{-# DEPRECATED gdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
