-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.DataSource
  ( DataSource (..),

    -- * Smart constructor
    mkDataSource,

    -- * Lenses
    dsStatus,
    dsNumberOfFiles,
    dsLastUpdatedAt,
    dsCreatedAt,
    dsComputeTime,
    dsDataSourceId,
    dsRDSMetadata,
    dsDataSizeInBytes,
    dsStartedAt,
    dsFinishedAt,
    dsCreatedByIAMUser,
    dsName,
    dsDataLocationS3,
    dsComputeStatistics,
    dsMessage,
    dsRedshiftMetadata,
    dsDataRearrangement,
    dsRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.RDSMetadata
import Network.AWS.MachineLearning.Types.RedshiftMetadata
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of the @GetDataSource@ operation.
--
-- The content consists of the detailed metadata and data file information and the current status of the @DataSource@ .
--
-- /See:/ 'mkDataSource' smart constructor.
data DataSource = DataSource'
  { status :: Lude.Maybe EntityStatus,
    numberOfFiles :: Lude.Maybe Lude.Integer,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    createdAt :: Lude.Maybe Lude.Timestamp,
    computeTime :: Lude.Maybe Lude.Integer,
    dataSourceId :: Lude.Maybe Lude.Text,
    rdsMetadata :: Lude.Maybe RDSMetadata,
    dataSizeInBytes :: Lude.Maybe Lude.Integer,
    startedAt :: Lude.Maybe Lude.Timestamp,
    finishedAt :: Lude.Maybe Lude.Timestamp,
    createdByIAMUser :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    dataLocationS3 :: Lude.Maybe Lude.Text,
    computeStatistics :: Lude.Maybe Lude.Bool,
    message :: Lude.Maybe Lude.Text,
    redshiftMetadata :: Lude.Maybe RedshiftMetadata,
    dataRearrangement :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- * 'computeStatistics' - The parameter is @true@ if statistics need to be generated from the observation data.
-- * 'computeTime' - Undocumented field.
-- * 'createdAt' - The time that the @DataSource@ was created. The time is expressed in epoch time.
-- * 'createdByIAMUser' - The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
-- * 'dataLocationS3' - The location and name of the data in Amazon Simple Storage Service (Amazon S3) that is used by a @DataSource@ .
-- * 'dataRearrangement' - A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
-- * 'dataSizeInBytes' - The total number of observations contained in the data files that the @DataSource@ references.
-- * 'dataSourceId' - The ID that is assigned to the @DataSource@ during creation.
-- * 'finishedAt' - Undocumented field.
-- * 'lastUpdatedAt' - The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
-- * 'message' - A description of the most recent details about creating the @DataSource@ .
-- * 'name' - A user-supplied name or description of the @DataSource@ .
-- * 'numberOfFiles' - The number of data files referenced by the @DataSource@ .
-- * 'rdsMetadata' - Undocumented field.
-- * 'redshiftMetadata' - Undocumented field.
-- * 'roleARN' - Undocumented field.
-- * 'startedAt' - Undocumented field.
-- * 'status' - The current status of the @DataSource@ . This element can have one of the following values:
--
--
--     * PENDING - Amazon Machine Learning (Amazon ML) submitted a request to create a @DataSource@ .
--
--     * INPROGRESS - The creation process is underway.
--
--     * FAILED - The request to create a @DataSource@ did not run to completion. It is not usable.
--
--     * COMPLETED - The creation process completed successfully.
--
--     * DELETED - The @DataSource@ is marked as deleted. It is not usable.
mkDataSource ::
  DataSource
mkDataSource =
  DataSource'
    { status = Lude.Nothing,
      numberOfFiles = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      createdAt = Lude.Nothing,
      computeTime = Lude.Nothing,
      dataSourceId = Lude.Nothing,
      rdsMetadata = Lude.Nothing,
      dataSizeInBytes = Lude.Nothing,
      startedAt = Lude.Nothing,
      finishedAt = Lude.Nothing,
      createdByIAMUser = Lude.Nothing,
      name = Lude.Nothing,
      dataLocationS3 = Lude.Nothing,
      computeStatistics = Lude.Nothing,
      message = Lude.Nothing,
      redshiftMetadata = Lude.Nothing,
      dataRearrangement = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The current status of the @DataSource@ . This element can have one of the following values:
--
--
--     * PENDING - Amazon Machine Learning (Amazon ML) submitted a request to create a @DataSource@ .
--
--     * INPROGRESS - The creation process is underway.
--
--     * FAILED - The request to create a @DataSource@ did not run to completion. It is not usable.
--
--     * COMPLETED - The creation process completed successfully.
--
--     * DELETED - The @DataSource@ is marked as deleted. It is not usable.
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStatus :: Lens.Lens' DataSource (Lude.Maybe EntityStatus)
dsStatus = Lens.lens (status :: DataSource -> Lude.Maybe EntityStatus) (\s a -> s {status = a} :: DataSource)
{-# DEPRECATED dsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The number of data files referenced by the @DataSource@ .
--
-- /Note:/ Consider using 'numberOfFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNumberOfFiles :: Lens.Lens' DataSource (Lude.Maybe Lude.Integer)
dsNumberOfFiles = Lens.lens (numberOfFiles :: DataSource -> Lude.Maybe Lude.Integer) (\s a -> s {numberOfFiles = a} :: DataSource)
{-# DEPRECATED dsNumberOfFiles "Use generic-lens or generic-optics with 'numberOfFiles' instead." #-}

-- | The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLastUpdatedAt :: Lens.Lens' DataSource (Lude.Maybe Lude.Timestamp)
dsLastUpdatedAt = Lens.lens (lastUpdatedAt :: DataSource -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: DataSource)
{-# DEPRECATED dsLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The time that the @DataSource@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreatedAt :: Lens.Lens' DataSource (Lude.Maybe Lude.Timestamp)
dsCreatedAt = Lens.lens (createdAt :: DataSource -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: DataSource)
{-# DEPRECATED dsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsComputeTime :: Lens.Lens' DataSource (Lude.Maybe Lude.Integer)
dsComputeTime = Lens.lens (computeTime :: DataSource -> Lude.Maybe Lude.Integer) (\s a -> s {computeTime = a} :: DataSource)
{-# DEPRECATED dsComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The ID that is assigned to the @DataSource@ during creation.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataSourceId :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsDataSourceId = Lens.lens (dataSourceId :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceId = a} :: DataSource)
{-# DEPRECATED dsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rdsMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRDSMetadata :: Lens.Lens' DataSource (Lude.Maybe RDSMetadata)
dsRDSMetadata = Lens.lens (rdsMetadata :: DataSource -> Lude.Maybe RDSMetadata) (\s a -> s {rdsMetadata = a} :: DataSource)
{-# DEPRECATED dsRDSMetadata "Use generic-lens or generic-optics with 'rdsMetadata' instead." #-}

-- | The total number of observations contained in the data files that the @DataSource@ references.
--
-- /Note:/ Consider using 'dataSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataSizeInBytes :: Lens.Lens' DataSource (Lude.Maybe Lude.Integer)
dsDataSizeInBytes = Lens.lens (dataSizeInBytes :: DataSource -> Lude.Maybe Lude.Integer) (\s a -> s {dataSizeInBytes = a} :: DataSource)
{-# DEPRECATED dsDataSizeInBytes "Use generic-lens or generic-optics with 'dataSizeInBytes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStartedAt :: Lens.Lens' DataSource (Lude.Maybe Lude.Timestamp)
dsStartedAt = Lens.lens (startedAt :: DataSource -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: DataSource)
{-# DEPRECATED dsStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFinishedAt :: Lens.Lens' DataSource (Lude.Maybe Lude.Timestamp)
dsFinishedAt = Lens.lens (finishedAt :: DataSource -> Lude.Maybe Lude.Timestamp) (\s a -> s {finishedAt = a} :: DataSource)
{-# DEPRECATED dsFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIAMUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreatedByIAMUser :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsCreatedByIAMUser = Lens.lens (createdByIAMUser :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {createdByIAMUser = a} :: DataSource)
{-# DEPRECATED dsCreatedByIAMUser "Use generic-lens or generic-optics with 'createdByIAMUser' instead." #-}

-- | A user-supplied name or description of the @DataSource@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsName :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsName = Lens.lens (name :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DataSource)
{-# DEPRECATED dsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The location and name of the data in Amazon Simple Storage Service (Amazon S3) that is used by a @DataSource@ .
--
-- /Note:/ Consider using 'dataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataLocationS3 :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsDataLocationS3 = Lens.lens (dataLocationS3 :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {dataLocationS3 = a} :: DataSource)
{-# DEPRECATED dsDataLocationS3 "Use generic-lens or generic-optics with 'dataLocationS3' instead." #-}

-- | The parameter is @true@ if statistics need to be generated from the observation data.
--
-- /Note:/ Consider using 'computeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsComputeStatistics :: Lens.Lens' DataSource (Lude.Maybe Lude.Bool)
dsComputeStatistics = Lens.lens (computeStatistics :: DataSource -> Lude.Maybe Lude.Bool) (\s a -> s {computeStatistics = a} :: DataSource)
{-# DEPRECATED dsComputeStatistics "Use generic-lens or generic-optics with 'computeStatistics' instead." #-}

-- | A description of the most recent details about creating the @DataSource@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMessage :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsMessage = Lens.lens (message :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DataSource)
{-# DEPRECATED dsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRedshiftMetadata :: Lens.Lens' DataSource (Lude.Maybe RedshiftMetadata)
dsRedshiftMetadata = Lens.lens (redshiftMetadata :: DataSource -> Lude.Maybe RedshiftMetadata) (\s a -> s {redshiftMetadata = a} :: DataSource)
{-# DEPRECATED dsRedshiftMetadata "Use generic-lens or generic-optics with 'redshiftMetadata' instead." #-}

-- | A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
--
-- /Note:/ Consider using 'dataRearrangement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataRearrangement :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsDataRearrangement = Lens.lens (dataRearrangement :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {dataRearrangement = a} :: DataSource)
{-# DEPRECATED dsDataRearrangement "Use generic-lens or generic-optics with 'dataRearrangement' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRoleARN :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsRoleARN = Lens.lens (roleARN :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DataSource)
{-# DEPRECATED dsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON DataSource where
  parseJSON =
    Lude.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "NumberOfFiles")
            Lude.<*> (x Lude..:? "LastUpdatedAt")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "ComputeTime")
            Lude.<*> (x Lude..:? "DataSourceId")
            Lude.<*> (x Lude..:? "RDSMetadata")
            Lude.<*> (x Lude..:? "DataSizeInBytes")
            Lude.<*> (x Lude..:? "StartedAt")
            Lude.<*> (x Lude..:? "FinishedAt")
            Lude.<*> (x Lude..:? "CreatedByIamUser")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "DataLocationS3")
            Lude.<*> (x Lude..:? "ComputeStatistics")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "RedshiftMetadata")
            Lude.<*> (x Lude..:? "DataRearrangement")
            Lude.<*> (x Lude..:? "RoleARN")
      )
