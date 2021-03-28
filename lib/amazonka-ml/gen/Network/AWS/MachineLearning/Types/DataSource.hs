{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.DataSource
  ( DataSource (..)
  -- * Smart constructor
  , mkDataSource
  -- * Lenses
  , dsComputeStatistics
  , dsComputeTime
  , dsCreatedAt
  , dsCreatedByIamUser
  , dsDataLocationS3
  , dsDataRearrangement
  , dsDataSizeInBytes
  , dsDataSourceId
  , dsFinishedAt
  , dsLastUpdatedAt
  , dsMessage
  , dsName
  , dsNumberOfFiles
  , dsRDSMetadata
  , dsRedshiftMetadata
  , dsRoleARN
  , dsStartedAt
  , dsStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.AwsUserArn as Types
import qualified Network.AWS.MachineLearning.Types.DataLocationS3 as Types
import qualified Network.AWS.MachineLearning.Types.DataRearrangement as Types
import qualified Network.AWS.MachineLearning.Types.DataSourceId as Types
import qualified Network.AWS.MachineLearning.Types.EntityName as Types
import qualified Network.AWS.MachineLearning.Types.EntityStatus as Types
import qualified Network.AWS.MachineLearning.Types.Message as Types
import qualified Network.AWS.MachineLearning.Types.RDSMetadata as Types
import qualified Network.AWS.MachineLearning.Types.RedshiftMetadata as Types
import qualified Network.AWS.MachineLearning.Types.RoleARN as Types
import qualified Network.AWS.Prelude as Core

-- | Represents the output of the @GetDataSource@ operation. 
--
-- The content consists of the detailed metadata and data file information and the current status of the @DataSource@ . 
--
-- /See:/ 'mkDataSource' smart constructor.
data DataSource = DataSource'
  { computeStatistics :: Core.Maybe Core.Bool
    -- ^ The parameter is @true@ if statistics need to be generated from the observation data. 
  , computeTime :: Core.Maybe Core.Integer
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the @DataSource@ was created. The time is expressed in epoch time.
  , createdByIamUser :: Core.Maybe Types.AwsUserArn
    -- ^ The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
  , dataLocationS3 :: Core.Maybe Types.DataLocationS3
    -- ^ The location and name of the data in Amazon Simple Storage Service (Amazon S3) that is used by a @DataSource@ .
  , dataRearrangement :: Core.Maybe Types.DataRearrangement
    -- ^ A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
  , dataSizeInBytes :: Core.Maybe Core.Integer
    -- ^ The total number of observations contained in the data files that the @DataSource@ references.
  , dataSourceId :: Core.Maybe Types.DataSourceId
    -- ^ The ID that is assigned to the @DataSource@ during creation.
  , finishedAt :: Core.Maybe Core.NominalDiffTime
  , lastUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
  , message :: Core.Maybe Types.Message
    -- ^ A description of the most recent details about creating the @DataSource@ .
  , name :: Core.Maybe Types.EntityName
    -- ^ A user-supplied name or description of the @DataSource@ .
  , numberOfFiles :: Core.Maybe Core.Integer
    -- ^ The number of data files referenced by the @DataSource@ .
  , rDSMetadata :: Core.Maybe Types.RDSMetadata
  , redshiftMetadata :: Core.Maybe Types.RedshiftMetadata
  , roleARN :: Core.Maybe Types.RoleARN
  , startedAt :: Core.Maybe Core.NominalDiffTime
  , status :: Core.Maybe Types.EntityStatus
    -- ^ The current status of the @DataSource@ . This element can have one of the following values: 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DataSource' value with any optional fields omitted.
mkDataSource
    :: DataSource
mkDataSource
  = DataSource'{computeStatistics = Core.Nothing,
                computeTime = Core.Nothing, createdAt = Core.Nothing,
                createdByIamUser = Core.Nothing, dataLocationS3 = Core.Nothing,
                dataRearrangement = Core.Nothing, dataSizeInBytes = Core.Nothing,
                dataSourceId = Core.Nothing, finishedAt = Core.Nothing,
                lastUpdatedAt = Core.Nothing, message = Core.Nothing,
                name = Core.Nothing, numberOfFiles = Core.Nothing,
                rDSMetadata = Core.Nothing, redshiftMetadata = Core.Nothing,
                roleARN = Core.Nothing, startedAt = Core.Nothing,
                status = Core.Nothing}

-- | The parameter is @true@ if statistics need to be generated from the observation data. 
--
-- /Note:/ Consider using 'computeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsComputeStatistics :: Lens.Lens' DataSource (Core.Maybe Core.Bool)
dsComputeStatistics = Lens.field @"computeStatistics"
{-# INLINEABLE dsComputeStatistics #-}
{-# DEPRECATED computeStatistics "Use generic-lens or generic-optics with 'computeStatistics' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsComputeTime :: Lens.Lens' DataSource (Core.Maybe Core.Integer)
dsComputeTime = Lens.field @"computeTime"
{-# INLINEABLE dsComputeTime #-}
{-# DEPRECATED computeTime "Use generic-lens or generic-optics with 'computeTime' instead"  #-}

-- | The time that the @DataSource@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreatedAt :: Lens.Lens' DataSource (Core.Maybe Core.NominalDiffTime)
dsCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE dsCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIamUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreatedByIamUser :: Lens.Lens' DataSource (Core.Maybe Types.AwsUserArn)
dsCreatedByIamUser = Lens.field @"createdByIamUser"
{-# INLINEABLE dsCreatedByIamUser #-}
{-# DEPRECATED createdByIamUser "Use generic-lens or generic-optics with 'createdByIamUser' instead"  #-}

-- | The location and name of the data in Amazon Simple Storage Service (Amazon S3) that is used by a @DataSource@ .
--
-- /Note:/ Consider using 'dataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataLocationS3 :: Lens.Lens' DataSource (Core.Maybe Types.DataLocationS3)
dsDataLocationS3 = Lens.field @"dataLocationS3"
{-# INLINEABLE dsDataLocationS3 #-}
{-# DEPRECATED dataLocationS3 "Use generic-lens or generic-optics with 'dataLocationS3' instead"  #-}

-- | A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
--
-- /Note:/ Consider using 'dataRearrangement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataRearrangement :: Lens.Lens' DataSource (Core.Maybe Types.DataRearrangement)
dsDataRearrangement = Lens.field @"dataRearrangement"
{-# INLINEABLE dsDataRearrangement #-}
{-# DEPRECATED dataRearrangement "Use generic-lens or generic-optics with 'dataRearrangement' instead"  #-}

-- | The total number of observations contained in the data files that the @DataSource@ references.
--
-- /Note:/ Consider using 'dataSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataSizeInBytes :: Lens.Lens' DataSource (Core.Maybe Core.Integer)
dsDataSizeInBytes = Lens.field @"dataSizeInBytes"
{-# INLINEABLE dsDataSizeInBytes #-}
{-# DEPRECATED dataSizeInBytes "Use generic-lens or generic-optics with 'dataSizeInBytes' instead"  #-}

-- | The ID that is assigned to the @DataSource@ during creation.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDataSourceId :: Lens.Lens' DataSource (Core.Maybe Types.DataSourceId)
dsDataSourceId = Lens.field @"dataSourceId"
{-# INLINEABLE dsDataSourceId #-}
{-# DEPRECATED dataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFinishedAt :: Lens.Lens' DataSource (Core.Maybe Core.NominalDiffTime)
dsFinishedAt = Lens.field @"finishedAt"
{-# INLINEABLE dsFinishedAt #-}
{-# DEPRECATED finishedAt "Use generic-lens or generic-optics with 'finishedAt' instead"  #-}

-- | The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLastUpdatedAt :: Lens.Lens' DataSource (Core.Maybe Core.NominalDiffTime)
dsLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE dsLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | A description of the most recent details about creating the @DataSource@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMessage :: Lens.Lens' DataSource (Core.Maybe Types.Message)
dsMessage = Lens.field @"message"
{-# INLINEABLE dsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | A user-supplied name or description of the @DataSource@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsName :: Lens.Lens' DataSource (Core.Maybe Types.EntityName)
dsName = Lens.field @"name"
{-# INLINEABLE dsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of data files referenced by the @DataSource@ .
--
-- /Note:/ Consider using 'numberOfFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNumberOfFiles :: Lens.Lens' DataSource (Core.Maybe Core.Integer)
dsNumberOfFiles = Lens.field @"numberOfFiles"
{-# INLINEABLE dsNumberOfFiles #-}
{-# DEPRECATED numberOfFiles "Use generic-lens or generic-optics with 'numberOfFiles' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rDSMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRDSMetadata :: Lens.Lens' DataSource (Core.Maybe Types.RDSMetadata)
dsRDSMetadata = Lens.field @"rDSMetadata"
{-# INLINEABLE dsRDSMetadata #-}
{-# DEPRECATED rDSMetadata "Use generic-lens or generic-optics with 'rDSMetadata' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRedshiftMetadata :: Lens.Lens' DataSource (Core.Maybe Types.RedshiftMetadata)
dsRedshiftMetadata = Lens.field @"redshiftMetadata"
{-# INLINEABLE dsRedshiftMetadata #-}
{-# DEPRECATED redshiftMetadata "Use generic-lens or generic-optics with 'redshiftMetadata' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRoleARN :: Lens.Lens' DataSource (Core.Maybe Types.RoleARN)
dsRoleARN = Lens.field @"roleARN"
{-# INLINEABLE dsRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStartedAt :: Lens.Lens' DataSource (Core.Maybe Core.NominalDiffTime)
dsStartedAt = Lens.field @"startedAt"
{-# INLINEABLE dsStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

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
dsStatus :: Lens.Lens' DataSource (Core.Maybe Types.EntityStatus)
dsStatus = Lens.field @"status"
{-# INLINEABLE dsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON DataSource where
        parseJSON
          = Core.withObject "DataSource" Core.$
              \ x ->
                DataSource' Core.<$>
                  (x Core..:? "ComputeStatistics") Core.<*> x Core..:? "ComputeTime"
                    Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "CreatedByIamUser"
                    Core.<*> x Core..:? "DataLocationS3"
                    Core.<*> x Core..:? "DataRearrangement"
                    Core.<*> x Core..:? "DataSizeInBytes"
                    Core.<*> x Core..:? "DataSourceId"
                    Core.<*> x Core..:? "FinishedAt"
                    Core.<*> x Core..:? "LastUpdatedAt"
                    Core.<*> x Core..:? "Message"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "NumberOfFiles"
                    Core.<*> x Core..:? "RDSMetadata"
                    Core.<*> x Core..:? "RedshiftMetadata"
                    Core.<*> x Core..:? "RoleARN"
                    Core.<*> x Core..:? "StartedAt"
                    Core.<*> x Core..:? "Status"
