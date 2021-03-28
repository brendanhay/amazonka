{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetDataSource (..)
    , mkGetDataSource
    -- ** Request lenses
    , gdsDataSourceId
    , gdsVerbose

    -- * Destructuring the response
    , GetDataSourceResponse (..)
    , mkGetDataSourceResponse
    -- ** Response lenses
    , gdsrrsComputeStatistics
    , gdsrrsComputeTime
    , gdsrrsCreatedAt
    , gdsrrsCreatedByIamUser
    , gdsrrsDataLocationS3
    , gdsrrsDataRearrangement
    , gdsrrsDataSizeInBytes
    , gdsrrsDataSourceId
    , gdsrrsDataSourceSchema
    , gdsrrsFinishedAt
    , gdsrrsLastUpdatedAt
    , gdsrrsLogUri
    , gdsrrsMessage
    , gdsrrsName
    , gdsrrsNumberOfFiles
    , gdsrrsRDSMetadata
    , gdsrrsRedshiftMetadata
    , gdsrrsRoleARN
    , gdsrrsStartedAt
    , gdsrrsStatus
    , gdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDataSource' smart constructor.
data GetDataSource = GetDataSource'
  { dataSourceId :: Types.DataSourceId
    -- ^ The ID assigned to the @DataSource@ at creation.
  , verbose :: Core.Maybe Core.Bool
    -- ^ Specifies whether the @GetDataSource@ operation should return @DataSourceSchema@ .
--
-- If true, @DataSourceSchema@ is returned.
-- If false, @DataSourceSchema@ is not returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataSource' value with any optional fields omitted.
mkGetDataSource
    :: Types.DataSourceId -- ^ 'dataSourceId'
    -> GetDataSource
mkGetDataSource dataSourceId
  = GetDataSource'{dataSourceId, verbose = Core.Nothing}

-- | The ID assigned to the @DataSource@ at creation.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDataSourceId :: Lens.Lens' GetDataSource Types.DataSourceId
gdsDataSourceId = Lens.field @"dataSourceId"
{-# INLINEABLE gdsDataSourceId #-}
{-# DEPRECATED dataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead"  #-}

-- | Specifies whether the @GetDataSource@ operation should return @DataSourceSchema@ .
--
-- If true, @DataSourceSchema@ is returned.
-- If false, @DataSourceSchema@ is not returned.
--
-- /Note:/ Consider using 'verbose' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsVerbose :: Lens.Lens' GetDataSource (Core.Maybe Core.Bool)
gdsVerbose = Lens.field @"verbose"
{-# INLINEABLE gdsVerbose #-}
{-# DEPRECATED verbose "Use generic-lens or generic-optics with 'verbose' instead"  #-}

instance Core.ToQuery GetDataSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDataSource where
        toHeaders GetDataSource{..}
          = Core.pure ("X-Amz-Target", "AmazonML_20141212.GetDataSource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDataSource where
        toJSON GetDataSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DataSourceId" Core..= dataSourceId),
                  ("Verbose" Core..=) Core.<$> verbose])

instance Core.AWSRequest GetDataSource where
        type Rs GetDataSource = GetDataSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDataSourceResponse' Core.<$>
                   (x Core..:? "ComputeStatistics") Core.<*> x Core..:? "ComputeTime"
                     Core.<*> x Core..:? "CreatedAt"
                     Core.<*> x Core..:? "CreatedByIamUser"
                     Core.<*> x Core..:? "DataLocationS3"
                     Core.<*> x Core..:? "DataRearrangement"
                     Core.<*> x Core..:? "DataSizeInBytes"
                     Core.<*> x Core..:? "DataSourceId"
                     Core.<*> x Core..:? "DataSourceSchema"
                     Core.<*> x Core..:? "FinishedAt"
                     Core.<*> x Core..:? "LastUpdatedAt"
                     Core.<*> x Core..:? "LogUri"
                     Core.<*> x Core..:? "Message"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "NumberOfFiles"
                     Core.<*> x Core..:? "RDSMetadata"
                     Core.<*> x Core..:? "RedshiftMetadata"
                     Core.<*> x Core..:? "RoleARN"
                     Core.<*> x Core..:? "StartedAt"
                     Core.<*> x Core..:? "Status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @GetDataSource@ operation and describes a @DataSource@ .
--
-- /See:/ 'mkGetDataSourceResponse' smart constructor.
data GetDataSourceResponse = GetDataSourceResponse'
  { computeStatistics :: Core.Maybe Core.Bool
    -- ^ The parameter is @true@ if statistics need to be generated from the observation data. 
  , computeTime :: Core.Maybe Core.Integer
    -- ^ The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @DataSource@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @DataSource@ is in the @COMPLETED@ state and the @ComputeStatistics@ is set to true.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the @DataSource@ was created. The time is expressed in epoch time.
  , createdByIamUser :: Core.Maybe Types.AwsUserArn
    -- ^ The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
  , dataLocationS3 :: Core.Maybe Types.DataLocationS3
    -- ^ The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
  , dataRearrangement :: Core.Maybe Types.DataRearrangement
    -- ^ A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
  , dataSizeInBytes :: Core.Maybe Core.Integer
    -- ^ The total size of observations in the data files.
  , dataSourceId :: Core.Maybe Types.DataSourceId
    -- ^ The ID assigned to the @DataSource@ at creation. This value should be identical to the value of the @DataSourceId@ in the request.
  , dataSourceSchema :: Core.Maybe Types.DataSchema
    -- ^ The schema used by all of the data files of this @DataSource@ .
  , finishedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The epoch time when Amazon Machine Learning marked the @DataSource@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @DataSource@ is in the @COMPLETED@ or @FAILED@ state.
  , lastUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the most recent edit to the @DataSource@ . The time is expressed in epoch time.
  , logUri :: Core.Maybe Types.LogUri
    -- ^ A link to the file containing logs of @CreateDataSourceFrom*@ operations.
  , message :: Core.Maybe Types.Message
    -- ^ The user-supplied description of the most recent details about creating the @DataSource@ .
  , name :: Core.Maybe Types.EntityName
    -- ^ A user-supplied name or description of the @DataSource@ .
  , numberOfFiles :: Core.Maybe Core.Integer
    -- ^ The number of data files referenced by the @DataSource@ .
  , rDSMetadata :: Core.Maybe Types.RDSMetadata
  , redshiftMetadata :: Core.Maybe Types.RedshiftMetadata
  , roleARN :: Core.Maybe Types.RoleARN
  , startedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The epoch time when Amazon Machine Learning marked the @DataSource@ as @INPROGRESS@ . @StartedAt@ isn't available if the @DataSource@ is in the @PENDING@ state.
  , status :: Core.Maybe Types.EntityStatus
    -- ^ The current status of the @DataSource@ . This element can have one of the following values:
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
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDataSourceResponse' value with any optional fields omitted.
mkGetDataSourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDataSourceResponse
mkGetDataSourceResponse responseStatus
  = GetDataSourceResponse'{computeStatistics = Core.Nothing,
                           computeTime = Core.Nothing, createdAt = Core.Nothing,
                           createdByIamUser = Core.Nothing, dataLocationS3 = Core.Nothing,
                           dataRearrangement = Core.Nothing, dataSizeInBytes = Core.Nothing,
                           dataSourceId = Core.Nothing, dataSourceSchema = Core.Nothing,
                           finishedAt = Core.Nothing, lastUpdatedAt = Core.Nothing,
                           logUri = Core.Nothing, message = Core.Nothing, name = Core.Nothing,
                           numberOfFiles = Core.Nothing, rDSMetadata = Core.Nothing,
                           redshiftMetadata = Core.Nothing, roleARN = Core.Nothing,
                           startedAt = Core.Nothing, status = Core.Nothing, responseStatus}

-- | The parameter is @true@ if statistics need to be generated from the observation data. 
--
-- /Note:/ Consider using 'computeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsComputeStatistics :: Lens.Lens' GetDataSourceResponse (Core.Maybe Core.Bool)
gdsrrsComputeStatistics = Lens.field @"computeStatistics"
{-# INLINEABLE gdsrrsComputeStatistics #-}
{-# DEPRECATED computeStatistics "Use generic-lens or generic-optics with 'computeStatistics' instead"  #-}

-- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @DataSource@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @DataSource@ is in the @COMPLETED@ state and the @ComputeStatistics@ is set to true.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsComputeTime :: Lens.Lens' GetDataSourceResponse (Core.Maybe Core.Integer)
gdsrrsComputeTime = Lens.field @"computeTime"
{-# INLINEABLE gdsrrsComputeTime #-}
{-# DEPRECATED computeTime "Use generic-lens or generic-optics with 'computeTime' instead"  #-}

-- | The time that the @DataSource@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsCreatedAt :: Lens.Lens' GetDataSourceResponse (Core.Maybe Core.NominalDiffTime)
gdsrrsCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE gdsrrsCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The AWS user account from which the @DataSource@ was created. The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIamUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsCreatedByIamUser :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.AwsUserArn)
gdsrrsCreatedByIamUser = Lens.field @"createdByIamUser"
{-# INLINEABLE gdsrrsCreatedByIamUser #-}
{-# DEPRECATED createdByIamUser "Use generic-lens or generic-optics with 'createdByIamUser' instead"  #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'dataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDataLocationS3 :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.DataLocationS3)
gdsrrsDataLocationS3 = Lens.field @"dataLocationS3"
{-# INLINEABLE gdsrrsDataLocationS3 #-}
{-# DEPRECATED dataLocationS3 "Use generic-lens or generic-optics with 'dataLocationS3' instead"  #-}

-- | A JSON string that represents the splitting and rearrangement requirement used when this @DataSource@ was created.
--
-- /Note:/ Consider using 'dataRearrangement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDataRearrangement :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.DataRearrangement)
gdsrrsDataRearrangement = Lens.field @"dataRearrangement"
{-# INLINEABLE gdsrrsDataRearrangement #-}
{-# DEPRECATED dataRearrangement "Use generic-lens or generic-optics with 'dataRearrangement' instead"  #-}

-- | The total size of observations in the data files.
--
-- /Note:/ Consider using 'dataSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDataSizeInBytes :: Lens.Lens' GetDataSourceResponse (Core.Maybe Core.Integer)
gdsrrsDataSizeInBytes = Lens.field @"dataSizeInBytes"
{-# INLINEABLE gdsrrsDataSizeInBytes #-}
{-# DEPRECATED dataSizeInBytes "Use generic-lens or generic-optics with 'dataSizeInBytes' instead"  #-}

-- | The ID assigned to the @DataSource@ at creation. This value should be identical to the value of the @DataSourceId@ in the request.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDataSourceId :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.DataSourceId)
gdsrrsDataSourceId = Lens.field @"dataSourceId"
{-# INLINEABLE gdsrrsDataSourceId #-}
{-# DEPRECATED dataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead"  #-}

-- | The schema used by all of the data files of this @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDataSourceSchema :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.DataSchema)
gdsrrsDataSourceSchema = Lens.field @"dataSourceSchema"
{-# INLINEABLE gdsrrsDataSourceSchema #-}
{-# DEPRECATED dataSourceSchema "Use generic-lens or generic-optics with 'dataSourceSchema' instead"  #-}

-- | The epoch time when Amazon Machine Learning marked the @DataSource@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @DataSource@ is in the @COMPLETED@ or @FAILED@ state.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsFinishedAt :: Lens.Lens' GetDataSourceResponse (Core.Maybe Core.NominalDiffTime)
gdsrrsFinishedAt = Lens.field @"finishedAt"
{-# INLINEABLE gdsrrsFinishedAt #-}
{-# DEPRECATED finishedAt "Use generic-lens or generic-optics with 'finishedAt' instead"  #-}

-- | The time of the most recent edit to the @DataSource@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsLastUpdatedAt :: Lens.Lens' GetDataSourceResponse (Core.Maybe Core.NominalDiffTime)
gdsrrsLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE gdsrrsLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | A link to the file containing logs of @CreateDataSourceFrom*@ operations.
--
-- /Note:/ Consider using 'logUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsLogUri :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.LogUri)
gdsrrsLogUri = Lens.field @"logUri"
{-# INLINEABLE gdsrrsLogUri #-}
{-# DEPRECATED logUri "Use generic-lens or generic-optics with 'logUri' instead"  #-}

-- | The user-supplied description of the most recent details about creating the @DataSource@ .
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsMessage :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.Message)
gdsrrsMessage = Lens.field @"message"
{-# INLINEABLE gdsrrsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | A user-supplied name or description of the @DataSource@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsName :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.EntityName)
gdsrrsName = Lens.field @"name"
{-# INLINEABLE gdsrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of data files referenced by the @DataSource@ .
--
-- /Note:/ Consider using 'numberOfFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsNumberOfFiles :: Lens.Lens' GetDataSourceResponse (Core.Maybe Core.Integer)
gdsrrsNumberOfFiles = Lens.field @"numberOfFiles"
{-# INLINEABLE gdsrrsNumberOfFiles #-}
{-# DEPRECATED numberOfFiles "Use generic-lens or generic-optics with 'numberOfFiles' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rDSMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsRDSMetadata :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.RDSMetadata)
gdsrrsRDSMetadata = Lens.field @"rDSMetadata"
{-# INLINEABLE gdsrrsRDSMetadata #-}
{-# DEPRECATED rDSMetadata "Use generic-lens or generic-optics with 'rDSMetadata' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsRedshiftMetadata :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.RedshiftMetadata)
gdsrrsRedshiftMetadata = Lens.field @"redshiftMetadata"
{-# INLINEABLE gdsrrsRedshiftMetadata #-}
{-# DEPRECATED redshiftMetadata "Use generic-lens or generic-optics with 'redshiftMetadata' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsRoleARN :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.RoleARN)
gdsrrsRoleARN = Lens.field @"roleARN"
{-# INLINEABLE gdsrrsRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The epoch time when Amazon Machine Learning marked the @DataSource@ as @INPROGRESS@ . @StartedAt@ isn't available if the @DataSource@ is in the @PENDING@ state.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsStartedAt :: Lens.Lens' GetDataSourceResponse (Core.Maybe Core.NominalDiffTime)
gdsrrsStartedAt = Lens.field @"startedAt"
{-# INLINEABLE gdsrrsStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

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
gdsrrsStatus :: Lens.Lens' GetDataSourceResponse (Core.Maybe Types.EntityStatus)
gdsrrsStatus = Lens.field @"status"
{-# INLINEABLE gdsrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsResponseStatus :: Lens.Lens' GetDataSourceResponse Core.Int
gdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
