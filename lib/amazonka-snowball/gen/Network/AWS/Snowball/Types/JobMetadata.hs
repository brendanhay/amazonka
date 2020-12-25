{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobMetadata
  ( JobMetadata (..),

    -- * Smart constructor
    mkJobMetadata,

    -- * Lenses
    jmAddressId,
    jmClusterId,
    jmCreationDate,
    jmDataTransferProgress,
    jmDescription,
    jmDeviceConfiguration,
    jmForwardingAddressId,
    jmJobId,
    jmJobLogInfo,
    jmJobState,
    jmJobType,
    jmKmsKeyARN,
    jmNotification,
    jmResources,
    jmRoleARN,
    jmShippingDetails,
    jmSnowballCapacityPreference,
    jmSnowballType,
    jmTaxDocuments,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.AddressId as Types
import qualified Network.AWS.Snowball.Types.DataTransfer as Types
import qualified Network.AWS.Snowball.Types.Description as Types
import qualified Network.AWS.Snowball.Types.DeviceConfiguration as Types
import qualified Network.AWS.Snowball.Types.ForwardingAddressId as Types
import qualified Network.AWS.Snowball.Types.JobId as Types
import qualified Network.AWS.Snowball.Types.JobLogs as Types
import qualified Network.AWS.Snowball.Types.JobResource as Types
import qualified Network.AWS.Snowball.Types.JobState as Types
import qualified Network.AWS.Snowball.Types.JobType as Types
import qualified Network.AWS.Snowball.Types.KmsKeyARN as Types
import qualified Network.AWS.Snowball.Types.Notification as Types
import qualified Network.AWS.Snowball.Types.RoleARN as Types
import qualified Network.AWS.Snowball.Types.ShippingDetails as Types
import qualified Network.AWS.Snowball.Types.SnowballCapacity as Types
import qualified Network.AWS.Snowball.Types.SnowballType as Types
import qualified Network.AWS.Snowball.Types.String as Types
import qualified Network.AWS.Snowball.Types.TaxDocuments as Types

-- | Contains information about a specific job including shipping information, job status, and other important metadata. This information is returned as a part of the response syntax of the @DescribeJob@ action.
--
-- /See:/ 'mkJobMetadata' smart constructor.
data JobMetadata = JobMetadata'
  { -- | The ID for the address that you want the Snow device shipped to.
    addressId :: Core.Maybe Types.AddressId,
    -- | The 39-character ID for the cluster, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
    clusterId :: Core.Maybe Types.String,
    -- | The creation date for this job.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | A value that defines the real-time status of a Snow device's data transfer while the device is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
    dataTransferProgress :: Core.Maybe Types.DataTransfer,
    -- | The description of the job, provided at job creation.
    description :: Core.Maybe Types.Description,
    deviceConfiguration :: Core.Maybe Types.DeviceConfiguration,
    -- | The ID of the address that you want a job shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
    forwardingAddressId :: Core.Maybe Types.ForwardingAddressId,
    -- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
    jobId :: Core.Maybe Types.JobId,
    -- | Links to Amazon S3 presigned URLs for the job report and logs. For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snow device for your job part is being delivered to you.
    jobLogInfo :: Core.Maybe Types.JobLogs,
    -- | The current status of the jobs.
    jobState :: Core.Maybe Types.JobState,
    -- | The type of job.
    jobType :: Core.Maybe Types.JobType,
    -- | The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS KMS) key associated with this job. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS KMS.
    kmsKeyARN :: Core.Maybe Types.KmsKeyARN,
    -- | The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
    notification :: Core.Maybe Types.Notification,
    -- | An array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
    resources :: Core.Maybe Types.JobResource,
    -- | The role ARN associated with this job. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
    roleARN :: Core.Maybe Types.RoleARN,
    -- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
    shippingDetails :: Core.Maybe Types.ShippingDetails,
    -- | The Snow device capacity preference for this job, specified at job creation. In US regions, you can choose between 50 TB and 80 TB Snowballs. All other regions use 80 TB capacity Snowballs.
    snowballCapacityPreference :: Core.Maybe Types.SnowballCapacity,
    -- | The type of device used with this job.
    snowballType :: Core.Maybe Types.SnowballType,
    -- | The metadata associated with the tax documents required in your AWS Region.
    taxDocuments :: Core.Maybe Types.TaxDocuments
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'JobMetadata' value with any optional fields omitted.
mkJobMetadata ::
  JobMetadata
mkJobMetadata =
  JobMetadata'
    { addressId = Core.Nothing,
      clusterId = Core.Nothing,
      creationDate = Core.Nothing,
      dataTransferProgress = Core.Nothing,
      description = Core.Nothing,
      deviceConfiguration = Core.Nothing,
      forwardingAddressId = Core.Nothing,
      jobId = Core.Nothing,
      jobLogInfo = Core.Nothing,
      jobState = Core.Nothing,
      jobType = Core.Nothing,
      kmsKeyARN = Core.Nothing,
      notification = Core.Nothing,
      resources = Core.Nothing,
      roleARN = Core.Nothing,
      shippingDetails = Core.Nothing,
      snowballCapacityPreference = Core.Nothing,
      snowballType = Core.Nothing,
      taxDocuments = Core.Nothing
    }

-- | The ID for the address that you want the Snow device shipped to.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmAddressId :: Lens.Lens' JobMetadata (Core.Maybe Types.AddressId)
jmAddressId = Lens.field @"addressId"
{-# DEPRECATED jmAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The 39-character ID for the cluster, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmClusterId :: Lens.Lens' JobMetadata (Core.Maybe Types.String)
jmClusterId = Lens.field @"clusterId"
{-# DEPRECATED jmClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The creation date for this job.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmCreationDate :: Lens.Lens' JobMetadata (Core.Maybe Core.NominalDiffTime)
jmCreationDate = Lens.field @"creationDate"
{-# DEPRECATED jmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A value that defines the real-time status of a Snow device's data transfer while the device is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
--
-- /Note:/ Consider using 'dataTransferProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmDataTransferProgress :: Lens.Lens' JobMetadata (Core.Maybe Types.DataTransfer)
jmDataTransferProgress = Lens.field @"dataTransferProgress"
{-# DEPRECATED jmDataTransferProgress "Use generic-lens or generic-optics with 'dataTransferProgress' instead." #-}

-- | The description of the job, provided at job creation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmDescription :: Lens.Lens' JobMetadata (Core.Maybe Types.Description)
jmDescription = Lens.field @"description"
{-# DEPRECATED jmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'deviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmDeviceConfiguration :: Lens.Lens' JobMetadata (Core.Maybe Types.DeviceConfiguration)
jmDeviceConfiguration = Lens.field @"deviceConfiguration"
{-# DEPRECATED jmDeviceConfiguration "Use generic-lens or generic-optics with 'deviceConfiguration' instead." #-}

-- | The ID of the address that you want a job shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmForwardingAddressId :: Lens.Lens' JobMetadata (Core.Maybe Types.ForwardingAddressId)
jmForwardingAddressId = Lens.field @"forwardingAddressId"
{-# DEPRECATED jmForwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead." #-}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmJobId :: Lens.Lens' JobMetadata (Core.Maybe Types.JobId)
jmJobId = Lens.field @"jobId"
{-# DEPRECATED jmJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Links to Amazon S3 presigned URLs for the job report and logs. For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snow device for your job part is being delivered to you.
--
-- /Note:/ Consider using 'jobLogInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmJobLogInfo :: Lens.Lens' JobMetadata (Core.Maybe Types.JobLogs)
jmJobLogInfo = Lens.field @"jobLogInfo"
{-# DEPRECATED jmJobLogInfo "Use generic-lens or generic-optics with 'jobLogInfo' instead." #-}

-- | The current status of the jobs.
--
-- /Note:/ Consider using 'jobState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmJobState :: Lens.Lens' JobMetadata (Core.Maybe Types.JobState)
jmJobState = Lens.field @"jobState"
{-# DEPRECATED jmJobState "Use generic-lens or generic-optics with 'jobState' instead." #-}

-- | The type of job.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmJobType :: Lens.Lens' JobMetadata (Core.Maybe Types.JobType)
jmJobType = Lens.field @"jobType"
{-# DEPRECATED jmJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS KMS) key associated with this job. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS KMS.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmKmsKeyARN :: Lens.Lens' JobMetadata (Core.Maybe Types.KmsKeyARN)
jmKmsKeyARN = Lens.field @"kmsKeyARN"
{-# DEPRECATED jmKmsKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmNotification :: Lens.Lens' JobMetadata (Core.Maybe Types.Notification)
jmNotification = Lens.field @"notification"
{-# DEPRECATED jmNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | An array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmResources :: Lens.Lens' JobMetadata (Core.Maybe Types.JobResource)
jmResources = Lens.field @"resources"
{-# DEPRECATED jmResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The role ARN associated with this job. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmRoleARN :: Lens.Lens' JobMetadata (Core.Maybe Types.RoleARN)
jmRoleARN = Lens.field @"roleARN"
{-# DEPRECATED jmRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
--
-- /Note:/ Consider using 'shippingDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmShippingDetails :: Lens.Lens' JobMetadata (Core.Maybe Types.ShippingDetails)
jmShippingDetails = Lens.field @"shippingDetails"
{-# DEPRECATED jmShippingDetails "Use generic-lens or generic-optics with 'shippingDetails' instead." #-}

-- | The Snow device capacity preference for this job, specified at job creation. In US regions, you can choose between 50 TB and 80 TB Snowballs. All other regions use 80 TB capacity Snowballs.
--
-- /Note:/ Consider using 'snowballCapacityPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmSnowballCapacityPreference :: Lens.Lens' JobMetadata (Core.Maybe Types.SnowballCapacity)
jmSnowballCapacityPreference = Lens.field @"snowballCapacityPreference"
{-# DEPRECATED jmSnowballCapacityPreference "Use generic-lens or generic-optics with 'snowballCapacityPreference' instead." #-}

-- | The type of device used with this job.
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmSnowballType :: Lens.Lens' JobMetadata (Core.Maybe Types.SnowballType)
jmSnowballType = Lens.field @"snowballType"
{-# DEPRECATED jmSnowballType "Use generic-lens or generic-optics with 'snowballType' instead." #-}

-- | The metadata associated with the tax documents required in your AWS Region.
--
-- /Note:/ Consider using 'taxDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmTaxDocuments :: Lens.Lens' JobMetadata (Core.Maybe Types.TaxDocuments)
jmTaxDocuments = Lens.field @"taxDocuments"
{-# DEPRECATED jmTaxDocuments "Use generic-lens or generic-optics with 'taxDocuments' instead." #-}

instance Core.FromJSON JobMetadata where
  parseJSON =
    Core.withObject "JobMetadata" Core.$
      \x ->
        JobMetadata'
          Core.<$> (x Core..:? "AddressId")
          Core.<*> (x Core..:? "ClusterId")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "DataTransferProgress")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "DeviceConfiguration")
          Core.<*> (x Core..:? "ForwardingAddressId")
          Core.<*> (x Core..:? "JobId")
          Core.<*> (x Core..:? "JobLogInfo")
          Core.<*> (x Core..:? "JobState")
          Core.<*> (x Core..:? "JobType")
          Core.<*> (x Core..:? "KmsKeyARN")
          Core.<*> (x Core..:? "Notification")
          Core.<*> (x Core..:? "Resources")
          Core.<*> (x Core..:? "RoleARN")
          Core.<*> (x Core..:? "ShippingDetails")
          Core.<*> (x Core..:? "SnowballCapacityPreference")
          Core.<*> (x Core..:? "SnowballType")
          Core.<*> (x Core..:? "TaxDocuments")
