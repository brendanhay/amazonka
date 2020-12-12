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
    jmJobType,
    jmKMSKeyARN,
    jmJobId,
    jmJobLogInfo,
    jmNotification,
    jmJobState,
    jmForwardingAddressId,
    jmShippingDetails,
    jmAddressId,
    jmSnowballType,
    jmDataTransferProgress,
    jmResources,
    jmClusterId,
    jmCreationDate,
    jmDeviceConfiguration,
    jmDescription,
    jmTaxDocuments,
    jmRoleARN,
    jmSnowballCapacityPreference,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.DataTransfer
import Network.AWS.Snowball.Types.DeviceConfiguration
import Network.AWS.Snowball.Types.JobLogs
import Network.AWS.Snowball.Types.JobResource
import Network.AWS.Snowball.Types.JobState
import Network.AWS.Snowball.Types.JobType
import Network.AWS.Snowball.Types.Notification
import Network.AWS.Snowball.Types.ShippingDetails
import Network.AWS.Snowball.Types.SnowballCapacity
import Network.AWS.Snowball.Types.SnowballType
import Network.AWS.Snowball.Types.TaxDocuments

-- | Contains information about a specific job including shipping information, job status, and other important metadata. This information is returned as a part of the response syntax of the @DescribeJob@ action.
--
-- /See:/ 'mkJobMetadata' smart constructor.
data JobMetadata = JobMetadata'
  { jobType :: Lude.Maybe JobType,
    kmsKeyARN :: Lude.Maybe Lude.Text,
    jobId :: Lude.Maybe Lude.Text,
    jobLogInfo :: Lude.Maybe JobLogs,
    notification :: Lude.Maybe Notification,
    jobState :: Lude.Maybe JobState,
    forwardingAddressId :: Lude.Maybe Lude.Text,
    shippingDetails :: Lude.Maybe ShippingDetails,
    addressId :: Lude.Maybe Lude.Text,
    snowballType :: Lude.Maybe SnowballType,
    dataTransferProgress :: Lude.Maybe DataTransfer,
    resources :: Lude.Maybe JobResource,
    clusterId :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    deviceConfiguration :: Lude.Maybe DeviceConfiguration,
    description :: Lude.Maybe Lude.Text,
    taxDocuments :: Lude.Maybe TaxDocuments,
    roleARN :: Lude.Maybe Lude.Text,
    snowballCapacityPreference :: Lude.Maybe SnowballCapacity
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobMetadata' with the minimum fields required to make a request.
--
-- * 'addressId' - The ID for the address that you want the Snow device shipped to.
-- * 'clusterId' - The 39-character ID for the cluster, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'creationDate' - The creation date for this job.
-- * 'dataTransferProgress' - A value that defines the real-time status of a Snow device's data transfer while the device is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
-- * 'description' - The description of the job, provided at job creation.
-- * 'deviceConfiguration' - Undocumented field.
-- * 'forwardingAddressId' - The ID of the address that you want a job shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
-- * 'jobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'jobLogInfo' - Links to Amazon S3 presigned URLs for the job report and logs. For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snow device for your job part is being delivered to you.
-- * 'jobState' - The current status of the jobs.
-- * 'jobType' - The type of job.
-- * 'kmsKeyARN' - The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS KMS) key associated with this job. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS KMS.
-- * 'notification' - The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
-- * 'resources' - An array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
-- * 'roleARN' - The role ARN associated with this job. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
-- * 'shippingDetails' - A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
-- * 'snowballCapacityPreference' - The Snow device capacity preference for this job, specified at job creation. In US regions, you can choose between 50 TB and 80 TB Snowballs. All other regions use 80 TB capacity Snowballs.
-- * 'snowballType' - The type of device used with this job.
-- * 'taxDocuments' - The metadata associated with the tax documents required in your AWS Region.
mkJobMetadata ::
  JobMetadata
mkJobMetadata =
  JobMetadata'
    { jobType = Lude.Nothing,
      kmsKeyARN = Lude.Nothing,
      jobId = Lude.Nothing,
      jobLogInfo = Lude.Nothing,
      notification = Lude.Nothing,
      jobState = Lude.Nothing,
      forwardingAddressId = Lude.Nothing,
      shippingDetails = Lude.Nothing,
      addressId = Lude.Nothing,
      snowballType = Lude.Nothing,
      dataTransferProgress = Lude.Nothing,
      resources = Lude.Nothing,
      clusterId = Lude.Nothing,
      creationDate = Lude.Nothing,
      deviceConfiguration = Lude.Nothing,
      description = Lude.Nothing,
      taxDocuments = Lude.Nothing,
      roleARN = Lude.Nothing,
      snowballCapacityPreference = Lude.Nothing
    }

-- | The type of job.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmJobType :: Lens.Lens' JobMetadata (Lude.Maybe JobType)
jmJobType = Lens.lens (jobType :: JobMetadata -> Lude.Maybe JobType) (\s a -> s {jobType = a} :: JobMetadata)
{-# DEPRECATED jmJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS KMS) key associated with this job. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS KMS.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmKMSKeyARN :: Lens.Lens' JobMetadata (Lude.Maybe Lude.Text)
jmKMSKeyARN = Lens.lens (kmsKeyARN :: JobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: JobMetadata)
{-# DEPRECATED jmKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmJobId :: Lens.Lens' JobMetadata (Lude.Maybe Lude.Text)
jmJobId = Lens.lens (jobId :: JobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: JobMetadata)
{-# DEPRECATED jmJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Links to Amazon S3 presigned URLs for the job report and logs. For import jobs, the PDF job report becomes available at the end of the import process. For export jobs, your job report typically becomes available while the Snow device for your job part is being delivered to you.
--
-- /Note:/ Consider using 'jobLogInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmJobLogInfo :: Lens.Lens' JobMetadata (Lude.Maybe JobLogs)
jmJobLogInfo = Lens.lens (jobLogInfo :: JobMetadata -> Lude.Maybe JobLogs) (\s a -> s {jobLogInfo = a} :: JobMetadata)
{-# DEPRECATED jmJobLogInfo "Use generic-lens or generic-optics with 'jobLogInfo' instead." #-}

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings associated with a specific job. The @Notification@ object is returned as a part of the response syntax of the @DescribeJob@ action in the @JobMetadata@ data type.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmNotification :: Lens.Lens' JobMetadata (Lude.Maybe Notification)
jmNotification = Lens.lens (notification :: JobMetadata -> Lude.Maybe Notification) (\s a -> s {notification = a} :: JobMetadata)
{-# DEPRECATED jmNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The current status of the jobs.
--
-- /Note:/ Consider using 'jobState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmJobState :: Lens.Lens' JobMetadata (Lude.Maybe JobState)
jmJobState = Lens.lens (jobState :: JobMetadata -> Lude.Maybe JobState) (\s a -> s {jobState = a} :: JobMetadata)
{-# DEPRECATED jmJobState "Use generic-lens or generic-optics with 'jobState' instead." #-}

-- | The ID of the address that you want a job shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmForwardingAddressId :: Lens.Lens' JobMetadata (Lude.Maybe Lude.Text)
jmForwardingAddressId = Lens.lens (forwardingAddressId :: JobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {forwardingAddressId = a} :: JobMetadata)
{-# DEPRECATED jmForwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead." #-}

-- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
--
-- /Note:/ Consider using 'shippingDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmShippingDetails :: Lens.Lens' JobMetadata (Lude.Maybe ShippingDetails)
jmShippingDetails = Lens.lens (shippingDetails :: JobMetadata -> Lude.Maybe ShippingDetails) (\s a -> s {shippingDetails = a} :: JobMetadata)
{-# DEPRECATED jmShippingDetails "Use generic-lens or generic-optics with 'shippingDetails' instead." #-}

-- | The ID for the address that you want the Snow device shipped to.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmAddressId :: Lens.Lens' JobMetadata (Lude.Maybe Lude.Text)
jmAddressId = Lens.lens (addressId :: JobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {addressId = a} :: JobMetadata)
{-# DEPRECATED jmAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The type of device used with this job.
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmSnowballType :: Lens.Lens' JobMetadata (Lude.Maybe SnowballType)
jmSnowballType = Lens.lens (snowballType :: JobMetadata -> Lude.Maybe SnowballType) (\s a -> s {snowballType = a} :: JobMetadata)
{-# DEPRECATED jmSnowballType "Use generic-lens or generic-optics with 'snowballType' instead." #-}

-- | A value that defines the real-time status of a Snow device's data transfer while the device is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
--
-- /Note:/ Consider using 'dataTransferProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmDataTransferProgress :: Lens.Lens' JobMetadata (Lude.Maybe DataTransfer)
jmDataTransferProgress = Lens.lens (dataTransferProgress :: JobMetadata -> Lude.Maybe DataTransfer) (\s a -> s {dataTransferProgress = a} :: JobMetadata)
{-# DEPRECATED jmDataTransferProgress "Use generic-lens or generic-optics with 'dataTransferProgress' instead." #-}

-- | An array of @S3Resource@ objects. Each @S3Resource@ object represents an Amazon S3 bucket that your transferred data will be exported from or imported into.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmResources :: Lens.Lens' JobMetadata (Lude.Maybe JobResource)
jmResources = Lens.lens (resources :: JobMetadata -> Lude.Maybe JobResource) (\s a -> s {resources = a} :: JobMetadata)
{-# DEPRECATED jmResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The 39-character ID for the cluster, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmClusterId :: Lens.Lens' JobMetadata (Lude.Maybe Lude.Text)
jmClusterId = Lens.lens (clusterId :: JobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: JobMetadata)
{-# DEPRECATED jmClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The creation date for this job.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmCreationDate :: Lens.Lens' JobMetadata (Lude.Maybe Lude.Timestamp)
jmCreationDate = Lens.lens (creationDate :: JobMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: JobMetadata)
{-# DEPRECATED jmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'deviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmDeviceConfiguration :: Lens.Lens' JobMetadata (Lude.Maybe DeviceConfiguration)
jmDeviceConfiguration = Lens.lens (deviceConfiguration :: JobMetadata -> Lude.Maybe DeviceConfiguration) (\s a -> s {deviceConfiguration = a} :: JobMetadata)
{-# DEPRECATED jmDeviceConfiguration "Use generic-lens or generic-optics with 'deviceConfiguration' instead." #-}

-- | The description of the job, provided at job creation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmDescription :: Lens.Lens' JobMetadata (Lude.Maybe Lude.Text)
jmDescription = Lens.lens (description :: JobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: JobMetadata)
{-# DEPRECATED jmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The metadata associated with the tax documents required in your AWS Region.
--
-- /Note:/ Consider using 'taxDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmTaxDocuments :: Lens.Lens' JobMetadata (Lude.Maybe TaxDocuments)
jmTaxDocuments = Lens.lens (taxDocuments :: JobMetadata -> Lude.Maybe TaxDocuments) (\s a -> s {taxDocuments = a} :: JobMetadata)
{-# DEPRECATED jmTaxDocuments "Use generic-lens or generic-optics with 'taxDocuments' instead." #-}

-- | The role ARN associated with this job. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmRoleARN :: Lens.Lens' JobMetadata (Lude.Maybe Lude.Text)
jmRoleARN = Lens.lens (roleARN :: JobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: JobMetadata)
{-# DEPRECATED jmRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The Snow device capacity preference for this job, specified at job creation. In US regions, you can choose between 50 TB and 80 TB Snowballs. All other regions use 80 TB capacity Snowballs.
--
-- /Note:/ Consider using 'snowballCapacityPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jmSnowballCapacityPreference :: Lens.Lens' JobMetadata (Lude.Maybe SnowballCapacity)
jmSnowballCapacityPreference = Lens.lens (snowballCapacityPreference :: JobMetadata -> Lude.Maybe SnowballCapacity) (\s a -> s {snowballCapacityPreference = a} :: JobMetadata)
{-# DEPRECATED jmSnowballCapacityPreference "Use generic-lens or generic-optics with 'snowballCapacityPreference' instead." #-}

instance Lude.FromJSON JobMetadata where
  parseJSON =
    Lude.withObject
      "JobMetadata"
      ( \x ->
          JobMetadata'
            Lude.<$> (x Lude..:? "JobType")
            Lude.<*> (x Lude..:? "KmsKeyARN")
            Lude.<*> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "JobLogInfo")
            Lude.<*> (x Lude..:? "Notification")
            Lude.<*> (x Lude..:? "JobState")
            Lude.<*> (x Lude..:? "ForwardingAddressId")
            Lude.<*> (x Lude..:? "ShippingDetails")
            Lude.<*> (x Lude..:? "AddressId")
            Lude.<*> (x Lude..:? "SnowballType")
            Lude.<*> (x Lude..:? "DataTransferProgress")
            Lude.<*> (x Lude..:? "Resources")
            Lude.<*> (x Lude..:? "ClusterId")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "DeviceConfiguration")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "TaxDocuments")
            Lude.<*> (x Lude..:? "RoleARN")
            Lude.<*> (x Lude..:? "SnowballCapacityPreference")
      )
