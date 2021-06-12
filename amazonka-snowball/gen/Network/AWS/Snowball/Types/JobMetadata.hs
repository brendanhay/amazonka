{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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

-- | Contains information about a specific job including shipping
-- information, job status, and other important metadata. This information
-- is returned as a part of the response syntax of the @DescribeJob@
-- action.
--
-- /See:/ 'newJobMetadata' smart constructor.
data JobMetadata = JobMetadata'
  { -- | The 39-character ID for the cluster, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Core.Maybe Core.Text,
    -- | The role ARN associated with this job. This ARN was created using the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in AWS Identity and Access Management (IAM).
    roleARN :: Core.Maybe Core.Text,
    -- | The current status of the jobs.
    jobState :: Core.Maybe JobState,
    deviceConfiguration :: Core.Maybe DeviceConfiguration,
    -- | The creation date for this job.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS
    -- KMS) key associated with this job. This ARN was created using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- API action in AWS KMS.
    kmsKeyARN :: Core.Maybe Core.Text,
    -- | The type of job.
    jobType :: Core.Maybe JobType,
    -- | An array of @S3Resource@ objects. Each @S3Resource@ object represents an
    -- Amazon S3 bucket that your transferred data will be exported from or
    -- imported into.
    resources :: Core.Maybe JobResource,
    -- | The metadata associated with the tax documents required in your AWS
    -- Region.
    taxDocuments :: Core.Maybe TaxDocuments,
    -- | The Snow device capacity preference for this job, specified at job
    -- creation. In US regions, you can choose between 50 TB and 80 TB
    -- Snowballs. All other regions use 80 TB capacity Snowballs.
    snowballCapacityPreference :: Core.Maybe SnowballCapacity,
    -- | The type of device used with this job.
    snowballType :: Core.Maybe SnowballType,
    -- | A value that defines the real-time status of a Snow device\'s data
    -- transfer while the device is at AWS. This data is only available while a
    -- job has a @JobState@ value of @InProgress@, for both import and export
    -- jobs.
    dataTransferProgress :: Core.Maybe DataTransfer,
    -- | The description of the job, provided at job creation.
    description :: Core.Maybe Core.Text,
    -- | The ID for the address that you want the Snow device shipped to.
    addressId :: Core.Maybe Core.Text,
    -- | The ID of the address that you want a job shipped to, after it will be
    -- shipped to its primary address. This field is not supported in most
    -- regions.
    forwardingAddressId :: Core.Maybe Core.Text,
    -- | A job\'s shipping information, including inbound and outbound tracking
    -- numbers and shipping speed options.
    shippingDetails :: Core.Maybe ShippingDetails,
    -- | The Amazon Simple Notification Service (Amazon SNS) notification
    -- settings associated with a specific job. The @Notification@ object is
    -- returned as a part of the response syntax of the @DescribeJob@ action in
    -- the @JobMetadata@ data type.
    notification :: Core.Maybe Notification,
    -- | Links to Amazon S3 presigned URLs for the job report and logs. For
    -- import jobs, the PDF job report becomes available at the end of the
    -- import process. For export jobs, your job report typically becomes
    -- available while the Snow device for your job part is being delivered to
    -- you.
    jobLogInfo :: Core.Maybe JobLogs,
    -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'jobMetadata_clusterId' - The 39-character ID for the cluster, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
--
-- 'roleARN', 'jobMetadata_roleARN' - The role ARN associated with this job. This ARN was created using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in AWS Identity and Access Management (IAM).
--
-- 'jobState', 'jobMetadata_jobState' - The current status of the jobs.
--
-- 'deviceConfiguration', 'jobMetadata_deviceConfiguration' - Undocumented member.
--
-- 'creationDate', 'jobMetadata_creationDate' - The creation date for this job.
--
-- 'kmsKeyARN', 'jobMetadata_kmsKeyARN' - The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS
-- KMS) key associated with this job. This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in AWS KMS.
--
-- 'jobType', 'jobMetadata_jobType' - The type of job.
--
-- 'resources', 'jobMetadata_resources' - An array of @S3Resource@ objects. Each @S3Resource@ object represents an
-- Amazon S3 bucket that your transferred data will be exported from or
-- imported into.
--
-- 'taxDocuments', 'jobMetadata_taxDocuments' - The metadata associated with the tax documents required in your AWS
-- Region.
--
-- 'snowballCapacityPreference', 'jobMetadata_snowballCapacityPreference' - The Snow device capacity preference for this job, specified at job
-- creation. In US regions, you can choose between 50 TB and 80 TB
-- Snowballs. All other regions use 80 TB capacity Snowballs.
--
-- 'snowballType', 'jobMetadata_snowballType' - The type of device used with this job.
--
-- 'dataTransferProgress', 'jobMetadata_dataTransferProgress' - A value that defines the real-time status of a Snow device\'s data
-- transfer while the device is at AWS. This data is only available while a
-- job has a @JobState@ value of @InProgress@, for both import and export
-- jobs.
--
-- 'description', 'jobMetadata_description' - The description of the job, provided at job creation.
--
-- 'addressId', 'jobMetadata_addressId' - The ID for the address that you want the Snow device shipped to.
--
-- 'forwardingAddressId', 'jobMetadata_forwardingAddressId' - The ID of the address that you want a job shipped to, after it will be
-- shipped to its primary address. This field is not supported in most
-- regions.
--
-- 'shippingDetails', 'jobMetadata_shippingDetails' - A job\'s shipping information, including inbound and outbound tracking
-- numbers and shipping speed options.
--
-- 'notification', 'jobMetadata_notification' - The Amazon Simple Notification Service (Amazon SNS) notification
-- settings associated with a specific job. The @Notification@ object is
-- returned as a part of the response syntax of the @DescribeJob@ action in
-- the @JobMetadata@ data type.
--
-- 'jobLogInfo', 'jobMetadata_jobLogInfo' - Links to Amazon S3 presigned URLs for the job report and logs. For
-- import jobs, the PDF job report becomes available at the end of the
-- import process. For export jobs, your job report typically becomes
-- available while the Snow device for your job part is being delivered to
-- you.
--
-- 'jobId', 'jobMetadata_jobId' - The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
newJobMetadata ::
  JobMetadata
newJobMetadata =
  JobMetadata'
    { clusterId = Core.Nothing,
      roleARN = Core.Nothing,
      jobState = Core.Nothing,
      deviceConfiguration = Core.Nothing,
      creationDate = Core.Nothing,
      kmsKeyARN = Core.Nothing,
      jobType = Core.Nothing,
      resources = Core.Nothing,
      taxDocuments = Core.Nothing,
      snowballCapacityPreference = Core.Nothing,
      snowballType = Core.Nothing,
      dataTransferProgress = Core.Nothing,
      description = Core.Nothing,
      addressId = Core.Nothing,
      forwardingAddressId = Core.Nothing,
      shippingDetails = Core.Nothing,
      notification = Core.Nothing,
      jobLogInfo = Core.Nothing,
      jobId = Core.Nothing
    }

-- | The 39-character ID for the cluster, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
jobMetadata_clusterId :: Lens.Lens' JobMetadata (Core.Maybe Core.Text)
jobMetadata_clusterId = Lens.lens (\JobMetadata' {clusterId} -> clusterId) (\s@JobMetadata' {} a -> s {clusterId = a} :: JobMetadata)

-- | The role ARN associated with this job. This ARN was created using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in AWS Identity and Access Management (IAM).
jobMetadata_roleARN :: Lens.Lens' JobMetadata (Core.Maybe Core.Text)
jobMetadata_roleARN = Lens.lens (\JobMetadata' {roleARN} -> roleARN) (\s@JobMetadata' {} a -> s {roleARN = a} :: JobMetadata)

-- | The current status of the jobs.
jobMetadata_jobState :: Lens.Lens' JobMetadata (Core.Maybe JobState)
jobMetadata_jobState = Lens.lens (\JobMetadata' {jobState} -> jobState) (\s@JobMetadata' {} a -> s {jobState = a} :: JobMetadata)

-- | Undocumented member.
jobMetadata_deviceConfiguration :: Lens.Lens' JobMetadata (Core.Maybe DeviceConfiguration)
jobMetadata_deviceConfiguration = Lens.lens (\JobMetadata' {deviceConfiguration} -> deviceConfiguration) (\s@JobMetadata' {} a -> s {deviceConfiguration = a} :: JobMetadata)

-- | The creation date for this job.
jobMetadata_creationDate :: Lens.Lens' JobMetadata (Core.Maybe Core.UTCTime)
jobMetadata_creationDate = Lens.lens (\JobMetadata' {creationDate} -> creationDate) (\s@JobMetadata' {} a -> s {creationDate = a} :: JobMetadata) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) for the AWS Key Management Service (AWS
-- KMS) key associated with this job. This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in AWS KMS.
jobMetadata_kmsKeyARN :: Lens.Lens' JobMetadata (Core.Maybe Core.Text)
jobMetadata_kmsKeyARN = Lens.lens (\JobMetadata' {kmsKeyARN} -> kmsKeyARN) (\s@JobMetadata' {} a -> s {kmsKeyARN = a} :: JobMetadata)

-- | The type of job.
jobMetadata_jobType :: Lens.Lens' JobMetadata (Core.Maybe JobType)
jobMetadata_jobType = Lens.lens (\JobMetadata' {jobType} -> jobType) (\s@JobMetadata' {} a -> s {jobType = a} :: JobMetadata)

-- | An array of @S3Resource@ objects. Each @S3Resource@ object represents an
-- Amazon S3 bucket that your transferred data will be exported from or
-- imported into.
jobMetadata_resources :: Lens.Lens' JobMetadata (Core.Maybe JobResource)
jobMetadata_resources = Lens.lens (\JobMetadata' {resources} -> resources) (\s@JobMetadata' {} a -> s {resources = a} :: JobMetadata)

-- | The metadata associated with the tax documents required in your AWS
-- Region.
jobMetadata_taxDocuments :: Lens.Lens' JobMetadata (Core.Maybe TaxDocuments)
jobMetadata_taxDocuments = Lens.lens (\JobMetadata' {taxDocuments} -> taxDocuments) (\s@JobMetadata' {} a -> s {taxDocuments = a} :: JobMetadata)

-- | The Snow device capacity preference for this job, specified at job
-- creation. In US regions, you can choose between 50 TB and 80 TB
-- Snowballs. All other regions use 80 TB capacity Snowballs.
jobMetadata_snowballCapacityPreference :: Lens.Lens' JobMetadata (Core.Maybe SnowballCapacity)
jobMetadata_snowballCapacityPreference = Lens.lens (\JobMetadata' {snowballCapacityPreference} -> snowballCapacityPreference) (\s@JobMetadata' {} a -> s {snowballCapacityPreference = a} :: JobMetadata)

-- | The type of device used with this job.
jobMetadata_snowballType :: Lens.Lens' JobMetadata (Core.Maybe SnowballType)
jobMetadata_snowballType = Lens.lens (\JobMetadata' {snowballType} -> snowballType) (\s@JobMetadata' {} a -> s {snowballType = a} :: JobMetadata)

-- | A value that defines the real-time status of a Snow device\'s data
-- transfer while the device is at AWS. This data is only available while a
-- job has a @JobState@ value of @InProgress@, for both import and export
-- jobs.
jobMetadata_dataTransferProgress :: Lens.Lens' JobMetadata (Core.Maybe DataTransfer)
jobMetadata_dataTransferProgress = Lens.lens (\JobMetadata' {dataTransferProgress} -> dataTransferProgress) (\s@JobMetadata' {} a -> s {dataTransferProgress = a} :: JobMetadata)

-- | The description of the job, provided at job creation.
jobMetadata_description :: Lens.Lens' JobMetadata (Core.Maybe Core.Text)
jobMetadata_description = Lens.lens (\JobMetadata' {description} -> description) (\s@JobMetadata' {} a -> s {description = a} :: JobMetadata)

-- | The ID for the address that you want the Snow device shipped to.
jobMetadata_addressId :: Lens.Lens' JobMetadata (Core.Maybe Core.Text)
jobMetadata_addressId = Lens.lens (\JobMetadata' {addressId} -> addressId) (\s@JobMetadata' {} a -> s {addressId = a} :: JobMetadata)

-- | The ID of the address that you want a job shipped to, after it will be
-- shipped to its primary address. This field is not supported in most
-- regions.
jobMetadata_forwardingAddressId :: Lens.Lens' JobMetadata (Core.Maybe Core.Text)
jobMetadata_forwardingAddressId = Lens.lens (\JobMetadata' {forwardingAddressId} -> forwardingAddressId) (\s@JobMetadata' {} a -> s {forwardingAddressId = a} :: JobMetadata)

-- | A job\'s shipping information, including inbound and outbound tracking
-- numbers and shipping speed options.
jobMetadata_shippingDetails :: Lens.Lens' JobMetadata (Core.Maybe ShippingDetails)
jobMetadata_shippingDetails = Lens.lens (\JobMetadata' {shippingDetails} -> shippingDetails) (\s@JobMetadata' {} a -> s {shippingDetails = a} :: JobMetadata)

-- | The Amazon Simple Notification Service (Amazon SNS) notification
-- settings associated with a specific job. The @Notification@ object is
-- returned as a part of the response syntax of the @DescribeJob@ action in
-- the @JobMetadata@ data type.
jobMetadata_notification :: Lens.Lens' JobMetadata (Core.Maybe Notification)
jobMetadata_notification = Lens.lens (\JobMetadata' {notification} -> notification) (\s@JobMetadata' {} a -> s {notification = a} :: JobMetadata)

-- | Links to Amazon S3 presigned URLs for the job report and logs. For
-- import jobs, the PDF job report becomes available at the end of the
-- import process. For export jobs, your job report typically becomes
-- available while the Snow device for your job part is being delivered to
-- you.
jobMetadata_jobLogInfo :: Lens.Lens' JobMetadata (Core.Maybe JobLogs)
jobMetadata_jobLogInfo = Lens.lens (\JobMetadata' {jobLogInfo} -> jobLogInfo) (\s@JobMetadata' {} a -> s {jobLogInfo = a} :: JobMetadata)

-- | The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
jobMetadata_jobId :: Lens.Lens' JobMetadata (Core.Maybe Core.Text)
jobMetadata_jobId = Lens.lens (\JobMetadata' {jobId} -> jobId) (\s@JobMetadata' {} a -> s {jobId = a} :: JobMetadata)

instance Core.FromJSON JobMetadata where
  parseJSON =
    Core.withObject
      "JobMetadata"
      ( \x ->
          JobMetadata'
            Core.<$> (x Core..:? "ClusterId")
            Core.<*> (x Core..:? "RoleARN")
            Core.<*> (x Core..:? "JobState")
            Core.<*> (x Core..:? "DeviceConfiguration")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "KmsKeyARN")
            Core.<*> (x Core..:? "JobType")
            Core.<*> (x Core..:? "Resources")
            Core.<*> (x Core..:? "TaxDocuments")
            Core.<*> (x Core..:? "SnowballCapacityPreference")
            Core.<*> (x Core..:? "SnowballType")
            Core.<*> (x Core..:? "DataTransferProgress")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "AddressId")
            Core.<*> (x Core..:? "ForwardingAddressId")
            Core.<*> (x Core..:? "ShippingDetails")
            Core.<*> (x Core..:? "Notification")
            Core.<*> (x Core..:? "JobLogInfo")
            Core.<*> (x Core..:? "JobId")
      )

instance Core.Hashable JobMetadata

instance Core.NFData JobMetadata
