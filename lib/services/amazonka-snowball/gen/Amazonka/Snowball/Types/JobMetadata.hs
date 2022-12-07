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
-- Module      : Amazonka.Snowball.Types.JobMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.JobMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.DataTransfer
import Amazonka.Snowball.Types.DeviceConfiguration
import Amazonka.Snowball.Types.JobLogs
import Amazonka.Snowball.Types.JobResource
import Amazonka.Snowball.Types.JobState
import Amazonka.Snowball.Types.JobType
import Amazonka.Snowball.Types.Notification
import Amazonka.Snowball.Types.OnDeviceServiceConfiguration
import Amazonka.Snowball.Types.RemoteManagement
import Amazonka.Snowball.Types.ShippingDetails
import Amazonka.Snowball.Types.SnowballCapacity
import Amazonka.Snowball.Types.SnowballType
import Amazonka.Snowball.Types.TaxDocuments

-- | Contains information about a specific job including shipping
-- information, job status, and other important metadata. This information
-- is returned as a part of the response syntax of the @DescribeJob@
-- action.
--
-- /See:/ 'newJobMetadata' smart constructor.
data JobMetadata = JobMetadata'
  { -- | A job\'s shipping information, including inbound and outbound tracking
    -- numbers and shipping speed options.
    shippingDetails :: Prelude.Maybe ShippingDetails,
    -- | The role ARN associated with this job. This ARN was created using the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in Identity and Access Management.
    roleARN :: Prelude.Maybe Prelude.Text,
    deviceConfiguration :: Prelude.Maybe DeviceConfiguration,
    -- | The creation date for this job.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the address that you want a job shipped to, after it will be
    -- shipped to its primary address. This field is not supported in most
    -- regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | The description of the job, provided at job creation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the Key Management Service (KMS) key
    -- associated with this job. This ARN was created using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- API action in KMS.
    kmsKeyARN :: Prelude.Maybe Prelude.Text,
    -- | The ID of the long-term pricing type for the device.
    longTermPricingId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) notification
    -- settings associated with a specific job. The @Notification@ object is
    -- returned as a part of the response syntax of the @DescribeJob@ action in
    -- the @JobMetadata@ data type.
    notification :: Prelude.Maybe Notification,
    -- | A value that defines the real-time status of a Snow device\'s data
    -- transfer while the device is at Amazon Web Services. This data is only
    -- available while a job has a @JobState@ value of @InProgress@, for both
    -- import and export jobs.
    dataTransferProgress :: Prelude.Maybe DataTransfer,
    -- | Allows you to securely operate and manage Snowcone devices remotely from
    -- outside of your internal network. When set to @INSTALLED_AUTOSTART@,
    -- remote management will automatically be available when the device
    -- arrives at your location. Otherwise, you need to use the Snowball Client
    -- to manage the device.
    remoteManagement :: Prelude.Maybe RemoteManagement,
    -- | The 39-character ID for the cluster, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the jobs.
    jobState :: Prelude.Maybe JobState,
    -- | The metadata associated with the tax documents required in your Amazon
    -- Web Services Region.
    taxDocuments :: Prelude.Maybe TaxDocuments,
    -- | An array of @S3Resource@ objects. Each @S3Resource@ object represents an
    -- Amazon S3 bucket that your transferred data will be exported from or
    -- imported into.
    resources :: Prelude.Maybe JobResource,
    -- | The Snow device capacity preference for this job, specified at job
    -- creation. In US regions, you can choose between 50 TB and 80 TB
    -- Snowballs. All other regions use 80 TB capacity Snowballs.
    --
    -- For more information, see
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
    snowballCapacityPreference :: Prelude.Maybe SnowballCapacity,
    -- | The ID for the address that you want the Snow device shipped to.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The type of device used with this job.
    snowballType :: Prelude.Maybe SnowballType,
    -- | The type of job.
    jobType :: Prelude.Maybe JobType,
    -- | Links to Amazon S3 presigned URLs for the job report and logs. For
    -- import jobs, the PDF job report becomes available at the end of the
    -- import process. For export jobs, your job report typically becomes
    -- available while the Snow device for your job part is being delivered to
    -- you.
    jobLogInfo :: Prelude.Maybe JobLogs,
    -- | Represents metadata and configuration settings for services on an Amazon
    -- Web Services Snow Family device.
    onDeviceServiceConfiguration :: Prelude.Maybe OnDeviceServiceConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shippingDetails', 'jobMetadata_shippingDetails' - A job\'s shipping information, including inbound and outbound tracking
-- numbers and shipping speed options.
--
-- 'roleARN', 'jobMetadata_roleARN' - The role ARN associated with this job. This ARN was created using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in Identity and Access Management.
--
-- 'deviceConfiguration', 'jobMetadata_deviceConfiguration' - Undocumented member.
--
-- 'creationDate', 'jobMetadata_creationDate' - The creation date for this job.
--
-- 'jobId', 'jobMetadata_jobId' - The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
--
-- 'forwardingAddressId', 'jobMetadata_forwardingAddressId' - The ID of the address that you want a job shipped to, after it will be
-- shipped to its primary address. This field is not supported in most
-- regions.
--
-- 'description', 'jobMetadata_description' - The description of the job, provided at job creation.
--
-- 'kmsKeyARN', 'jobMetadata_kmsKeyARN' - The Amazon Resource Name (ARN) for the Key Management Service (KMS) key
-- associated with this job. This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in KMS.
--
-- 'longTermPricingId', 'jobMetadata_longTermPricingId' - The ID of the long-term pricing type for the device.
--
-- 'notification', 'jobMetadata_notification' - The Amazon Simple Notification Service (Amazon SNS) notification
-- settings associated with a specific job. The @Notification@ object is
-- returned as a part of the response syntax of the @DescribeJob@ action in
-- the @JobMetadata@ data type.
--
-- 'dataTransferProgress', 'jobMetadata_dataTransferProgress' - A value that defines the real-time status of a Snow device\'s data
-- transfer while the device is at Amazon Web Services. This data is only
-- available while a job has a @JobState@ value of @InProgress@, for both
-- import and export jobs.
--
-- 'remoteManagement', 'jobMetadata_remoteManagement' - Allows you to securely operate and manage Snowcone devices remotely from
-- outside of your internal network. When set to @INSTALLED_AUTOSTART@,
-- remote management will automatically be available when the device
-- arrives at your location. Otherwise, you need to use the Snowball Client
-- to manage the device.
--
-- 'clusterId', 'jobMetadata_clusterId' - The 39-character ID for the cluster, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
--
-- 'jobState', 'jobMetadata_jobState' - The current status of the jobs.
--
-- 'taxDocuments', 'jobMetadata_taxDocuments' - The metadata associated with the tax documents required in your Amazon
-- Web Services Region.
--
-- 'resources', 'jobMetadata_resources' - An array of @S3Resource@ objects. Each @S3Resource@ object represents an
-- Amazon S3 bucket that your transferred data will be exported from or
-- imported into.
--
-- 'snowballCapacityPreference', 'jobMetadata_snowballCapacityPreference' - The Snow device capacity preference for this job, specified at job
-- creation. In US regions, you can choose between 50 TB and 80 TB
-- Snowballs. All other regions use 80 TB capacity Snowballs.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
--
-- 'addressId', 'jobMetadata_addressId' - The ID for the address that you want the Snow device shipped to.
--
-- 'snowballType', 'jobMetadata_snowballType' - The type of device used with this job.
--
-- 'jobType', 'jobMetadata_jobType' - The type of job.
--
-- 'jobLogInfo', 'jobMetadata_jobLogInfo' - Links to Amazon S3 presigned URLs for the job report and logs. For
-- import jobs, the PDF job report becomes available at the end of the
-- import process. For export jobs, your job report typically becomes
-- available while the Snow device for your job part is being delivered to
-- you.
--
-- 'onDeviceServiceConfiguration', 'jobMetadata_onDeviceServiceConfiguration' - Represents metadata and configuration settings for services on an Amazon
-- Web Services Snow Family device.
newJobMetadata ::
  JobMetadata
newJobMetadata =
  JobMetadata'
    { shippingDetails = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      deviceConfiguration = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      jobId = Prelude.Nothing,
      forwardingAddressId = Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyARN = Prelude.Nothing,
      longTermPricingId = Prelude.Nothing,
      notification = Prelude.Nothing,
      dataTransferProgress = Prelude.Nothing,
      remoteManagement = Prelude.Nothing,
      clusterId = Prelude.Nothing,
      jobState = Prelude.Nothing,
      taxDocuments = Prelude.Nothing,
      resources = Prelude.Nothing,
      snowballCapacityPreference = Prelude.Nothing,
      addressId = Prelude.Nothing,
      snowballType = Prelude.Nothing,
      jobType = Prelude.Nothing,
      jobLogInfo = Prelude.Nothing,
      onDeviceServiceConfiguration = Prelude.Nothing
    }

-- | A job\'s shipping information, including inbound and outbound tracking
-- numbers and shipping speed options.
jobMetadata_shippingDetails :: Lens.Lens' JobMetadata (Prelude.Maybe ShippingDetails)
jobMetadata_shippingDetails = Lens.lens (\JobMetadata' {shippingDetails} -> shippingDetails) (\s@JobMetadata' {} a -> s {shippingDetails = a} :: JobMetadata)

-- | The role ARN associated with this job. This ARN was created using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in Identity and Access Management.
jobMetadata_roleARN :: Lens.Lens' JobMetadata (Prelude.Maybe Prelude.Text)
jobMetadata_roleARN = Lens.lens (\JobMetadata' {roleARN} -> roleARN) (\s@JobMetadata' {} a -> s {roleARN = a} :: JobMetadata)

-- | Undocumented member.
jobMetadata_deviceConfiguration :: Lens.Lens' JobMetadata (Prelude.Maybe DeviceConfiguration)
jobMetadata_deviceConfiguration = Lens.lens (\JobMetadata' {deviceConfiguration} -> deviceConfiguration) (\s@JobMetadata' {} a -> s {deviceConfiguration = a} :: JobMetadata)

-- | The creation date for this job.
jobMetadata_creationDate :: Lens.Lens' JobMetadata (Prelude.Maybe Prelude.UTCTime)
jobMetadata_creationDate = Lens.lens (\JobMetadata' {creationDate} -> creationDate) (\s@JobMetadata' {} a -> s {creationDate = a} :: JobMetadata) Prelude.. Lens.mapping Data._Time

-- | The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
jobMetadata_jobId :: Lens.Lens' JobMetadata (Prelude.Maybe Prelude.Text)
jobMetadata_jobId = Lens.lens (\JobMetadata' {jobId} -> jobId) (\s@JobMetadata' {} a -> s {jobId = a} :: JobMetadata)

-- | The ID of the address that you want a job shipped to, after it will be
-- shipped to its primary address. This field is not supported in most
-- regions.
jobMetadata_forwardingAddressId :: Lens.Lens' JobMetadata (Prelude.Maybe Prelude.Text)
jobMetadata_forwardingAddressId = Lens.lens (\JobMetadata' {forwardingAddressId} -> forwardingAddressId) (\s@JobMetadata' {} a -> s {forwardingAddressId = a} :: JobMetadata)

-- | The description of the job, provided at job creation.
jobMetadata_description :: Lens.Lens' JobMetadata (Prelude.Maybe Prelude.Text)
jobMetadata_description = Lens.lens (\JobMetadata' {description} -> description) (\s@JobMetadata' {} a -> s {description = a} :: JobMetadata)

-- | The Amazon Resource Name (ARN) for the Key Management Service (KMS) key
-- associated with this job. This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in KMS.
jobMetadata_kmsKeyARN :: Lens.Lens' JobMetadata (Prelude.Maybe Prelude.Text)
jobMetadata_kmsKeyARN = Lens.lens (\JobMetadata' {kmsKeyARN} -> kmsKeyARN) (\s@JobMetadata' {} a -> s {kmsKeyARN = a} :: JobMetadata)

-- | The ID of the long-term pricing type for the device.
jobMetadata_longTermPricingId :: Lens.Lens' JobMetadata (Prelude.Maybe Prelude.Text)
jobMetadata_longTermPricingId = Lens.lens (\JobMetadata' {longTermPricingId} -> longTermPricingId) (\s@JobMetadata' {} a -> s {longTermPricingId = a} :: JobMetadata)

-- | The Amazon Simple Notification Service (Amazon SNS) notification
-- settings associated with a specific job. The @Notification@ object is
-- returned as a part of the response syntax of the @DescribeJob@ action in
-- the @JobMetadata@ data type.
jobMetadata_notification :: Lens.Lens' JobMetadata (Prelude.Maybe Notification)
jobMetadata_notification = Lens.lens (\JobMetadata' {notification} -> notification) (\s@JobMetadata' {} a -> s {notification = a} :: JobMetadata)

-- | A value that defines the real-time status of a Snow device\'s data
-- transfer while the device is at Amazon Web Services. This data is only
-- available while a job has a @JobState@ value of @InProgress@, for both
-- import and export jobs.
jobMetadata_dataTransferProgress :: Lens.Lens' JobMetadata (Prelude.Maybe DataTransfer)
jobMetadata_dataTransferProgress = Lens.lens (\JobMetadata' {dataTransferProgress} -> dataTransferProgress) (\s@JobMetadata' {} a -> s {dataTransferProgress = a} :: JobMetadata)

-- | Allows you to securely operate and manage Snowcone devices remotely from
-- outside of your internal network. When set to @INSTALLED_AUTOSTART@,
-- remote management will automatically be available when the device
-- arrives at your location. Otherwise, you need to use the Snowball Client
-- to manage the device.
jobMetadata_remoteManagement :: Lens.Lens' JobMetadata (Prelude.Maybe RemoteManagement)
jobMetadata_remoteManagement = Lens.lens (\JobMetadata' {remoteManagement} -> remoteManagement) (\s@JobMetadata' {} a -> s {remoteManagement = a} :: JobMetadata)

-- | The 39-character ID for the cluster, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
jobMetadata_clusterId :: Lens.Lens' JobMetadata (Prelude.Maybe Prelude.Text)
jobMetadata_clusterId = Lens.lens (\JobMetadata' {clusterId} -> clusterId) (\s@JobMetadata' {} a -> s {clusterId = a} :: JobMetadata)

-- | The current status of the jobs.
jobMetadata_jobState :: Lens.Lens' JobMetadata (Prelude.Maybe JobState)
jobMetadata_jobState = Lens.lens (\JobMetadata' {jobState} -> jobState) (\s@JobMetadata' {} a -> s {jobState = a} :: JobMetadata)

-- | The metadata associated with the tax documents required in your Amazon
-- Web Services Region.
jobMetadata_taxDocuments :: Lens.Lens' JobMetadata (Prelude.Maybe TaxDocuments)
jobMetadata_taxDocuments = Lens.lens (\JobMetadata' {taxDocuments} -> taxDocuments) (\s@JobMetadata' {} a -> s {taxDocuments = a} :: JobMetadata)

-- | An array of @S3Resource@ objects. Each @S3Resource@ object represents an
-- Amazon S3 bucket that your transferred data will be exported from or
-- imported into.
jobMetadata_resources :: Lens.Lens' JobMetadata (Prelude.Maybe JobResource)
jobMetadata_resources = Lens.lens (\JobMetadata' {resources} -> resources) (\s@JobMetadata' {} a -> s {resources = a} :: JobMetadata)

-- | The Snow device capacity preference for this job, specified at job
-- creation. In US regions, you can choose between 50 TB and 80 TB
-- Snowballs. All other regions use 80 TB capacity Snowballs.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
jobMetadata_snowballCapacityPreference :: Lens.Lens' JobMetadata (Prelude.Maybe SnowballCapacity)
jobMetadata_snowballCapacityPreference = Lens.lens (\JobMetadata' {snowballCapacityPreference} -> snowballCapacityPreference) (\s@JobMetadata' {} a -> s {snowballCapacityPreference = a} :: JobMetadata)

-- | The ID for the address that you want the Snow device shipped to.
jobMetadata_addressId :: Lens.Lens' JobMetadata (Prelude.Maybe Prelude.Text)
jobMetadata_addressId = Lens.lens (\JobMetadata' {addressId} -> addressId) (\s@JobMetadata' {} a -> s {addressId = a} :: JobMetadata)

-- | The type of device used with this job.
jobMetadata_snowballType :: Lens.Lens' JobMetadata (Prelude.Maybe SnowballType)
jobMetadata_snowballType = Lens.lens (\JobMetadata' {snowballType} -> snowballType) (\s@JobMetadata' {} a -> s {snowballType = a} :: JobMetadata)

-- | The type of job.
jobMetadata_jobType :: Lens.Lens' JobMetadata (Prelude.Maybe JobType)
jobMetadata_jobType = Lens.lens (\JobMetadata' {jobType} -> jobType) (\s@JobMetadata' {} a -> s {jobType = a} :: JobMetadata)

-- | Links to Amazon S3 presigned URLs for the job report and logs. For
-- import jobs, the PDF job report becomes available at the end of the
-- import process. For export jobs, your job report typically becomes
-- available while the Snow device for your job part is being delivered to
-- you.
jobMetadata_jobLogInfo :: Lens.Lens' JobMetadata (Prelude.Maybe JobLogs)
jobMetadata_jobLogInfo = Lens.lens (\JobMetadata' {jobLogInfo} -> jobLogInfo) (\s@JobMetadata' {} a -> s {jobLogInfo = a} :: JobMetadata)

-- | Represents metadata and configuration settings for services on an Amazon
-- Web Services Snow Family device.
jobMetadata_onDeviceServiceConfiguration :: Lens.Lens' JobMetadata (Prelude.Maybe OnDeviceServiceConfiguration)
jobMetadata_onDeviceServiceConfiguration = Lens.lens (\JobMetadata' {onDeviceServiceConfiguration} -> onDeviceServiceConfiguration) (\s@JobMetadata' {} a -> s {onDeviceServiceConfiguration = a} :: JobMetadata)

instance Data.FromJSON JobMetadata where
  parseJSON =
    Data.withObject
      "JobMetadata"
      ( \x ->
          JobMetadata'
            Prelude.<$> (x Data..:? "ShippingDetails")
            Prelude.<*> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..:? "DeviceConfiguration")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "ForwardingAddressId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "KmsKeyARN")
            Prelude.<*> (x Data..:? "LongTermPricingId")
            Prelude.<*> (x Data..:? "Notification")
            Prelude.<*> (x Data..:? "DataTransferProgress")
            Prelude.<*> (x Data..:? "RemoteManagement")
            Prelude.<*> (x Data..:? "ClusterId")
            Prelude.<*> (x Data..:? "JobState")
            Prelude.<*> (x Data..:? "TaxDocuments")
            Prelude.<*> (x Data..:? "Resources")
            Prelude.<*> (x Data..:? "SnowballCapacityPreference")
            Prelude.<*> (x Data..:? "AddressId")
            Prelude.<*> (x Data..:? "SnowballType")
            Prelude.<*> (x Data..:? "JobType")
            Prelude.<*> (x Data..:? "JobLogInfo")
            Prelude.<*> (x Data..:? "OnDeviceServiceConfiguration")
      )

instance Prelude.Hashable JobMetadata where
  hashWithSalt _salt JobMetadata' {..} =
    _salt `Prelude.hashWithSalt` shippingDetails
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` deviceConfiguration
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` forwardingAddressId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyARN
      `Prelude.hashWithSalt` longTermPricingId
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` dataTransferProgress
      `Prelude.hashWithSalt` remoteManagement
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` jobState
      `Prelude.hashWithSalt` taxDocuments
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` snowballCapacityPreference
      `Prelude.hashWithSalt` addressId
      `Prelude.hashWithSalt` snowballType
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` jobLogInfo
      `Prelude.hashWithSalt` onDeviceServiceConfiguration

instance Prelude.NFData JobMetadata where
  rnf JobMetadata' {..} =
    Prelude.rnf shippingDetails
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf deviceConfiguration
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf forwardingAddressId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyARN
      `Prelude.seq` Prelude.rnf longTermPricingId
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf dataTransferProgress
      `Prelude.seq` Prelude.rnf remoteManagement
      `Prelude.seq` Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf jobState
      `Prelude.seq` Prelude.rnf taxDocuments
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf
        snowballCapacityPreference
      `Prelude.seq` Prelude.rnf addressId
      `Prelude.seq` Prelude.rnf snowballType
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf jobLogInfo
      `Prelude.seq` Prelude.rnf
        onDeviceServiceConfiguration
