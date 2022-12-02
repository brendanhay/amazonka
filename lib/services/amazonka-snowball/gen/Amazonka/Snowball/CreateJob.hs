{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Snowball.CreateJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job to import or export data between Amazon S3 and your
-- on-premises data center. Your Amazon Web Services account must have the
-- right trust policies and permissions in place to create a job for a Snow
-- device. If you\'re creating a job for a node in a cluster, you only need
-- to provide the @clusterId@ value; the other job attributes are inherited
-- from the cluster.
--
-- Only the Snowball; Edge device type is supported when ordering clustered
-- jobs.
--
-- The device capacity is optional.
--
-- Availability of device types differ by Amazon Web Services Region. For
-- more information about Region availability, see
-- <https://aws.amazon.com/about-aws/global-infrastructure/regional-product-services/?p=ngi&loc=4 Amazon Web Services Regional Services>.
--
-- __Snow Family devices and their capacities.__
--
-- -   Snow Family device type: __SNC1_SSD__
--
--     -   Capacity: T14
--
--     -   Description: Snowcone
--
-- -   Snow Family device type: __SNC1_HDD__
--
--     -   Capacity: T8
--
--     -   Description: Snowcone
--
-- -   Device type: __EDGE_S__
--
--     -   Capacity: T98
--
--     -   Description: Snowball Edge Storage Optimized for data transfer
--         only
--
-- -   Device type: __EDGE_CG__
--
--     -   Capacity: T42
--
--     -   Description: Snowball Edge Compute Optimized with GPU
--
-- -   Device type: __EDGE_C__
--
--     -   Capacity: T42
--
--     -   Description: Snowball Edge Compute Optimized without GPU
--
-- -   Device type: __EDGE__
--
--     -   Capacity: T100
--
--     -   Description: Snowball Edge Storage Optimized with EC2 Compute
--
-- -   Device type: __V3_5C__
--
--     -   Capacity: T32
--
--     -   Description: Snowball Edge Compute Optimized without GPU
--
-- -   Device type: __STANDARD__
--
--     -   Capacity: T50
--
--     -   Description: Original Snowball device
--
--         This device is only available in the Ningxia, Beijing, and
--         Singapore Amazon Web Services Region
--
-- -   Device type: __STANDARD__
--
--     -   Capacity: T80
--
--     -   Description: Original Snowball device
--
--         This device is only available in the Ningxia, Beijing, and
--         Singapore Amazon Web Services Region.
module Amazonka.Snowball.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_roleARN,
    createJob_deviceConfiguration,
    createJob_forwardingAddressId,
    createJob_description,
    createJob_kmsKeyARN,
    createJob_longTermPricingId,
    createJob_notification,
    createJob_remoteManagement,
    createJob_clusterId,
    createJob_taxDocuments,
    createJob_resources,
    createJob_snowballCapacityPreference,
    createJob_addressId,
    createJob_snowballType,
    createJob_jobType,
    createJob_shippingOption,
    createJob_onDeviceServiceConfiguration,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_jobId,
    createJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | The @RoleARN@ that you want to associate with this job. @RoleArn@s are
    -- created using the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- Identity and Access Management (IAM) API action.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Defines the device configuration for an Snowcone job.
    --
    -- For more information, see
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
    deviceConfiguration :: Prelude.Maybe DeviceConfiguration,
    -- | The forwarding address ID for a job. This field is not supported in most
    -- Regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | Defines an optional description of this specific job, for example
    -- @Important Photos 2016-08-11@.
    description :: Prelude.Maybe Prelude.Text,
    -- | The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@s
    -- are created using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- Key Management Service (KMS) API action.
    kmsKeyARN :: Prelude.Maybe Prelude.Text,
    -- | The ID of the long-term pricing type for the device.
    longTermPricingId :: Prelude.Maybe Prelude.Text,
    -- | Defines the Amazon Simple Notification Service (Amazon SNS) notification
    -- settings for this job.
    notification :: Prelude.Maybe Notification,
    -- | Allows you to securely operate and manage Snowcone devices remotely from
    -- outside of your internal network. When set to @INSTALLED_AUTOSTART@,
    -- remote management will automatically be available when the device
    -- arrives at your location. Otherwise, you need to use the Snowball Client
    -- to manage the device.
    remoteManagement :: Prelude.Maybe RemoteManagement,
    -- | The ID of a cluster. If you\'re creating a job for a node in a cluster,
    -- you need to provide only this @clusterId@ value. The other job
    -- attributes are inherited from the cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The tax documents required in your Amazon Web Services Region.
    taxDocuments :: Prelude.Maybe TaxDocuments,
    -- | Defines the Amazon S3 buckets associated with this job.
    --
    -- With @IMPORT@ jobs, you specify the bucket or buckets that your
    -- transferred data will be imported into.
    --
    -- With @EXPORT@ jobs, you specify the bucket or buckets that your
    -- transferred data will be exported from. Optionally, you can also specify
    -- a @KeyRange@ value. If you choose to export a range, you define the
    -- length of the range by providing either an inclusive @BeginMarker@
    -- value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary
    -- sorted.
    resources :: Prelude.Maybe JobResource,
    -- | If your job is being created in one of the US regions, you have the
    -- option of specifying what size Snow device you\'d like for this job. In
    -- all other regions, Snowballs come with 80 TB in storage capacity.
    --
    -- For more information, see
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
    snowballCapacityPreference :: Prelude.Maybe SnowballCapacity,
    -- | The ID for the address that you want the Snow device shipped to.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The type of Snow Family devices to use for this job.
    --
    -- For cluster jobs, Amazon Web Services Snow Family currently supports
    -- only the @EDGE@ device type.
    --
    -- The type of Amazon Web Services Snow device to use for this job.
    -- Currently, the only supported device type for cluster jobs is @EDGE@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options>
    -- in the Snowball Edge Developer Guide.
    --
    -- For more information, see
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
    snowballType :: Prelude.Maybe SnowballType,
    -- | Defines the type of job that you\'re creating.
    jobType :: Prelude.Maybe JobType,
    -- | The shipping speed for this job. This speed doesn\'t dictate how soon
    -- you\'ll get the Snow device, rather it represents how quickly the Snow
    -- device moves to its destination while in transit. Regional shipping
    -- speeds are as follows:
    --
    -- -   In Australia, you have access to express shipping. Typically, Snow
    --     devices shipped express are delivered in about a day.
    --
    -- -   In the European Union (EU), you have access to express shipping.
    --     Typically, Snow devices shipped express are delivered in about a
    --     day. In addition, most countries in the EU have access to standard
    --     shipping, which typically takes less than a week, one way.
    --
    -- -   In India, Snow devices are delivered in one to seven days.
    --
    -- -   In the US, you have access to one-day shipping and two-day shipping.
    shippingOption :: Prelude.Maybe ShippingOption,
    -- | Specifies the service or services on the Snow Family device that your
    -- transferred data will be exported from or imported into. Amazon Web
    -- Services Snow Family supports Amazon S3 and NFS (Network File System)
    -- and the Amazon Web Services Storage Gateway service Tape Gateway type.
    onDeviceServiceConfiguration :: Prelude.Maybe OnDeviceServiceConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'createJob_roleARN' - The @RoleARN@ that you want to associate with this job. @RoleArn@s are
-- created using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- Identity and Access Management (IAM) API action.
--
-- 'deviceConfiguration', 'createJob_deviceConfiguration' - Defines the device configuration for an Snowcone job.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
--
-- 'forwardingAddressId', 'createJob_forwardingAddressId' - The forwarding address ID for a job. This field is not supported in most
-- Regions.
--
-- 'description', 'createJob_description' - Defines an optional description of this specific job, for example
-- @Important Photos 2016-08-11@.
--
-- 'kmsKeyARN', 'createJob_kmsKeyARN' - The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@s
-- are created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- Key Management Service (KMS) API action.
--
-- 'longTermPricingId', 'createJob_longTermPricingId' - The ID of the long-term pricing type for the device.
--
-- 'notification', 'createJob_notification' - Defines the Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this job.
--
-- 'remoteManagement', 'createJob_remoteManagement' - Allows you to securely operate and manage Snowcone devices remotely from
-- outside of your internal network. When set to @INSTALLED_AUTOSTART@,
-- remote management will automatically be available when the device
-- arrives at your location. Otherwise, you need to use the Snowball Client
-- to manage the device.
--
-- 'clusterId', 'createJob_clusterId' - The ID of a cluster. If you\'re creating a job for a node in a cluster,
-- you need to provide only this @clusterId@ value. The other job
-- attributes are inherited from the cluster.
--
-- 'taxDocuments', 'createJob_taxDocuments' - The tax documents required in your Amazon Web Services Region.
--
-- 'resources', 'createJob_resources' - Defines the Amazon S3 buckets associated with this job.
--
-- With @IMPORT@ jobs, you specify the bucket or buckets that your
-- transferred data will be imported into.
--
-- With @EXPORT@ jobs, you specify the bucket or buckets that your
-- transferred data will be exported from. Optionally, you can also specify
-- a @KeyRange@ value. If you choose to export a range, you define the
-- length of the range by providing either an inclusive @BeginMarker@
-- value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary
-- sorted.
--
-- 'snowballCapacityPreference', 'createJob_snowballCapacityPreference' - If your job is being created in one of the US regions, you have the
-- option of specifying what size Snow device you\'d like for this job. In
-- all other regions, Snowballs come with 80 TB in storage capacity.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
--
-- 'addressId', 'createJob_addressId' - The ID for the address that you want the Snow device shipped to.
--
-- 'snowballType', 'createJob_snowballType' - The type of Snow Family devices to use for this job.
--
-- For cluster jobs, Amazon Web Services Snow Family currently supports
-- only the @EDGE@ device type.
--
-- The type of Amazon Web Services Snow device to use for this job.
-- Currently, the only supported device type for cluster jobs is @EDGE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options>
-- in the Snowball Edge Developer Guide.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
--
-- 'jobType', 'createJob_jobType' - Defines the type of job that you\'re creating.
--
-- 'shippingOption', 'createJob_shippingOption' - The shipping speed for this job. This speed doesn\'t dictate how soon
-- you\'ll get the Snow device, rather it represents how quickly the Snow
-- device moves to its destination while in transit. Regional shipping
-- speeds are as follows:
--
-- -   In Australia, you have access to express shipping. Typically, Snow
--     devices shipped express are delivered in about a day.
--
-- -   In the European Union (EU), you have access to express shipping.
--     Typically, Snow devices shipped express are delivered in about a
--     day. In addition, most countries in the EU have access to standard
--     shipping, which typically takes less than a week, one way.
--
-- -   In India, Snow devices are delivered in one to seven days.
--
-- -   In the US, you have access to one-day shipping and two-day shipping.
--
-- 'onDeviceServiceConfiguration', 'createJob_onDeviceServiceConfiguration' - Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. Amazon Web
-- Services Snow Family supports Amazon S3 and NFS (Network File System)
-- and the Amazon Web Services Storage Gateway service Tape Gateway type.
newCreateJob ::
  CreateJob
newCreateJob =
  CreateJob'
    { roleARN = Prelude.Nothing,
      deviceConfiguration = Prelude.Nothing,
      forwardingAddressId = Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyARN = Prelude.Nothing,
      longTermPricingId = Prelude.Nothing,
      notification = Prelude.Nothing,
      remoteManagement = Prelude.Nothing,
      clusterId = Prelude.Nothing,
      taxDocuments = Prelude.Nothing,
      resources = Prelude.Nothing,
      snowballCapacityPreference = Prelude.Nothing,
      addressId = Prelude.Nothing,
      snowballType = Prelude.Nothing,
      jobType = Prelude.Nothing,
      shippingOption = Prelude.Nothing,
      onDeviceServiceConfiguration = Prelude.Nothing
    }

-- | The @RoleARN@ that you want to associate with this job. @RoleArn@s are
-- created using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- Identity and Access Management (IAM) API action.
createJob_roleARN :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_roleARN = Lens.lens (\CreateJob' {roleARN} -> roleARN) (\s@CreateJob' {} a -> s {roleARN = a} :: CreateJob)

-- | Defines the device configuration for an Snowcone job.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
createJob_deviceConfiguration :: Lens.Lens' CreateJob (Prelude.Maybe DeviceConfiguration)
createJob_deviceConfiguration = Lens.lens (\CreateJob' {deviceConfiguration} -> deviceConfiguration) (\s@CreateJob' {} a -> s {deviceConfiguration = a} :: CreateJob)

-- | The forwarding address ID for a job. This field is not supported in most
-- Regions.
createJob_forwardingAddressId :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_forwardingAddressId = Lens.lens (\CreateJob' {forwardingAddressId} -> forwardingAddressId) (\s@CreateJob' {} a -> s {forwardingAddressId = a} :: CreateJob)

-- | Defines an optional description of this specific job, for example
-- @Important Photos 2016-08-11@.
createJob_description :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_description = Lens.lens (\CreateJob' {description} -> description) (\s@CreateJob' {} a -> s {description = a} :: CreateJob)

-- | The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@s
-- are created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- Key Management Service (KMS) API action.
createJob_kmsKeyARN :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_kmsKeyARN = Lens.lens (\CreateJob' {kmsKeyARN} -> kmsKeyARN) (\s@CreateJob' {} a -> s {kmsKeyARN = a} :: CreateJob)

-- | The ID of the long-term pricing type for the device.
createJob_longTermPricingId :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_longTermPricingId = Lens.lens (\CreateJob' {longTermPricingId} -> longTermPricingId) (\s@CreateJob' {} a -> s {longTermPricingId = a} :: CreateJob)

-- | Defines the Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this job.
createJob_notification :: Lens.Lens' CreateJob (Prelude.Maybe Notification)
createJob_notification = Lens.lens (\CreateJob' {notification} -> notification) (\s@CreateJob' {} a -> s {notification = a} :: CreateJob)

-- | Allows you to securely operate and manage Snowcone devices remotely from
-- outside of your internal network. When set to @INSTALLED_AUTOSTART@,
-- remote management will automatically be available when the device
-- arrives at your location. Otherwise, you need to use the Snowball Client
-- to manage the device.
createJob_remoteManagement :: Lens.Lens' CreateJob (Prelude.Maybe RemoteManagement)
createJob_remoteManagement = Lens.lens (\CreateJob' {remoteManagement} -> remoteManagement) (\s@CreateJob' {} a -> s {remoteManagement = a} :: CreateJob)

-- | The ID of a cluster. If you\'re creating a job for a node in a cluster,
-- you need to provide only this @clusterId@ value. The other job
-- attributes are inherited from the cluster.
createJob_clusterId :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_clusterId = Lens.lens (\CreateJob' {clusterId} -> clusterId) (\s@CreateJob' {} a -> s {clusterId = a} :: CreateJob)

-- | The tax documents required in your Amazon Web Services Region.
createJob_taxDocuments :: Lens.Lens' CreateJob (Prelude.Maybe TaxDocuments)
createJob_taxDocuments = Lens.lens (\CreateJob' {taxDocuments} -> taxDocuments) (\s@CreateJob' {} a -> s {taxDocuments = a} :: CreateJob)

-- | Defines the Amazon S3 buckets associated with this job.
--
-- With @IMPORT@ jobs, you specify the bucket or buckets that your
-- transferred data will be imported into.
--
-- With @EXPORT@ jobs, you specify the bucket or buckets that your
-- transferred data will be exported from. Optionally, you can also specify
-- a @KeyRange@ value. If you choose to export a range, you define the
-- length of the range by providing either an inclusive @BeginMarker@
-- value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary
-- sorted.
createJob_resources :: Lens.Lens' CreateJob (Prelude.Maybe JobResource)
createJob_resources = Lens.lens (\CreateJob' {resources} -> resources) (\s@CreateJob' {} a -> s {resources = a} :: CreateJob)

-- | If your job is being created in one of the US regions, you have the
-- option of specifying what size Snow device you\'d like for this job. In
-- all other regions, Snowballs come with 80 TB in storage capacity.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
createJob_snowballCapacityPreference :: Lens.Lens' CreateJob (Prelude.Maybe SnowballCapacity)
createJob_snowballCapacityPreference = Lens.lens (\CreateJob' {snowballCapacityPreference} -> snowballCapacityPreference) (\s@CreateJob' {} a -> s {snowballCapacityPreference = a} :: CreateJob)

-- | The ID for the address that you want the Snow device shipped to.
createJob_addressId :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_addressId = Lens.lens (\CreateJob' {addressId} -> addressId) (\s@CreateJob' {} a -> s {addressId = a} :: CreateJob)

-- | The type of Snow Family devices to use for this job.
--
-- For cluster jobs, Amazon Web Services Snow Family currently supports
-- only the @EDGE@ device type.
--
-- The type of Amazon Web Services Snow device to use for this job.
-- Currently, the only supported device type for cluster jobs is @EDGE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options>
-- in the Snowball Edge Developer Guide.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
createJob_snowballType :: Lens.Lens' CreateJob (Prelude.Maybe SnowballType)
createJob_snowballType = Lens.lens (\CreateJob' {snowballType} -> snowballType) (\s@CreateJob' {} a -> s {snowballType = a} :: CreateJob)

-- | Defines the type of job that you\'re creating.
createJob_jobType :: Lens.Lens' CreateJob (Prelude.Maybe JobType)
createJob_jobType = Lens.lens (\CreateJob' {jobType} -> jobType) (\s@CreateJob' {} a -> s {jobType = a} :: CreateJob)

-- | The shipping speed for this job. This speed doesn\'t dictate how soon
-- you\'ll get the Snow device, rather it represents how quickly the Snow
-- device moves to its destination while in transit. Regional shipping
-- speeds are as follows:
--
-- -   In Australia, you have access to express shipping. Typically, Snow
--     devices shipped express are delivered in about a day.
--
-- -   In the European Union (EU), you have access to express shipping.
--     Typically, Snow devices shipped express are delivered in about a
--     day. In addition, most countries in the EU have access to standard
--     shipping, which typically takes less than a week, one way.
--
-- -   In India, Snow devices are delivered in one to seven days.
--
-- -   In the US, you have access to one-day shipping and two-day shipping.
createJob_shippingOption :: Lens.Lens' CreateJob (Prelude.Maybe ShippingOption)
createJob_shippingOption = Lens.lens (\CreateJob' {shippingOption} -> shippingOption) (\s@CreateJob' {} a -> s {shippingOption = a} :: CreateJob)

-- | Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. Amazon Web
-- Services Snow Family supports Amazon S3 and NFS (Network File System)
-- and the Amazon Web Services Storage Gateway service Tape Gateway type.
createJob_onDeviceServiceConfiguration :: Lens.Lens' CreateJob (Prelude.Maybe OnDeviceServiceConfiguration)
createJob_onDeviceServiceConfiguration = Lens.lens (\CreateJob' {onDeviceServiceConfiguration} -> onDeviceServiceConfiguration) (\s@CreateJob' {} a -> s {onDeviceServiceConfiguration = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJob where
  hashWithSalt _salt CreateJob' {..} =
    _salt `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` deviceConfiguration
      `Prelude.hashWithSalt` forwardingAddressId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyARN
      `Prelude.hashWithSalt` longTermPricingId
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` remoteManagement
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` taxDocuments
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` snowballCapacityPreference
      `Prelude.hashWithSalt` addressId
      `Prelude.hashWithSalt` snowballType
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` shippingOption
      `Prelude.hashWithSalt` onDeviceServiceConfiguration

instance Prelude.NFData CreateJob where
  rnf CreateJob' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf deviceConfiguration
      `Prelude.seq` Prelude.rnf forwardingAddressId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyARN
      `Prelude.seq` Prelude.rnf longTermPricingId
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf remoteManagement
      `Prelude.seq` Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf taxDocuments
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf snowballCapacityPreference
      `Prelude.seq` Prelude.rnf addressId
      `Prelude.seq` Prelude.rnf snowballType
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf shippingOption
      `Prelude.seq` Prelude.rnf
        onDeviceServiceConfiguration

instance Data.ToHeaders CreateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.CreateJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleARN" Data..=) Prelude.<$> roleARN,
            ("DeviceConfiguration" Data..=)
              Prelude.<$> deviceConfiguration,
            ("ForwardingAddressId" Data..=)
              Prelude.<$> forwardingAddressId,
            ("Description" Data..=) Prelude.<$> description,
            ("KmsKeyARN" Data..=) Prelude.<$> kmsKeyARN,
            ("LongTermPricingId" Data..=)
              Prelude.<$> longTermPricingId,
            ("Notification" Data..=) Prelude.<$> notification,
            ("RemoteManagement" Data..=)
              Prelude.<$> remoteManagement,
            ("ClusterId" Data..=) Prelude.<$> clusterId,
            ("TaxDocuments" Data..=) Prelude.<$> taxDocuments,
            ("Resources" Data..=) Prelude.<$> resources,
            ("SnowballCapacityPreference" Data..=)
              Prelude.<$> snowballCapacityPreference,
            ("AddressId" Data..=) Prelude.<$> addressId,
            ("SnowballType" Data..=) Prelude.<$> snowballType,
            ("JobType" Data..=) Prelude.<$> jobType,
            ("ShippingOption" Data..=)
              Prelude.<$> shippingOption,
            ("OnDeviceServiceConfiguration" Data..=)
              Prelude.<$> onDeviceServiceConfiguration
          ]
      )

instance Data.ToPath CreateJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'createJobResponse_jobId' - The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
newCreateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
createJobResponse_jobId :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_jobId = Lens.lens (\CreateJobResponse' {jobId} -> jobId) (\s@CreateJobResponse' {} a -> s {jobId = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse where
  rnf CreateJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
