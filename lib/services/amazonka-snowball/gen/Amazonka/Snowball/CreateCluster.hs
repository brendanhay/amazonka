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
-- Module      : Amazonka.Snowball.CreateCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty cluster. Each cluster supports five nodes. You use the
-- CreateJob action separately to create the jobs for each of these nodes.
-- The cluster does not ship until these five node jobs have been created.
module Amazonka.Snowball.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_description,
    createCluster_forceCreateJobs,
    createCluster_forwardingAddressId,
    createCluster_initialClusterSize,
    createCluster_kmsKeyARN,
    createCluster_longTermPricingIds,
    createCluster_notification,
    createCluster_onDeviceServiceConfiguration,
    createCluster_remoteManagement,
    createCluster_resources,
    createCluster_roleARN,
    createCluster_snowballCapacityPreference,
    createCluster_taxDocuments,
    createCluster_jobType,
    createCluster_addressId,
    createCluster_snowballType,
    createCluster_shippingOption,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_clusterId,
    createClusterResponse_jobListEntries,
    createClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | An optional description of this specific cluster, for example
    -- @Environmental Data Cluster-01@.
    description :: Prelude.Maybe Prelude.Text,
    -- | Force to create cluster when user attempts to overprovision or
    -- underprovision a cluster. A cluster is overprovisioned or
    -- underprovisioned if the initial size of the cluster is more
    -- (overprovisioned) or less (underprovisioned) than what needed to meet
    -- capacity requirement specified with @OnDeviceServiceConfiguration@.
    forceCreateJobs :: Prelude.Maybe Prelude.Bool,
    -- | The forwarding address ID for a cluster. This field is not supported in
    -- most regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | If provided, each job will be automatically created and associated with
    -- the new cluster. If not provided, will be treated as 0.
    initialClusterSize :: Prelude.Maybe Prelude.Natural,
    -- | The @KmsKeyARN@ value that you want to associate with this cluster.
    -- @KmsKeyARN@ values are created by using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- API action in Key Management Service (KMS).
    kmsKeyARN :: Prelude.Maybe Prelude.Text,
    -- | Lists long-term pricing id that will be used to associate with jobs
    -- automatically created for the new cluster.
    longTermPricingIds :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Simple Notification Service (Amazon SNS) notification
    -- settings for this cluster.
    notification :: Prelude.Maybe Notification,
    -- | Specifies the service or services on the Snow Family device that your
    -- transferred data will be exported from or imported into. Amazon Web
    -- Services Snow Family device clusters support Amazon S3 and NFS (Network
    -- File System).
    onDeviceServiceConfiguration :: Prelude.Maybe OnDeviceServiceConfiguration,
    -- | Allows you to securely operate and manage Snow devices in a cluster
    -- remotely from outside of your internal network. When set to
    -- @INSTALLED_AUTOSTART@, remote management will automatically be available
    -- when the device arrives at your location. Otherwise, you need to use the
    -- Snowball Client to manage the device.
    remoteManagement :: Prelude.Maybe RemoteManagement,
    -- | The resources associated with the cluster job. These resources include
    -- Amazon S3 buckets and optional Lambda functions written in the Python
    -- language.
    resources :: Prelude.Maybe JobResource,
    -- | The @RoleARN@ that you want to associate with this cluster. @RoleArn@
    -- values are created by using the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in Identity and Access Management (IAM).
    roleARN :: Prelude.Maybe Prelude.Text,
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
    -- | The tax documents required in your Amazon Web Services Region.
    taxDocuments :: Prelude.Maybe TaxDocuments,
    -- | The type of job for this cluster. Currently, the only job type supported
    -- for clusters is @LOCAL_USE@.
    --
    -- For more information, see
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
    jobType :: JobType,
    -- | The ID for the address that you want the cluster shipped to.
    addressId :: Prelude.Text,
    -- | The type of Snow Family devices to use for this cluster.
    --
    -- For cluster jobs, Amazon Web Services Snow Family currently supports
    -- only the @EDGE@ device type.
    --
    -- For more information, see
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
    -- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
    -- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
    snowballType :: SnowballType,
    -- | The shipping speed for each node in this cluster. This speed doesn\'t
    -- dictate how soon you\'ll get each Snowball Edge device, rather it
    -- represents how quickly each device moves to its destination while in
    -- transit. Regional shipping speeds are as follows:
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
    -- -   In the United States of America (US), you have access to one-day
    --     shipping and two-day shipping.
    --
    -- -   In Australia, you have access to express shipping. Typically,
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
    shippingOption :: ShippingOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createCluster_description' - An optional description of this specific cluster, for example
-- @Environmental Data Cluster-01@.
--
-- 'forceCreateJobs', 'createCluster_forceCreateJobs' - Force to create cluster when user attempts to overprovision or
-- underprovision a cluster. A cluster is overprovisioned or
-- underprovisioned if the initial size of the cluster is more
-- (overprovisioned) or less (underprovisioned) than what needed to meet
-- capacity requirement specified with @OnDeviceServiceConfiguration@.
--
-- 'forwardingAddressId', 'createCluster_forwardingAddressId' - The forwarding address ID for a cluster. This field is not supported in
-- most regions.
--
-- 'initialClusterSize', 'createCluster_initialClusterSize' - If provided, each job will be automatically created and associated with
-- the new cluster. If not provided, will be treated as 0.
--
-- 'kmsKeyARN', 'createCluster_kmsKeyARN' - The @KmsKeyARN@ value that you want to associate with this cluster.
-- @KmsKeyARN@ values are created by using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in Key Management Service (KMS).
--
-- 'longTermPricingIds', 'createCluster_longTermPricingIds' - Lists long-term pricing id that will be used to associate with jobs
-- automatically created for the new cluster.
--
-- 'notification', 'createCluster_notification' - The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
--
-- 'onDeviceServiceConfiguration', 'createCluster_onDeviceServiceConfiguration' - Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. Amazon Web
-- Services Snow Family device clusters support Amazon S3 and NFS (Network
-- File System).
--
-- 'remoteManagement', 'createCluster_remoteManagement' - Allows you to securely operate and manage Snow devices in a cluster
-- remotely from outside of your internal network. When set to
-- @INSTALLED_AUTOSTART@, remote management will automatically be available
-- when the device arrives at your location. Otherwise, you need to use the
-- Snowball Client to manage the device.
--
-- 'resources', 'createCluster_resources' - The resources associated with the cluster job. These resources include
-- Amazon S3 buckets and optional Lambda functions written in the Python
-- language.
--
-- 'roleARN', 'createCluster_roleARN' - The @RoleARN@ that you want to associate with this cluster. @RoleArn@
-- values are created by using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in Identity and Access Management (IAM).
--
-- 'snowballCapacityPreference', 'createCluster_snowballCapacityPreference' - If your job is being created in one of the US regions, you have the
-- option of specifying what size Snow device you\'d like for this job. In
-- all other regions, Snowballs come with 80 TB in storage capacity.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
--
-- 'taxDocuments', 'createCluster_taxDocuments' - The tax documents required in your Amazon Web Services Region.
--
-- 'jobType', 'createCluster_jobType' - The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
--
-- 'addressId', 'createCluster_addressId' - The ID for the address that you want the cluster shipped to.
--
-- 'snowballType', 'createCluster_snowballType' - The type of Snow Family devices to use for this cluster.
--
-- For cluster jobs, Amazon Web Services Snow Family currently supports
-- only the @EDGE@ device type.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
--
-- 'shippingOption', 'createCluster_shippingOption' - The shipping speed for each node in this cluster. This speed doesn\'t
-- dictate how soon you\'ll get each Snowball Edge device, rather it
-- represents how quickly each device moves to its destination while in
-- transit. Regional shipping speeds are as follows:
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
-- -   In the United States of America (US), you have access to one-day
--     shipping and two-day shipping.
--
-- -   In Australia, you have access to express shipping. Typically,
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
newCreateCluster ::
  -- | 'jobType'
  JobType ->
  -- | 'addressId'
  Prelude.Text ->
  -- | 'snowballType'
  SnowballType ->
  -- | 'shippingOption'
  ShippingOption ->
  CreateCluster
newCreateCluster
  pJobType_
  pAddressId_
  pSnowballType_
  pShippingOption_ =
    CreateCluster'
      { description = Prelude.Nothing,
        forceCreateJobs = Prelude.Nothing,
        forwardingAddressId = Prelude.Nothing,
        initialClusterSize = Prelude.Nothing,
        kmsKeyARN = Prelude.Nothing,
        longTermPricingIds = Prelude.Nothing,
        notification = Prelude.Nothing,
        onDeviceServiceConfiguration = Prelude.Nothing,
        remoteManagement = Prelude.Nothing,
        resources = Prelude.Nothing,
        roleARN = Prelude.Nothing,
        snowballCapacityPreference = Prelude.Nothing,
        taxDocuments = Prelude.Nothing,
        jobType = pJobType_,
        addressId = pAddressId_,
        snowballType = pSnowballType_,
        shippingOption = pShippingOption_
      }

-- | An optional description of this specific cluster, for example
-- @Environmental Data Cluster-01@.
createCluster_description :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_description = Lens.lens (\CreateCluster' {description} -> description) (\s@CreateCluster' {} a -> s {description = a} :: CreateCluster)

-- | Force to create cluster when user attempts to overprovision or
-- underprovision a cluster. A cluster is overprovisioned or
-- underprovisioned if the initial size of the cluster is more
-- (overprovisioned) or less (underprovisioned) than what needed to meet
-- capacity requirement specified with @OnDeviceServiceConfiguration@.
createCluster_forceCreateJobs :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Bool)
createCluster_forceCreateJobs = Lens.lens (\CreateCluster' {forceCreateJobs} -> forceCreateJobs) (\s@CreateCluster' {} a -> s {forceCreateJobs = a} :: CreateCluster)

-- | The forwarding address ID for a cluster. This field is not supported in
-- most regions.
createCluster_forwardingAddressId :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_forwardingAddressId = Lens.lens (\CreateCluster' {forwardingAddressId} -> forwardingAddressId) (\s@CreateCluster' {} a -> s {forwardingAddressId = a} :: CreateCluster)

-- | If provided, each job will be automatically created and associated with
-- the new cluster. If not provided, will be treated as 0.
createCluster_initialClusterSize :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Natural)
createCluster_initialClusterSize = Lens.lens (\CreateCluster' {initialClusterSize} -> initialClusterSize) (\s@CreateCluster' {} a -> s {initialClusterSize = a} :: CreateCluster)

-- | The @KmsKeyARN@ value that you want to associate with this cluster.
-- @KmsKeyARN@ values are created by using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in Key Management Service (KMS).
createCluster_kmsKeyARN :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_kmsKeyARN = Lens.lens (\CreateCluster' {kmsKeyARN} -> kmsKeyARN) (\s@CreateCluster' {} a -> s {kmsKeyARN = a} :: CreateCluster)

-- | Lists long-term pricing id that will be used to associate with jobs
-- automatically created for the new cluster.
createCluster_longTermPricingIds :: Lens.Lens' CreateCluster (Prelude.Maybe [Prelude.Text])
createCluster_longTermPricingIds = Lens.lens (\CreateCluster' {longTermPricingIds} -> longTermPricingIds) (\s@CreateCluster' {} a -> s {longTermPricingIds = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
createCluster_notification :: Lens.Lens' CreateCluster (Prelude.Maybe Notification)
createCluster_notification = Lens.lens (\CreateCluster' {notification} -> notification) (\s@CreateCluster' {} a -> s {notification = a} :: CreateCluster)

-- | Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. Amazon Web
-- Services Snow Family device clusters support Amazon S3 and NFS (Network
-- File System).
createCluster_onDeviceServiceConfiguration :: Lens.Lens' CreateCluster (Prelude.Maybe OnDeviceServiceConfiguration)
createCluster_onDeviceServiceConfiguration = Lens.lens (\CreateCluster' {onDeviceServiceConfiguration} -> onDeviceServiceConfiguration) (\s@CreateCluster' {} a -> s {onDeviceServiceConfiguration = a} :: CreateCluster)

-- | Allows you to securely operate and manage Snow devices in a cluster
-- remotely from outside of your internal network. When set to
-- @INSTALLED_AUTOSTART@, remote management will automatically be available
-- when the device arrives at your location. Otherwise, you need to use the
-- Snowball Client to manage the device.
createCluster_remoteManagement :: Lens.Lens' CreateCluster (Prelude.Maybe RemoteManagement)
createCluster_remoteManagement = Lens.lens (\CreateCluster' {remoteManagement} -> remoteManagement) (\s@CreateCluster' {} a -> s {remoteManagement = a} :: CreateCluster)

-- | The resources associated with the cluster job. These resources include
-- Amazon S3 buckets and optional Lambda functions written in the Python
-- language.
createCluster_resources :: Lens.Lens' CreateCluster (Prelude.Maybe JobResource)
createCluster_resources = Lens.lens (\CreateCluster' {resources} -> resources) (\s@CreateCluster' {} a -> s {resources = a} :: CreateCluster)

-- | The @RoleARN@ that you want to associate with this cluster. @RoleArn@
-- values are created by using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in Identity and Access Management (IAM).
createCluster_roleARN :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_roleARN = Lens.lens (\CreateCluster' {roleARN} -> roleARN) (\s@CreateCluster' {} a -> s {roleARN = a} :: CreateCluster)

-- | If your job is being created in one of the US regions, you have the
-- option of specifying what size Snow device you\'d like for this job. In
-- all other regions, Snowballs come with 80 TB in storage capacity.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
createCluster_snowballCapacityPreference :: Lens.Lens' CreateCluster (Prelude.Maybe SnowballCapacity)
createCluster_snowballCapacityPreference = Lens.lens (\CreateCluster' {snowballCapacityPreference} -> snowballCapacityPreference) (\s@CreateCluster' {} a -> s {snowballCapacityPreference = a} :: CreateCluster)

-- | The tax documents required in your Amazon Web Services Region.
createCluster_taxDocuments :: Lens.Lens' CreateCluster (Prelude.Maybe TaxDocuments)
createCluster_taxDocuments = Lens.lens (\CreateCluster' {taxDocuments} -> taxDocuments) (\s@CreateCluster' {} a -> s {taxDocuments = a} :: CreateCluster)

-- | The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
createCluster_jobType :: Lens.Lens' CreateCluster JobType
createCluster_jobType = Lens.lens (\CreateCluster' {jobType} -> jobType) (\s@CreateCluster' {} a -> s {jobType = a} :: CreateCluster)

-- | The ID for the address that you want the cluster shipped to.
createCluster_addressId :: Lens.Lens' CreateCluster Prelude.Text
createCluster_addressId = Lens.lens (\CreateCluster' {addressId} -> addressId) (\s@CreateCluster' {} a -> s {addressId = a} :: CreateCluster)

-- | The type of Snow Family devices to use for this cluster.
--
-- For cluster jobs, Amazon Web Services Snow Family currently supports
-- only the @EDGE@ device type.
--
-- For more information, see
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/snowcone-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/ or
-- \"https:\/\/docs.aws.amazon.com\/snowball\/latest\/developer-guide\/snow-device-types.html\"
-- (Snow Family Devices and Capacity) in the /Snowcone User Guide/.
createCluster_snowballType :: Lens.Lens' CreateCluster SnowballType
createCluster_snowballType = Lens.lens (\CreateCluster' {snowballType} -> snowballType) (\s@CreateCluster' {} a -> s {snowballType = a} :: CreateCluster)

-- | The shipping speed for each node in this cluster. This speed doesn\'t
-- dictate how soon you\'ll get each Snowball Edge device, rather it
-- represents how quickly each device moves to its destination while in
-- transit. Regional shipping speeds are as follows:
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
-- -   In the United States of America (US), you have access to one-day
--     shipping and two-day shipping.
--
-- -   In Australia, you have access to express shipping. Typically,
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
createCluster_shippingOption :: Lens.Lens' CreateCluster ShippingOption
createCluster_shippingOption = Lens.lens (\CreateCluster' {shippingOption} -> shippingOption) (\s@CreateCluster' {} a -> s {shippingOption = a} :: CreateCluster)

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Data..?> "ClusterId")
            Prelude.<*> (x Data..?> "JobListEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster where
  hashWithSalt _salt CreateCluster' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` forceCreateJobs
      `Prelude.hashWithSalt` forwardingAddressId
      `Prelude.hashWithSalt` initialClusterSize
      `Prelude.hashWithSalt` kmsKeyARN
      `Prelude.hashWithSalt` longTermPricingIds
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` onDeviceServiceConfiguration
      `Prelude.hashWithSalt` remoteManagement
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` snowballCapacityPreference
      `Prelude.hashWithSalt` taxDocuments
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` addressId
      `Prelude.hashWithSalt` snowballType
      `Prelude.hashWithSalt` shippingOption

instance Prelude.NFData CreateCluster where
  rnf CreateCluster' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf forceCreateJobs
      `Prelude.seq` Prelude.rnf forwardingAddressId
      `Prelude.seq` Prelude.rnf initialClusterSize
      `Prelude.seq` Prelude.rnf kmsKeyARN
      `Prelude.seq` Prelude.rnf longTermPricingIds
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf onDeviceServiceConfiguration
      `Prelude.seq` Prelude.rnf remoteManagement
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf snowballCapacityPreference
      `Prelude.seq` Prelude.rnf taxDocuments
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf addressId
      `Prelude.seq` Prelude.rnf snowballType
      `Prelude.seq` Prelude.rnf shippingOption

instance Data.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.CreateCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("ForceCreateJobs" Data..=)
              Prelude.<$> forceCreateJobs,
            ("ForwardingAddressId" Data..=)
              Prelude.<$> forwardingAddressId,
            ("InitialClusterSize" Data..=)
              Prelude.<$> initialClusterSize,
            ("KmsKeyARN" Data..=) Prelude.<$> kmsKeyARN,
            ("LongTermPricingIds" Data..=)
              Prelude.<$> longTermPricingIds,
            ("Notification" Data..=) Prelude.<$> notification,
            ("OnDeviceServiceConfiguration" Data..=)
              Prelude.<$> onDeviceServiceConfiguration,
            ("RemoteManagement" Data..=)
              Prelude.<$> remoteManagement,
            ("Resources" Data..=) Prelude.<$> resources,
            ("RoleARN" Data..=) Prelude.<$> roleARN,
            ("SnowballCapacityPreference" Data..=)
              Prelude.<$> snowballCapacityPreference,
            ("TaxDocuments" Data..=) Prelude.<$> taxDocuments,
            Prelude.Just ("JobType" Data..= jobType),
            Prelude.Just ("AddressId" Data..= addressId),
            Prelude.Just ("SnowballType" Data..= snowballType),
            Prelude.Just
              ("ShippingOption" Data..= shippingOption)
          ]
      )

instance Data.ToPath CreateCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The automatically generated ID for a cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | List of jobs created for this cluster. For syntax, see
    -- <https://docs.aws.amazon.com/snowball/latest/api-reference/API_ListJobs.html#API_ListJobs_ResponseSyntax ListJobsResult$JobListEntries>
    -- in this guide.
    jobListEntries :: Prelude.Maybe [JobListEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'createClusterResponse_clusterId' - The automatically generated ID for a cluster.
--
-- 'jobListEntries', 'createClusterResponse_jobListEntries' - List of jobs created for this cluster. For syntax, see
-- <https://docs.aws.amazon.com/snowball/latest/api-reference/API_ListJobs.html#API_ListJobs_ResponseSyntax ListJobsResult$JobListEntries>
-- in this guide.
--
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { clusterId = Prelude.Nothing,
      jobListEntries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The automatically generated ID for a cluster.
createClusterResponse_clusterId :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Prelude.Text)
createClusterResponse_clusterId = Lens.lens (\CreateClusterResponse' {clusterId} -> clusterId) (\s@CreateClusterResponse' {} a -> s {clusterId = a} :: CreateClusterResponse)

-- | List of jobs created for this cluster. For syntax, see
-- <https://docs.aws.amazon.com/snowball/latest/api-reference/API_ListJobs.html#API_ListJobs_ResponseSyntax ListJobsResult$JobListEntries>
-- in this guide.
createClusterResponse_jobListEntries :: Lens.Lens' CreateClusterResponse (Prelude.Maybe [JobListEntry])
createClusterResponse_jobListEntries = Lens.lens (\CreateClusterResponse' {jobListEntries} -> jobListEntries) (\s@CreateClusterResponse' {} a -> s {jobListEntries = a} :: CreateClusterResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse where
  rnf CreateClusterResponse' {..} =
    Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf jobListEntries
      `Prelude.seq` Prelude.rnf httpStatus
