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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createCluster_kmsKeyARN,
    createCluster_remoteManagement,
    createCluster_notification,
    createCluster_forwardingAddressId,
    createCluster_onDeviceServiceConfiguration,
    createCluster_description,
    createCluster_taxDocuments,
    createCluster_jobType,
    createCluster_resources,
    createCluster_addressId,
    createCluster_roleARN,
    createCluster_snowballType,
    createCluster_shippingOption,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_clusterId,
    createClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | The @KmsKeyARN@ value that you want to associate with this cluster.
    -- @KmsKeyARN@ values are created by using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- API action in AWS Key Management Service (AWS KMS).
    kmsKeyARN :: Prelude.Maybe Prelude.Text,
    -- | Allows you to securely operate and manage Snow devices in a cluster
    -- remotely from outside of your internal network. When set to
    -- @INSTALLED_AUTOSTART@, remote management will automatically be available
    -- when the device arrives at your location. Otherwise, you need to use the
    -- Snowball Client to manage the device.
    remoteManagement :: Prelude.Maybe RemoteManagement,
    -- | The Amazon Simple Notification Service (Amazon SNS) notification
    -- settings for this cluster.
    notification :: Prelude.Maybe Notification,
    -- | The forwarding address ID for a cluster. This field is not supported in
    -- most regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the service or services on the Snow Family device that your
    -- transferred data will be exported from or imported into. AWS Snow Family
    -- supports Amazon S3 and NFS (Network File System).
    onDeviceServiceConfiguration :: Prelude.Maybe OnDeviceServiceConfiguration,
    -- | An optional description of this specific cluster, for example
    -- @Environmental Data Cluster-01@.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tax documents required in your AWS Region.
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
    -- | The resources associated with the cluster job. These resources include
    -- Amazon S3 buckets and optional AWS Lambda functions written in the
    -- Python language.
    resources :: JobResource,
    -- | The ID for the address that you want the cluster shipped to.
    addressId :: Prelude.Text,
    -- | The @RoleARN@ that you want to associate with this cluster. @RoleArn@
    -- values are created by using the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in AWS Identity and Access Management (IAM).
    roleARN :: Prelude.Text,
    -- | The type of AWS Snow Family device to use for this cluster.
    --
    -- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
    -- device type.
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
-- 'kmsKeyARN', 'createCluster_kmsKeyARN' - The @KmsKeyARN@ value that you want to associate with this cluster.
-- @KmsKeyARN@ values are created by using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in AWS Key Management Service (AWS KMS).
--
-- 'remoteManagement', 'createCluster_remoteManagement' - Allows you to securely operate and manage Snow devices in a cluster
-- remotely from outside of your internal network. When set to
-- @INSTALLED_AUTOSTART@, remote management will automatically be available
-- when the device arrives at your location. Otherwise, you need to use the
-- Snowball Client to manage the device.
--
-- 'notification', 'createCluster_notification' - The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
--
-- 'forwardingAddressId', 'createCluster_forwardingAddressId' - The forwarding address ID for a cluster. This field is not supported in
-- most regions.
--
-- 'onDeviceServiceConfiguration', 'createCluster_onDeviceServiceConfiguration' - Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. AWS Snow Family
-- supports Amazon S3 and NFS (Network File System).
--
-- 'description', 'createCluster_description' - An optional description of this specific cluster, for example
-- @Environmental Data Cluster-01@.
--
-- 'taxDocuments', 'createCluster_taxDocuments' - The tax documents required in your AWS Region.
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
-- 'resources', 'createCluster_resources' - The resources associated with the cluster job. These resources include
-- Amazon S3 buckets and optional AWS Lambda functions written in the
-- Python language.
--
-- 'addressId', 'createCluster_addressId' - The ID for the address that you want the cluster shipped to.
--
-- 'roleARN', 'createCluster_roleARN' - The @RoleARN@ that you want to associate with this cluster. @RoleArn@
-- values are created by using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in AWS Identity and Access Management (IAM).
--
-- 'snowballType', 'createCluster_snowballType' - The type of AWS Snow Family device to use for this cluster.
--
-- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
-- device type.
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
  -- | 'resources'
  JobResource ->
  -- | 'addressId'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  -- | 'snowballType'
  SnowballType ->
  -- | 'shippingOption'
  ShippingOption ->
  CreateCluster
newCreateCluster
  pJobType_
  pResources_
  pAddressId_
  pRoleARN_
  pSnowballType_
  pShippingOption_ =
    CreateCluster'
      { kmsKeyARN = Prelude.Nothing,
        remoteManagement = Prelude.Nothing,
        notification = Prelude.Nothing,
        forwardingAddressId = Prelude.Nothing,
        onDeviceServiceConfiguration = Prelude.Nothing,
        description = Prelude.Nothing,
        taxDocuments = Prelude.Nothing,
        jobType = pJobType_,
        resources = pResources_,
        addressId = pAddressId_,
        roleARN = pRoleARN_,
        snowballType = pSnowballType_,
        shippingOption = pShippingOption_
      }

-- | The @KmsKeyARN@ value that you want to associate with this cluster.
-- @KmsKeyARN@ values are created by using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in AWS Key Management Service (AWS KMS).
createCluster_kmsKeyARN :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_kmsKeyARN = Lens.lens (\CreateCluster' {kmsKeyARN} -> kmsKeyARN) (\s@CreateCluster' {} a -> s {kmsKeyARN = a} :: CreateCluster)

-- | Allows you to securely operate and manage Snow devices in a cluster
-- remotely from outside of your internal network. When set to
-- @INSTALLED_AUTOSTART@, remote management will automatically be available
-- when the device arrives at your location. Otherwise, you need to use the
-- Snowball Client to manage the device.
createCluster_remoteManagement :: Lens.Lens' CreateCluster (Prelude.Maybe RemoteManagement)
createCluster_remoteManagement = Lens.lens (\CreateCluster' {remoteManagement} -> remoteManagement) (\s@CreateCluster' {} a -> s {remoteManagement = a} :: CreateCluster)

-- | The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
createCluster_notification :: Lens.Lens' CreateCluster (Prelude.Maybe Notification)
createCluster_notification = Lens.lens (\CreateCluster' {notification} -> notification) (\s@CreateCluster' {} a -> s {notification = a} :: CreateCluster)

-- | The forwarding address ID for a cluster. This field is not supported in
-- most regions.
createCluster_forwardingAddressId :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_forwardingAddressId = Lens.lens (\CreateCluster' {forwardingAddressId} -> forwardingAddressId) (\s@CreateCluster' {} a -> s {forwardingAddressId = a} :: CreateCluster)

-- | Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. AWS Snow Family
-- supports Amazon S3 and NFS (Network File System).
createCluster_onDeviceServiceConfiguration :: Lens.Lens' CreateCluster (Prelude.Maybe OnDeviceServiceConfiguration)
createCluster_onDeviceServiceConfiguration = Lens.lens (\CreateCluster' {onDeviceServiceConfiguration} -> onDeviceServiceConfiguration) (\s@CreateCluster' {} a -> s {onDeviceServiceConfiguration = a} :: CreateCluster)

-- | An optional description of this specific cluster, for example
-- @Environmental Data Cluster-01@.
createCluster_description :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_description = Lens.lens (\CreateCluster' {description} -> description) (\s@CreateCluster' {} a -> s {description = a} :: CreateCluster)

-- | The tax documents required in your AWS Region.
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

-- | The resources associated with the cluster job. These resources include
-- Amazon S3 buckets and optional AWS Lambda functions written in the
-- Python language.
createCluster_resources :: Lens.Lens' CreateCluster JobResource
createCluster_resources = Lens.lens (\CreateCluster' {resources} -> resources) (\s@CreateCluster' {} a -> s {resources = a} :: CreateCluster)

-- | The ID for the address that you want the cluster shipped to.
createCluster_addressId :: Lens.Lens' CreateCluster Prelude.Text
createCluster_addressId = Lens.lens (\CreateCluster' {addressId} -> addressId) (\s@CreateCluster' {} a -> s {addressId = a} :: CreateCluster)

-- | The @RoleARN@ that you want to associate with this cluster. @RoleArn@
-- values are created by using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in AWS Identity and Access Management (IAM).
createCluster_roleARN :: Lens.Lens' CreateCluster Prelude.Text
createCluster_roleARN = Lens.lens (\CreateCluster' {roleARN} -> roleARN) (\s@CreateCluster' {} a -> s {roleARN = a} :: CreateCluster)

-- | The type of AWS Snow Family device to use for this cluster.
--
-- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
-- device type.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Core..?> "ClusterId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster

instance Prelude.NFData CreateCluster

instance Core.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.CreateCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKeyARN" Core..=) Prelude.<$> kmsKeyARN,
            ("RemoteManagement" Core..=)
              Prelude.<$> remoteManagement,
            ("Notification" Core..=) Prelude.<$> notification,
            ("ForwardingAddressId" Core..=)
              Prelude.<$> forwardingAddressId,
            ("OnDeviceServiceConfiguration" Core..=)
              Prelude.<$> onDeviceServiceConfiguration,
            ("Description" Core..=) Prelude.<$> description,
            ("TaxDocuments" Core..=) Prelude.<$> taxDocuments,
            Prelude.Just ("JobType" Core..= jobType),
            Prelude.Just ("Resources" Core..= resources),
            Prelude.Just ("AddressId" Core..= addressId),
            Prelude.Just ("RoleARN" Core..= roleARN),
            Prelude.Just ("SnowballType" Core..= snowballType),
            Prelude.Just
              ("ShippingOption" Core..= shippingOption)
          ]
      )

instance Core.ToPath CreateCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The automatically generated ID for a cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
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
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { clusterId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The automatically generated ID for a cluster.
createClusterResponse_clusterId :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Prelude.Text)
createClusterResponse_clusterId = Lens.lens (\CreateClusterResponse' {clusterId} -> clusterId) (\s@CreateClusterResponse' {} a -> s {clusterId = a} :: CreateClusterResponse)

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse
