{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Snowball.CreateCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty cluster. Each cluster supports five nodes. You use the
-- CreateJob action separately to create the jobs for each of these nodes.
-- The cluster does not ship until these five node jobs have been created.
module Network.AWS.Snowball.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_kmsKeyARN,
    createCluster_taxDocuments,
    createCluster_snowballType,
    createCluster_description,
    createCluster_forwardingAddressId,
    createCluster_notification,
    createCluster_jobType,
    createCluster_resources,
    createCluster_addressId,
    createCluster_roleARN,
    createCluster_shippingOption,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_clusterId,
    createClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | The @KmsKeyARN@ value that you want to associate with this cluster.
    -- @KmsKeyARN@ values are created by using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- API action in AWS Key Management Service (AWS KMS).
    kmsKeyARN :: Prelude.Maybe Prelude.Text,
    -- | The tax documents required in your AWS Region.
    taxDocuments :: Prelude.Maybe TaxDocuments,
    -- | The type of AWS Snow Family device to use for this cluster.
    --
    -- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
    -- device type.
    snowballType :: Prelude.Maybe SnowballType,
    -- | An optional description of this specific cluster, for example
    -- @Environmental Data Cluster-01@.
    description :: Prelude.Maybe Prelude.Text,
    -- | The forwarding address ID for a cluster. This field is not supported in
    -- most regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) notification
    -- settings for this cluster.
    notification :: Prelude.Maybe Notification,
    -- | The type of job for this cluster. Currently, the only job type supported
    -- for clusters is @LOCAL_USE@.
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
    -- -   In India, Snow device are delivered in one to seven days.
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
    -- -   In India, Snow device are delivered in one to seven days.
    --
    -- -   In the US, you have access to one-day shipping and two-day shipping.
    shippingOption :: ShippingOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'taxDocuments', 'createCluster_taxDocuments' - The tax documents required in your AWS Region.
--
-- 'snowballType', 'createCluster_snowballType' - The type of AWS Snow Family device to use for this cluster.
--
-- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
-- device type.
--
-- 'description', 'createCluster_description' - An optional description of this specific cluster, for example
-- @Environmental Data Cluster-01@.
--
-- 'forwardingAddressId', 'createCluster_forwardingAddressId' - The forwarding address ID for a cluster. This field is not supported in
-- most regions.
--
-- 'notification', 'createCluster_notification' - The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
--
-- 'jobType', 'createCluster_jobType' - The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
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
-- -   In India, Snow device are delivered in one to seven days.
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
-- -   In India, Snow device are delivered in one to seven days.
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
  -- | 'shippingOption'
  ShippingOption ->
  CreateCluster
newCreateCluster
  pJobType_
  pResources_
  pAddressId_
  pRoleARN_
  pShippingOption_ =
    CreateCluster'
      { kmsKeyARN = Prelude.Nothing,
        taxDocuments = Prelude.Nothing,
        snowballType = Prelude.Nothing,
        description = Prelude.Nothing,
        forwardingAddressId = Prelude.Nothing,
        notification = Prelude.Nothing,
        jobType = pJobType_,
        resources = pResources_,
        addressId = pAddressId_,
        roleARN = pRoleARN_,
        shippingOption = pShippingOption_
      }

-- | The @KmsKeyARN@ value that you want to associate with this cluster.
-- @KmsKeyARN@ values are created by using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in AWS Key Management Service (AWS KMS).
createCluster_kmsKeyARN :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_kmsKeyARN = Lens.lens (\CreateCluster' {kmsKeyARN} -> kmsKeyARN) (\s@CreateCluster' {} a -> s {kmsKeyARN = a} :: CreateCluster)

-- | The tax documents required in your AWS Region.
createCluster_taxDocuments :: Lens.Lens' CreateCluster (Prelude.Maybe TaxDocuments)
createCluster_taxDocuments = Lens.lens (\CreateCluster' {taxDocuments} -> taxDocuments) (\s@CreateCluster' {} a -> s {taxDocuments = a} :: CreateCluster)

-- | The type of AWS Snow Family device to use for this cluster.
--
-- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
-- device type.
createCluster_snowballType :: Lens.Lens' CreateCluster (Prelude.Maybe SnowballType)
createCluster_snowballType = Lens.lens (\CreateCluster' {snowballType} -> snowballType) (\s@CreateCluster' {} a -> s {snowballType = a} :: CreateCluster)

-- | An optional description of this specific cluster, for example
-- @Environmental Data Cluster-01@.
createCluster_description :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_description = Lens.lens (\CreateCluster' {description} -> description) (\s@CreateCluster' {} a -> s {description = a} :: CreateCluster)

-- | The forwarding address ID for a cluster. This field is not supported in
-- most regions.
createCluster_forwardingAddressId :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_forwardingAddressId = Lens.lens (\CreateCluster' {forwardingAddressId} -> forwardingAddressId) (\s@CreateCluster' {} a -> s {forwardingAddressId = a} :: CreateCluster)

-- | The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
createCluster_notification :: Lens.Lens' CreateCluster (Prelude.Maybe Notification)
createCluster_notification = Lens.lens (\CreateCluster' {notification} -> notification) (\s@CreateCluster' {} a -> s {notification = a} :: CreateCluster)

-- | The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
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
-- -   In India, Snow device are delivered in one to seven days.
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
-- -   In India, Snow device are delivered in one to seven days.
--
-- -   In the US, you have access to one-day shipping and two-day shipping.
createCluster_shippingOption :: Lens.Lens' CreateCluster ShippingOption
createCluster_shippingOption = Lens.lens (\CreateCluster' {shippingOption} -> shippingOption) (\s@CreateCluster' {} a -> s {shippingOption = a} :: CreateCluster)

instance Prelude.AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Prelude..?> "ClusterId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster

instance Prelude.NFData CreateCluster

instance Prelude.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.CreateCluster" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KmsKeyARN" Prelude..=) Prelude.<$> kmsKeyARN,
            ("TaxDocuments" Prelude..=) Prelude.<$> taxDocuments,
            ("SnowballType" Prelude..=) Prelude.<$> snowballType,
            ("Description" Prelude..=) Prelude.<$> description,
            ("ForwardingAddressId" Prelude..=)
              Prelude.<$> forwardingAddressId,
            ("Notification" Prelude..=) Prelude.<$> notification,
            Prelude.Just ("JobType" Prelude..= jobType),
            Prelude.Just ("Resources" Prelude..= resources),
            Prelude.Just ("AddressId" Prelude..= addressId),
            Prelude.Just ("RoleARN" Prelude..= roleARN),
            Prelude.Just
              ("ShippingOption" Prelude..= shippingOption)
          ]
      )

instance Prelude.ToPath CreateCluster where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | The automatically generated ID for a cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
