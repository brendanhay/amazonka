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
-- Module      : Network.AWS.Snowball.Types.ClusterMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Snowball.Types.ClusterState
import Network.AWS.Snowball.Types.JobResource
import Network.AWS.Snowball.Types.JobType
import Network.AWS.Snowball.Types.Notification
import Network.AWS.Snowball.Types.ShippingOption
import Network.AWS.Snowball.Types.SnowballType
import Network.AWS.Snowball.Types.TaxDocuments

-- | Contains metadata about a specific cluster.
--
-- /See:/ 'newClusterMetadata' smart constructor.
data ClusterMetadata = ClusterMetadata'
  { -- | The automatically generated ID for a cluster.
    clusterId :: Core.Maybe Core.Text,
    -- | The role ARN associated with this cluster. This ARN was created using
    -- the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in AWS Identity and Access Management (IAM).
    roleARN :: Core.Maybe Core.Text,
    -- | The shipping speed for each node in this cluster. This speed doesn\'t
    -- dictate how soon you\'ll get each device, rather it represents how
    -- quickly each device moves to its destination while in transit. Regional
    -- shipping speeds are as follows:
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
    shippingOption :: Core.Maybe ShippingOption,
    -- | The creation date for this cluster.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster.
    -- This ARN was created using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- API action in AWS Key Management Service (AWS KMS).
    kmsKeyARN :: Core.Maybe Core.Text,
    -- | The type of job for this cluster. Currently, the only job type supported
    -- for clusters is @LOCAL_USE@.
    jobType :: Core.Maybe JobType,
    -- | The arrays of JobResource objects that can include updated S3Resource
    -- objects or LambdaResource objects.
    resources :: Core.Maybe JobResource,
    -- | The tax documents required in your AWS Region.
    taxDocuments :: Core.Maybe TaxDocuments,
    -- | The type of AWS Snow device to use for this cluster.
    --
    -- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
    -- device type.
    snowballType :: Core.Maybe SnowballType,
    -- | The optional description of the cluster.
    description :: Core.Maybe Core.Text,
    -- | The automatically generated ID for a specific address.
    addressId :: Core.Maybe Core.Text,
    -- | The ID of the address that you want a cluster shipped to, after it will
    -- be shipped to its primary address. This field is not supported in most
    -- regions.
    forwardingAddressId :: Core.Maybe Core.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) notification
    -- settings for this cluster.
    notification :: Core.Maybe Notification,
    -- | The current status of the cluster.
    clusterState :: Core.Maybe ClusterState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'clusterMetadata_clusterId' - The automatically generated ID for a cluster.
--
-- 'roleARN', 'clusterMetadata_roleARN' - The role ARN associated with this cluster. This ARN was created using
-- the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in AWS Identity and Access Management (IAM).
--
-- 'shippingOption', 'clusterMetadata_shippingOption' - The shipping speed for each node in this cluster. This speed doesn\'t
-- dictate how soon you\'ll get each device, rather it represents how
-- quickly each device moves to its destination while in transit. Regional
-- shipping speeds are as follows:
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
--
-- 'creationDate', 'clusterMetadata_creationDate' - The creation date for this cluster.
--
-- 'kmsKeyARN', 'clusterMetadata_kmsKeyARN' - The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster.
-- This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in AWS Key Management Service (AWS KMS).
--
-- 'jobType', 'clusterMetadata_jobType' - The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
--
-- 'resources', 'clusterMetadata_resources' - The arrays of JobResource objects that can include updated S3Resource
-- objects or LambdaResource objects.
--
-- 'taxDocuments', 'clusterMetadata_taxDocuments' - The tax documents required in your AWS Region.
--
-- 'snowballType', 'clusterMetadata_snowballType' - The type of AWS Snow device to use for this cluster.
--
-- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
-- device type.
--
-- 'description', 'clusterMetadata_description' - The optional description of the cluster.
--
-- 'addressId', 'clusterMetadata_addressId' - The automatically generated ID for a specific address.
--
-- 'forwardingAddressId', 'clusterMetadata_forwardingAddressId' - The ID of the address that you want a cluster shipped to, after it will
-- be shipped to its primary address. This field is not supported in most
-- regions.
--
-- 'notification', 'clusterMetadata_notification' - The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
--
-- 'clusterState', 'clusterMetadata_clusterState' - The current status of the cluster.
newClusterMetadata ::
  ClusterMetadata
newClusterMetadata =
  ClusterMetadata'
    { clusterId = Core.Nothing,
      roleARN = Core.Nothing,
      shippingOption = Core.Nothing,
      creationDate = Core.Nothing,
      kmsKeyARN = Core.Nothing,
      jobType = Core.Nothing,
      resources = Core.Nothing,
      taxDocuments = Core.Nothing,
      snowballType = Core.Nothing,
      description = Core.Nothing,
      addressId = Core.Nothing,
      forwardingAddressId = Core.Nothing,
      notification = Core.Nothing,
      clusterState = Core.Nothing
    }

-- | The automatically generated ID for a cluster.
clusterMetadata_clusterId :: Lens.Lens' ClusterMetadata (Core.Maybe Core.Text)
clusterMetadata_clusterId = Lens.lens (\ClusterMetadata' {clusterId} -> clusterId) (\s@ClusterMetadata' {} a -> s {clusterId = a} :: ClusterMetadata)

-- | The role ARN associated with this cluster. This ARN was created using
-- the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in AWS Identity and Access Management (IAM).
clusterMetadata_roleARN :: Lens.Lens' ClusterMetadata (Core.Maybe Core.Text)
clusterMetadata_roleARN = Lens.lens (\ClusterMetadata' {roleARN} -> roleARN) (\s@ClusterMetadata' {} a -> s {roleARN = a} :: ClusterMetadata)

-- | The shipping speed for each node in this cluster. This speed doesn\'t
-- dictate how soon you\'ll get each device, rather it represents how
-- quickly each device moves to its destination while in transit. Regional
-- shipping speeds are as follows:
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
clusterMetadata_shippingOption :: Lens.Lens' ClusterMetadata (Core.Maybe ShippingOption)
clusterMetadata_shippingOption = Lens.lens (\ClusterMetadata' {shippingOption} -> shippingOption) (\s@ClusterMetadata' {} a -> s {shippingOption = a} :: ClusterMetadata)

-- | The creation date for this cluster.
clusterMetadata_creationDate :: Lens.Lens' ClusterMetadata (Core.Maybe Core.UTCTime)
clusterMetadata_creationDate = Lens.lens (\ClusterMetadata' {creationDate} -> creationDate) (\s@ClusterMetadata' {} a -> s {creationDate = a} :: ClusterMetadata) Core.. Lens.mapping Core._Time

-- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster.
-- This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in AWS Key Management Service (AWS KMS).
clusterMetadata_kmsKeyARN :: Lens.Lens' ClusterMetadata (Core.Maybe Core.Text)
clusterMetadata_kmsKeyARN = Lens.lens (\ClusterMetadata' {kmsKeyARN} -> kmsKeyARN) (\s@ClusterMetadata' {} a -> s {kmsKeyARN = a} :: ClusterMetadata)

-- | The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
clusterMetadata_jobType :: Lens.Lens' ClusterMetadata (Core.Maybe JobType)
clusterMetadata_jobType = Lens.lens (\ClusterMetadata' {jobType} -> jobType) (\s@ClusterMetadata' {} a -> s {jobType = a} :: ClusterMetadata)

-- | The arrays of JobResource objects that can include updated S3Resource
-- objects or LambdaResource objects.
clusterMetadata_resources :: Lens.Lens' ClusterMetadata (Core.Maybe JobResource)
clusterMetadata_resources = Lens.lens (\ClusterMetadata' {resources} -> resources) (\s@ClusterMetadata' {} a -> s {resources = a} :: ClusterMetadata)

-- | The tax documents required in your AWS Region.
clusterMetadata_taxDocuments :: Lens.Lens' ClusterMetadata (Core.Maybe TaxDocuments)
clusterMetadata_taxDocuments = Lens.lens (\ClusterMetadata' {taxDocuments} -> taxDocuments) (\s@ClusterMetadata' {} a -> s {taxDocuments = a} :: ClusterMetadata)

-- | The type of AWS Snow device to use for this cluster.
--
-- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
-- device type.
clusterMetadata_snowballType :: Lens.Lens' ClusterMetadata (Core.Maybe SnowballType)
clusterMetadata_snowballType = Lens.lens (\ClusterMetadata' {snowballType} -> snowballType) (\s@ClusterMetadata' {} a -> s {snowballType = a} :: ClusterMetadata)

-- | The optional description of the cluster.
clusterMetadata_description :: Lens.Lens' ClusterMetadata (Core.Maybe Core.Text)
clusterMetadata_description = Lens.lens (\ClusterMetadata' {description} -> description) (\s@ClusterMetadata' {} a -> s {description = a} :: ClusterMetadata)

-- | The automatically generated ID for a specific address.
clusterMetadata_addressId :: Lens.Lens' ClusterMetadata (Core.Maybe Core.Text)
clusterMetadata_addressId = Lens.lens (\ClusterMetadata' {addressId} -> addressId) (\s@ClusterMetadata' {} a -> s {addressId = a} :: ClusterMetadata)

-- | The ID of the address that you want a cluster shipped to, after it will
-- be shipped to its primary address. This field is not supported in most
-- regions.
clusterMetadata_forwardingAddressId :: Lens.Lens' ClusterMetadata (Core.Maybe Core.Text)
clusterMetadata_forwardingAddressId = Lens.lens (\ClusterMetadata' {forwardingAddressId} -> forwardingAddressId) (\s@ClusterMetadata' {} a -> s {forwardingAddressId = a} :: ClusterMetadata)

-- | The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
clusterMetadata_notification :: Lens.Lens' ClusterMetadata (Core.Maybe Notification)
clusterMetadata_notification = Lens.lens (\ClusterMetadata' {notification} -> notification) (\s@ClusterMetadata' {} a -> s {notification = a} :: ClusterMetadata)

-- | The current status of the cluster.
clusterMetadata_clusterState :: Lens.Lens' ClusterMetadata (Core.Maybe ClusterState)
clusterMetadata_clusterState = Lens.lens (\ClusterMetadata' {clusterState} -> clusterState) (\s@ClusterMetadata' {} a -> s {clusterState = a} :: ClusterMetadata)

instance Core.FromJSON ClusterMetadata where
  parseJSON =
    Core.withObject
      "ClusterMetadata"
      ( \x ->
          ClusterMetadata'
            Core.<$> (x Core..:? "ClusterId")
            Core.<*> (x Core..:? "RoleARN")
            Core.<*> (x Core..:? "ShippingOption")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "KmsKeyARN")
            Core.<*> (x Core..:? "JobType")
            Core.<*> (x Core..:? "Resources")
            Core.<*> (x Core..:? "TaxDocuments")
            Core.<*> (x Core..:? "SnowballType")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "AddressId")
            Core.<*> (x Core..:? "ForwardingAddressId")
            Core.<*> (x Core..:? "Notification")
            Core.<*> (x Core..:? "ClusterState")
      )

instance Core.Hashable ClusterMetadata

instance Core.NFData ClusterMetadata
