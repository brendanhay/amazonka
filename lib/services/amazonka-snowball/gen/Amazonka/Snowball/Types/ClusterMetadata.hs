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
-- Module      : Amazonka.Snowball.Types.ClusterMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.ClusterMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.ClusterState
import Amazonka.Snowball.Types.JobResource
import Amazonka.Snowball.Types.JobType
import Amazonka.Snowball.Types.Notification
import Amazonka.Snowball.Types.OnDeviceServiceConfiguration
import Amazonka.Snowball.Types.ShippingOption
import Amazonka.Snowball.Types.SnowballType
import Amazonka.Snowball.Types.TaxDocuments

-- | Contains metadata about a specific cluster.
--
-- /See:/ 'newClusterMetadata' smart constructor.
data ClusterMetadata = ClusterMetadata'
  { -- | The role ARN associated with this cluster. This ARN was created using
    -- the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in Identity and Access Management (IAM).
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The creation date for this cluster.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The ID of the address that you want a cluster shipped to, after it will
    -- be shipped to its primary address. This field is not supported in most
    -- regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | The optional description of the cluster.
    description :: Prelude.Maybe Prelude.Text,
    -- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster.
    -- This ARN was created using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- API action in Key Management Service (KMS.
    kmsKeyARN :: Prelude.Maybe Prelude.Text,
    -- | The current status of the cluster.
    clusterState :: Prelude.Maybe ClusterState,
    -- | The Amazon Simple Notification Service (Amazon SNS) notification
    -- settings for this cluster.
    notification :: Prelude.Maybe Notification,
    -- | The automatically generated ID for a cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The tax documents required in your Amazon Web Services Region.
    taxDocuments :: Prelude.Maybe TaxDocuments,
    -- | The arrays of JobResource objects that can include updated S3Resource
    -- objects or LambdaResource objects.
    resources :: Prelude.Maybe JobResource,
    -- | The automatically generated ID for a specific address.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The type of Snowcone device to use for this cluster.
    --
    -- For cluster jobs, Amazon Web Services Snow Family currently supports
    -- only the @EDGE@ device type.
    snowballType :: Prelude.Maybe SnowballType,
    -- | The type of job for this cluster. Currently, the only job type supported
    -- for clusters is @LOCAL_USE@.
    jobType :: Prelude.Maybe JobType,
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
    shippingOption :: Prelude.Maybe ShippingOption,
    -- | Represents metadata and configuration settings for services on an Amazon
    -- Web Services Snow Family device.
    onDeviceServiceConfiguration :: Prelude.Maybe OnDeviceServiceConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'clusterMetadata_roleARN' - The role ARN associated with this cluster. This ARN was created using
-- the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in Identity and Access Management (IAM).
--
-- 'creationDate', 'clusterMetadata_creationDate' - The creation date for this cluster.
--
-- 'forwardingAddressId', 'clusterMetadata_forwardingAddressId' - The ID of the address that you want a cluster shipped to, after it will
-- be shipped to its primary address. This field is not supported in most
-- regions.
--
-- 'description', 'clusterMetadata_description' - The optional description of the cluster.
--
-- 'kmsKeyARN', 'clusterMetadata_kmsKeyARN' - The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster.
-- This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in Key Management Service (KMS.
--
-- 'clusterState', 'clusterMetadata_clusterState' - The current status of the cluster.
--
-- 'notification', 'clusterMetadata_notification' - The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
--
-- 'clusterId', 'clusterMetadata_clusterId' - The automatically generated ID for a cluster.
--
-- 'taxDocuments', 'clusterMetadata_taxDocuments' - The tax documents required in your Amazon Web Services Region.
--
-- 'resources', 'clusterMetadata_resources' - The arrays of JobResource objects that can include updated S3Resource
-- objects or LambdaResource objects.
--
-- 'addressId', 'clusterMetadata_addressId' - The automatically generated ID for a specific address.
--
-- 'snowballType', 'clusterMetadata_snowballType' - The type of Snowcone device to use for this cluster.
--
-- For cluster jobs, Amazon Web Services Snow Family currently supports
-- only the @EDGE@ device type.
--
-- 'jobType', 'clusterMetadata_jobType' - The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
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
-- 'onDeviceServiceConfiguration', 'clusterMetadata_onDeviceServiceConfiguration' - Represents metadata and configuration settings for services on an Amazon
-- Web Services Snow Family device.
newClusterMetadata ::
  ClusterMetadata
newClusterMetadata =
  ClusterMetadata'
    { roleARN = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      forwardingAddressId = Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyARN = Prelude.Nothing,
      clusterState = Prelude.Nothing,
      notification = Prelude.Nothing,
      clusterId = Prelude.Nothing,
      taxDocuments = Prelude.Nothing,
      resources = Prelude.Nothing,
      addressId = Prelude.Nothing,
      snowballType = Prelude.Nothing,
      jobType = Prelude.Nothing,
      shippingOption = Prelude.Nothing,
      onDeviceServiceConfiguration = Prelude.Nothing
    }

-- | The role ARN associated with this cluster. This ARN was created using
-- the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in Identity and Access Management (IAM).
clusterMetadata_roleARN :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_roleARN = Lens.lens (\ClusterMetadata' {roleARN} -> roleARN) (\s@ClusterMetadata' {} a -> s {roleARN = a} :: ClusterMetadata)

-- | The creation date for this cluster.
clusterMetadata_creationDate :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.UTCTime)
clusterMetadata_creationDate = Lens.lens (\ClusterMetadata' {creationDate} -> creationDate) (\s@ClusterMetadata' {} a -> s {creationDate = a} :: ClusterMetadata) Prelude.. Lens.mapping Core._Time

-- | The ID of the address that you want a cluster shipped to, after it will
-- be shipped to its primary address. This field is not supported in most
-- regions.
clusterMetadata_forwardingAddressId :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_forwardingAddressId = Lens.lens (\ClusterMetadata' {forwardingAddressId} -> forwardingAddressId) (\s@ClusterMetadata' {} a -> s {forwardingAddressId = a} :: ClusterMetadata)

-- | The optional description of the cluster.
clusterMetadata_description :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_description = Lens.lens (\ClusterMetadata' {description} -> description) (\s@ClusterMetadata' {} a -> s {description = a} :: ClusterMetadata)

-- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster.
-- This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in Key Management Service (KMS.
clusterMetadata_kmsKeyARN :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_kmsKeyARN = Lens.lens (\ClusterMetadata' {kmsKeyARN} -> kmsKeyARN) (\s@ClusterMetadata' {} a -> s {kmsKeyARN = a} :: ClusterMetadata)

-- | The current status of the cluster.
clusterMetadata_clusterState :: Lens.Lens' ClusterMetadata (Prelude.Maybe ClusterState)
clusterMetadata_clusterState = Lens.lens (\ClusterMetadata' {clusterState} -> clusterState) (\s@ClusterMetadata' {} a -> s {clusterState = a} :: ClusterMetadata)

-- | The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
clusterMetadata_notification :: Lens.Lens' ClusterMetadata (Prelude.Maybe Notification)
clusterMetadata_notification = Lens.lens (\ClusterMetadata' {notification} -> notification) (\s@ClusterMetadata' {} a -> s {notification = a} :: ClusterMetadata)

-- | The automatically generated ID for a cluster.
clusterMetadata_clusterId :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_clusterId = Lens.lens (\ClusterMetadata' {clusterId} -> clusterId) (\s@ClusterMetadata' {} a -> s {clusterId = a} :: ClusterMetadata)

-- | The tax documents required in your Amazon Web Services Region.
clusterMetadata_taxDocuments :: Lens.Lens' ClusterMetadata (Prelude.Maybe TaxDocuments)
clusterMetadata_taxDocuments = Lens.lens (\ClusterMetadata' {taxDocuments} -> taxDocuments) (\s@ClusterMetadata' {} a -> s {taxDocuments = a} :: ClusterMetadata)

-- | The arrays of JobResource objects that can include updated S3Resource
-- objects or LambdaResource objects.
clusterMetadata_resources :: Lens.Lens' ClusterMetadata (Prelude.Maybe JobResource)
clusterMetadata_resources = Lens.lens (\ClusterMetadata' {resources} -> resources) (\s@ClusterMetadata' {} a -> s {resources = a} :: ClusterMetadata)

-- | The automatically generated ID for a specific address.
clusterMetadata_addressId :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_addressId = Lens.lens (\ClusterMetadata' {addressId} -> addressId) (\s@ClusterMetadata' {} a -> s {addressId = a} :: ClusterMetadata)

-- | The type of Snowcone device to use for this cluster.
--
-- For cluster jobs, Amazon Web Services Snow Family currently supports
-- only the @EDGE@ device type.
clusterMetadata_snowballType :: Lens.Lens' ClusterMetadata (Prelude.Maybe SnowballType)
clusterMetadata_snowballType = Lens.lens (\ClusterMetadata' {snowballType} -> snowballType) (\s@ClusterMetadata' {} a -> s {snowballType = a} :: ClusterMetadata)

-- | The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
clusterMetadata_jobType :: Lens.Lens' ClusterMetadata (Prelude.Maybe JobType)
clusterMetadata_jobType = Lens.lens (\ClusterMetadata' {jobType} -> jobType) (\s@ClusterMetadata' {} a -> s {jobType = a} :: ClusterMetadata)

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
clusterMetadata_shippingOption :: Lens.Lens' ClusterMetadata (Prelude.Maybe ShippingOption)
clusterMetadata_shippingOption = Lens.lens (\ClusterMetadata' {shippingOption} -> shippingOption) (\s@ClusterMetadata' {} a -> s {shippingOption = a} :: ClusterMetadata)

-- | Represents metadata and configuration settings for services on an Amazon
-- Web Services Snow Family device.
clusterMetadata_onDeviceServiceConfiguration :: Lens.Lens' ClusterMetadata (Prelude.Maybe OnDeviceServiceConfiguration)
clusterMetadata_onDeviceServiceConfiguration = Lens.lens (\ClusterMetadata' {onDeviceServiceConfiguration} -> onDeviceServiceConfiguration) (\s@ClusterMetadata' {} a -> s {onDeviceServiceConfiguration = a} :: ClusterMetadata)

instance Core.FromJSON ClusterMetadata where
  parseJSON =
    Core.withObject
      "ClusterMetadata"
      ( \x ->
          ClusterMetadata'
            Prelude.<$> (x Core..:? "RoleARN")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "ForwardingAddressId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "KmsKeyARN")
            Prelude.<*> (x Core..:? "ClusterState")
            Prelude.<*> (x Core..:? "Notification")
            Prelude.<*> (x Core..:? "ClusterId")
            Prelude.<*> (x Core..:? "TaxDocuments")
            Prelude.<*> (x Core..:? "Resources")
            Prelude.<*> (x Core..:? "AddressId")
            Prelude.<*> (x Core..:? "SnowballType")
            Prelude.<*> (x Core..:? "JobType")
            Prelude.<*> (x Core..:? "ShippingOption")
            Prelude.<*> (x Core..:? "OnDeviceServiceConfiguration")
      )

instance Prelude.Hashable ClusterMetadata where
  hashWithSalt _salt ClusterMetadata' {..} =
    _salt `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` forwardingAddressId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyARN
      `Prelude.hashWithSalt` clusterState
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` taxDocuments
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` addressId
      `Prelude.hashWithSalt` snowballType
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` shippingOption
      `Prelude.hashWithSalt` onDeviceServiceConfiguration

instance Prelude.NFData ClusterMetadata where
  rnf ClusterMetadata' {..} =
    Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf forwardingAddressId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyARN
      `Prelude.seq` Prelude.rnf clusterState
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf taxDocuments
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf addressId
      `Prelude.seq` Prelude.rnf snowballType
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf shippingOption
      `Prelude.seq` Prelude.rnf onDeviceServiceConfiguration
