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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.ClusterMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The automatically generated ID for a specific address.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The automatically generated ID for a cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the cluster.
    clusterState :: Prelude.Maybe ClusterState,
    -- | The creation date for this cluster.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The optional description of the cluster.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the address that you want a cluster shipped to, after it will
    -- be shipped to its primary address. This field is not supported in most
    -- regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | The type of job for this cluster. Currently, the only job type supported
    -- for clusters is @LOCAL_USE@.
    jobType :: Prelude.Maybe JobType,
    -- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster.
    -- This ARN was created using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- API action in Key Management Service (KMS.
    kmsKeyARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) notification
    -- settings for this cluster.
    notification :: Prelude.Maybe Notification,
    -- | Represents metadata and configuration settings for services on an Amazon
    -- Web Services Snow Family device.
    onDeviceServiceConfiguration :: Prelude.Maybe OnDeviceServiceConfiguration,
    -- | The arrays of JobResource objects that can include updated S3Resource
    -- objects or LambdaResource objects.
    resources :: Prelude.Maybe JobResource,
    -- | The role ARN associated with this cluster. This ARN was created using
    -- the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- API action in Identity and Access Management (IAM).
    roleARN :: Prelude.Maybe Prelude.Text,
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
    -- | The type of Snowcone device to use for this cluster.
    --
    -- For cluster jobs, Amazon Web Services Snow Family currently supports
    -- only the @EDGE@ device type.
    snowballType :: Prelude.Maybe SnowballType,
    -- | The tax documents required in your Amazon Web Services Region.
    taxDocuments :: Prelude.Maybe TaxDocuments
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
-- 'addressId', 'clusterMetadata_addressId' - The automatically generated ID for a specific address.
--
-- 'clusterId', 'clusterMetadata_clusterId' - The automatically generated ID for a cluster.
--
-- 'clusterState', 'clusterMetadata_clusterState' - The current status of the cluster.
--
-- 'creationDate', 'clusterMetadata_creationDate' - The creation date for this cluster.
--
-- 'description', 'clusterMetadata_description' - The optional description of the cluster.
--
-- 'forwardingAddressId', 'clusterMetadata_forwardingAddressId' - The ID of the address that you want a cluster shipped to, after it will
-- be shipped to its primary address. This field is not supported in most
-- regions.
--
-- 'jobType', 'clusterMetadata_jobType' - The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
--
-- 'kmsKeyARN', 'clusterMetadata_kmsKeyARN' - The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster.
-- This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in Key Management Service (KMS.
--
-- 'notification', 'clusterMetadata_notification' - The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
--
-- 'onDeviceServiceConfiguration', 'clusterMetadata_onDeviceServiceConfiguration' - Represents metadata and configuration settings for services on an Amazon
-- Web Services Snow Family device.
--
-- 'resources', 'clusterMetadata_resources' - The arrays of JobResource objects that can include updated S3Resource
-- objects or LambdaResource objects.
--
-- 'roleARN', 'clusterMetadata_roleARN' - The role ARN associated with this cluster. This ARN was created using
-- the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in Identity and Access Management (IAM).
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
-- 'snowballType', 'clusterMetadata_snowballType' - The type of Snowcone device to use for this cluster.
--
-- For cluster jobs, Amazon Web Services Snow Family currently supports
-- only the @EDGE@ device type.
--
-- 'taxDocuments', 'clusterMetadata_taxDocuments' - The tax documents required in your Amazon Web Services Region.
newClusterMetadata ::
  ClusterMetadata
newClusterMetadata =
  ClusterMetadata'
    { addressId = Prelude.Nothing,
      clusterId = Prelude.Nothing,
      clusterState = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      forwardingAddressId = Prelude.Nothing,
      jobType = Prelude.Nothing,
      kmsKeyARN = Prelude.Nothing,
      notification = Prelude.Nothing,
      onDeviceServiceConfiguration = Prelude.Nothing,
      resources = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      shippingOption = Prelude.Nothing,
      snowballType = Prelude.Nothing,
      taxDocuments = Prelude.Nothing
    }

-- | The automatically generated ID for a specific address.
clusterMetadata_addressId :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_addressId = Lens.lens (\ClusterMetadata' {addressId} -> addressId) (\s@ClusterMetadata' {} a -> s {addressId = a} :: ClusterMetadata)

-- | The automatically generated ID for a cluster.
clusterMetadata_clusterId :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_clusterId = Lens.lens (\ClusterMetadata' {clusterId} -> clusterId) (\s@ClusterMetadata' {} a -> s {clusterId = a} :: ClusterMetadata)

-- | The current status of the cluster.
clusterMetadata_clusterState :: Lens.Lens' ClusterMetadata (Prelude.Maybe ClusterState)
clusterMetadata_clusterState = Lens.lens (\ClusterMetadata' {clusterState} -> clusterState) (\s@ClusterMetadata' {} a -> s {clusterState = a} :: ClusterMetadata)

-- | The creation date for this cluster.
clusterMetadata_creationDate :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.UTCTime)
clusterMetadata_creationDate = Lens.lens (\ClusterMetadata' {creationDate} -> creationDate) (\s@ClusterMetadata' {} a -> s {creationDate = a} :: ClusterMetadata) Prelude.. Lens.mapping Data._Time

-- | The optional description of the cluster.
clusterMetadata_description :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_description = Lens.lens (\ClusterMetadata' {description} -> description) (\s@ClusterMetadata' {} a -> s {description = a} :: ClusterMetadata)

-- | The ID of the address that you want a cluster shipped to, after it will
-- be shipped to its primary address. This field is not supported in most
-- regions.
clusterMetadata_forwardingAddressId :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_forwardingAddressId = Lens.lens (\ClusterMetadata' {forwardingAddressId} -> forwardingAddressId) (\s@ClusterMetadata' {} a -> s {forwardingAddressId = a} :: ClusterMetadata)

-- | The type of job for this cluster. Currently, the only job type supported
-- for clusters is @LOCAL_USE@.
clusterMetadata_jobType :: Lens.Lens' ClusterMetadata (Prelude.Maybe JobType)
clusterMetadata_jobType = Lens.lens (\ClusterMetadata' {jobType} -> jobType) (\s@ClusterMetadata' {} a -> s {jobType = a} :: ClusterMetadata)

-- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster.
-- This ARN was created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- API action in Key Management Service (KMS.
clusterMetadata_kmsKeyARN :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
clusterMetadata_kmsKeyARN = Lens.lens (\ClusterMetadata' {kmsKeyARN} -> kmsKeyARN) (\s@ClusterMetadata' {} a -> s {kmsKeyARN = a} :: ClusterMetadata)

-- | The Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this cluster.
clusterMetadata_notification :: Lens.Lens' ClusterMetadata (Prelude.Maybe Notification)
clusterMetadata_notification = Lens.lens (\ClusterMetadata' {notification} -> notification) (\s@ClusterMetadata' {} a -> s {notification = a} :: ClusterMetadata)

-- | Represents metadata and configuration settings for services on an Amazon
-- Web Services Snow Family device.
clusterMetadata_onDeviceServiceConfiguration :: Lens.Lens' ClusterMetadata (Prelude.Maybe OnDeviceServiceConfiguration)
clusterMetadata_onDeviceServiceConfiguration = Lens.lens (\ClusterMetadata' {onDeviceServiceConfiguration} -> onDeviceServiceConfiguration) (\s@ClusterMetadata' {} a -> s {onDeviceServiceConfiguration = a} :: ClusterMetadata)

-- | The arrays of JobResource objects that can include updated S3Resource
-- objects or LambdaResource objects.
clusterMetadata_resources :: Lens.Lens' ClusterMetadata (Prelude.Maybe JobResource)
clusterMetadata_resources = Lens.lens (\ClusterMetadata' {resources} -> resources) (\s@ClusterMetadata' {} a -> s {resources = a} :: ClusterMetadata)

-- | The role ARN associated with this cluster. This ARN was created using
-- the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- API action in Identity and Access Management (IAM).
clusterMetadata_roleARN :: Lens.Lens' ClusterMetadata (Prelude.Maybe Prelude.Text)
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
clusterMetadata_shippingOption :: Lens.Lens' ClusterMetadata (Prelude.Maybe ShippingOption)
clusterMetadata_shippingOption = Lens.lens (\ClusterMetadata' {shippingOption} -> shippingOption) (\s@ClusterMetadata' {} a -> s {shippingOption = a} :: ClusterMetadata)

-- | The type of Snowcone device to use for this cluster.
--
-- For cluster jobs, Amazon Web Services Snow Family currently supports
-- only the @EDGE@ device type.
clusterMetadata_snowballType :: Lens.Lens' ClusterMetadata (Prelude.Maybe SnowballType)
clusterMetadata_snowballType = Lens.lens (\ClusterMetadata' {snowballType} -> snowballType) (\s@ClusterMetadata' {} a -> s {snowballType = a} :: ClusterMetadata)

-- | The tax documents required in your Amazon Web Services Region.
clusterMetadata_taxDocuments :: Lens.Lens' ClusterMetadata (Prelude.Maybe TaxDocuments)
clusterMetadata_taxDocuments = Lens.lens (\ClusterMetadata' {taxDocuments} -> taxDocuments) (\s@ClusterMetadata' {} a -> s {taxDocuments = a} :: ClusterMetadata)

instance Data.FromJSON ClusterMetadata where
  parseJSON =
    Data.withObject
      "ClusterMetadata"
      ( \x ->
          ClusterMetadata'
            Prelude.<$> (x Data..:? "AddressId")
            Prelude.<*> (x Data..:? "ClusterId")
            Prelude.<*> (x Data..:? "ClusterState")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ForwardingAddressId")
            Prelude.<*> (x Data..:? "JobType")
            Prelude.<*> (x Data..:? "KmsKeyARN")
            Prelude.<*> (x Data..:? "Notification")
            Prelude.<*> (x Data..:? "OnDeviceServiceConfiguration")
            Prelude.<*> (x Data..:? "Resources")
            Prelude.<*> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..:? "ShippingOption")
            Prelude.<*> (x Data..:? "SnowballType")
            Prelude.<*> (x Data..:? "TaxDocuments")
      )

instance Prelude.Hashable ClusterMetadata where
  hashWithSalt _salt ClusterMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` addressId
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` clusterState
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` forwardingAddressId
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` kmsKeyARN
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` onDeviceServiceConfiguration
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` shippingOption
      `Prelude.hashWithSalt` snowballType
      `Prelude.hashWithSalt` taxDocuments

instance Prelude.NFData ClusterMetadata where
  rnf ClusterMetadata' {..} =
    Prelude.rnf addressId `Prelude.seq`
      Prelude.rnf clusterId `Prelude.seq`
        Prelude.rnf clusterState `Prelude.seq`
          Prelude.rnf creationDate `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf forwardingAddressId `Prelude.seq`
                Prelude.rnf jobType `Prelude.seq`
                  Prelude.rnf kmsKeyARN `Prelude.seq`
                    Prelude.rnf notification `Prelude.seq`
                      Prelude.rnf onDeviceServiceConfiguration `Prelude.seq`
                        Prelude.rnf resources `Prelude.seq`
                          Prelude.rnf roleARN `Prelude.seq`
                            Prelude.rnf shippingOption `Prelude.seq`
                              Prelude.rnf snowballType `Prelude.seq`
                                Prelude.rnf taxDocuments
