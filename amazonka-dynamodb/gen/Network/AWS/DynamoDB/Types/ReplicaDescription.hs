{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaDescription where

import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.ReplicaStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the details of the replica.
--
-- /See:/ 'newReplicaDescription' smart constructor.
data ReplicaDescription = ReplicaDescription'
  { -- | The name of the Region.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Prelude.Maybe [ReplicaGlobalSecondaryIndexDescription],
    -- | Replica-specific provisioned throughput. If not described, uses the
    -- source table\'s provisioned throughput settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | The AWS KMS customer master key (CMK) of the replica that will be used
    -- for AWS KMS encryption.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about the replica status.
    replicaStatusDescription :: Prelude.Maybe Prelude.Text,
    -- | Specifies the progress of a Create, Update, or Delete action on the
    -- replica as a percentage.
    replicaStatusPercentProgress :: Prelude.Maybe Prelude.Text,
    -- | The current state of the replica:
    --
    -- -   @CREATING@ - The replica is being created.
    --
    -- -   @UPDATING@ - The replica is being updated.
    --
    -- -   @DELETING@ - The replica is being deleted.
    --
    -- -   @ACTIVE@ - The replica is ready for use.
    --
    -- -   @REGION_DISABLED@ - The replica is inaccessible because the AWS
    --     Region has been disabled.
    --
    --     If the AWS Region remains inaccessible for more than 20 hours,
    --     DynamoDB will remove this replica from the replication group. The
    --     replica will not be deleted and replication will stop from and to
    --     this region.
    --
    -- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The AWS KMS key used to
    --     encrypt the table is inaccessible.
    --
    --     If the AWS KMS key remains inaccessible for more than 20 hours,
    --     DynamoDB will remove this replica from the replication group. The
    --     replica will not be deleted and replication will stop from and to
    --     this region.
    replicaStatus :: Prelude.Maybe ReplicaStatus,
    -- | The time at which the replica was first detected as inaccessible. To
    -- determine cause of inaccessibility check the @ReplicaStatus@ property.
    replicaInaccessibleDateTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplicaDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'replicaDescription_regionName' - The name of the Region.
--
-- 'globalSecondaryIndexes', 'replicaDescription_globalSecondaryIndexes' - Replica-specific global secondary index settings.
--
-- 'provisionedThroughputOverride', 'replicaDescription_provisionedThroughputOverride' - Replica-specific provisioned throughput. If not described, uses the
-- source table\'s provisioned throughput settings.
--
-- 'kmsMasterKeyId', 'replicaDescription_kmsMasterKeyId' - The AWS KMS customer master key (CMK) of the replica that will be used
-- for AWS KMS encryption.
--
-- 'replicaStatusDescription', 'replicaDescription_replicaStatusDescription' - Detailed information about the replica status.
--
-- 'replicaStatusPercentProgress', 'replicaDescription_replicaStatusPercentProgress' - Specifies the progress of a Create, Update, or Delete action on the
-- replica as a percentage.
--
-- 'replicaStatus', 'replicaDescription_replicaStatus' - The current state of the replica:
--
-- -   @CREATING@ - The replica is being created.
--
-- -   @UPDATING@ - The replica is being updated.
--
-- -   @DELETING@ - The replica is being deleted.
--
-- -   @ACTIVE@ - The replica is ready for use.
--
-- -   @REGION_DISABLED@ - The replica is inaccessible because the AWS
--     Region has been disabled.
--
--     If the AWS Region remains inaccessible for more than 20 hours,
--     DynamoDB will remove this replica from the replication group. The
--     replica will not be deleted and replication will stop from and to
--     this region.
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The AWS KMS key used to
--     encrypt the table is inaccessible.
--
--     If the AWS KMS key remains inaccessible for more than 20 hours,
--     DynamoDB will remove this replica from the replication group. The
--     replica will not be deleted and replication will stop from and to
--     this region.
--
-- 'replicaInaccessibleDateTime', 'replicaDescription_replicaInaccessibleDateTime' - The time at which the replica was first detected as inaccessible. To
-- determine cause of inaccessibility check the @ReplicaStatus@ property.
newReplicaDescription ::
  ReplicaDescription
newReplicaDescription =
  ReplicaDescription'
    { regionName = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      provisionedThroughputOverride = Prelude.Nothing,
      kmsMasterKeyId = Prelude.Nothing,
      replicaStatusDescription = Prelude.Nothing,
      replicaStatusPercentProgress = Prelude.Nothing,
      replicaStatus = Prelude.Nothing,
      replicaInaccessibleDateTime = Prelude.Nothing
    }

-- | The name of the Region.
replicaDescription_regionName :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_regionName = Lens.lens (\ReplicaDescription' {regionName} -> regionName) (\s@ReplicaDescription' {} a -> s {regionName = a} :: ReplicaDescription)

-- | Replica-specific global secondary index settings.
replicaDescription_globalSecondaryIndexes :: Lens.Lens' ReplicaDescription (Prelude.Maybe [ReplicaGlobalSecondaryIndexDescription])
replicaDescription_globalSecondaryIndexes = Lens.lens (\ReplicaDescription' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@ReplicaDescription' {} a -> s {globalSecondaryIndexes = a} :: ReplicaDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | Replica-specific provisioned throughput. If not described, uses the
-- source table\'s provisioned throughput settings.
replicaDescription_provisionedThroughputOverride :: Lens.Lens' ReplicaDescription (Prelude.Maybe ProvisionedThroughputOverride)
replicaDescription_provisionedThroughputOverride = Lens.lens (\ReplicaDescription' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@ReplicaDescription' {} a -> s {provisionedThroughputOverride = a} :: ReplicaDescription)

-- | The AWS KMS customer master key (CMK) of the replica that will be used
-- for AWS KMS encryption.
replicaDescription_kmsMasterKeyId :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_kmsMasterKeyId = Lens.lens (\ReplicaDescription' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@ReplicaDescription' {} a -> s {kmsMasterKeyId = a} :: ReplicaDescription)

-- | Detailed information about the replica status.
replicaDescription_replicaStatusDescription :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_replicaStatusDescription = Lens.lens (\ReplicaDescription' {replicaStatusDescription} -> replicaStatusDescription) (\s@ReplicaDescription' {} a -> s {replicaStatusDescription = a} :: ReplicaDescription)

-- | Specifies the progress of a Create, Update, or Delete action on the
-- replica as a percentage.
replicaDescription_replicaStatusPercentProgress :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_replicaStatusPercentProgress = Lens.lens (\ReplicaDescription' {replicaStatusPercentProgress} -> replicaStatusPercentProgress) (\s@ReplicaDescription' {} a -> s {replicaStatusPercentProgress = a} :: ReplicaDescription)

-- | The current state of the replica:
--
-- -   @CREATING@ - The replica is being created.
--
-- -   @UPDATING@ - The replica is being updated.
--
-- -   @DELETING@ - The replica is being deleted.
--
-- -   @ACTIVE@ - The replica is ready for use.
--
-- -   @REGION_DISABLED@ - The replica is inaccessible because the AWS
--     Region has been disabled.
--
--     If the AWS Region remains inaccessible for more than 20 hours,
--     DynamoDB will remove this replica from the replication group. The
--     replica will not be deleted and replication will stop from and to
--     this region.
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The AWS KMS key used to
--     encrypt the table is inaccessible.
--
--     If the AWS KMS key remains inaccessible for more than 20 hours,
--     DynamoDB will remove this replica from the replication group. The
--     replica will not be deleted and replication will stop from and to
--     this region.
replicaDescription_replicaStatus :: Lens.Lens' ReplicaDescription (Prelude.Maybe ReplicaStatus)
replicaDescription_replicaStatus = Lens.lens (\ReplicaDescription' {replicaStatus} -> replicaStatus) (\s@ReplicaDescription' {} a -> s {replicaStatus = a} :: ReplicaDescription)

-- | The time at which the replica was first detected as inaccessible. To
-- determine cause of inaccessibility check the @ReplicaStatus@ property.
replicaDescription_replicaInaccessibleDateTime :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.UTCTime)
replicaDescription_replicaInaccessibleDateTime = Lens.lens (\ReplicaDescription' {replicaInaccessibleDateTime} -> replicaInaccessibleDateTime) (\s@ReplicaDescription' {} a -> s {replicaInaccessibleDateTime = a} :: ReplicaDescription) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ReplicaDescription where
  parseJSON =
    Prelude.withObject
      "ReplicaDescription"
      ( \x ->
          ReplicaDescription'
            Prelude.<$> (x Prelude..:? "RegionName")
            Prelude.<*> ( x Prelude..:? "GlobalSecondaryIndexes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ProvisionedThroughputOverride")
            Prelude.<*> (x Prelude..:? "KMSMasterKeyId")
            Prelude.<*> (x Prelude..:? "ReplicaStatusDescription")
            Prelude.<*> (x Prelude..:? "ReplicaStatusPercentProgress")
            Prelude.<*> (x Prelude..:? "ReplicaStatus")
            Prelude.<*> (x Prelude..:? "ReplicaInaccessibleDateTime")
      )

instance Prelude.Hashable ReplicaDescription

instance Prelude.NFData ReplicaDescription
