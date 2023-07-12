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
-- Module      : Amazonka.DynamoDB.Types.ReplicaDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ProvisionedThroughputOverride
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
import Amazonka.DynamoDB.Types.ReplicaStatus
import Amazonka.DynamoDB.Types.TableClassSummary
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of the replica.
--
-- /See:/ 'newReplicaDescription' smart constructor.
data ReplicaDescription = ReplicaDescription'
  { -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Prelude.Maybe [ReplicaGlobalSecondaryIndexDescription],
    -- | The KMS key of the replica that will be used for KMS encryption.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific provisioned throughput. If not described, uses the
    -- source table\'s provisioned throughput settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | The name of the Region.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | The time at which the replica was first detected as inaccessible. To
    -- determine cause of inaccessibility check the @ReplicaStatus@ property.
    replicaInaccessibleDateTime :: Prelude.Maybe Data.POSIX,
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
    -- -   @REGION_DISABLED@ - The replica is inaccessible because the Amazon
    --     Web Services Region has been disabled.
    --
    --     If the Amazon Web Services Region remains inaccessible for more than
    --     20 hours, DynamoDB will remove this replica from the replication
    --     group. The replica will not be deleted and replication will stop
    --     from and to this region.
    --
    -- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The KMS key used to encrypt
    --     the table is inaccessible.
    --
    --     If the KMS key remains inaccessible for more than 20 hours, DynamoDB
    --     will remove this replica from the replication group. The replica
    --     will not be deleted and replication will stop from and to this
    --     region.
    replicaStatus :: Prelude.Maybe ReplicaStatus,
    -- | Detailed information about the replica status.
    replicaStatusDescription :: Prelude.Maybe Prelude.Text,
    -- | Specifies the progress of a Create, Update, or Delete action on the
    -- replica as a percentage.
    replicaStatusPercentProgress :: Prelude.Maybe Prelude.Text,
    replicaTableClassSummary :: Prelude.Maybe TableClassSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalSecondaryIndexes', 'replicaDescription_globalSecondaryIndexes' - Replica-specific global secondary index settings.
--
-- 'kmsMasterKeyId', 'replicaDescription_kmsMasterKeyId' - The KMS key of the replica that will be used for KMS encryption.
--
-- 'provisionedThroughputOverride', 'replicaDescription_provisionedThroughputOverride' - Replica-specific provisioned throughput. If not described, uses the
-- source table\'s provisioned throughput settings.
--
-- 'regionName', 'replicaDescription_regionName' - The name of the Region.
--
-- 'replicaInaccessibleDateTime', 'replicaDescription_replicaInaccessibleDateTime' - The time at which the replica was first detected as inaccessible. To
-- determine cause of inaccessibility check the @ReplicaStatus@ property.
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
-- -   @REGION_DISABLED@ - The replica is inaccessible because the Amazon
--     Web Services Region has been disabled.
--
--     If the Amazon Web Services Region remains inaccessible for more than
--     20 hours, DynamoDB will remove this replica from the replication
--     group. The replica will not be deleted and replication will stop
--     from and to this region.
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The KMS key used to encrypt
--     the table is inaccessible.
--
--     If the KMS key remains inaccessible for more than 20 hours, DynamoDB
--     will remove this replica from the replication group. The replica
--     will not be deleted and replication will stop from and to this
--     region.
--
-- 'replicaStatusDescription', 'replicaDescription_replicaStatusDescription' - Detailed information about the replica status.
--
-- 'replicaStatusPercentProgress', 'replicaDescription_replicaStatusPercentProgress' - Specifies the progress of a Create, Update, or Delete action on the
-- replica as a percentage.
--
-- 'replicaTableClassSummary', 'replicaDescription_replicaTableClassSummary' - Undocumented member.
newReplicaDescription ::
  ReplicaDescription
newReplicaDescription =
  ReplicaDescription'
    { globalSecondaryIndexes =
        Prelude.Nothing,
      kmsMasterKeyId = Prelude.Nothing,
      provisionedThroughputOverride = Prelude.Nothing,
      regionName = Prelude.Nothing,
      replicaInaccessibleDateTime = Prelude.Nothing,
      replicaStatus = Prelude.Nothing,
      replicaStatusDescription = Prelude.Nothing,
      replicaStatusPercentProgress = Prelude.Nothing,
      replicaTableClassSummary = Prelude.Nothing
    }

-- | Replica-specific global secondary index settings.
replicaDescription_globalSecondaryIndexes :: Lens.Lens' ReplicaDescription (Prelude.Maybe [ReplicaGlobalSecondaryIndexDescription])
replicaDescription_globalSecondaryIndexes = Lens.lens (\ReplicaDescription' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@ReplicaDescription' {} a -> s {globalSecondaryIndexes = a} :: ReplicaDescription) Prelude.. Lens.mapping Lens.coerced

-- | The KMS key of the replica that will be used for KMS encryption.
replicaDescription_kmsMasterKeyId :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_kmsMasterKeyId = Lens.lens (\ReplicaDescription' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@ReplicaDescription' {} a -> s {kmsMasterKeyId = a} :: ReplicaDescription)

-- | Replica-specific provisioned throughput. If not described, uses the
-- source table\'s provisioned throughput settings.
replicaDescription_provisionedThroughputOverride :: Lens.Lens' ReplicaDescription (Prelude.Maybe ProvisionedThroughputOverride)
replicaDescription_provisionedThroughputOverride = Lens.lens (\ReplicaDescription' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@ReplicaDescription' {} a -> s {provisionedThroughputOverride = a} :: ReplicaDescription)

-- | The name of the Region.
replicaDescription_regionName :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_regionName = Lens.lens (\ReplicaDescription' {regionName} -> regionName) (\s@ReplicaDescription' {} a -> s {regionName = a} :: ReplicaDescription)

-- | The time at which the replica was first detected as inaccessible. To
-- determine cause of inaccessibility check the @ReplicaStatus@ property.
replicaDescription_replicaInaccessibleDateTime :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.UTCTime)
replicaDescription_replicaInaccessibleDateTime = Lens.lens (\ReplicaDescription' {replicaInaccessibleDateTime} -> replicaInaccessibleDateTime) (\s@ReplicaDescription' {} a -> s {replicaInaccessibleDateTime = a} :: ReplicaDescription) Prelude.. Lens.mapping Data._Time

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
-- -   @REGION_DISABLED@ - The replica is inaccessible because the Amazon
--     Web Services Region has been disabled.
--
--     If the Amazon Web Services Region remains inaccessible for more than
--     20 hours, DynamoDB will remove this replica from the replication
--     group. The replica will not be deleted and replication will stop
--     from and to this region.
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS @ - The KMS key used to encrypt
--     the table is inaccessible.
--
--     If the KMS key remains inaccessible for more than 20 hours, DynamoDB
--     will remove this replica from the replication group. The replica
--     will not be deleted and replication will stop from and to this
--     region.
replicaDescription_replicaStatus :: Lens.Lens' ReplicaDescription (Prelude.Maybe ReplicaStatus)
replicaDescription_replicaStatus = Lens.lens (\ReplicaDescription' {replicaStatus} -> replicaStatus) (\s@ReplicaDescription' {} a -> s {replicaStatus = a} :: ReplicaDescription)

-- | Detailed information about the replica status.
replicaDescription_replicaStatusDescription :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_replicaStatusDescription = Lens.lens (\ReplicaDescription' {replicaStatusDescription} -> replicaStatusDescription) (\s@ReplicaDescription' {} a -> s {replicaStatusDescription = a} :: ReplicaDescription)

-- | Specifies the progress of a Create, Update, or Delete action on the
-- replica as a percentage.
replicaDescription_replicaStatusPercentProgress :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_replicaStatusPercentProgress = Lens.lens (\ReplicaDescription' {replicaStatusPercentProgress} -> replicaStatusPercentProgress) (\s@ReplicaDescription' {} a -> s {replicaStatusPercentProgress = a} :: ReplicaDescription)

-- | Undocumented member.
replicaDescription_replicaTableClassSummary :: Lens.Lens' ReplicaDescription (Prelude.Maybe TableClassSummary)
replicaDescription_replicaTableClassSummary = Lens.lens (\ReplicaDescription' {replicaTableClassSummary} -> replicaTableClassSummary) (\s@ReplicaDescription' {} a -> s {replicaTableClassSummary = a} :: ReplicaDescription)

instance Data.FromJSON ReplicaDescription where
  parseJSON =
    Data.withObject
      "ReplicaDescription"
      ( \x ->
          ReplicaDescription'
            Prelude.<$> ( x
                            Data..:? "GlobalSecondaryIndexes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "KMSMasterKeyId")
            Prelude.<*> (x Data..:? "ProvisionedThroughputOverride")
            Prelude.<*> (x Data..:? "RegionName")
            Prelude.<*> (x Data..:? "ReplicaInaccessibleDateTime")
            Prelude.<*> (x Data..:? "ReplicaStatus")
            Prelude.<*> (x Data..:? "ReplicaStatusDescription")
            Prelude.<*> (x Data..:? "ReplicaStatusPercentProgress")
            Prelude.<*> (x Data..:? "ReplicaTableClassSummary")
      )

instance Prelude.Hashable ReplicaDescription where
  hashWithSalt _salt ReplicaDescription' {..} =
    _salt
      `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` kmsMasterKeyId
      `Prelude.hashWithSalt` provisionedThroughputOverride
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` replicaInaccessibleDateTime
      `Prelude.hashWithSalt` replicaStatus
      `Prelude.hashWithSalt` replicaStatusDescription
      `Prelude.hashWithSalt` replicaStatusPercentProgress
      `Prelude.hashWithSalt` replicaTableClassSummary

instance Prelude.NFData ReplicaDescription where
  rnf ReplicaDescription' {..} =
    Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf kmsMasterKeyId
      `Prelude.seq` Prelude.rnf provisionedThroughputOverride
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf replicaInaccessibleDateTime
      `Prelude.seq` Prelude.rnf replicaStatus
      `Prelude.seq` Prelude.rnf replicaStatusDescription
      `Prelude.seq` Prelude.rnf replicaStatusPercentProgress
      `Prelude.seq` Prelude.rnf replicaTableClassSummary
