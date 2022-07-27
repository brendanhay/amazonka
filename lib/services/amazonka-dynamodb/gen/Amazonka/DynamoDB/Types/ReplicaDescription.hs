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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaDescription where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ProvisionedThroughputOverride
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
import Amazonka.DynamoDB.Types.ReplicaStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of the replica.
--
-- /See:/ 'newReplicaDescription' smart constructor.
data ReplicaDescription = ReplicaDescription'
  { -- | The AWS KMS customer master key (CMK) of the replica that will be used
    -- for AWS KMS encryption.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | The time at which the replica was first detected as inaccessible. To
    -- determine cause of inaccessibility check the @ReplicaStatus@ property.
    replicaInaccessibleDateTime :: Prelude.Maybe Core.POSIX,
    -- | Replica-specific provisioned throughput. If not described, uses the
    -- source table\'s provisioned throughput settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | The name of the Region.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the progress of a Create, Update, or Delete action on the
    -- replica as a percentage.
    replicaStatusPercentProgress :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about the replica status.
    replicaStatusDescription :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Prelude.Maybe [ReplicaGlobalSecondaryIndexDescription],
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
    replicaStatus :: Prelude.Maybe ReplicaStatus
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
-- 'kmsMasterKeyId', 'replicaDescription_kmsMasterKeyId' - The AWS KMS customer master key (CMK) of the replica that will be used
-- for AWS KMS encryption.
--
-- 'replicaInaccessibleDateTime', 'replicaDescription_replicaInaccessibleDateTime' - The time at which the replica was first detected as inaccessible. To
-- determine cause of inaccessibility check the @ReplicaStatus@ property.
--
-- 'provisionedThroughputOverride', 'replicaDescription_provisionedThroughputOverride' - Replica-specific provisioned throughput. If not described, uses the
-- source table\'s provisioned throughput settings.
--
-- 'regionName', 'replicaDescription_regionName' - The name of the Region.
--
-- 'replicaStatusPercentProgress', 'replicaDescription_replicaStatusPercentProgress' - Specifies the progress of a Create, Update, or Delete action on the
-- replica as a percentage.
--
-- 'replicaStatusDescription', 'replicaDescription_replicaStatusDescription' - Detailed information about the replica status.
--
-- 'globalSecondaryIndexes', 'replicaDescription_globalSecondaryIndexes' - Replica-specific global secondary index settings.
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
newReplicaDescription ::
  ReplicaDescription
newReplicaDescription =
  ReplicaDescription'
    { kmsMasterKeyId =
        Prelude.Nothing,
      replicaInaccessibleDateTime = Prelude.Nothing,
      provisionedThroughputOverride = Prelude.Nothing,
      regionName = Prelude.Nothing,
      replicaStatusPercentProgress = Prelude.Nothing,
      replicaStatusDescription = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      replicaStatus = Prelude.Nothing
    }

-- | The AWS KMS customer master key (CMK) of the replica that will be used
-- for AWS KMS encryption.
replicaDescription_kmsMasterKeyId :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_kmsMasterKeyId = Lens.lens (\ReplicaDescription' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@ReplicaDescription' {} a -> s {kmsMasterKeyId = a} :: ReplicaDescription)

-- | The time at which the replica was first detected as inaccessible. To
-- determine cause of inaccessibility check the @ReplicaStatus@ property.
replicaDescription_replicaInaccessibleDateTime :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.UTCTime)
replicaDescription_replicaInaccessibleDateTime = Lens.lens (\ReplicaDescription' {replicaInaccessibleDateTime} -> replicaInaccessibleDateTime) (\s@ReplicaDescription' {} a -> s {replicaInaccessibleDateTime = a} :: ReplicaDescription) Prelude.. Lens.mapping Core._Time

-- | Replica-specific provisioned throughput. If not described, uses the
-- source table\'s provisioned throughput settings.
replicaDescription_provisionedThroughputOverride :: Lens.Lens' ReplicaDescription (Prelude.Maybe ProvisionedThroughputOverride)
replicaDescription_provisionedThroughputOverride = Lens.lens (\ReplicaDescription' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@ReplicaDescription' {} a -> s {provisionedThroughputOverride = a} :: ReplicaDescription)

-- | The name of the Region.
replicaDescription_regionName :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_regionName = Lens.lens (\ReplicaDescription' {regionName} -> regionName) (\s@ReplicaDescription' {} a -> s {regionName = a} :: ReplicaDescription)

-- | Specifies the progress of a Create, Update, or Delete action on the
-- replica as a percentage.
replicaDescription_replicaStatusPercentProgress :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_replicaStatusPercentProgress = Lens.lens (\ReplicaDescription' {replicaStatusPercentProgress} -> replicaStatusPercentProgress) (\s@ReplicaDescription' {} a -> s {replicaStatusPercentProgress = a} :: ReplicaDescription)

-- | Detailed information about the replica status.
replicaDescription_replicaStatusDescription :: Lens.Lens' ReplicaDescription (Prelude.Maybe Prelude.Text)
replicaDescription_replicaStatusDescription = Lens.lens (\ReplicaDescription' {replicaStatusDescription} -> replicaStatusDescription) (\s@ReplicaDescription' {} a -> s {replicaStatusDescription = a} :: ReplicaDescription)

-- | Replica-specific global secondary index settings.
replicaDescription_globalSecondaryIndexes :: Lens.Lens' ReplicaDescription (Prelude.Maybe [ReplicaGlobalSecondaryIndexDescription])
replicaDescription_globalSecondaryIndexes = Lens.lens (\ReplicaDescription' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@ReplicaDescription' {} a -> s {globalSecondaryIndexes = a} :: ReplicaDescription) Prelude.. Lens.mapping Lens.coerced

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

instance Core.FromJSON ReplicaDescription where
  parseJSON =
    Core.withObject
      "ReplicaDescription"
      ( \x ->
          ReplicaDescription'
            Prelude.<$> (x Core..:? "KMSMasterKeyId")
            Prelude.<*> (x Core..:? "ReplicaInaccessibleDateTime")
            Prelude.<*> (x Core..:? "ProvisionedThroughputOverride")
            Prelude.<*> (x Core..:? "RegionName")
            Prelude.<*> (x Core..:? "ReplicaStatusPercentProgress")
            Prelude.<*> (x Core..:? "ReplicaStatusDescription")
            Prelude.<*> ( x Core..:? "GlobalSecondaryIndexes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ReplicaStatus")
      )

instance Prelude.Hashable ReplicaDescription where
  hashWithSalt _salt ReplicaDescription' {..} =
    _salt `Prelude.hashWithSalt` kmsMasterKeyId
      `Prelude.hashWithSalt` replicaInaccessibleDateTime
      `Prelude.hashWithSalt` provisionedThroughputOverride
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` replicaStatusPercentProgress
      `Prelude.hashWithSalt` replicaStatusDescription
      `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` replicaStatus

instance Prelude.NFData ReplicaDescription where
  rnf ReplicaDescription' {..} =
    Prelude.rnf kmsMasterKeyId
      `Prelude.seq` Prelude.rnf replicaInaccessibleDateTime
      `Prelude.seq` Prelude.rnf provisionedThroughputOverride
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf replicaStatusPercentProgress
      `Prelude.seq` Prelude.rnf replicaStatusDescription
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf replicaStatus
