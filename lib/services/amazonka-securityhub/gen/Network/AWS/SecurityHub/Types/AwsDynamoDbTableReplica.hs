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
-- Module      : Network.AWS.SecurityHub.Types.AwsDynamoDbTableReplica
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsDynamoDbTableReplica where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughputOverride
import Network.AWS.SecurityHub.Types.AwsDynamoDbTableReplicaGlobalSecondaryIndex

-- | Information about a replica of a DynamoDB table.
--
-- /See:/ 'newAwsDynamoDbTableReplica' smart constructor.
data AwsDynamoDbTableReplica = AwsDynamoDbTableReplica'
  { -- | The current status of the replica.
    replicaStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the Region where the replica is located.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about the replica status.
    replicaStatusDescription :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the KMS key that will be used for KMS encryption for
    -- the replica.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific configuration for the provisioned throughput.
    provisionedThroughputOverride :: Prelude.Maybe AwsDynamoDbTableProvisionedThroughputOverride,
    -- | List of global secondary indexes for the replica.
    globalSecondaryIndexes :: Prelude.Maybe [AwsDynamoDbTableReplicaGlobalSecondaryIndex]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableReplica' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaStatus', 'awsDynamoDbTableReplica_replicaStatus' - The current status of the replica.
--
-- 'regionName', 'awsDynamoDbTableReplica_regionName' - The name of the Region where the replica is located.
--
-- 'replicaStatusDescription', 'awsDynamoDbTableReplica_replicaStatusDescription' - Detailed information about the replica status.
--
-- 'kmsMasterKeyId', 'awsDynamoDbTableReplica_kmsMasterKeyId' - The identifier of the KMS key that will be used for KMS encryption for
-- the replica.
--
-- 'provisionedThroughputOverride', 'awsDynamoDbTableReplica_provisionedThroughputOverride' - Replica-specific configuration for the provisioned throughput.
--
-- 'globalSecondaryIndexes', 'awsDynamoDbTableReplica_globalSecondaryIndexes' - List of global secondary indexes for the replica.
newAwsDynamoDbTableReplica ::
  AwsDynamoDbTableReplica
newAwsDynamoDbTableReplica =
  AwsDynamoDbTableReplica'
    { replicaStatus =
        Prelude.Nothing,
      regionName = Prelude.Nothing,
      replicaStatusDescription = Prelude.Nothing,
      kmsMasterKeyId = Prelude.Nothing,
      provisionedThroughputOverride = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing
    }

-- | The current status of the replica.
awsDynamoDbTableReplica_replicaStatus :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe Prelude.Text)
awsDynamoDbTableReplica_replicaStatus = Lens.lens (\AwsDynamoDbTableReplica' {replicaStatus} -> replicaStatus) (\s@AwsDynamoDbTableReplica' {} a -> s {replicaStatus = a} :: AwsDynamoDbTableReplica)

-- | The name of the Region where the replica is located.
awsDynamoDbTableReplica_regionName :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe Prelude.Text)
awsDynamoDbTableReplica_regionName = Lens.lens (\AwsDynamoDbTableReplica' {regionName} -> regionName) (\s@AwsDynamoDbTableReplica' {} a -> s {regionName = a} :: AwsDynamoDbTableReplica)

-- | Detailed information about the replica status.
awsDynamoDbTableReplica_replicaStatusDescription :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe Prelude.Text)
awsDynamoDbTableReplica_replicaStatusDescription = Lens.lens (\AwsDynamoDbTableReplica' {replicaStatusDescription} -> replicaStatusDescription) (\s@AwsDynamoDbTableReplica' {} a -> s {replicaStatusDescription = a} :: AwsDynamoDbTableReplica)

-- | The identifier of the KMS key that will be used for KMS encryption for
-- the replica.
awsDynamoDbTableReplica_kmsMasterKeyId :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe Prelude.Text)
awsDynamoDbTableReplica_kmsMasterKeyId = Lens.lens (\AwsDynamoDbTableReplica' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@AwsDynamoDbTableReplica' {} a -> s {kmsMasterKeyId = a} :: AwsDynamoDbTableReplica)

-- | Replica-specific configuration for the provisioned throughput.
awsDynamoDbTableReplica_provisionedThroughputOverride :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe AwsDynamoDbTableProvisionedThroughputOverride)
awsDynamoDbTableReplica_provisionedThroughputOverride = Lens.lens (\AwsDynamoDbTableReplica' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@AwsDynamoDbTableReplica' {} a -> s {provisionedThroughputOverride = a} :: AwsDynamoDbTableReplica)

-- | List of global secondary indexes for the replica.
awsDynamoDbTableReplica_globalSecondaryIndexes :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe [AwsDynamoDbTableReplicaGlobalSecondaryIndex])
awsDynamoDbTableReplica_globalSecondaryIndexes = Lens.lens (\AwsDynamoDbTableReplica' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@AwsDynamoDbTableReplica' {} a -> s {globalSecondaryIndexes = a} :: AwsDynamoDbTableReplica) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsDynamoDbTableReplica where
  parseJSON =
    Core.withObject
      "AwsDynamoDbTableReplica"
      ( \x ->
          AwsDynamoDbTableReplica'
            Prelude.<$> (x Core..:? "ReplicaStatus")
            Prelude.<*> (x Core..:? "RegionName")
            Prelude.<*> (x Core..:? "ReplicaStatusDescription")
            Prelude.<*> (x Core..:? "KmsMasterKeyId")
            Prelude.<*> (x Core..:? "ProvisionedThroughputOverride")
            Prelude.<*> ( x Core..:? "GlobalSecondaryIndexes"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsDynamoDbTableReplica

instance Prelude.NFData AwsDynamoDbTableReplica

instance Core.ToJSON AwsDynamoDbTableReplica where
  toJSON AwsDynamoDbTableReplica' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ReplicaStatus" Core..=) Prelude.<$> replicaStatus,
            ("RegionName" Core..=) Prelude.<$> regionName,
            ("ReplicaStatusDescription" Core..=)
              Prelude.<$> replicaStatusDescription,
            ("KmsMasterKeyId" Core..=)
              Prelude.<$> kmsMasterKeyId,
            ("ProvisionedThroughputOverride" Core..=)
              Prelude.<$> provisionedThroughputOverride,
            ("GlobalSecondaryIndexes" Core..=)
              Prelude.<$> globalSecondaryIndexes
          ]
      )
