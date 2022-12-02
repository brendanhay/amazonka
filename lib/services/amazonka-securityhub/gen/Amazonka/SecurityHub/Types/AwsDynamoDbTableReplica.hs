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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableReplica
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableReplica where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsDynamoDbTableProvisionedThroughputOverride
import Amazonka.SecurityHub.Types.AwsDynamoDbTableReplicaGlobalSecondaryIndex

-- | Information about a replica of a DynamoDB table.
--
-- /See:/ 'newAwsDynamoDbTableReplica' smart constructor.
data AwsDynamoDbTableReplica = AwsDynamoDbTableReplica'
  { -- | The identifier of the KMS key that will be used for KMS encryption for
    -- the replica.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific configuration for the provisioned throughput.
    provisionedThroughputOverride :: Prelude.Maybe AwsDynamoDbTableProvisionedThroughputOverride,
    -- | The name of the Region where the replica is located.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about the replica status.
    replicaStatusDescription :: Prelude.Maybe Prelude.Text,
    -- | List of global secondary indexes for the replica.
    globalSecondaryIndexes :: Prelude.Maybe [AwsDynamoDbTableReplicaGlobalSecondaryIndex],
    -- | The current status of the replica. Valid values are as follows:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATING@
    --
    -- -   @CREATION_FAILED@
    --
    -- -   @DELETING@
    --
    -- -   @UPDATING@
    replicaStatus :: Prelude.Maybe Prelude.Text
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
-- 'kmsMasterKeyId', 'awsDynamoDbTableReplica_kmsMasterKeyId' - The identifier of the KMS key that will be used for KMS encryption for
-- the replica.
--
-- 'provisionedThroughputOverride', 'awsDynamoDbTableReplica_provisionedThroughputOverride' - Replica-specific configuration for the provisioned throughput.
--
-- 'regionName', 'awsDynamoDbTableReplica_regionName' - The name of the Region where the replica is located.
--
-- 'replicaStatusDescription', 'awsDynamoDbTableReplica_replicaStatusDescription' - Detailed information about the replica status.
--
-- 'globalSecondaryIndexes', 'awsDynamoDbTableReplica_globalSecondaryIndexes' - List of global secondary indexes for the replica.
--
-- 'replicaStatus', 'awsDynamoDbTableReplica_replicaStatus' - The current status of the replica. Valid values are as follows:
--
-- -   @ACTIVE@
--
-- -   @CREATING@
--
-- -   @CREATION_FAILED@
--
-- -   @DELETING@
--
-- -   @UPDATING@
newAwsDynamoDbTableReplica ::
  AwsDynamoDbTableReplica
newAwsDynamoDbTableReplica =
  AwsDynamoDbTableReplica'
    { kmsMasterKeyId =
        Prelude.Nothing,
      provisionedThroughputOverride = Prelude.Nothing,
      regionName = Prelude.Nothing,
      replicaStatusDescription = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      replicaStatus = Prelude.Nothing
    }

-- | The identifier of the KMS key that will be used for KMS encryption for
-- the replica.
awsDynamoDbTableReplica_kmsMasterKeyId :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe Prelude.Text)
awsDynamoDbTableReplica_kmsMasterKeyId = Lens.lens (\AwsDynamoDbTableReplica' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@AwsDynamoDbTableReplica' {} a -> s {kmsMasterKeyId = a} :: AwsDynamoDbTableReplica)

-- | Replica-specific configuration for the provisioned throughput.
awsDynamoDbTableReplica_provisionedThroughputOverride :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe AwsDynamoDbTableProvisionedThroughputOverride)
awsDynamoDbTableReplica_provisionedThroughputOverride = Lens.lens (\AwsDynamoDbTableReplica' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@AwsDynamoDbTableReplica' {} a -> s {provisionedThroughputOverride = a} :: AwsDynamoDbTableReplica)

-- | The name of the Region where the replica is located.
awsDynamoDbTableReplica_regionName :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe Prelude.Text)
awsDynamoDbTableReplica_regionName = Lens.lens (\AwsDynamoDbTableReplica' {regionName} -> regionName) (\s@AwsDynamoDbTableReplica' {} a -> s {regionName = a} :: AwsDynamoDbTableReplica)

-- | Detailed information about the replica status.
awsDynamoDbTableReplica_replicaStatusDescription :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe Prelude.Text)
awsDynamoDbTableReplica_replicaStatusDescription = Lens.lens (\AwsDynamoDbTableReplica' {replicaStatusDescription} -> replicaStatusDescription) (\s@AwsDynamoDbTableReplica' {} a -> s {replicaStatusDescription = a} :: AwsDynamoDbTableReplica)

-- | List of global secondary indexes for the replica.
awsDynamoDbTableReplica_globalSecondaryIndexes :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe [AwsDynamoDbTableReplicaGlobalSecondaryIndex])
awsDynamoDbTableReplica_globalSecondaryIndexes = Lens.lens (\AwsDynamoDbTableReplica' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@AwsDynamoDbTableReplica' {} a -> s {globalSecondaryIndexes = a} :: AwsDynamoDbTableReplica) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the replica. Valid values are as follows:
--
-- -   @ACTIVE@
--
-- -   @CREATING@
--
-- -   @CREATION_FAILED@
--
-- -   @DELETING@
--
-- -   @UPDATING@
awsDynamoDbTableReplica_replicaStatus :: Lens.Lens' AwsDynamoDbTableReplica (Prelude.Maybe Prelude.Text)
awsDynamoDbTableReplica_replicaStatus = Lens.lens (\AwsDynamoDbTableReplica' {replicaStatus} -> replicaStatus) (\s@AwsDynamoDbTableReplica' {} a -> s {replicaStatus = a} :: AwsDynamoDbTableReplica)

instance Data.FromJSON AwsDynamoDbTableReplica where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableReplica"
      ( \x ->
          AwsDynamoDbTableReplica'
            Prelude.<$> (x Data..:? "KmsMasterKeyId")
            Prelude.<*> (x Data..:? "ProvisionedThroughputOverride")
            Prelude.<*> (x Data..:? "RegionName")
            Prelude.<*> (x Data..:? "ReplicaStatusDescription")
            Prelude.<*> ( x Data..:? "GlobalSecondaryIndexes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ReplicaStatus")
      )

instance Prelude.Hashable AwsDynamoDbTableReplica where
  hashWithSalt _salt AwsDynamoDbTableReplica' {..} =
    _salt `Prelude.hashWithSalt` kmsMasterKeyId
      `Prelude.hashWithSalt` provisionedThroughputOverride
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` replicaStatusDescription
      `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` replicaStatus

instance Prelude.NFData AwsDynamoDbTableReplica where
  rnf AwsDynamoDbTableReplica' {..} =
    Prelude.rnf kmsMasterKeyId
      `Prelude.seq` Prelude.rnf provisionedThroughputOverride
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf replicaStatusDescription
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf replicaStatus

instance Data.ToJSON AwsDynamoDbTableReplica where
  toJSON AwsDynamoDbTableReplica' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsMasterKeyId" Data..=)
              Prelude.<$> kmsMasterKeyId,
            ("ProvisionedThroughputOverride" Data..=)
              Prelude.<$> provisionedThroughputOverride,
            ("RegionName" Data..=) Prelude.<$> regionName,
            ("ReplicaStatusDescription" Data..=)
              Prelude.<$> replicaStatusDescription,
            ("GlobalSecondaryIndexes" Data..=)
              Prelude.<$> globalSecondaryIndexes,
            ("ReplicaStatus" Data..=) Prelude.<$> replicaStatus
          ]
      )
