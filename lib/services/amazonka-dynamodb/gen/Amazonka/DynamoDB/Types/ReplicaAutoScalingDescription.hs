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
-- Module      : Amazonka.DynamoDB.Types.ReplicaAutoScalingDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaAutoScalingDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingSettingsDescription
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
import Amazonka.DynamoDB.Types.ReplicaStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling settings of the replica.
--
-- /See:/ 'newReplicaAutoScalingDescription' smart constructor.
data ReplicaAutoScalingDescription = ReplicaAutoScalingDescription'
  { replicaProvisionedReadCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    replicaProvisionedWriteCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    -- | The Region where the replica exists.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific global secondary index auto scaling settings.
    globalSecondaryIndexes :: Prelude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingDescription],
    -- | The current state of the replica:
    --
    -- -   @CREATING@ - The replica is being created.
    --
    -- -   @UPDATING@ - The replica is being updated.
    --
    -- -   @DELETING@ - The replica is being deleted.
    --
    -- -   @ACTIVE@ - The replica is ready for use.
    replicaStatus :: Prelude.Maybe ReplicaStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaAutoScalingDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaProvisionedReadCapacityAutoScalingSettings', 'replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings' - Undocumented member.
--
-- 'replicaProvisionedWriteCapacityAutoScalingSettings', 'replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings' - Undocumented member.
--
-- 'regionName', 'replicaAutoScalingDescription_regionName' - The Region where the replica exists.
--
-- 'globalSecondaryIndexes', 'replicaAutoScalingDescription_globalSecondaryIndexes' - Replica-specific global secondary index auto scaling settings.
--
-- 'replicaStatus', 'replicaAutoScalingDescription_replicaStatus' - The current state of the replica:
--
-- -   @CREATING@ - The replica is being created.
--
-- -   @UPDATING@ - The replica is being updated.
--
-- -   @DELETING@ - The replica is being deleted.
--
-- -   @ACTIVE@ - The replica is ready for use.
newReplicaAutoScalingDescription ::
  ReplicaAutoScalingDescription
newReplicaAutoScalingDescription =
  ReplicaAutoScalingDescription'
    { replicaProvisionedReadCapacityAutoScalingSettings =
        Prelude.Nothing,
      replicaProvisionedWriteCapacityAutoScalingSettings =
        Prelude.Nothing,
      regionName = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      replicaStatus = Prelude.Nothing
    }

-- | Undocumented member.
replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaAutoScalingDescription' {replicaProvisionedReadCapacityAutoScalingSettings} -> replicaProvisionedReadCapacityAutoScalingSettings) (\s@ReplicaAutoScalingDescription' {} a -> s {replicaProvisionedReadCapacityAutoScalingSettings = a} :: ReplicaAutoScalingDescription)

-- | Undocumented member.
replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaAutoScalingDescription' {replicaProvisionedWriteCapacityAutoScalingSettings} -> replicaProvisionedWriteCapacityAutoScalingSettings) (\s@ReplicaAutoScalingDescription' {} a -> s {replicaProvisionedWriteCapacityAutoScalingSettings = a} :: ReplicaAutoScalingDescription)

-- | The Region where the replica exists.
replicaAutoScalingDescription_regionName :: Lens.Lens' ReplicaAutoScalingDescription (Prelude.Maybe Prelude.Text)
replicaAutoScalingDescription_regionName = Lens.lens (\ReplicaAutoScalingDescription' {regionName} -> regionName) (\s@ReplicaAutoScalingDescription' {} a -> s {regionName = a} :: ReplicaAutoScalingDescription)

-- | Replica-specific global secondary index auto scaling settings.
replicaAutoScalingDescription_globalSecondaryIndexes :: Lens.Lens' ReplicaAutoScalingDescription (Prelude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingDescription])
replicaAutoScalingDescription_globalSecondaryIndexes = Lens.lens (\ReplicaAutoScalingDescription' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@ReplicaAutoScalingDescription' {} a -> s {globalSecondaryIndexes = a} :: ReplicaAutoScalingDescription) Prelude.. Lens.mapping Lens.coerced

-- | The current state of the replica:
--
-- -   @CREATING@ - The replica is being created.
--
-- -   @UPDATING@ - The replica is being updated.
--
-- -   @DELETING@ - The replica is being deleted.
--
-- -   @ACTIVE@ - The replica is ready for use.
replicaAutoScalingDescription_replicaStatus :: Lens.Lens' ReplicaAutoScalingDescription (Prelude.Maybe ReplicaStatus)
replicaAutoScalingDescription_replicaStatus = Lens.lens (\ReplicaAutoScalingDescription' {replicaStatus} -> replicaStatus) (\s@ReplicaAutoScalingDescription' {} a -> s {replicaStatus = a} :: ReplicaAutoScalingDescription)

instance Core.FromJSON ReplicaAutoScalingDescription where
  parseJSON =
    Core.withObject
      "ReplicaAutoScalingDescription"
      ( \x ->
          ReplicaAutoScalingDescription'
            Prelude.<$> ( x
                            Core..:? "ReplicaProvisionedReadCapacityAutoScalingSettings"
                        )
            Prelude.<*> ( x
                            Core..:? "ReplicaProvisionedWriteCapacityAutoScalingSettings"
                        )
            Prelude.<*> (x Core..:? "RegionName")
            Prelude.<*> ( x Core..:? "GlobalSecondaryIndexes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ReplicaStatus")
      )

instance
  Prelude.Hashable
    ReplicaAutoScalingDescription
  where
  hashWithSalt _salt ReplicaAutoScalingDescription' {..} =
    _salt
      `Prelude.hashWithSalt` replicaProvisionedReadCapacityAutoScalingSettings
      `Prelude.hashWithSalt` replicaProvisionedWriteCapacityAutoScalingSettings
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` replicaStatus

instance Prelude.NFData ReplicaAutoScalingDescription where
  rnf ReplicaAutoScalingDescription' {..} =
    Prelude.rnf
      replicaProvisionedReadCapacityAutoScalingSettings
      `Prelude.seq` Prelude.rnf
        replicaProvisionedWriteCapacityAutoScalingSettings
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf replicaStatus
