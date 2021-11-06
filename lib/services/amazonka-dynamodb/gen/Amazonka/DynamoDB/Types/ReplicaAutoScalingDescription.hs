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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaAutoScalingDescription where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.AutoScalingSettingsDescription
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
import Amazonka.DynamoDB.Types.ReplicaStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling settings of the replica.
--
-- /See:/ 'newReplicaAutoScalingDescription' smart constructor.
data ReplicaAutoScalingDescription = ReplicaAutoScalingDescription'
  { -- | The current state of the replica:
    --
    -- -   @CREATING@ - The replica is being created.
    --
    -- -   @UPDATING@ - The replica is being updated.
    --
    -- -   @DELETING@ - The replica is being deleted.
    --
    -- -   @ACTIVE@ - The replica is ready for use.
    replicaStatus :: Prelude.Maybe ReplicaStatus,
    -- | The Region where the replica exists.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific global secondary index auto scaling settings.
    globalSecondaryIndexes :: Prelude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingDescription],
    replicaProvisionedWriteCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    replicaProvisionedReadCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription
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
-- 'replicaStatus', 'replicaAutoScalingDescription_replicaStatus' - The current state of the replica:
--
-- -   @CREATING@ - The replica is being created.
--
-- -   @UPDATING@ - The replica is being updated.
--
-- -   @DELETING@ - The replica is being deleted.
--
-- -   @ACTIVE@ - The replica is ready for use.
--
-- 'regionName', 'replicaAutoScalingDescription_regionName' - The Region where the replica exists.
--
-- 'globalSecondaryIndexes', 'replicaAutoScalingDescription_globalSecondaryIndexes' - Replica-specific global secondary index auto scaling settings.
--
-- 'replicaProvisionedWriteCapacityAutoScalingSettings', 'replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings' - Undocumented member.
--
-- 'replicaProvisionedReadCapacityAutoScalingSettings', 'replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings' - Undocumented member.
newReplicaAutoScalingDescription ::
  ReplicaAutoScalingDescription
newReplicaAutoScalingDescription =
  ReplicaAutoScalingDescription'
    { replicaStatus =
        Prelude.Nothing,
      regionName = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      replicaProvisionedWriteCapacityAutoScalingSettings =
        Prelude.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettings =
        Prelude.Nothing
    }

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

-- | The Region where the replica exists.
replicaAutoScalingDescription_regionName :: Lens.Lens' ReplicaAutoScalingDescription (Prelude.Maybe Prelude.Text)
replicaAutoScalingDescription_regionName = Lens.lens (\ReplicaAutoScalingDescription' {regionName} -> regionName) (\s@ReplicaAutoScalingDescription' {} a -> s {regionName = a} :: ReplicaAutoScalingDescription)

-- | Replica-specific global secondary index auto scaling settings.
replicaAutoScalingDescription_globalSecondaryIndexes :: Lens.Lens' ReplicaAutoScalingDescription (Prelude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingDescription])
replicaAutoScalingDescription_globalSecondaryIndexes = Lens.lens (\ReplicaAutoScalingDescription' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@ReplicaAutoScalingDescription' {} a -> s {globalSecondaryIndexes = a} :: ReplicaAutoScalingDescription) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaAutoScalingDescription' {replicaProvisionedWriteCapacityAutoScalingSettings} -> replicaProvisionedWriteCapacityAutoScalingSettings) (\s@ReplicaAutoScalingDescription' {} a -> s {replicaProvisionedWriteCapacityAutoScalingSettings = a} :: ReplicaAutoScalingDescription)

-- | Undocumented member.
replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaAutoScalingDescription' {replicaProvisionedReadCapacityAutoScalingSettings} -> replicaProvisionedReadCapacityAutoScalingSettings) (\s@ReplicaAutoScalingDescription' {} a -> s {replicaProvisionedReadCapacityAutoScalingSettings = a} :: ReplicaAutoScalingDescription)

instance Core.FromJSON ReplicaAutoScalingDescription where
  parseJSON =
    Core.withObject
      "ReplicaAutoScalingDescription"
      ( \x ->
          ReplicaAutoScalingDescription'
            Prelude.<$> (x Core..:? "ReplicaStatus")
            Prelude.<*> (x Core..:? "RegionName")
            Prelude.<*> ( x Core..:? "GlobalSecondaryIndexes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Core..:? "ReplicaProvisionedWriteCapacityAutoScalingSettings"
                        )
            Prelude.<*> ( x
                            Core..:? "ReplicaProvisionedReadCapacityAutoScalingSettings"
                        )
      )

instance
  Prelude.Hashable
    ReplicaAutoScalingDescription

instance Prelude.NFData ReplicaAutoScalingDescription
