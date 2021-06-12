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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
import Network.AWS.DynamoDB.Types.ReplicaStatus
import qualified Network.AWS.Lens as Lens

-- | Represents the auto scaling settings of the replica.
--
-- /See:/ 'newReplicaAutoScalingDescription' smart constructor.
data ReplicaAutoScalingDescription = ReplicaAutoScalingDescription'
  { -- | The Region where the replica exists.
    regionName :: Core.Maybe Core.Text,
    replicaProvisionedReadCapacityAutoScalingSettings :: Core.Maybe AutoScalingSettingsDescription,
    -- | Replica-specific global secondary index auto scaling settings.
    globalSecondaryIndexes :: Core.Maybe [ReplicaGlobalSecondaryIndexAutoScalingDescription],
    replicaProvisionedWriteCapacityAutoScalingSettings :: Core.Maybe AutoScalingSettingsDescription,
    -- | The current state of the replica:
    --
    -- -   @CREATING@ - The replica is being created.
    --
    -- -   @UPDATING@ - The replica is being updated.
    --
    -- -   @DELETING@ - The replica is being deleted.
    --
    -- -   @ACTIVE@ - The replica is ready for use.
    replicaStatus :: Core.Maybe ReplicaStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicaAutoScalingDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'replicaAutoScalingDescription_regionName' - The Region where the replica exists.
--
-- 'replicaProvisionedReadCapacityAutoScalingSettings', 'replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings' - Undocumented member.
--
-- 'globalSecondaryIndexes', 'replicaAutoScalingDescription_globalSecondaryIndexes' - Replica-specific global secondary index auto scaling settings.
--
-- 'replicaProvisionedWriteCapacityAutoScalingSettings', 'replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings' - Undocumented member.
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
    { regionName =
        Core.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettings =
        Core.Nothing,
      globalSecondaryIndexes = Core.Nothing,
      replicaProvisionedWriteCapacityAutoScalingSettings =
        Core.Nothing,
      replicaStatus = Core.Nothing
    }

-- | The Region where the replica exists.
replicaAutoScalingDescription_regionName :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe Core.Text)
replicaAutoScalingDescription_regionName = Lens.lens (\ReplicaAutoScalingDescription' {regionName} -> regionName) (\s@ReplicaAutoScalingDescription' {} a -> s {regionName = a} :: ReplicaAutoScalingDescription)

-- | Undocumented member.
replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe AutoScalingSettingsDescription)
replicaAutoScalingDescription_replicaProvisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaAutoScalingDescription' {replicaProvisionedReadCapacityAutoScalingSettings} -> replicaProvisionedReadCapacityAutoScalingSettings) (\s@ReplicaAutoScalingDescription' {} a -> s {replicaProvisionedReadCapacityAutoScalingSettings = a} :: ReplicaAutoScalingDescription)

-- | Replica-specific global secondary index auto scaling settings.
replicaAutoScalingDescription_globalSecondaryIndexes :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe [ReplicaGlobalSecondaryIndexAutoScalingDescription])
replicaAutoScalingDescription_globalSecondaryIndexes = Lens.lens (\ReplicaAutoScalingDescription' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@ReplicaAutoScalingDescription' {} a -> s {globalSecondaryIndexes = a} :: ReplicaAutoScalingDescription) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe AutoScalingSettingsDescription)
replicaAutoScalingDescription_replicaProvisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaAutoScalingDescription' {replicaProvisionedWriteCapacityAutoScalingSettings} -> replicaProvisionedWriteCapacityAutoScalingSettings) (\s@ReplicaAutoScalingDescription' {} a -> s {replicaProvisionedWriteCapacityAutoScalingSettings = a} :: ReplicaAutoScalingDescription)

-- | The current state of the replica:
--
-- -   @CREATING@ - The replica is being created.
--
-- -   @UPDATING@ - The replica is being updated.
--
-- -   @DELETING@ - The replica is being deleted.
--
-- -   @ACTIVE@ - The replica is ready for use.
replicaAutoScalingDescription_replicaStatus :: Lens.Lens' ReplicaAutoScalingDescription (Core.Maybe ReplicaStatus)
replicaAutoScalingDescription_replicaStatus = Lens.lens (\ReplicaAutoScalingDescription' {replicaStatus} -> replicaStatus) (\s@ReplicaAutoScalingDescription' {} a -> s {replicaStatus = a} :: ReplicaAutoScalingDescription)

instance Core.FromJSON ReplicaAutoScalingDescription where
  parseJSON =
    Core.withObject
      "ReplicaAutoScalingDescription"
      ( \x ->
          ReplicaAutoScalingDescription'
            Core.<$> (x Core..:? "RegionName")
            Core.<*> ( x
                         Core..:? "ReplicaProvisionedReadCapacityAutoScalingSettings"
                     )
            Core.<*> ( x Core..:? "GlobalSecondaryIndexes"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x
                         Core..:? "ReplicaProvisionedWriteCapacityAutoScalingSettings"
                     )
            Core.<*> (x Core..:? "ReplicaStatus")
      )

instance Core.Hashable ReplicaAutoScalingDescription

instance Core.NFData ReplicaAutoScalingDescription
