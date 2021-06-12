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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
import qualified Network.AWS.Lens as Lens

-- | Represents the auto scaling settings of a replica that will be modified.
--
-- /See:/ 'newReplicaAutoScalingUpdate' smart constructor.
data ReplicaAutoScalingUpdate = ReplicaAutoScalingUpdate'
  { replicaProvisionedReadCapacityAutoScalingUpdate :: Core.Maybe AutoScalingSettingsUpdate,
    -- | Represents the auto scaling settings of global secondary indexes that
    -- will be modified.
    replicaGlobalSecondaryIndexUpdates :: Core.Maybe [ReplicaGlobalSecondaryIndexAutoScalingUpdate],
    -- | The Region where the replica exists.
    regionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicaAutoScalingUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaProvisionedReadCapacityAutoScalingUpdate', 'replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate' - Undocumented member.
--
-- 'replicaGlobalSecondaryIndexUpdates', 'replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates' - Represents the auto scaling settings of global secondary indexes that
-- will be modified.
--
-- 'regionName', 'replicaAutoScalingUpdate_regionName' - The Region where the replica exists.
newReplicaAutoScalingUpdate ::
  -- | 'regionName'
  Core.Text ->
  ReplicaAutoScalingUpdate
newReplicaAutoScalingUpdate pRegionName_ =
  ReplicaAutoScalingUpdate'
    { replicaProvisionedReadCapacityAutoScalingUpdate =
        Core.Nothing,
      replicaGlobalSecondaryIndexUpdates = Core.Nothing,
      regionName = pRegionName_
    }

-- | Undocumented member.
replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaAutoScalingUpdate (Core.Maybe AutoScalingSettingsUpdate)
replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate = Lens.lens (\ReplicaAutoScalingUpdate' {replicaProvisionedReadCapacityAutoScalingUpdate} -> replicaProvisionedReadCapacityAutoScalingUpdate) (\s@ReplicaAutoScalingUpdate' {} a -> s {replicaProvisionedReadCapacityAutoScalingUpdate = a} :: ReplicaAutoScalingUpdate)

-- | Represents the auto scaling settings of global secondary indexes that
-- will be modified.
replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates :: Lens.Lens' ReplicaAutoScalingUpdate (Core.Maybe [ReplicaGlobalSecondaryIndexAutoScalingUpdate])
replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates = Lens.lens (\ReplicaAutoScalingUpdate' {replicaGlobalSecondaryIndexUpdates} -> replicaGlobalSecondaryIndexUpdates) (\s@ReplicaAutoScalingUpdate' {} a -> s {replicaGlobalSecondaryIndexUpdates = a} :: ReplicaAutoScalingUpdate) Core.. Lens.mapping Lens._Coerce

-- | The Region where the replica exists.
replicaAutoScalingUpdate_regionName :: Lens.Lens' ReplicaAutoScalingUpdate Core.Text
replicaAutoScalingUpdate_regionName = Lens.lens (\ReplicaAutoScalingUpdate' {regionName} -> regionName) (\s@ReplicaAutoScalingUpdate' {} a -> s {regionName = a} :: ReplicaAutoScalingUpdate)

instance Core.Hashable ReplicaAutoScalingUpdate

instance Core.NFData ReplicaAutoScalingUpdate

instance Core.ToJSON ReplicaAutoScalingUpdate where
  toJSON ReplicaAutoScalingUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ( "ReplicaProvisionedReadCapacityAutoScalingUpdate"
                Core..=
            )
              Core.<$> replicaProvisionedReadCapacityAutoScalingUpdate,
            ("ReplicaGlobalSecondaryIndexUpdates" Core..=)
              Core.<$> replicaGlobalSecondaryIndexUpdates,
            Core.Just ("RegionName" Core..= regionName)
          ]
      )
