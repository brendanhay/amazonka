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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
import qualified Network.AWS.Lens as Lens

-- | Represents the settings for a global table in a Region that will be
-- modified.
--
-- /See:/ 'newReplicaSettingsUpdate' smart constructor.
data ReplicaSettingsUpdate = ReplicaSettingsUpdate'
  { -- | Represents the settings of a global secondary index for a global table
    -- that will be modified.
    replicaGlobalSecondaryIndexSettingsUpdate :: Core.Maybe (Core.NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate),
    -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
    -- in the /Amazon DynamoDB Developer Guide/.
    replicaProvisionedReadCapacityUnits :: Core.Maybe Core.Natural,
    -- | Auto scaling settings for managing a global table replica\'s read
    -- capacity units.
    replicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Core.Maybe AutoScalingSettingsUpdate,
    -- | The Region of the replica to be added.
    regionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicaSettingsUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaGlobalSecondaryIndexSettingsUpdate', 'replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate' - Represents the settings of a global secondary index for a global table
-- that will be modified.
--
-- 'replicaProvisionedReadCapacityUnits', 'replicaSettingsUpdate_replicaProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'replicaProvisionedReadCapacityAutoScalingSettingsUpdate', 'replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing a global table replica\'s read
-- capacity units.
--
-- 'regionName', 'replicaSettingsUpdate_regionName' - The Region of the replica to be added.
newReplicaSettingsUpdate ::
  -- | 'regionName'
  Core.Text ->
  ReplicaSettingsUpdate
newReplicaSettingsUpdate pRegionName_ =
  ReplicaSettingsUpdate'
    { replicaGlobalSecondaryIndexSettingsUpdate =
        Core.Nothing,
      replicaProvisionedReadCapacityUnits = Core.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettingsUpdate =
        Core.Nothing,
      regionName = pRegionName_
    }

-- | Represents the settings of a global secondary index for a global table
-- that will be modified.
replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Core.Maybe (Core.NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate))
replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate = Lens.lens (\ReplicaSettingsUpdate' {replicaGlobalSecondaryIndexSettingsUpdate} -> replicaGlobalSecondaryIndexSettingsUpdate) (\s@ReplicaSettingsUpdate' {} a -> s {replicaGlobalSecondaryIndexSettingsUpdate = a} :: ReplicaSettingsUpdate) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
replicaSettingsUpdate_replicaProvisionedReadCapacityUnits :: Lens.Lens' ReplicaSettingsUpdate (Core.Maybe Core.Natural)
replicaSettingsUpdate_replicaProvisionedReadCapacityUnits = Lens.lens (\ReplicaSettingsUpdate' {replicaProvisionedReadCapacityUnits} -> replicaProvisionedReadCapacityUnits) (\s@ReplicaSettingsUpdate' {} a -> s {replicaProvisionedReadCapacityUnits = a} :: ReplicaSettingsUpdate)

-- | Auto scaling settings for managing a global table replica\'s read
-- capacity units.
replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Core.Maybe AutoScalingSettingsUpdate)
replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate = Lens.lens (\ReplicaSettingsUpdate' {replicaProvisionedReadCapacityAutoScalingSettingsUpdate} -> replicaProvisionedReadCapacityAutoScalingSettingsUpdate) (\s@ReplicaSettingsUpdate' {} a -> s {replicaProvisionedReadCapacityAutoScalingSettingsUpdate = a} :: ReplicaSettingsUpdate)

-- | The Region of the replica to be added.
replicaSettingsUpdate_regionName :: Lens.Lens' ReplicaSettingsUpdate Core.Text
replicaSettingsUpdate_regionName = Lens.lens (\ReplicaSettingsUpdate' {regionName} -> regionName) (\s@ReplicaSettingsUpdate' {} a -> s {regionName = a} :: ReplicaSettingsUpdate)

instance Core.Hashable ReplicaSettingsUpdate

instance Core.NFData ReplicaSettingsUpdate

instance Core.ToJSON ReplicaSettingsUpdate where
  toJSON ReplicaSettingsUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ( "ReplicaGlobalSecondaryIndexSettingsUpdate"
                Core..=
            )
              Core.<$> replicaGlobalSecondaryIndexSettingsUpdate,
            ("ReplicaProvisionedReadCapacityUnits" Core..=)
              Core.<$> replicaProvisionedReadCapacityUnits,
            ( "ReplicaProvisionedReadCapacityAutoScalingSettingsUpdate"
                Core..=
            )
              Core.<$> replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
            Core.Just ("RegionName" Core..= regionName)
          ]
      )
