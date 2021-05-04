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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the auto scaling settings of a replica that will be modified.
--
-- /See:/ 'newReplicaAutoScalingUpdate' smart constructor.
data ReplicaAutoScalingUpdate = ReplicaAutoScalingUpdate'
  { replicaProvisionedReadCapacityAutoScalingUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | Represents the auto scaling settings of global secondary indexes that
    -- will be modified.
    replicaGlobalSecondaryIndexUpdates :: Prelude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingUpdate],
    -- | The Region where the replica exists.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ReplicaAutoScalingUpdate
newReplicaAutoScalingUpdate pRegionName_ =
  ReplicaAutoScalingUpdate'
    { replicaProvisionedReadCapacityAutoScalingUpdate =
        Prelude.Nothing,
      replicaGlobalSecondaryIndexUpdates =
        Prelude.Nothing,
      regionName = pRegionName_
    }

-- | Undocumented member.
replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaAutoScalingUpdate (Prelude.Maybe AutoScalingSettingsUpdate)
replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate = Lens.lens (\ReplicaAutoScalingUpdate' {replicaProvisionedReadCapacityAutoScalingUpdate} -> replicaProvisionedReadCapacityAutoScalingUpdate) (\s@ReplicaAutoScalingUpdate' {} a -> s {replicaProvisionedReadCapacityAutoScalingUpdate = a} :: ReplicaAutoScalingUpdate)

-- | Represents the auto scaling settings of global secondary indexes that
-- will be modified.
replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates :: Lens.Lens' ReplicaAutoScalingUpdate (Prelude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingUpdate])
replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates = Lens.lens (\ReplicaAutoScalingUpdate' {replicaGlobalSecondaryIndexUpdates} -> replicaGlobalSecondaryIndexUpdates) (\s@ReplicaAutoScalingUpdate' {} a -> s {replicaGlobalSecondaryIndexUpdates = a} :: ReplicaAutoScalingUpdate) Prelude.. Lens.mapping Prelude._Coerce

-- | The Region where the replica exists.
replicaAutoScalingUpdate_regionName :: Lens.Lens' ReplicaAutoScalingUpdate Prelude.Text
replicaAutoScalingUpdate_regionName = Lens.lens (\ReplicaAutoScalingUpdate' {regionName} -> regionName) (\s@ReplicaAutoScalingUpdate' {} a -> s {regionName = a} :: ReplicaAutoScalingUpdate)

instance Prelude.Hashable ReplicaAutoScalingUpdate

instance Prelude.NFData ReplicaAutoScalingUpdate

instance Prelude.ToJSON ReplicaAutoScalingUpdate where
  toJSON ReplicaAutoScalingUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ( "ReplicaProvisionedReadCapacityAutoScalingUpdate"
                Prelude..=
            )
              Prelude.<$> replicaProvisionedReadCapacityAutoScalingUpdate,
            ("ReplicaGlobalSecondaryIndexUpdates" Prelude..=)
              Prelude.<$> replicaGlobalSecondaryIndexUpdates,
            Prelude.Just ("RegionName" Prelude..= regionName)
          ]
      )
