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
-- Module      : Amazonka.DynamoDB.Types.ReplicaAutoScalingUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaAutoScalingUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingSettingsUpdate
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling settings of a replica that will be modified.
--
-- /See:/ 'newReplicaAutoScalingUpdate' smart constructor.
data ReplicaAutoScalingUpdate = ReplicaAutoScalingUpdate'
  { -- | Represents the auto scaling settings of global secondary indexes that
    -- will be modified.
    replicaGlobalSecondaryIndexUpdates :: Prelude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingUpdate],
    replicaProvisionedReadCapacityAutoScalingUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | The Region where the replica exists.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaAutoScalingUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaGlobalSecondaryIndexUpdates', 'replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates' - Represents the auto scaling settings of global secondary indexes that
-- will be modified.
--
-- 'replicaProvisionedReadCapacityAutoScalingUpdate', 'replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate' - Undocumented member.
--
-- 'regionName', 'replicaAutoScalingUpdate_regionName' - The Region where the replica exists.
newReplicaAutoScalingUpdate ::
  -- | 'regionName'
  Prelude.Text ->
  ReplicaAutoScalingUpdate
newReplicaAutoScalingUpdate pRegionName_ =
  ReplicaAutoScalingUpdate'
    { replicaGlobalSecondaryIndexUpdates =
        Prelude.Nothing,
      replicaProvisionedReadCapacityAutoScalingUpdate =
        Prelude.Nothing,
      regionName = pRegionName_
    }

-- | Represents the auto scaling settings of global secondary indexes that
-- will be modified.
replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates :: Lens.Lens' ReplicaAutoScalingUpdate (Prelude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingUpdate])
replicaAutoScalingUpdate_replicaGlobalSecondaryIndexUpdates = Lens.lens (\ReplicaAutoScalingUpdate' {replicaGlobalSecondaryIndexUpdates} -> replicaGlobalSecondaryIndexUpdates) (\s@ReplicaAutoScalingUpdate' {} a -> s {replicaGlobalSecondaryIndexUpdates = a} :: ReplicaAutoScalingUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaAutoScalingUpdate (Prelude.Maybe AutoScalingSettingsUpdate)
replicaAutoScalingUpdate_replicaProvisionedReadCapacityAutoScalingUpdate = Lens.lens (\ReplicaAutoScalingUpdate' {replicaProvisionedReadCapacityAutoScalingUpdate} -> replicaProvisionedReadCapacityAutoScalingUpdate) (\s@ReplicaAutoScalingUpdate' {} a -> s {replicaProvisionedReadCapacityAutoScalingUpdate = a} :: ReplicaAutoScalingUpdate)

-- | The Region where the replica exists.
replicaAutoScalingUpdate_regionName :: Lens.Lens' ReplicaAutoScalingUpdate Prelude.Text
replicaAutoScalingUpdate_regionName = Lens.lens (\ReplicaAutoScalingUpdate' {regionName} -> regionName) (\s@ReplicaAutoScalingUpdate' {} a -> s {regionName = a} :: ReplicaAutoScalingUpdate)

instance Prelude.Hashable ReplicaAutoScalingUpdate where
  hashWithSalt _salt ReplicaAutoScalingUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` replicaGlobalSecondaryIndexUpdates
      `Prelude.hashWithSalt` replicaProvisionedReadCapacityAutoScalingUpdate
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData ReplicaAutoScalingUpdate where
  rnf ReplicaAutoScalingUpdate' {..} =
    Prelude.rnf replicaGlobalSecondaryIndexUpdates
      `Prelude.seq` Prelude.rnf
        replicaProvisionedReadCapacityAutoScalingUpdate
      `Prelude.seq` Prelude.rnf regionName

instance Data.ToJSON ReplicaAutoScalingUpdate where
  toJSON ReplicaAutoScalingUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReplicaGlobalSecondaryIndexUpdates" Data..=)
              Prelude.<$> replicaGlobalSecondaryIndexUpdates,
            ( "ReplicaProvisionedReadCapacityAutoScalingUpdate"
                Data..=
            )
              Prelude.<$> replicaProvisionedReadCapacityAutoScalingUpdate,
            Prelude.Just ("RegionName" Data..= regionName)
          ]
      )
