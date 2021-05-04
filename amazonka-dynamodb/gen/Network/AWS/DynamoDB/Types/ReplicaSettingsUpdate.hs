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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the settings for a global table in a Region that will be
-- modified.
--
-- /See:/ 'newReplicaSettingsUpdate' smart constructor.
data ReplicaSettingsUpdate = ReplicaSettingsUpdate'
  { -- | Represents the settings of a global secondary index for a global table
    -- that will be modified.
    replicaGlobalSecondaryIndexSettingsUpdate :: Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate),
    -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
    -- in the /Amazon DynamoDB Developer Guide/.
    replicaProvisionedReadCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | Auto scaling settings for managing a global table replica\'s read
    -- capacity units.
    replicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | The Region of the replica to be added.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ReplicaSettingsUpdate
newReplicaSettingsUpdate pRegionName_ =
  ReplicaSettingsUpdate'
    { replicaGlobalSecondaryIndexSettingsUpdate =
        Prelude.Nothing,
      replicaProvisionedReadCapacityUnits =
        Prelude.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettingsUpdate =
        Prelude.Nothing,
      regionName = pRegionName_
    }

-- | Represents the settings of a global secondary index for a global table
-- that will be modified.
replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate))
replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate = Lens.lens (\ReplicaSettingsUpdate' {replicaGlobalSecondaryIndexSettingsUpdate} -> replicaGlobalSecondaryIndexSettingsUpdate) (\s@ReplicaSettingsUpdate' {} a -> s {replicaGlobalSecondaryIndexSettingsUpdate = a} :: ReplicaSettingsUpdate) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
replicaSettingsUpdate_replicaProvisionedReadCapacityUnits :: Lens.Lens' ReplicaSettingsUpdate (Prelude.Maybe Prelude.Natural)
replicaSettingsUpdate_replicaProvisionedReadCapacityUnits = Lens.lens (\ReplicaSettingsUpdate' {replicaProvisionedReadCapacityUnits} -> replicaProvisionedReadCapacityUnits) (\s@ReplicaSettingsUpdate' {} a -> s {replicaProvisionedReadCapacityUnits = a} :: ReplicaSettingsUpdate)

-- | Auto scaling settings for managing a global table replica\'s read
-- capacity units.
replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Prelude.Maybe AutoScalingSettingsUpdate)
replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate = Lens.lens (\ReplicaSettingsUpdate' {replicaProvisionedReadCapacityAutoScalingSettingsUpdate} -> replicaProvisionedReadCapacityAutoScalingSettingsUpdate) (\s@ReplicaSettingsUpdate' {} a -> s {replicaProvisionedReadCapacityAutoScalingSettingsUpdate = a} :: ReplicaSettingsUpdate)

-- | The Region of the replica to be added.
replicaSettingsUpdate_regionName :: Lens.Lens' ReplicaSettingsUpdate Prelude.Text
replicaSettingsUpdate_regionName = Lens.lens (\ReplicaSettingsUpdate' {regionName} -> regionName) (\s@ReplicaSettingsUpdate' {} a -> s {regionName = a} :: ReplicaSettingsUpdate)

instance Prelude.Hashable ReplicaSettingsUpdate

instance Prelude.NFData ReplicaSettingsUpdate

instance Prelude.ToJSON ReplicaSettingsUpdate where
  toJSON ReplicaSettingsUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ( "ReplicaGlobalSecondaryIndexSettingsUpdate"
                Prelude..=
            )
              Prelude.<$> replicaGlobalSecondaryIndexSettingsUpdate,
            ("ReplicaProvisionedReadCapacityUnits" Prelude..=)
              Prelude.<$> replicaProvisionedReadCapacityUnits,
            ( "ReplicaProvisionedReadCapacityAutoScalingSettingsUpdate"
                Prelude..=
            )
              Prelude.<$> replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
            Prelude.Just ("RegionName" Prelude..= regionName)
          ]
      )
