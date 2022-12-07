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
-- Module      : Amazonka.DynamoDB.Types.ReplicaSettingsUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaSettingsUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingSettingsUpdate
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
import Amazonka.DynamoDB.Types.TableClass
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the settings for a global table in a Region that will be
-- modified.
--
-- /See:/ 'newReplicaSettingsUpdate' smart constructor.
data ReplicaSettingsUpdate = ReplicaSettingsUpdate'
  { -- | Auto scaling settings for managing a global table replica\'s read
    -- capacity units.
    replicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | Represents the settings of a global secondary index for a global table
    -- that will be modified.
    replicaGlobalSecondaryIndexSettingsUpdate :: Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate),
    -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
    -- in the /Amazon DynamoDB Developer Guide/.
    replicaProvisionedReadCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | Replica-specific table class. If not specified, uses the source table\'s
    -- table class.
    replicaTableClass :: Prelude.Maybe TableClass,
    -- | The Region of the replica to be added.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaSettingsUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaProvisionedReadCapacityAutoScalingSettingsUpdate', 'replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing a global table replica\'s read
-- capacity units.
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
-- 'replicaTableClass', 'replicaSettingsUpdate_replicaTableClass' - Replica-specific table class. If not specified, uses the source table\'s
-- table class.
--
-- 'regionName', 'replicaSettingsUpdate_regionName' - The Region of the replica to be added.
newReplicaSettingsUpdate ::
  -- | 'regionName'
  Prelude.Text ->
  ReplicaSettingsUpdate
newReplicaSettingsUpdate pRegionName_ =
  ReplicaSettingsUpdate'
    { replicaProvisionedReadCapacityAutoScalingSettingsUpdate =
        Prelude.Nothing,
      replicaGlobalSecondaryIndexSettingsUpdate =
        Prelude.Nothing,
      replicaProvisionedReadCapacityUnits =
        Prelude.Nothing,
      replicaTableClass = Prelude.Nothing,
      regionName = pRegionName_
    }

-- | Auto scaling settings for managing a global table replica\'s read
-- capacity units.
replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Prelude.Maybe AutoScalingSettingsUpdate)
replicaSettingsUpdate_replicaProvisionedReadCapacityAutoScalingSettingsUpdate = Lens.lens (\ReplicaSettingsUpdate' {replicaProvisionedReadCapacityAutoScalingSettingsUpdate} -> replicaProvisionedReadCapacityAutoScalingSettingsUpdate) (\s@ReplicaSettingsUpdate' {} a -> s {replicaProvisionedReadCapacityAutoScalingSettingsUpdate = a} :: ReplicaSettingsUpdate)

-- | Represents the settings of a global secondary index for a global table
-- that will be modified.
replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate))
replicaSettingsUpdate_replicaGlobalSecondaryIndexSettingsUpdate = Lens.lens (\ReplicaSettingsUpdate' {replicaGlobalSecondaryIndexSettingsUpdate} -> replicaGlobalSecondaryIndexSettingsUpdate) (\s@ReplicaSettingsUpdate' {} a -> s {replicaGlobalSecondaryIndexSettingsUpdate = a} :: ReplicaSettingsUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
replicaSettingsUpdate_replicaProvisionedReadCapacityUnits :: Lens.Lens' ReplicaSettingsUpdate (Prelude.Maybe Prelude.Natural)
replicaSettingsUpdate_replicaProvisionedReadCapacityUnits = Lens.lens (\ReplicaSettingsUpdate' {replicaProvisionedReadCapacityUnits} -> replicaProvisionedReadCapacityUnits) (\s@ReplicaSettingsUpdate' {} a -> s {replicaProvisionedReadCapacityUnits = a} :: ReplicaSettingsUpdate)

-- | Replica-specific table class. If not specified, uses the source table\'s
-- table class.
replicaSettingsUpdate_replicaTableClass :: Lens.Lens' ReplicaSettingsUpdate (Prelude.Maybe TableClass)
replicaSettingsUpdate_replicaTableClass = Lens.lens (\ReplicaSettingsUpdate' {replicaTableClass} -> replicaTableClass) (\s@ReplicaSettingsUpdate' {} a -> s {replicaTableClass = a} :: ReplicaSettingsUpdate)

-- | The Region of the replica to be added.
replicaSettingsUpdate_regionName :: Lens.Lens' ReplicaSettingsUpdate Prelude.Text
replicaSettingsUpdate_regionName = Lens.lens (\ReplicaSettingsUpdate' {regionName} -> regionName) (\s@ReplicaSettingsUpdate' {} a -> s {regionName = a} :: ReplicaSettingsUpdate)

instance Prelude.Hashable ReplicaSettingsUpdate where
  hashWithSalt _salt ReplicaSettingsUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` replicaProvisionedReadCapacityAutoScalingSettingsUpdate
      `Prelude.hashWithSalt` replicaGlobalSecondaryIndexSettingsUpdate
      `Prelude.hashWithSalt` replicaProvisionedReadCapacityUnits
      `Prelude.hashWithSalt` replicaTableClass
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData ReplicaSettingsUpdate where
  rnf ReplicaSettingsUpdate' {..} =
    Prelude.rnf
      replicaProvisionedReadCapacityAutoScalingSettingsUpdate
      `Prelude.seq` Prelude.rnf replicaGlobalSecondaryIndexSettingsUpdate
      `Prelude.seq` Prelude.rnf replicaProvisionedReadCapacityUnits
      `Prelude.seq` Prelude.rnf replicaTableClass
      `Prelude.seq` Prelude.rnf regionName

instance Data.ToJSON ReplicaSettingsUpdate where
  toJSON ReplicaSettingsUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ( "ReplicaProvisionedReadCapacityAutoScalingSettingsUpdate"
                Data..=
            )
              Prelude.<$> replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
            ("ReplicaGlobalSecondaryIndexSettingsUpdate" Data..=)
              Prelude.<$> replicaGlobalSecondaryIndexSettingsUpdate,
            ("ReplicaProvisionedReadCapacityUnits" Data..=)
              Prelude.<$> replicaProvisionedReadCapacityUnits,
            ("ReplicaTableClass" Data..=)
              Prelude.<$> replicaTableClass,
            Prelude.Just ("RegionName" Data..= regionName)
          ]
      )
