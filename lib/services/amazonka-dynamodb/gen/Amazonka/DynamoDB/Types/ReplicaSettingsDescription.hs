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
-- Module      : Amazonka.DynamoDB.Types.ReplicaSettingsDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaSettingsDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingSettingsDescription
import Amazonka.DynamoDB.Types.BillingModeSummary
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
import Amazonka.DynamoDB.Types.ReplicaStatus
import Amazonka.DynamoDB.Types.TableClassSummary
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a replica.
--
-- /See:/ 'newReplicaSettingsDescription' smart constructor.
data ReplicaSettingsDescription = ReplicaSettingsDescription'
  { -- | The read\/write capacity mode of the replica.
    replicaBillingModeSummary :: Prelude.Maybe BillingModeSummary,
    -- | Replica global secondary index settings for the global table.
    replicaGlobalSecondaryIndexSettings :: Prelude.Maybe [ReplicaGlobalSecondaryIndexSettingsDescription],
    -- | Auto scaling settings for a global table replica\'s read capacity units.
    replicaProvisionedReadCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
    -- in the /Amazon DynamoDB Developer Guide/.
    replicaProvisionedReadCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | Auto scaling settings for a global table replica\'s write capacity
    -- units.
    replicaProvisionedWriteCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException@. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
    -- in the /Amazon DynamoDB Developer Guide/.
    replicaProvisionedWriteCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The current state of the Region:
    --
    -- -   @CREATING@ - The Region is being created.
    --
    -- -   @UPDATING@ - The Region is being updated.
    --
    -- -   @DELETING@ - The Region is being deleted.
    --
    -- -   @ACTIVE@ - The Region is ready for use.
    replicaStatus :: Prelude.Maybe ReplicaStatus,
    replicaTableClassSummary :: Prelude.Maybe TableClassSummary,
    -- | The Region name of the replica.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaBillingModeSummary', 'replicaSettingsDescription_replicaBillingModeSummary' - The read\/write capacity mode of the replica.
--
-- 'replicaGlobalSecondaryIndexSettings', 'replicaSettingsDescription_replicaGlobalSecondaryIndexSettings' - Replica global secondary index settings for the global table.
--
-- 'replicaProvisionedReadCapacityAutoScalingSettings', 'replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings' - Auto scaling settings for a global table replica\'s read capacity units.
--
-- 'replicaProvisionedReadCapacityUnits', 'replicaSettingsDescription_replicaProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'replicaProvisionedWriteCapacityAutoScalingSettings', 'replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings' - Auto scaling settings for a global table replica\'s write capacity
-- units.
--
-- 'replicaProvisionedWriteCapacityUnits', 'replicaSettingsDescription_replicaProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'replicaStatus', 'replicaSettingsDescription_replicaStatus' - The current state of the Region:
--
-- -   @CREATING@ - The Region is being created.
--
-- -   @UPDATING@ - The Region is being updated.
--
-- -   @DELETING@ - The Region is being deleted.
--
-- -   @ACTIVE@ - The Region is ready for use.
--
-- 'replicaTableClassSummary', 'replicaSettingsDescription_replicaTableClassSummary' - Undocumented member.
--
-- 'regionName', 'replicaSettingsDescription_regionName' - The Region name of the replica.
newReplicaSettingsDescription ::
  -- | 'regionName'
  Prelude.Text ->
  ReplicaSettingsDescription
newReplicaSettingsDescription pRegionName_ =
  ReplicaSettingsDescription'
    { replicaBillingModeSummary =
        Prelude.Nothing,
      replicaGlobalSecondaryIndexSettings =
        Prelude.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettings =
        Prelude.Nothing,
      replicaProvisionedReadCapacityUnits =
        Prelude.Nothing,
      replicaProvisionedWriteCapacityAutoScalingSettings =
        Prelude.Nothing,
      replicaProvisionedWriteCapacityUnits =
        Prelude.Nothing,
      replicaStatus = Prelude.Nothing,
      replicaTableClassSummary = Prelude.Nothing,
      regionName = pRegionName_
    }

-- | The read\/write capacity mode of the replica.
replicaSettingsDescription_replicaBillingModeSummary :: Lens.Lens' ReplicaSettingsDescription (Prelude.Maybe BillingModeSummary)
replicaSettingsDescription_replicaBillingModeSummary = Lens.lens (\ReplicaSettingsDescription' {replicaBillingModeSummary} -> replicaBillingModeSummary) (\s@ReplicaSettingsDescription' {} a -> s {replicaBillingModeSummary = a} :: ReplicaSettingsDescription)

-- | Replica global secondary index settings for the global table.
replicaSettingsDescription_replicaGlobalSecondaryIndexSettings :: Lens.Lens' ReplicaSettingsDescription (Prelude.Maybe [ReplicaGlobalSecondaryIndexSettingsDescription])
replicaSettingsDescription_replicaGlobalSecondaryIndexSettings = Lens.lens (\ReplicaSettingsDescription' {replicaGlobalSecondaryIndexSettings} -> replicaGlobalSecondaryIndexSettings) (\s@ReplicaSettingsDescription' {} a -> s {replicaGlobalSecondaryIndexSettings = a} :: ReplicaSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | Auto scaling settings for a global table replica\'s read capacity units.
replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaSettingsDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaSettingsDescription' {replicaProvisionedReadCapacityAutoScalingSettings} -> replicaProvisionedReadCapacityAutoScalingSettings) (\s@ReplicaSettingsDescription' {} a -> s {replicaProvisionedReadCapacityAutoScalingSettings = a} :: ReplicaSettingsDescription)

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
replicaSettingsDescription_replicaProvisionedReadCapacityUnits :: Lens.Lens' ReplicaSettingsDescription (Prelude.Maybe Prelude.Natural)
replicaSettingsDescription_replicaProvisionedReadCapacityUnits = Lens.lens (\ReplicaSettingsDescription' {replicaProvisionedReadCapacityUnits} -> replicaProvisionedReadCapacityUnits) (\s@ReplicaSettingsDescription' {} a -> s {replicaProvisionedReadCapacityUnits = a} :: ReplicaSettingsDescription)

-- | Auto scaling settings for a global table replica\'s write capacity
-- units.
replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaSettingsDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaSettingsDescription' {replicaProvisionedWriteCapacityAutoScalingSettings} -> replicaProvisionedWriteCapacityAutoScalingSettings) (\s@ReplicaSettingsDescription' {} a -> s {replicaProvisionedWriteCapacityAutoScalingSettings = a} :: ReplicaSettingsDescription)

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
replicaSettingsDescription_replicaProvisionedWriteCapacityUnits :: Lens.Lens' ReplicaSettingsDescription (Prelude.Maybe Prelude.Natural)
replicaSettingsDescription_replicaProvisionedWriteCapacityUnits = Lens.lens (\ReplicaSettingsDescription' {replicaProvisionedWriteCapacityUnits} -> replicaProvisionedWriteCapacityUnits) (\s@ReplicaSettingsDescription' {} a -> s {replicaProvisionedWriteCapacityUnits = a} :: ReplicaSettingsDescription)

-- | The current state of the Region:
--
-- -   @CREATING@ - The Region is being created.
--
-- -   @UPDATING@ - The Region is being updated.
--
-- -   @DELETING@ - The Region is being deleted.
--
-- -   @ACTIVE@ - The Region is ready for use.
replicaSettingsDescription_replicaStatus :: Lens.Lens' ReplicaSettingsDescription (Prelude.Maybe ReplicaStatus)
replicaSettingsDescription_replicaStatus = Lens.lens (\ReplicaSettingsDescription' {replicaStatus} -> replicaStatus) (\s@ReplicaSettingsDescription' {} a -> s {replicaStatus = a} :: ReplicaSettingsDescription)

-- | Undocumented member.
replicaSettingsDescription_replicaTableClassSummary :: Lens.Lens' ReplicaSettingsDescription (Prelude.Maybe TableClassSummary)
replicaSettingsDescription_replicaTableClassSummary = Lens.lens (\ReplicaSettingsDescription' {replicaTableClassSummary} -> replicaTableClassSummary) (\s@ReplicaSettingsDescription' {} a -> s {replicaTableClassSummary = a} :: ReplicaSettingsDescription)

-- | The Region name of the replica.
replicaSettingsDescription_regionName :: Lens.Lens' ReplicaSettingsDescription Prelude.Text
replicaSettingsDescription_regionName = Lens.lens (\ReplicaSettingsDescription' {regionName} -> regionName) (\s@ReplicaSettingsDescription' {} a -> s {regionName = a} :: ReplicaSettingsDescription)

instance Data.FromJSON ReplicaSettingsDescription where
  parseJSON =
    Data.withObject
      "ReplicaSettingsDescription"
      ( \x ->
          ReplicaSettingsDescription'
            Prelude.<$> (x Data..:? "ReplicaBillingModeSummary")
            Prelude.<*> ( x Data..:? "ReplicaGlobalSecondaryIndexSettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ReplicaProvisionedReadCapacityAutoScalingSettings"
                        )
            Prelude.<*> (x Data..:? "ReplicaProvisionedReadCapacityUnits")
            Prelude.<*> ( x
                            Data..:? "ReplicaProvisionedWriteCapacityAutoScalingSettings"
                        )
            Prelude.<*> (x Data..:? "ReplicaProvisionedWriteCapacityUnits")
            Prelude.<*> (x Data..:? "ReplicaStatus")
            Prelude.<*> (x Data..:? "ReplicaTableClassSummary")
            Prelude.<*> (x Data..: "RegionName")
      )

instance Prelude.Hashable ReplicaSettingsDescription where
  hashWithSalt _salt ReplicaSettingsDescription' {..} =
    _salt
      `Prelude.hashWithSalt` replicaBillingModeSummary
      `Prelude.hashWithSalt` replicaGlobalSecondaryIndexSettings
      `Prelude.hashWithSalt` replicaProvisionedReadCapacityAutoScalingSettings
      `Prelude.hashWithSalt` replicaProvisionedReadCapacityUnits
      `Prelude.hashWithSalt` replicaProvisionedWriteCapacityAutoScalingSettings
      `Prelude.hashWithSalt` replicaProvisionedWriteCapacityUnits
      `Prelude.hashWithSalt` replicaStatus
      `Prelude.hashWithSalt` replicaTableClassSummary
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData ReplicaSettingsDescription where
  rnf ReplicaSettingsDescription' {..} =
    Prelude.rnf replicaBillingModeSummary
      `Prelude.seq` Prelude.rnf replicaGlobalSecondaryIndexSettings
      `Prelude.seq` Prelude.rnf
        replicaProvisionedReadCapacityAutoScalingSettings
      `Prelude.seq` Prelude.rnf replicaProvisionedReadCapacityUnits
      `Prelude.seq` Prelude.rnf
        replicaProvisionedWriteCapacityAutoScalingSettings
      `Prelude.seq` Prelude.rnf replicaProvisionedWriteCapacityUnits
      `Prelude.seq` Prelude.rnf replicaStatus
      `Prelude.seq` Prelude.rnf replicaTableClassSummary
      `Prelude.seq` Prelude.rnf regionName
