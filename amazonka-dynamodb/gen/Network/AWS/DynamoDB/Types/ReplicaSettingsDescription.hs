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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaSettingsDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaSettingsDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.BillingModeSummary
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaStatus
import qualified Network.AWS.Lens as Lens

-- | Represents the properties of a replica.
--
-- /See:/ 'newReplicaSettingsDescription' smart constructor.
data ReplicaSettingsDescription = ReplicaSettingsDescription'
  { -- | The read\/write capacity mode of the replica.
    replicaBillingModeSummary :: Core.Maybe BillingModeSummary,
    -- | Auto scaling settings for a global table replica\'s read capacity units.
    replicaProvisionedReadCapacityAutoScalingSettings :: Core.Maybe AutoScalingSettingsDescription,
    -- | Auto scaling settings for a global table replica\'s write capacity
    -- units.
    replicaProvisionedWriteCapacityAutoScalingSettings :: Core.Maybe AutoScalingSettingsDescription,
    -- | Replica global secondary index settings for the global table.
    replicaGlobalSecondaryIndexSettings :: Core.Maybe [ReplicaGlobalSecondaryIndexSettingsDescription],
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException@. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
    -- in the /Amazon DynamoDB Developer Guide/.
    replicaProvisionedWriteCapacityUnits :: Core.Maybe Core.Natural,
    -- | The current state of the Region:
    --
    -- -   @CREATING@ - The Region is being created.
    --
    -- -   @UPDATING@ - The Region is being updated.
    --
    -- -   @DELETING@ - The Region is being deleted.
    --
    -- -   @ACTIVE@ - The Region is ready for use.
    replicaStatus :: Core.Maybe ReplicaStatus,
    -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
    -- in the /Amazon DynamoDB Developer Guide/.
    replicaProvisionedReadCapacityUnits :: Core.Maybe Core.Natural,
    -- | The Region name of the replica.
    regionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'replicaProvisionedReadCapacityAutoScalingSettings', 'replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings' - Auto scaling settings for a global table replica\'s read capacity units.
--
-- 'replicaProvisionedWriteCapacityAutoScalingSettings', 'replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings' - Auto scaling settings for a global table replica\'s write capacity
-- units.
--
-- 'replicaGlobalSecondaryIndexSettings', 'replicaSettingsDescription_replicaGlobalSecondaryIndexSettings' - Replica global secondary index settings for the global table.
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
-- 'replicaProvisionedReadCapacityUnits', 'replicaSettingsDescription_replicaProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'regionName', 'replicaSettingsDescription_regionName' - The Region name of the replica.
newReplicaSettingsDescription ::
  -- | 'regionName'
  Core.Text ->
  ReplicaSettingsDescription
newReplicaSettingsDescription pRegionName_ =
  ReplicaSettingsDescription'
    { replicaBillingModeSummary =
        Core.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettings =
        Core.Nothing,
      replicaProvisionedWriteCapacityAutoScalingSettings =
        Core.Nothing,
      replicaGlobalSecondaryIndexSettings =
        Core.Nothing,
      replicaProvisionedWriteCapacityUnits =
        Core.Nothing,
      replicaStatus = Core.Nothing,
      replicaProvisionedReadCapacityUnits =
        Core.Nothing,
      regionName = pRegionName_
    }

-- | The read\/write capacity mode of the replica.
replicaSettingsDescription_replicaBillingModeSummary :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe BillingModeSummary)
replicaSettingsDescription_replicaBillingModeSummary = Lens.lens (\ReplicaSettingsDescription' {replicaBillingModeSummary} -> replicaBillingModeSummary) (\s@ReplicaSettingsDescription' {} a -> s {replicaBillingModeSummary = a} :: ReplicaSettingsDescription)

-- | Auto scaling settings for a global table replica\'s read capacity units.
replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe AutoScalingSettingsDescription)
replicaSettingsDescription_replicaProvisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaSettingsDescription' {replicaProvisionedReadCapacityAutoScalingSettings} -> replicaProvisionedReadCapacityAutoScalingSettings) (\s@ReplicaSettingsDescription' {} a -> s {replicaProvisionedReadCapacityAutoScalingSettings = a} :: ReplicaSettingsDescription)

-- | Auto scaling settings for a global table replica\'s write capacity
-- units.
replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe AutoScalingSettingsDescription)
replicaSettingsDescription_replicaProvisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaSettingsDescription' {replicaProvisionedWriteCapacityAutoScalingSettings} -> replicaProvisionedWriteCapacityAutoScalingSettings) (\s@ReplicaSettingsDescription' {} a -> s {replicaProvisionedWriteCapacityAutoScalingSettings = a} :: ReplicaSettingsDescription)

-- | Replica global secondary index settings for the global table.
replicaSettingsDescription_replicaGlobalSecondaryIndexSettings :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe [ReplicaGlobalSecondaryIndexSettingsDescription])
replicaSettingsDescription_replicaGlobalSecondaryIndexSettings = Lens.lens (\ReplicaSettingsDescription' {replicaGlobalSecondaryIndexSettings} -> replicaGlobalSecondaryIndexSettings) (\s@ReplicaSettingsDescription' {} a -> s {replicaGlobalSecondaryIndexSettings = a} :: ReplicaSettingsDescription) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
replicaSettingsDescription_replicaProvisionedWriteCapacityUnits :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe Core.Natural)
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
replicaSettingsDescription_replicaStatus :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe ReplicaStatus)
replicaSettingsDescription_replicaStatus = Lens.lens (\ReplicaSettingsDescription' {replicaStatus} -> replicaStatus) (\s@ReplicaSettingsDescription' {} a -> s {replicaStatus = a} :: ReplicaSettingsDescription)

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements>
-- in the /Amazon DynamoDB Developer Guide/.
replicaSettingsDescription_replicaProvisionedReadCapacityUnits :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe Core.Natural)
replicaSettingsDescription_replicaProvisionedReadCapacityUnits = Lens.lens (\ReplicaSettingsDescription' {replicaProvisionedReadCapacityUnits} -> replicaProvisionedReadCapacityUnits) (\s@ReplicaSettingsDescription' {} a -> s {replicaProvisionedReadCapacityUnits = a} :: ReplicaSettingsDescription)

-- | The Region name of the replica.
replicaSettingsDescription_regionName :: Lens.Lens' ReplicaSettingsDescription Core.Text
replicaSettingsDescription_regionName = Lens.lens (\ReplicaSettingsDescription' {regionName} -> regionName) (\s@ReplicaSettingsDescription' {} a -> s {regionName = a} :: ReplicaSettingsDescription)

instance Core.FromJSON ReplicaSettingsDescription where
  parseJSON =
    Core.withObject
      "ReplicaSettingsDescription"
      ( \x ->
          ReplicaSettingsDescription'
            Core.<$> (x Core..:? "ReplicaBillingModeSummary")
            Core.<*> ( x
                         Core..:? "ReplicaProvisionedReadCapacityAutoScalingSettings"
                     )
            Core.<*> ( x
                         Core..:? "ReplicaProvisionedWriteCapacityAutoScalingSettings"
                     )
            Core.<*> ( x Core..:? "ReplicaGlobalSecondaryIndexSettings"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ReplicaProvisionedWriteCapacityUnits")
            Core.<*> (x Core..:? "ReplicaStatus")
            Core.<*> (x Core..:? "ReplicaProvisionedReadCapacityUnits")
            Core.<*> (x Core..: "RegionName")
      )

instance Core.Hashable ReplicaSettingsDescription

instance Core.NFData ReplicaSettingsDescription
