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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.IndexStatus
import qualified Network.AWS.Lens as Lens

-- | Represents the auto scaling configuration for a replica global secondary
-- index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexAutoScalingDescription' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingDescription = ReplicaGlobalSecondaryIndexAutoScalingDescription'
  { -- | The name of the global secondary index.
    indexName :: Core.Maybe Core.Text,
    provisionedReadCapacityAutoScalingSettings :: Core.Maybe AutoScalingSettingsDescription,
    provisionedWriteCapacityAutoScalingSettings :: Core.Maybe AutoScalingSettingsDescription,
    -- | The current state of the replica global secondary index:
    --
    -- -   @CREATING@ - The index is being created.
    --
    -- -   @UPDATING@ - The index is being updated.
    --
    -- -   @DELETING@ - The index is being deleted.
    --
    -- -   @ACTIVE@ - The index is ready for use.
    indexStatus :: Core.Maybe IndexStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicaGlobalSecondaryIndexAutoScalingDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'replicaGlobalSecondaryIndexAutoScalingDescription_indexName' - The name of the global secondary index.
--
-- 'provisionedReadCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings' - Undocumented member.
--
-- 'provisionedWriteCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings' - Undocumented member.
--
-- 'indexStatus', 'replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus' - The current state of the replica global secondary index:
--
-- -   @CREATING@ - The index is being created.
--
-- -   @UPDATING@ - The index is being updated.
--
-- -   @DELETING@ - The index is being deleted.
--
-- -   @ACTIVE@ - The index is ready for use.
newReplicaGlobalSecondaryIndexAutoScalingDescription ::
  ReplicaGlobalSecondaryIndexAutoScalingDescription
newReplicaGlobalSecondaryIndexAutoScalingDescription =
  ReplicaGlobalSecondaryIndexAutoScalingDescription'
    { indexName =
        Core.Nothing,
      provisionedReadCapacityAutoScalingSettings =
        Core.Nothing,
      provisionedWriteCapacityAutoScalingSettings =
        Core.Nothing,
      indexStatus =
        Core.Nothing
    }

-- | The name of the global secondary index.
replicaGlobalSecondaryIndexAutoScalingDescription_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Core.Maybe Core.Text)
replicaGlobalSecondaryIndexAutoScalingDescription_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Core.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {provisionedReadCapacityAutoScalingSettings} -> provisionedReadCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {provisionedReadCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Core.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {provisionedWriteCapacityAutoScalingSettings} -> provisionedWriteCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {provisionedWriteCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | The current state of the replica global secondary index:
--
-- -   @CREATING@ - The index is being created.
--
-- -   @UPDATING@ - The index is being updated.
--
-- -   @DELETING@ - The index is being deleted.
--
-- -   @ACTIVE@ - The index is ready for use.
replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Core.Maybe IndexStatus)
replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {indexStatus} -> indexStatus) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {indexStatus = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

instance
  Core.FromJSON
    ReplicaGlobalSecondaryIndexAutoScalingDescription
  where
  parseJSON =
    Core.withObject
      "ReplicaGlobalSecondaryIndexAutoScalingDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexAutoScalingDescription'
            Core.<$> (x Core..:? "IndexName")
              Core.<*> ( x
                           Core..:? "ProvisionedReadCapacityAutoScalingSettings"
                       )
              Core.<*> ( x
                           Core..:? "ProvisionedWriteCapacityAutoScalingSettings"
                       )
              Core.<*> (x Core..:? "IndexStatus")
      )

instance
  Core.Hashable
    ReplicaGlobalSecondaryIndexAutoScalingDescription

instance
  Core.NFData
    ReplicaGlobalSecondaryIndexAutoScalingDescription
