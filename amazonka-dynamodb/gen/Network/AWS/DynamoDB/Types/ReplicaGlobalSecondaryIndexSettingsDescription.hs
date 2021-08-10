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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.IndexStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexSettingsDescription' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsDescription = ReplicaGlobalSecondaryIndexSettingsDescription'
  { -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException@.
    provisionedWriteCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@.
    provisionedReadCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | Auto scaling settings for a global secondary index replica\'s read
    -- capacity units.
    provisionedReadCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    -- | Auto scaling settings for a global secondary index replica\'s write
    -- capacity units.
    provisionedWriteCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    -- | The current status of the global secondary index:
    --
    -- -   @CREATING@ - The global secondary index is being created.
    --
    -- -   @UPDATING@ - The global secondary index is being updated.
    --
    -- -   @DELETING@ - The global secondary index is being deleted.
    --
    -- -   @ACTIVE@ - The global secondary index is ready for use.
    indexStatus :: Prelude.Maybe IndexStatus,
    -- | The name of the global secondary index. The name must be unique among
    -- all other indexes on this table.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaGlobalSecondaryIndexSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedWriteCapacityUnits', 'replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@.
--
-- 'provisionedReadCapacityUnits', 'replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@.
--
-- 'provisionedReadCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings' - Auto scaling settings for a global secondary index replica\'s read
-- capacity units.
--
-- 'provisionedWriteCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings' - Auto scaling settings for a global secondary index replica\'s write
-- capacity units.
--
-- 'indexStatus', 'replicaGlobalSecondaryIndexSettingsDescription_indexStatus' - The current status of the global secondary index:
--
-- -   @CREATING@ - The global secondary index is being created.
--
-- -   @UPDATING@ - The global secondary index is being updated.
--
-- -   @DELETING@ - The global secondary index is being deleted.
--
-- -   @ACTIVE@ - The global secondary index is ready for use.
--
-- 'indexName', 'replicaGlobalSecondaryIndexSettingsDescription_indexName' - The name of the global secondary index. The name must be unique among
-- all other indexes on this table.
newReplicaGlobalSecondaryIndexSettingsDescription ::
  -- | 'indexName'
  Prelude.Text ->
  ReplicaGlobalSecondaryIndexSettingsDescription
newReplicaGlobalSecondaryIndexSettingsDescription
  pIndexName_ =
    ReplicaGlobalSecondaryIndexSettingsDescription'
      { provisionedWriteCapacityUnits =
          Prelude.Nothing,
        provisionedReadCapacityUnits =
          Prelude.Nothing,
        provisionedReadCapacityAutoScalingSettings =
          Prelude.Nothing,
        provisionedWriteCapacityAutoScalingSettings =
          Prelude.Nothing,
        indexStatus =
          Prelude.Nothing,
        indexName = pIndexName_
      }

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@.
replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Prelude.Maybe Prelude.Natural)
replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {provisionedWriteCapacityUnits} -> provisionedWriteCapacityUnits) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {provisionedWriteCapacityUnits = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@.
replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Prelude.Maybe Prelude.Natural)
replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {provisionedReadCapacityUnits} -> provisionedReadCapacityUnits) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {provisionedReadCapacityUnits = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

-- | Auto scaling settings for a global secondary index replica\'s read
-- capacity units.
replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {provisionedReadCapacityAutoScalingSettings} -> provisionedReadCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {provisionedReadCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

-- | Auto scaling settings for a global secondary index replica\'s write
-- capacity units.
replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {provisionedWriteCapacityAutoScalingSettings} -> provisionedWriteCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {provisionedWriteCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

-- | The current status of the global secondary index:
--
-- -   @CREATING@ - The global secondary index is being created.
--
-- -   @UPDATING@ - The global secondary index is being updated.
--
-- -   @DELETING@ - The global secondary index is being deleted.
--
-- -   @ACTIVE@ - The global secondary index is ready for use.
replicaGlobalSecondaryIndexSettingsDescription_indexStatus :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Prelude.Maybe IndexStatus)
replicaGlobalSecondaryIndexSettingsDescription_indexStatus = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {indexStatus} -> indexStatus) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {indexStatus = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

-- | The name of the global secondary index. The name must be unique among
-- all other indexes on this table.
replicaGlobalSecondaryIndexSettingsDescription_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription Prelude.Text
replicaGlobalSecondaryIndexSettingsDescription_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

instance
  Core.FromJSON
    ReplicaGlobalSecondaryIndexSettingsDescription
  where
  parseJSON =
    Core.withObject
      "ReplicaGlobalSecondaryIndexSettingsDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexSettingsDescription'
            Prelude.<$> (x Core..:? "ProvisionedWriteCapacityUnits")
              Prelude.<*> (x Core..:? "ProvisionedReadCapacityUnits")
              Prelude.<*> ( x
                              Core..:? "ProvisionedReadCapacityAutoScalingSettings"
                          )
              Prelude.<*> ( x
                              Core..:? "ProvisionedWriteCapacityAutoScalingSettings"
                          )
              Prelude.<*> (x Core..:? "IndexStatus")
              Prelude.<*> (x Core..: "IndexName")
      )

instance
  Prelude.Hashable
    ReplicaGlobalSecondaryIndexSettingsDescription

instance
  Prelude.NFData
    ReplicaGlobalSecondaryIndexSettingsDescription
