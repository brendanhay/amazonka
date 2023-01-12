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
-- Module      : Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingSettingsDescription
import Amazonka.DynamoDB.Types.IndexStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexSettingsDescription' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsDescription = ReplicaGlobalSecondaryIndexSettingsDescription'
  { -- | The current status of the global secondary index:
    --
    -- -   @CREATING@ - The global secondary index is being created.
    --
    -- -   @UPDATING@ - The global secondary index is being updated.
    --
    -- -   @DELETING@ - The global secondary index is being deleted.
    --
    -- -   @ACTIVE@ - The global secondary index is ready for use.
    indexStatus :: Prelude.Maybe IndexStatus,
    -- | Auto scaling settings for a global secondary index replica\'s read
    -- capacity units.
    provisionedReadCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    -- | The maximum number of strongly consistent reads consumed per second
    -- before DynamoDB returns a @ThrottlingException@.
    provisionedReadCapacityUnits :: Prelude.Maybe Prelude.Natural,
    -- | Auto scaling settings for a global secondary index replica\'s write
    -- capacity units.
    provisionedWriteCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    -- | The maximum number of writes consumed per second before DynamoDB returns
    -- a @ThrottlingException@.
    provisionedWriteCapacityUnits :: Prelude.Maybe Prelude.Natural,
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
-- 'provisionedReadCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings' - Auto scaling settings for a global secondary index replica\'s read
-- capacity units.
--
-- 'provisionedReadCapacityUnits', 'replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@.
--
-- 'provisionedWriteCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings' - Auto scaling settings for a global secondary index replica\'s write
-- capacity units.
--
-- 'provisionedWriteCapacityUnits', 'replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@.
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
      { indexStatus =
          Prelude.Nothing,
        provisionedReadCapacityAutoScalingSettings =
          Prelude.Nothing,
        provisionedReadCapacityUnits =
          Prelude.Nothing,
        provisionedWriteCapacityAutoScalingSettings =
          Prelude.Nothing,
        provisionedWriteCapacityUnits =
          Prelude.Nothing,
        indexName = pIndexName_
      }

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

-- | Auto scaling settings for a global secondary index replica\'s read
-- capacity units.
replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {provisionedReadCapacityAutoScalingSettings} -> provisionedReadCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {provisionedReadCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

-- | The maximum number of strongly consistent reads consumed per second
-- before DynamoDB returns a @ThrottlingException@.
replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Prelude.Maybe Prelude.Natural)
replicaGlobalSecondaryIndexSettingsDescription_provisionedReadCapacityUnits = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {provisionedReadCapacityUnits} -> provisionedReadCapacityUnits) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {provisionedReadCapacityUnits = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

-- | Auto scaling settings for a global secondary index replica\'s write
-- capacity units.
replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {provisionedWriteCapacityAutoScalingSettings} -> provisionedWriteCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {provisionedWriteCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

-- | The maximum number of writes consumed per second before DynamoDB returns
-- a @ThrottlingException@.
replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Prelude.Maybe Prelude.Natural)
replicaGlobalSecondaryIndexSettingsDescription_provisionedWriteCapacityUnits = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {provisionedWriteCapacityUnits} -> provisionedWriteCapacityUnits) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {provisionedWriteCapacityUnits = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

-- | The name of the global secondary index. The name must be unique among
-- all other indexes on this table.
replicaGlobalSecondaryIndexSettingsDescription_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription Prelude.Text
replicaGlobalSecondaryIndexSettingsDescription_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexSettingsDescription' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexSettingsDescription' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexSettingsDescription)

instance
  Data.FromJSON
    ReplicaGlobalSecondaryIndexSettingsDescription
  where
  parseJSON =
    Data.withObject
      "ReplicaGlobalSecondaryIndexSettingsDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexSettingsDescription'
            Prelude.<$> (x Data..:? "IndexStatus")
              Prelude.<*> ( x
                              Data..:? "ProvisionedReadCapacityAutoScalingSettings"
                          )
              Prelude.<*> (x Data..:? "ProvisionedReadCapacityUnits")
              Prelude.<*> ( x
                              Data..:? "ProvisionedWriteCapacityAutoScalingSettings"
                          )
              Prelude.<*> (x Data..:? "ProvisionedWriteCapacityUnits")
              Prelude.<*> (x Data..: "IndexName")
      )

instance
  Prelude.Hashable
    ReplicaGlobalSecondaryIndexSettingsDescription
  where
  hashWithSalt
    _salt
    ReplicaGlobalSecondaryIndexSettingsDescription' {..} =
      _salt `Prelude.hashWithSalt` indexStatus
        `Prelude.hashWithSalt` provisionedReadCapacityAutoScalingSettings
        `Prelude.hashWithSalt` provisionedReadCapacityUnits
        `Prelude.hashWithSalt` provisionedWriteCapacityAutoScalingSettings
        `Prelude.hashWithSalt` provisionedWriteCapacityUnits
        `Prelude.hashWithSalt` indexName

instance
  Prelude.NFData
    ReplicaGlobalSecondaryIndexSettingsDescription
  where
  rnf
    ReplicaGlobalSecondaryIndexSettingsDescription' {..} =
      Prelude.rnf indexStatus
        `Prelude.seq` Prelude.rnf
          provisionedReadCapacityAutoScalingSettings
        `Prelude.seq` Prelude.rnf provisionedReadCapacityUnits
        `Prelude.seq` Prelude.rnf
          provisionedWriteCapacityAutoScalingSettings
        `Prelude.seq` Prelude.rnf provisionedWriteCapacityUnits
        `Prelude.seq` Prelude.rnf indexName
