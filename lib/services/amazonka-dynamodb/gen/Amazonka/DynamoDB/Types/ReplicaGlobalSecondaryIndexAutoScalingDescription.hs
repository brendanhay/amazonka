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
-- Module      : Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.AutoScalingSettingsDescription
import Amazonka.DynamoDB.Types.IndexStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling configuration for a replica global secondary
-- index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexAutoScalingDescription' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingDescription = ReplicaGlobalSecondaryIndexAutoScalingDescription'
  { -- | The current state of the replica global secondary index:
    --
    -- -   @CREATING@ - The index is being created.
    --
    -- -   @UPDATING@ - The index is being updated.
    --
    -- -   @DELETING@ - The index is being deleted.
    --
    -- -   @ACTIVE@ - The index is ready for use.
    indexStatus :: Prelude.Maybe IndexStatus,
    provisionedWriteCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    provisionedReadCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaGlobalSecondaryIndexAutoScalingDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'provisionedWriteCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings' - Undocumented member.
--
-- 'provisionedReadCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings' - Undocumented member.
--
-- 'indexName', 'replicaGlobalSecondaryIndexAutoScalingDescription_indexName' - The name of the global secondary index.
newReplicaGlobalSecondaryIndexAutoScalingDescription ::
  ReplicaGlobalSecondaryIndexAutoScalingDescription
newReplicaGlobalSecondaryIndexAutoScalingDescription =
  ReplicaGlobalSecondaryIndexAutoScalingDescription'
    { indexStatus =
        Prelude.Nothing,
      provisionedWriteCapacityAutoScalingSettings =
        Prelude.Nothing,
      provisionedReadCapacityAutoScalingSettings =
        Prelude.Nothing,
      indexName =
        Prelude.Nothing
    }

-- | The current state of the replica global secondary index:
--
-- -   @CREATING@ - The index is being created.
--
-- -   @UPDATING@ - The index is being updated.
--
-- -   @DELETING@ - The index is being deleted.
--
-- -   @ACTIVE@ - The index is ready for use.
replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe IndexStatus)
replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {indexStatus} -> indexStatus) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {indexStatus = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {provisionedWriteCapacityAutoScalingSettings} -> provisionedWriteCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {provisionedWriteCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {provisionedReadCapacityAutoScalingSettings} -> provisionedReadCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {provisionedReadCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | The name of the global secondary index.
replicaGlobalSecondaryIndexAutoScalingDescription_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe Prelude.Text)
replicaGlobalSecondaryIndexAutoScalingDescription_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

instance
  Core.FromJSON
    ReplicaGlobalSecondaryIndexAutoScalingDescription
  where
  parseJSON =
    Core.withObject
      "ReplicaGlobalSecondaryIndexAutoScalingDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexAutoScalingDescription'
            Prelude.<$> (x Core..:? "IndexStatus")
              Prelude.<*> ( x
                              Core..:? "ProvisionedWriteCapacityAutoScalingSettings"
                          )
              Prelude.<*> ( x
                              Core..:? "ProvisionedReadCapacityAutoScalingSettings"
                          )
              Prelude.<*> (x Core..:? "IndexName")
      )

instance
  Prelude.Hashable
    ReplicaGlobalSecondaryIndexAutoScalingDescription
  where
  hashWithSalt
    salt'
    ReplicaGlobalSecondaryIndexAutoScalingDescription' {..} =
      salt' `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` provisionedReadCapacityAutoScalingSettings
        `Prelude.hashWithSalt` provisionedWriteCapacityAutoScalingSettings
        `Prelude.hashWithSalt` indexStatus

instance
  Prelude.NFData
    ReplicaGlobalSecondaryIndexAutoScalingDescription
  where
  rnf
    ReplicaGlobalSecondaryIndexAutoScalingDescription' {..} =
      Prelude.rnf indexStatus
        `Prelude.seq` Prelude.rnf indexName
        `Prelude.seq` Prelude.rnf
          provisionedReadCapacityAutoScalingSettings
        `Prelude.seq` Prelude.rnf
          provisionedWriteCapacityAutoScalingSettings
