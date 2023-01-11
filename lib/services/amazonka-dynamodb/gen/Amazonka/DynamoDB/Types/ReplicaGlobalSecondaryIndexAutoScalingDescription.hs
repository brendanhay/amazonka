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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingSettingsDescription
import Amazonka.DynamoDB.Types.IndexStatus
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling configuration for a replica global secondary
-- index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexAutoScalingDescription' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingDescription = ReplicaGlobalSecondaryIndexAutoScalingDescription'
  { -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the replica global secondary index:
    --
    -- -   @CREATING@ - The index is being created.
    --
    -- -   @UPDATING@ - The table\/index configuration is being updated. The
    --     table\/index remains available for data operations when @UPDATING@
    --
    -- -   @DELETING@ - The index is being deleted.
    --
    -- -   @ACTIVE@ - The index is ready for use.
    indexStatus :: Prelude.Maybe IndexStatus,
    provisionedReadCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    provisionedWriteCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription
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
-- 'indexName', 'replicaGlobalSecondaryIndexAutoScalingDescription_indexName' - The name of the global secondary index.
--
-- 'indexStatus', 'replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus' - The current state of the replica global secondary index:
--
-- -   @CREATING@ - The index is being created.
--
-- -   @UPDATING@ - The table\/index configuration is being updated. The
--     table\/index remains available for data operations when @UPDATING@
--
-- -   @DELETING@ - The index is being deleted.
--
-- -   @ACTIVE@ - The index is ready for use.
--
-- 'provisionedReadCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings' - Undocumented member.
--
-- 'provisionedWriteCapacityAutoScalingSettings', 'replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings' - Undocumented member.
newReplicaGlobalSecondaryIndexAutoScalingDescription ::
  ReplicaGlobalSecondaryIndexAutoScalingDescription
newReplicaGlobalSecondaryIndexAutoScalingDescription =
  ReplicaGlobalSecondaryIndexAutoScalingDescription'
    { indexName =
        Prelude.Nothing,
      indexStatus =
        Prelude.Nothing,
      provisionedReadCapacityAutoScalingSettings =
        Prelude.Nothing,
      provisionedWriteCapacityAutoScalingSettings =
        Prelude.Nothing
    }

-- | The name of the global secondary index.
replicaGlobalSecondaryIndexAutoScalingDescription_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe Prelude.Text)
replicaGlobalSecondaryIndexAutoScalingDescription_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | The current state of the replica global secondary index:
--
-- -   @CREATING@ - The index is being created.
--
-- -   @UPDATING@ - The table\/index configuration is being updated. The
--     table\/index remains available for data operations when @UPDATING@
--
-- -   @DELETING@ - The index is being deleted.
--
-- -   @ACTIVE@ - The index is ready for use.
replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe IndexStatus)
replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {indexStatus} -> indexStatus) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {indexStatus = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {provisionedReadCapacityAutoScalingSettings} -> provisionedReadCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {provisionedReadCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {provisionedWriteCapacityAutoScalingSettings} -> provisionedWriteCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {provisionedWriteCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

instance
  Data.FromJSON
    ReplicaGlobalSecondaryIndexAutoScalingDescription
  where
  parseJSON =
    Data.withObject
      "ReplicaGlobalSecondaryIndexAutoScalingDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexAutoScalingDescription'
            Prelude.<$> (x Data..:? "IndexName")
              Prelude.<*> (x Data..:? "IndexStatus")
              Prelude.<*> ( x
                              Data..:? "ProvisionedReadCapacityAutoScalingSettings"
                          )
              Prelude.<*> ( x
                              Data..:? "ProvisionedWriteCapacityAutoScalingSettings"
                          )
      )

instance
  Prelude.Hashable
    ReplicaGlobalSecondaryIndexAutoScalingDescription
  where
  hashWithSalt
    _salt
    ReplicaGlobalSecondaryIndexAutoScalingDescription' {..} =
      _salt `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` indexStatus
        `Prelude.hashWithSalt` provisionedReadCapacityAutoScalingSettings
        `Prelude.hashWithSalt` provisionedWriteCapacityAutoScalingSettings

instance
  Prelude.NFData
    ReplicaGlobalSecondaryIndexAutoScalingDescription
  where
  rnf
    ReplicaGlobalSecondaryIndexAutoScalingDescription' {..} =
      Prelude.rnf indexName
        `Prelude.seq` Prelude.rnf indexStatus
        `Prelude.seq` Prelude.rnf
          provisionedReadCapacityAutoScalingSettings
        `Prelude.seq` Prelude.rnf
          provisionedWriteCapacityAutoScalingSettings
