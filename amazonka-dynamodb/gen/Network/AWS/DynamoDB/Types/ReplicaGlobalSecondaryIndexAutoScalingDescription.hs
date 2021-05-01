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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.IndexStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the auto scaling configuration for a replica global secondary
-- index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexAutoScalingDescription' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingDescription = ReplicaGlobalSecondaryIndexAutoScalingDescription'
  { -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text,
    provisionedReadCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    provisionedWriteCapacityAutoScalingSettings :: Prelude.Maybe AutoScalingSettingsDescription,
    -- | The current state of the replica global secondary index:
    --
    -- -   @CREATING@ - The index is being created.
    --
    -- -   @UPDATING@ - The index is being updated.
    --
    -- -   @DELETING@ - The index is being deleted.
    --
    -- -   @ACTIVE@ - The index is ready for use.
    indexStatus :: Prelude.Maybe IndexStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      provisionedReadCapacityAutoScalingSettings =
        Prelude.Nothing,
      provisionedWriteCapacityAutoScalingSettings =
        Prelude.Nothing,
      indexStatus =
        Prelude.Nothing
    }

-- | The name of the global secondary index.
replicaGlobalSecondaryIndexAutoScalingDescription_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe Prelude.Text)
replicaGlobalSecondaryIndexAutoScalingDescription_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedReadCapacityAutoScalingSettings = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {provisionedReadCapacityAutoScalingSettings} -> provisionedReadCapacityAutoScalingSettings) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {provisionedReadCapacityAutoScalingSettings = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingDescription_provisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe AutoScalingSettingsDescription)
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
replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Prelude.Maybe IndexStatus)
replicaGlobalSecondaryIndexAutoScalingDescription_indexStatus = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingDescription' {indexStatus} -> indexStatus) (\s@ReplicaGlobalSecondaryIndexAutoScalingDescription' {} a -> s {indexStatus = a} :: ReplicaGlobalSecondaryIndexAutoScalingDescription)

instance
  Prelude.FromJSON
    ReplicaGlobalSecondaryIndexAutoScalingDescription
  where
  parseJSON =
    Prelude.withObject
      "ReplicaGlobalSecondaryIndexAutoScalingDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexAutoScalingDescription'
            Prelude.<$> (x Prelude..:? "IndexName")
              Prelude.<*> ( x
                              Prelude..:? "ProvisionedReadCapacityAutoScalingSettings"
                          )
              Prelude.<*> ( x
                              Prelude..:? "ProvisionedWriteCapacityAutoScalingSettings"
                          )
              Prelude.<*> (x Prelude..:? "IndexStatus")
      )

instance
  Prelude.Hashable
    ReplicaGlobalSecondaryIndexAutoScalingDescription

instance
  Prelude.NFData
    ReplicaGlobalSecondaryIndexAutoScalingDescription
