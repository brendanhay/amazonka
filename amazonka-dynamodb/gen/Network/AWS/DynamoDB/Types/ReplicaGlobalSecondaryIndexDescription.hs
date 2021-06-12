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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import qualified Network.AWS.Lens as Lens

-- | Represents the properties of a replica global secondary index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexDescription' smart constructor.
data ReplicaGlobalSecondaryIndexDescription = ReplicaGlobalSecondaryIndexDescription'
  { -- | The name of the global secondary index.
    indexName :: Core.Maybe Core.Text,
    -- | If not described, uses the source table GSI\'s read capacity settings.
    provisionedThroughputOverride :: Core.Maybe ProvisionedThroughputOverride
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicaGlobalSecondaryIndexDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'replicaGlobalSecondaryIndexDescription_indexName' - The name of the global secondary index.
--
-- 'provisionedThroughputOverride', 'replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride' - If not described, uses the source table GSI\'s read capacity settings.
newReplicaGlobalSecondaryIndexDescription ::
  ReplicaGlobalSecondaryIndexDescription
newReplicaGlobalSecondaryIndexDescription =
  ReplicaGlobalSecondaryIndexDescription'
    { indexName =
        Core.Nothing,
      provisionedThroughputOverride =
        Core.Nothing
    }

-- | The name of the global secondary index.
replicaGlobalSecondaryIndexDescription_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexDescription (Core.Maybe Core.Text)
replicaGlobalSecondaryIndexDescription_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexDescription' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexDescription' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexDescription)

-- | If not described, uses the source table GSI\'s read capacity settings.
replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride :: Lens.Lens' ReplicaGlobalSecondaryIndexDescription (Core.Maybe ProvisionedThroughputOverride)
replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride = Lens.lens (\ReplicaGlobalSecondaryIndexDescription' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@ReplicaGlobalSecondaryIndexDescription' {} a -> s {provisionedThroughputOverride = a} :: ReplicaGlobalSecondaryIndexDescription)

instance
  Core.FromJSON
    ReplicaGlobalSecondaryIndexDescription
  where
  parseJSON =
    Core.withObject
      "ReplicaGlobalSecondaryIndexDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexDescription'
            Core.<$> (x Core..:? "IndexName")
            Core.<*> (x Core..:? "ProvisionedThroughputOverride")
      )

instance
  Core.Hashable
    ReplicaGlobalSecondaryIndexDescription

instance
  Core.NFData
    ReplicaGlobalSecondaryIndexDescription
