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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import qualified Network.AWS.Lens as Lens

-- | Represents the properties of a replica global secondary index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndex' smart constructor.
data ReplicaGlobalSecondaryIndex = ReplicaGlobalSecondaryIndex'
  { -- | Replica table GSI-specific provisioned throughput. If not specified,
    -- uses the source table GSI\'s read capacity settings.
    provisionedThroughputOverride :: Core.Maybe ProvisionedThroughputOverride,
    -- | The name of the global secondary index.
    indexName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicaGlobalSecondaryIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughputOverride', 'replicaGlobalSecondaryIndex_provisionedThroughputOverride' - Replica table GSI-specific provisioned throughput. If not specified,
-- uses the source table GSI\'s read capacity settings.
--
-- 'indexName', 'replicaGlobalSecondaryIndex_indexName' - The name of the global secondary index.
newReplicaGlobalSecondaryIndex ::
  -- | 'indexName'
  Core.Text ->
  ReplicaGlobalSecondaryIndex
newReplicaGlobalSecondaryIndex pIndexName_ =
  ReplicaGlobalSecondaryIndex'
    { provisionedThroughputOverride =
        Core.Nothing,
      indexName = pIndexName_
    }

-- | Replica table GSI-specific provisioned throughput. If not specified,
-- uses the source table GSI\'s read capacity settings.
replicaGlobalSecondaryIndex_provisionedThroughputOverride :: Lens.Lens' ReplicaGlobalSecondaryIndex (Core.Maybe ProvisionedThroughputOverride)
replicaGlobalSecondaryIndex_provisionedThroughputOverride = Lens.lens (\ReplicaGlobalSecondaryIndex' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@ReplicaGlobalSecondaryIndex' {} a -> s {provisionedThroughputOverride = a} :: ReplicaGlobalSecondaryIndex)

-- | The name of the global secondary index.
replicaGlobalSecondaryIndex_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndex Core.Text
replicaGlobalSecondaryIndex_indexName = Lens.lens (\ReplicaGlobalSecondaryIndex' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndex' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndex)

instance Core.Hashable ReplicaGlobalSecondaryIndex

instance Core.NFData ReplicaGlobalSecondaryIndex

instance Core.ToJSON ReplicaGlobalSecondaryIndex where
  toJSON ReplicaGlobalSecondaryIndex' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProvisionedThroughputOverride" Core..=)
              Core.<$> provisionedThroughputOverride,
            Core.Just ("IndexName" Core..= indexName)
          ]
      )
