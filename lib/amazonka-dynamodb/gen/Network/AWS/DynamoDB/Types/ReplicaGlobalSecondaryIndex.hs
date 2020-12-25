{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
  ( ReplicaGlobalSecondaryIndex (..),

    -- * Smart constructor
    mkReplicaGlobalSecondaryIndex,

    -- * Lenses
    rgsiIndexName,
    rgsiProvisionedThroughputOverride,
  )
where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a replica global secondary index.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndex' smart constructor.
data ReplicaGlobalSecondaryIndex = ReplicaGlobalSecondaryIndex'
  { -- | The name of the global secondary index.
    indexName :: Types.IndexName,
    -- | Replica table GSI-specific provisioned throughput. If not specified, uses the source table GSI's read capacity settings.
    provisionedThroughputOverride :: Core.Maybe Types.ProvisionedThroughputOverride
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaGlobalSecondaryIndex' value with any optional fields omitted.
mkReplicaGlobalSecondaryIndex ::
  -- | 'indexName'
  Types.IndexName ->
  ReplicaGlobalSecondaryIndex
mkReplicaGlobalSecondaryIndex indexName =
  ReplicaGlobalSecondaryIndex'
    { indexName,
      provisionedThroughputOverride = Core.Nothing
    }

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndex Types.IndexName
rgsiIndexName = Lens.field @"indexName"
{-# DEPRECATED rgsiIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | Replica table GSI-specific provisioned throughput. If not specified, uses the source table GSI's read capacity settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiProvisionedThroughputOverride :: Lens.Lens' ReplicaGlobalSecondaryIndex (Core.Maybe Types.ProvisionedThroughputOverride)
rgsiProvisionedThroughputOverride = Lens.field @"provisionedThroughputOverride"
{-# DEPRECATED rgsiProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

instance Core.FromJSON ReplicaGlobalSecondaryIndex where
  toJSON ReplicaGlobalSecondaryIndex {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IndexName" Core..= indexName),
            ("ProvisionedThroughputOverride" Core..=)
              Core.<$> provisionedThroughputOverride
          ]
      )
