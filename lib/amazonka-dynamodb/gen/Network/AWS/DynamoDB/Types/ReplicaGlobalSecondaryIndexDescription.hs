{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
  ( ReplicaGlobalSecondaryIndexDescription (..),

    -- * Smart constructor
    mkReplicaGlobalSecondaryIndexDescription,

    -- * Lenses
    rgsidIndexName,
    rgsidProvisionedThroughputOverride,
  )
where

import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a replica global secondary index.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexDescription' smart constructor.
data ReplicaGlobalSecondaryIndexDescription = ReplicaGlobalSecondaryIndexDescription'
  { -- | The name of the global secondary index.
    indexName :: Core.Maybe Types.IndexName,
    -- | If not described, uses the source table GSI's read capacity settings.
    provisionedThroughputOverride :: Core.Maybe Types.ProvisionedThroughputOverride
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaGlobalSecondaryIndexDescription' value with any optional fields omitted.
mkReplicaGlobalSecondaryIndexDescription ::
  ReplicaGlobalSecondaryIndexDescription
mkReplicaGlobalSecondaryIndexDescription =
  ReplicaGlobalSecondaryIndexDescription'
    { indexName = Core.Nothing,
      provisionedThroughputOverride = Core.Nothing
    }

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsidIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexDescription (Core.Maybe Types.IndexName)
rgsidIndexName = Lens.field @"indexName"
{-# DEPRECATED rgsidIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | If not described, uses the source table GSI's read capacity settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsidProvisionedThroughputOverride :: Lens.Lens' ReplicaGlobalSecondaryIndexDescription (Core.Maybe Types.ProvisionedThroughputOverride)
rgsidProvisionedThroughputOverride = Lens.field @"provisionedThroughputOverride"
{-# DEPRECATED rgsidProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

instance Core.FromJSON ReplicaGlobalSecondaryIndexDescription where
  parseJSON =
    Core.withObject "ReplicaGlobalSecondaryIndexDescription" Core.$
      \x ->
        ReplicaGlobalSecondaryIndexDescription'
          Core.<$> (x Core..:? "IndexName")
          Core.<*> (x Core..:? "ProvisionedThroughputOverride")
