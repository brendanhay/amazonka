{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionIndexDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionIndexDescriptor
  ( PartitionIndexDescriptor (..),

    -- * Smart constructor
    mkPartitionIndexDescriptor,

    -- * Lenses
    pidIndexName,
    pidKeys,
    pidIndexStatus,
    pidBackfillErrors,
  )
where

import qualified Network.AWS.Glue.Types.BackfillError as Types
import qualified Network.AWS.Glue.Types.IndexName as Types
import qualified Network.AWS.Glue.Types.KeySchemaElement as Types
import qualified Network.AWS.Glue.Types.PartitionIndexStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A descriptor for a partition index in a table.
--
-- /See:/ 'mkPartitionIndexDescriptor' smart constructor.
data PartitionIndexDescriptor = PartitionIndexDescriptor'
  { -- | The name of the partition index.
    indexName :: Types.IndexName,
    -- | A list of one or more keys, as @KeySchemaElement@ structures, for the partition index.
    keys :: Core.NonEmpty Types.KeySchemaElement,
    -- | The status of the partition index.
    --
    -- The possible statuses are:
    --
    --     * CREATING: The index is being created. When an index is in a CREATING state, the index or its table cannot be deleted.
    --
    --
    --     * ACTIVE: The index creation succeeds.
    --
    --
    --     * FAILED: The index creation fails.
    --
    --
    --     * DELETING: The index is deleted from the list of indexes.
    indexStatus :: Types.PartitionIndexStatus,
    -- | A list of errors that can occur when registering partition indexes for an existing table.
    backfillErrors :: Core.Maybe [Types.BackfillError]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PartitionIndexDescriptor' value with any optional fields omitted.
mkPartitionIndexDescriptor ::
  -- | 'indexName'
  Types.IndexName ->
  -- | 'keys'
  Core.NonEmpty Types.KeySchemaElement ->
  -- | 'indexStatus'
  Types.PartitionIndexStatus ->
  PartitionIndexDescriptor
mkPartitionIndexDescriptor indexName keys indexStatus =
  PartitionIndexDescriptor'
    { indexName,
      keys,
      indexStatus,
      backfillErrors = Core.Nothing
    }

-- | The name of the partition index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pidIndexName :: Lens.Lens' PartitionIndexDescriptor Types.IndexName
pidIndexName = Lens.field @"indexName"
{-# DEPRECATED pidIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | A list of one or more keys, as @KeySchemaElement@ structures, for the partition index.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pidKeys :: Lens.Lens' PartitionIndexDescriptor (Core.NonEmpty Types.KeySchemaElement)
pidKeys = Lens.field @"keys"
{-# DEPRECATED pidKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | The status of the partition index.
--
-- The possible statuses are:
--
--     * CREATING: The index is being created. When an index is in a CREATING state, the index or its table cannot be deleted.
--
--
--     * ACTIVE: The index creation succeeds.
--
--
--     * FAILED: The index creation fails.
--
--
--     * DELETING: The index is deleted from the list of indexes.
--
--
--
-- /Note:/ Consider using 'indexStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pidIndexStatus :: Lens.Lens' PartitionIndexDescriptor Types.PartitionIndexStatus
pidIndexStatus = Lens.field @"indexStatus"
{-# DEPRECATED pidIndexStatus "Use generic-lens or generic-optics with 'indexStatus' instead." #-}

-- | A list of errors that can occur when registering partition indexes for an existing table.
--
-- /Note:/ Consider using 'backfillErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pidBackfillErrors :: Lens.Lens' PartitionIndexDescriptor (Core.Maybe [Types.BackfillError])
pidBackfillErrors = Lens.field @"backfillErrors"
{-# DEPRECATED pidBackfillErrors "Use generic-lens or generic-optics with 'backfillErrors' instead." #-}

instance Core.FromJSON PartitionIndexDescriptor where
  parseJSON =
    Core.withObject "PartitionIndexDescriptor" Core.$
      \x ->
        PartitionIndexDescriptor'
          Core.<$> (x Core..: "IndexName")
          Core.<*> (x Core..: "Keys")
          Core.<*> (x Core..: "IndexStatus")
          Core.<*> (x Core..:? "BackfillErrors")
