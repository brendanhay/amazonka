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
    pidBackfillErrors,
    pidIndexName,
    pidKeys,
    pidIndexStatus,
  )
where

import Network.AWS.Glue.Types.BackfillError
import Network.AWS.Glue.Types.KeySchemaElement
import Network.AWS.Glue.Types.PartitionIndexStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A descriptor for a partition index in a table.
--
-- /See:/ 'mkPartitionIndexDescriptor' smart constructor.
data PartitionIndexDescriptor = PartitionIndexDescriptor'
  { backfillErrors ::
      Lude.Maybe [BackfillError],
    indexName :: Lude.Text,
    keys :: Lude.NonEmpty KeySchemaElement,
    indexStatus :: PartitionIndexStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartitionIndexDescriptor' with the minimum fields required to make a request.
--
-- * 'backfillErrors' - A list of errors that can occur when registering partition indexes for an existing table.
-- * 'indexName' - The name of the partition index.
-- * 'indexStatus' - The status of the partition index.
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
-- * 'keys' - A list of one or more keys, as @KeySchemaElement@ structures, for the partition index.
mkPartitionIndexDescriptor ::
  -- | 'indexName'
  Lude.Text ->
  -- | 'keys'
  Lude.NonEmpty KeySchemaElement ->
  -- | 'indexStatus'
  PartitionIndexStatus ->
  PartitionIndexDescriptor
mkPartitionIndexDescriptor pIndexName_ pKeys_ pIndexStatus_ =
  PartitionIndexDescriptor'
    { backfillErrors = Lude.Nothing,
      indexName = pIndexName_,
      keys = pKeys_,
      indexStatus = pIndexStatus_
    }

-- | A list of errors that can occur when registering partition indexes for an existing table.
--
-- /Note:/ Consider using 'backfillErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pidBackfillErrors :: Lens.Lens' PartitionIndexDescriptor (Lude.Maybe [BackfillError])
pidBackfillErrors = Lens.lens (backfillErrors :: PartitionIndexDescriptor -> Lude.Maybe [BackfillError]) (\s a -> s {backfillErrors = a} :: PartitionIndexDescriptor)
{-# DEPRECATED pidBackfillErrors "Use generic-lens or generic-optics with 'backfillErrors' instead." #-}

-- | The name of the partition index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pidIndexName :: Lens.Lens' PartitionIndexDescriptor Lude.Text
pidIndexName = Lens.lens (indexName :: PartitionIndexDescriptor -> Lude.Text) (\s a -> s {indexName = a} :: PartitionIndexDescriptor)
{-# DEPRECATED pidIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | A list of one or more keys, as @KeySchemaElement@ structures, for the partition index.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pidKeys :: Lens.Lens' PartitionIndexDescriptor (Lude.NonEmpty KeySchemaElement)
pidKeys = Lens.lens (keys :: PartitionIndexDescriptor -> Lude.NonEmpty KeySchemaElement) (\s a -> s {keys = a} :: PartitionIndexDescriptor)
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
pidIndexStatus :: Lens.Lens' PartitionIndexDescriptor PartitionIndexStatus
pidIndexStatus = Lens.lens (indexStatus :: PartitionIndexDescriptor -> PartitionIndexStatus) (\s a -> s {indexStatus = a} :: PartitionIndexDescriptor)
{-# DEPRECATED pidIndexStatus "Use generic-lens or generic-optics with 'indexStatus' instead." #-}

instance Lude.FromJSON PartitionIndexDescriptor where
  parseJSON =
    Lude.withObject
      "PartitionIndexDescriptor"
      ( \x ->
          PartitionIndexDescriptor'
            Lude.<$> (x Lude..:? "BackfillErrors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "IndexName")
            Lude.<*> (x Lude..: "Keys")
            Lude.<*> (x Lude..: "IndexStatus")
      )
