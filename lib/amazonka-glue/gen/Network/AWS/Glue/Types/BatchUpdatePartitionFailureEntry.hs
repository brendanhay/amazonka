{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry
  ( BatchUpdatePartitionFailureEntry (..),

    -- * Smart constructor
    mkBatchUpdatePartitionFailureEntry,

    -- * Lenses
    bupfePartitionValueList,
    bupfeErrorDetail,
  )
where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a batch update partition error.
--
-- /See:/ 'mkBatchUpdatePartitionFailureEntry' smart constructor.
data BatchUpdatePartitionFailureEntry = BatchUpdatePartitionFailureEntry'
  { -- | A list of values defining the partitions.
    partitionValueList :: Lude.Maybe [Lude.Text],
    -- | The details about the batch update partition error.
    errorDetail :: Lude.Maybe ErrorDetail
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchUpdatePartitionFailureEntry' with the minimum fields required to make a request.
--
-- * 'partitionValueList' - A list of values defining the partitions.
-- * 'errorDetail' - The details about the batch update partition error.
mkBatchUpdatePartitionFailureEntry ::
  BatchUpdatePartitionFailureEntry
mkBatchUpdatePartitionFailureEntry =
  BatchUpdatePartitionFailureEntry'
    { partitionValueList =
        Lude.Nothing,
      errorDetail = Lude.Nothing
    }

-- | A list of values defining the partitions.
--
-- /Note:/ Consider using 'partitionValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupfePartitionValueList :: Lens.Lens' BatchUpdatePartitionFailureEntry (Lude.Maybe [Lude.Text])
bupfePartitionValueList = Lens.lens (partitionValueList :: BatchUpdatePartitionFailureEntry -> Lude.Maybe [Lude.Text]) (\s a -> s {partitionValueList = a} :: BatchUpdatePartitionFailureEntry)
{-# DEPRECATED bupfePartitionValueList "Use generic-lens or generic-optics with 'partitionValueList' instead." #-}

-- | The details about the batch update partition error.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bupfeErrorDetail :: Lens.Lens' BatchUpdatePartitionFailureEntry (Lude.Maybe ErrorDetail)
bupfeErrorDetail = Lens.lens (errorDetail :: BatchUpdatePartitionFailureEntry -> Lude.Maybe ErrorDetail) (\s a -> s {errorDetail = a} :: BatchUpdatePartitionFailureEntry)
{-# DEPRECATED bupfeErrorDetail "Use generic-lens or generic-optics with 'errorDetail' instead." #-}

instance Lude.FromJSON BatchUpdatePartitionFailureEntry where
  parseJSON =
    Lude.withObject
      "BatchUpdatePartitionFailureEntry"
      ( \x ->
          BatchUpdatePartitionFailureEntry'
            Lude.<$> (x Lude..:? "PartitionValueList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ErrorDetail")
      )
