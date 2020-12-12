{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionError
  ( PartitionError (..),

    -- * Smart constructor
    mkPartitionError,

    -- * Lenses
    pePartitionValues,
    peErrorDetail,
  )
where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a partition error.
--
-- /See:/ 'mkPartitionError' smart constructor.
data PartitionError = PartitionError'
  { partitionValues ::
      Lude.Maybe [Lude.Text],
    errorDetail :: Lude.Maybe ErrorDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartitionError' with the minimum fields required to make a request.
--
-- * 'errorDetail' - The details about the partition error.
-- * 'partitionValues' - The values that define the partition.
mkPartitionError ::
  PartitionError
mkPartitionError =
  PartitionError'
    { partitionValues = Lude.Nothing,
      errorDetail = Lude.Nothing
    }

-- | The values that define the partition.
--
-- /Note:/ Consider using 'partitionValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pePartitionValues :: Lens.Lens' PartitionError (Lude.Maybe [Lude.Text])
pePartitionValues = Lens.lens (partitionValues :: PartitionError -> Lude.Maybe [Lude.Text]) (\s a -> s {partitionValues = a} :: PartitionError)
{-# DEPRECATED pePartitionValues "Use generic-lens or generic-optics with 'partitionValues' instead." #-}

-- | The details about the partition error.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peErrorDetail :: Lens.Lens' PartitionError (Lude.Maybe ErrorDetail)
peErrorDetail = Lens.lens (errorDetail :: PartitionError -> Lude.Maybe ErrorDetail) (\s a -> s {errorDetail = a} :: PartitionError)
{-# DEPRECATED peErrorDetail "Use generic-lens or generic-optics with 'errorDetail' instead." #-}

instance Lude.FromJSON PartitionError where
  parseJSON =
    Lude.withObject
      "PartitionError"
      ( \x ->
          PartitionError'
            Lude.<$> (x Lude..:? "PartitionValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ErrorDetail")
      )
