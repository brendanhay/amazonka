{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionValueList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionValueList
  ( PartitionValueList (..),

    -- * Smart constructor
    mkPartitionValueList,

    -- * Lenses
    pvlValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a list of values defining partitions.
--
-- /See:/ 'mkPartitionValueList' smart constructor.
newtype PartitionValueList = PartitionValueList'
  { values ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartitionValueList' with the minimum fields required to make a request.
--
-- * 'values' - The list of values.
mkPartitionValueList ::
  PartitionValueList
mkPartitionValueList = PartitionValueList' {values = Lude.mempty}

-- | The list of values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvlValues :: Lens.Lens' PartitionValueList [Lude.Text]
pvlValues = Lens.lens (values :: PartitionValueList -> [Lude.Text]) (\s a -> s {values = a} :: PartitionValueList)
{-# DEPRECATED pvlValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.FromJSON PartitionValueList where
  parseJSON =
    Lude.withObject
      "PartitionValueList"
      ( \x ->
          PartitionValueList'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PartitionValueList where
  toJSON PartitionValueList' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Values" Lude..= values)])
