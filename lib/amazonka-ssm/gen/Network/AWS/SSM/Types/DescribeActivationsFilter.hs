{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DescribeActivationsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DescribeActivationsFilter
  ( DescribeActivationsFilter (..),

    -- * Smart constructor
    mkDescribeActivationsFilter,

    -- * Lenses
    dafFilterKey,
    dafFilterValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.DescribeActivationsFilterKeys

-- | Filter for the DescribeActivation API.
--
-- /See:/ 'mkDescribeActivationsFilter' smart constructor.
data DescribeActivationsFilter = DescribeActivationsFilter'
  { -- | The name of the filter.
    filterKey :: Lude.Maybe DescribeActivationsFilterKeys,
    -- | The filter values.
    filterValues :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeActivationsFilter' with the minimum fields required to make a request.
--
-- * 'filterKey' - The name of the filter.
-- * 'filterValues' - The filter values.
mkDescribeActivationsFilter ::
  DescribeActivationsFilter
mkDescribeActivationsFilter =
  DescribeActivationsFilter'
    { filterKey = Lude.Nothing,
      filterValues = Lude.Nothing
    }

-- | The name of the filter.
--
-- /Note:/ Consider using 'filterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafFilterKey :: Lens.Lens' DescribeActivationsFilter (Lude.Maybe DescribeActivationsFilterKeys)
dafFilterKey = Lens.lens (filterKey :: DescribeActivationsFilter -> Lude.Maybe DescribeActivationsFilterKeys) (\s a -> s {filterKey = a} :: DescribeActivationsFilter)
{-# DEPRECATED dafFilterKey "Use generic-lens or generic-optics with 'filterKey' instead." #-}

-- | The filter values.
--
-- /Note:/ Consider using 'filterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafFilterValues :: Lens.Lens' DescribeActivationsFilter (Lude.Maybe [Lude.Text])
dafFilterValues = Lens.lens (filterValues :: DescribeActivationsFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {filterValues = a} :: DescribeActivationsFilter)
{-# DEPRECATED dafFilterValues "Use generic-lens or generic-optics with 'filterValues' instead." #-}

instance Lude.ToJSON DescribeActivationsFilter where
  toJSON DescribeActivationsFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FilterKey" Lude..=) Lude.<$> filterKey,
            ("FilterValues" Lude..=) Lude.<$> filterValues
          ]
      )
