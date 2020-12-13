{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fValues,
    fName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A filter name and value pair that is used to return a more specific list of results from a describe operation. Filters can be used to match a set of resources by specific criteria, such as IDs. The filters supported by a describe operation are documented with the describe operation.
--
-- The following actions can be filtered:
--
--     * @DescribeDBClusterBacktracks@
--
--
--     * @DescribeDBClusterEndpoints@
--
--
--     * @DescribeDBClusters@
--
--
--     * @DescribeDBInstances@
--
--
--     * @DescribePendingMaintenanceActions@
--
--
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { -- | One or more filter values. Filter values are case-sensitive.
    values :: [Lude.Text],
    -- | The name of the filter. Filter names are case-sensitive.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- * 'values' - One or more filter values. Filter values are case-sensitive.
-- * 'name' - The name of the filter. Filter names are case-sensitive.
mkFilter ::
  -- | 'name'
  Lude.Text ->
  Filter
mkFilter pName_ = Filter' {values = Lude.mempty, name = pName_}

-- | One or more filter values. Filter values are case-sensitive.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter [Lude.Text]
fValues = Lens.lens (values :: Filter -> [Lude.Text]) (\s a -> s {values = a} :: Filter)
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the filter. Filter names are case-sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Lude.Text
fName = Lens.lens (name :: Filter -> Lude.Text) (\s a -> s {name = a} :: Filter)
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToQuery Filter where
  toQuery Filter' {..} =
    Lude.mconcat
      [ "Values" Lude.=: Lude.toQueryList "Value" values,
        "Name" Lude.=: name
      ]
