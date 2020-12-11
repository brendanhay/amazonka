-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Filter
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

-- | A filter name and value pair that is used to return a more specific list of results from a describe operation. Filters can be used to match a set of resources by specific criteria, such as tags, attributes, or IDs. The filters supported by a describe operation are documented with the describe operation. For example:
--
--
--     * 'DescribeAvailabilityZones'
--
--
--     * 'DescribeImages'
--
--
--     * 'DescribeInstances'
--
--
--     * 'DescribeKeyPairs'
--
--
--     * 'DescribeSecurityGroups'
--
--
--     * 'DescribeSnapshots'
--
--
--     * 'DescribeSubnets'
--
--
--     * 'DescribeTags'
--
--
--     * 'DescribeVolumes'
--
--
--     * 'DescribeVpcs'
--
--
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { values :: Lude.Maybe [Lude.Text],
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- * 'name' - The name of the filter. Filter names are case-sensitive.
-- * 'values' - The filter values. Filter values are case-sensitive.
mkFilter ::
  -- | 'name'
  Lude.Text ->
  Filter
mkFilter pName_ = Filter' {values = Lude.Nothing, name = pName_}

-- | The filter values. Filter values are case-sensitive.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter (Lude.Maybe [Lude.Text])
fValues = Lens.lens (values :: Filter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: Filter)
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
      [ Lude.toQuery (Lude.toQueryList "Value" Lude.<$> values),
        "Name" Lude.=: name
      ]
