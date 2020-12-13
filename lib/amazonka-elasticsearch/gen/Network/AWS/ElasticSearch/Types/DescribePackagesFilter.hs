{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DescribePackagesFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DescribePackagesFilter
  ( DescribePackagesFilter (..),

    -- * Smart constructor
    mkDescribePackagesFilter,

    -- * Lenses
    dpfValue,
    dpfName,
  )
where

import Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filter to apply in @DescribePackage@ response.
--
-- /See:/ 'mkDescribePackagesFilter' smart constructor.
data DescribePackagesFilter = DescribePackagesFilter'
  { -- | A list of values for the specified field.
    value :: Lude.Maybe [Lude.Text],
    -- | Any field from @PackageDetails@ .
    name :: Lude.Maybe DescribePackagesFilterName
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePackagesFilter' with the minimum fields required to make a request.
--
-- * 'value' - A list of values for the specified field.
-- * 'name' - Any field from @PackageDetails@ .
mkDescribePackagesFilter ::
  DescribePackagesFilter
mkDescribePackagesFilter =
  DescribePackagesFilter'
    { value = Lude.Nothing,
      name = Lude.Nothing
    }

-- | A list of values for the specified field.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfValue :: Lens.Lens' DescribePackagesFilter (Lude.Maybe [Lude.Text])
dpfValue = Lens.lens (value :: DescribePackagesFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {value = a} :: DescribePackagesFilter)
{-# DEPRECATED dpfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Any field from @PackageDetails@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfName :: Lens.Lens' DescribePackagesFilter (Lude.Maybe DescribePackagesFilterName)
dpfName = Lens.lens (name :: DescribePackagesFilter -> Lude.Maybe DescribePackagesFilterName) (\s a -> s {name = a} :: DescribePackagesFilter)
{-# DEPRECATED dpfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON DescribePackagesFilter where
  toJSON DescribePackagesFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Name" Lude..=) Lude.<$> name]
      )
