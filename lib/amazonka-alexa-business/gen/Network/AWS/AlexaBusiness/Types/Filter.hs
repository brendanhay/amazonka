-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fKey,
    fValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria.
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter' {key :: Lude.Text, values :: [Lude.Text]}
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
-- * 'key' - The key of a filter.
-- * 'values' - The values of a filter.
mkFilter ::
  -- | 'key'
  Lude.Text ->
  Filter
mkFilter pKey_ = Filter' {key = pKey_, values = Lude.mempty}

-- | The key of a filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKey :: Lens.Lens' Filter Lude.Text
fKey = Lens.lens (key :: Filter -> Lude.Text) (\s a -> s {key = a} :: Filter)
{-# DEPRECATED fKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The values of a filter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter [Lude.Text]
fValues = Lens.lens (values :: Filter -> [Lude.Text]) (\s a -> s {values = a} :: Filter)
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.ToJSON Filter where
  toJSON Filter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Key" Lude..= key),
            Lude.Just ("Values" Lude..= values)
          ]
      )
