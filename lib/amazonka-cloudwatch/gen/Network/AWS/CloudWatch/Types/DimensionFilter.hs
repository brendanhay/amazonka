-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.DimensionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.DimensionFilter
  ( DimensionFilter (..),

    -- * Smart constructor
    mkDimensionFilter,

    -- * Lenses
    dfValue,
    dfName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents filters for a dimension.
--
-- /See:/ 'mkDimensionFilter' smart constructor.
data DimensionFilter = DimensionFilter'
  { value ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DimensionFilter' with the minimum fields required to make a request.
--
-- * 'name' - The dimension name to be matched.
-- * 'value' - The value of the dimension to be matched.
mkDimensionFilter ::
  -- | 'name'
  Lude.Text ->
  DimensionFilter
mkDimensionFilter pName_ =
  DimensionFilter' {value = Lude.Nothing, name = pName_}

-- | The value of the dimension to be matched.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfValue :: Lens.Lens' DimensionFilter (Lude.Maybe Lude.Text)
dfValue = Lens.lens (value :: DimensionFilter -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: DimensionFilter)
{-# DEPRECATED dfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The dimension name to be matched.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' DimensionFilter Lude.Text
dfName = Lens.lens (name :: DimensionFilter -> Lude.Text) (\s a -> s {name = a} :: DimensionFilter)
{-# DEPRECATED dfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToQuery DimensionFilter where
  toQuery DimensionFilter' {..} =
    Lude.mconcat ["Value" Lude.=: value, "Name" Lude.=: name]
