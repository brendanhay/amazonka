-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
  ( DimensionValuesWithAttributes (..),

    -- * Smart constructor
    mkDimensionValuesWithAttributes,

    -- * Lenses
    dvwaValue,
    dvwaAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata of a specific type that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'mkDimensionValuesWithAttributes' smart constructor.
data DimensionValuesWithAttributes = DimensionValuesWithAttributes'
  { value ::
      Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DimensionValuesWithAttributes' with the minimum fields required to make a request.
--
-- * 'attributes' - The attribute that applies to a specific @Dimension@ .
-- * 'value' - The value of a dimension with a specific attribute.
mkDimensionValuesWithAttributes ::
  DimensionValuesWithAttributes
mkDimensionValuesWithAttributes =
  DimensionValuesWithAttributes'
    { value = Lude.Nothing,
      attributes = Lude.Nothing
    }

-- | The value of a dimension with a specific attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvwaValue :: Lens.Lens' DimensionValuesWithAttributes (Lude.Maybe Lude.Text)
dvwaValue = Lens.lens (value :: DimensionValuesWithAttributes -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: DimensionValuesWithAttributes)
{-# DEPRECATED dvwaValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The attribute that applies to a specific @Dimension@ .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvwaAttributes :: Lens.Lens' DimensionValuesWithAttributes (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dvwaAttributes = Lens.lens (attributes :: DimensionValuesWithAttributes -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: DimensionValuesWithAttributes)
{-# DEPRECATED dvwaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON DimensionValuesWithAttributes where
  parseJSON =
    Lude.withObject
      "DimensionValuesWithAttributes"
      ( \x ->
          DimensionValuesWithAttributes'
            Lude.<$> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
      )
