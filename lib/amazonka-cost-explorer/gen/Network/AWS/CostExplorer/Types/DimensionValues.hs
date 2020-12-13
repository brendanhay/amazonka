{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.DimensionValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DimensionValues
  ( DimensionValues (..),

    -- * Smart constructor
    mkDimensionValues,

    -- * Lenses
    dvValues,
    dvKey,
    dvMatchOptions,
  )
where

import Network.AWS.CostExplorer.Types.Dimension
import Network.AWS.CostExplorer.Types.MatchOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'mkDimensionValues' smart constructor.
data DimensionValues = DimensionValues'
  { -- | The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
    values :: Lude.Maybe [Lude.Text],
    -- | The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
    key :: Lude.Maybe Dimension,
    -- | The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
    matchOptions :: Lude.Maybe [MatchOption]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DimensionValues' with the minimum fields required to make a request.
--
-- * 'values' - The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
-- * 'key' - The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
-- * 'matchOptions' - The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
mkDimensionValues ::
  DimensionValues
mkDimensionValues =
  DimensionValues'
    { values = Lude.Nothing,
      key = Lude.Nothing,
      matchOptions = Lude.Nothing
    }

-- | The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvValues :: Lens.Lens' DimensionValues (Lude.Maybe [Lude.Text])
dvValues = Lens.lens (values :: DimensionValues -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: DimensionValues)
{-# DEPRECATED dvValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvKey :: Lens.Lens' DimensionValues (Lude.Maybe Dimension)
dvKey = Lens.lens (key :: DimensionValues -> Lude.Maybe Dimension) (\s a -> s {key = a} :: DimensionValues)
{-# DEPRECATED dvKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
--
-- /Note:/ Consider using 'matchOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvMatchOptions :: Lens.Lens' DimensionValues (Lude.Maybe [MatchOption])
dvMatchOptions = Lens.lens (matchOptions :: DimensionValues -> Lude.Maybe [MatchOption]) (\s a -> s {matchOptions = a} :: DimensionValues)
{-# DEPRECATED dvMatchOptions "Use generic-lens or generic-optics with 'matchOptions' instead." #-}

instance Lude.FromJSON DimensionValues where
  parseJSON =
    Lude.withObject
      "DimensionValues"
      ( \x ->
          DimensionValues'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "MatchOptions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON DimensionValues where
  toJSON DimensionValues' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            ("Key" Lude..=) Lude.<$> key,
            ("MatchOptions" Lude..=) Lude.<$> matchOptions
          ]
      )
