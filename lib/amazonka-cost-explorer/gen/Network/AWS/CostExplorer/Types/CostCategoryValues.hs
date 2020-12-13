{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryValues
  ( CostCategoryValues (..),

    -- * Smart constructor
    mkCostCategoryValues,

    -- * Lenses
    ccvValues,
    ccvKey,
    ccvMatchOptions,
  )
where

import Network.AWS.CostExplorer.Types.MatchOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Cost Categories values used for filtering the costs.
--
-- /See:/ 'mkCostCategoryValues' smart constructor.
data CostCategoryValues = CostCategoryValues'
  { -- | The specific value of the Cost Category.
    values :: Lude.Maybe [Lude.Text],
    key :: Lude.Maybe Lude.Text,
    -- | The match options that you can use to filter your results. MatchOptions is only applicable for only applicable for actions related to cost category. The default values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@ .
    matchOptions :: Lude.Maybe [MatchOption]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CostCategoryValues' with the minimum fields required to make a request.
--
-- * 'values' - The specific value of the Cost Category.
-- * 'key' -
-- * 'matchOptions' - The match options that you can use to filter your results. MatchOptions is only applicable for only applicable for actions related to cost category. The default values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@ .
mkCostCategoryValues ::
  CostCategoryValues
mkCostCategoryValues =
  CostCategoryValues'
    { values = Lude.Nothing,
      key = Lude.Nothing,
      matchOptions = Lude.Nothing
    }

-- | The specific value of the Cost Category.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvValues :: Lens.Lens' CostCategoryValues (Lude.Maybe [Lude.Text])
ccvValues = Lens.lens (values :: CostCategoryValues -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: CostCategoryValues)
{-# DEPRECATED ccvValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvKey :: Lens.Lens' CostCategoryValues (Lude.Maybe Lude.Text)
ccvKey = Lens.lens (key :: CostCategoryValues -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: CostCategoryValues)
{-# DEPRECATED ccvKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The match options that you can use to filter your results. MatchOptions is only applicable for only applicable for actions related to cost category. The default values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@ .
--
-- /Note:/ Consider using 'matchOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvMatchOptions :: Lens.Lens' CostCategoryValues (Lude.Maybe [MatchOption])
ccvMatchOptions = Lens.lens (matchOptions :: CostCategoryValues -> Lude.Maybe [MatchOption]) (\s a -> s {matchOptions = a} :: CostCategoryValues)
{-# DEPRECATED ccvMatchOptions "Use generic-lens or generic-optics with 'matchOptions' instead." #-}

instance Lude.FromJSON CostCategoryValues where
  parseJSON =
    Lude.withObject
      "CostCategoryValues"
      ( \x ->
          CostCategoryValues'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "MatchOptions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON CostCategoryValues where
  toJSON CostCategoryValues' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            ("Key" Lude..=) Lude.<$> key,
            ("MatchOptions" Lude..=) Lude.<$> matchOptions
          ]
      )
