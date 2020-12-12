{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryReference
  ( CostCategoryReference (..),

    -- * Smart constructor
    mkCostCategoryReference,

    -- * Lenses
    ccrEffectiveStart,
    ccrValues,
    ccrCostCategoryARN,
    ccrProcessingStatus,
    ccrNumberOfRules,
    ccrName,
    ccrEffectiveEnd,
  )
where

import Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A reference to a Cost Category containing only enough information to identify the Cost Category.
--
-- You can use this information to retrieve the full Cost Category information using @DescribeCostCategory@ .
--
-- /See:/ 'mkCostCategoryReference' smart constructor.
data CostCategoryReference = CostCategoryReference'
  { effectiveStart ::
      Lude.Maybe Lude.Text,
    values :: Lude.Maybe [Lude.Text],
    costCategoryARN :: Lude.Maybe Lude.Text,
    processingStatus ::
      Lude.Maybe [CostCategoryProcessingStatus],
    numberOfRules :: Lude.Maybe Lude.Natural,
    name :: Lude.Maybe Lude.Text,
    effectiveEnd :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CostCategoryReference' with the minimum fields required to make a request.
--
-- * 'costCategoryARN' - The unique identifier for your Cost Category.
-- * 'effectiveEnd' - The Cost Category's effective end date.
-- * 'effectiveStart' - The Cost Category's effective start date.
-- * 'name' - Undocumented field.
-- * 'numberOfRules' - The number of rules associated with a specific Cost Category.
-- * 'processingStatus' - The list of processing statuses for Cost Management products for a specific cost category.
-- * 'values' - A list of unique cost category values in a specific cost category.
mkCostCategoryReference ::
  CostCategoryReference
mkCostCategoryReference =
  CostCategoryReference'
    { effectiveStart = Lude.Nothing,
      values = Lude.Nothing,
      costCategoryARN = Lude.Nothing,
      processingStatus = Lude.Nothing,
      numberOfRules = Lude.Nothing,
      name = Lude.Nothing,
      effectiveEnd = Lude.Nothing
    }

-- | The Cost Category's effective start date.
--
-- /Note:/ Consider using 'effectiveStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrEffectiveStart :: Lens.Lens' CostCategoryReference (Lude.Maybe Lude.Text)
ccrEffectiveStart = Lens.lens (effectiveStart :: CostCategoryReference -> Lude.Maybe Lude.Text) (\s a -> s {effectiveStart = a} :: CostCategoryReference)
{-# DEPRECATED ccrEffectiveStart "Use generic-lens or generic-optics with 'effectiveStart' instead." #-}

-- | A list of unique cost category values in a specific cost category.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrValues :: Lens.Lens' CostCategoryReference (Lude.Maybe [Lude.Text])
ccrValues = Lens.lens (values :: CostCategoryReference -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: CostCategoryReference)
{-# DEPRECATED ccrValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrCostCategoryARN :: Lens.Lens' CostCategoryReference (Lude.Maybe Lude.Text)
ccrCostCategoryARN = Lens.lens (costCategoryARN :: CostCategoryReference -> Lude.Maybe Lude.Text) (\s a -> s {costCategoryARN = a} :: CostCategoryReference)
{-# DEPRECATED ccrCostCategoryARN "Use generic-lens or generic-optics with 'costCategoryARN' instead." #-}

-- | The list of processing statuses for Cost Management products for a specific cost category.
--
-- /Note:/ Consider using 'processingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrProcessingStatus :: Lens.Lens' CostCategoryReference (Lude.Maybe [CostCategoryProcessingStatus])
ccrProcessingStatus = Lens.lens (processingStatus :: CostCategoryReference -> Lude.Maybe [CostCategoryProcessingStatus]) (\s a -> s {processingStatus = a} :: CostCategoryReference)
{-# DEPRECATED ccrProcessingStatus "Use generic-lens or generic-optics with 'processingStatus' instead." #-}

-- | The number of rules associated with a specific Cost Category.
--
-- /Note:/ Consider using 'numberOfRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrNumberOfRules :: Lens.Lens' CostCategoryReference (Lude.Maybe Lude.Natural)
ccrNumberOfRules = Lens.lens (numberOfRules :: CostCategoryReference -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfRules = a} :: CostCategoryReference)
{-# DEPRECATED ccrNumberOfRules "Use generic-lens or generic-optics with 'numberOfRules' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrName :: Lens.Lens' CostCategoryReference (Lude.Maybe Lude.Text)
ccrName = Lens.lens (name :: CostCategoryReference -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CostCategoryReference)
{-# DEPRECATED ccrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Cost Category's effective end date.
--
-- /Note:/ Consider using 'effectiveEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrEffectiveEnd :: Lens.Lens' CostCategoryReference (Lude.Maybe Lude.Text)
ccrEffectiveEnd = Lens.lens (effectiveEnd :: CostCategoryReference -> Lude.Maybe Lude.Text) (\s a -> s {effectiveEnd = a} :: CostCategoryReference)
{-# DEPRECATED ccrEffectiveEnd "Use generic-lens or generic-optics with 'effectiveEnd' instead." #-}

instance Lude.FromJSON CostCategoryReference where
  parseJSON =
    Lude.withObject
      "CostCategoryReference"
      ( \x ->
          CostCategoryReference'
            Lude.<$> (x Lude..:? "EffectiveStart")
            Lude.<*> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CostCategoryArn")
            Lude.<*> (x Lude..:? "ProcessingStatus" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NumberOfRules")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "EffectiveEnd")
      )
