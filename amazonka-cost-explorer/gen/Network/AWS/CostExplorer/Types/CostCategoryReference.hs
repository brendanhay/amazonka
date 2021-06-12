{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryReference where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
import qualified Network.AWS.Lens as Lens

-- | A reference to a Cost Category containing only enough information to
-- identify the Cost Category.
--
-- You can use this information to retrieve the full Cost Category
-- information using @DescribeCostCategory@.
--
-- /See:/ 'newCostCategoryReference' smart constructor.
data CostCategoryReference = CostCategoryReference'
  { -- | The number of rules associated with a specific Cost Category.
    numberOfRules :: Core.Maybe Core.Natural,
    -- | The unique identifier for your Cost Category.
    costCategoryArn :: Core.Maybe Core.Text,
    -- | A list of unique cost category values in a specific cost category.
    values :: Core.Maybe [Core.Text],
    -- | The list of processing statuses for Cost Management products for a
    -- specific cost category.
    processingStatus :: Core.Maybe [CostCategoryProcessingStatus],
    name :: Core.Maybe Core.Text,
    -- | The Cost Category\'s effective start date.
    effectiveStart :: Core.Maybe Core.Text,
    -- | The Cost Category\'s effective end date.
    effectiveEnd :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CostCategoryReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfRules', 'costCategoryReference_numberOfRules' - The number of rules associated with a specific Cost Category.
--
-- 'costCategoryArn', 'costCategoryReference_costCategoryArn' - The unique identifier for your Cost Category.
--
-- 'values', 'costCategoryReference_values' - A list of unique cost category values in a specific cost category.
--
-- 'processingStatus', 'costCategoryReference_processingStatus' - The list of processing statuses for Cost Management products for a
-- specific cost category.
--
-- 'name', 'costCategoryReference_name' - Undocumented member.
--
-- 'effectiveStart', 'costCategoryReference_effectiveStart' - The Cost Category\'s effective start date.
--
-- 'effectiveEnd', 'costCategoryReference_effectiveEnd' - The Cost Category\'s effective end date.
newCostCategoryReference ::
  CostCategoryReference
newCostCategoryReference =
  CostCategoryReference'
    { numberOfRules =
        Core.Nothing,
      costCategoryArn = Core.Nothing,
      values = Core.Nothing,
      processingStatus = Core.Nothing,
      name = Core.Nothing,
      effectiveStart = Core.Nothing,
      effectiveEnd = Core.Nothing
    }

-- | The number of rules associated with a specific Cost Category.
costCategoryReference_numberOfRules :: Lens.Lens' CostCategoryReference (Core.Maybe Core.Natural)
costCategoryReference_numberOfRules = Lens.lens (\CostCategoryReference' {numberOfRules} -> numberOfRules) (\s@CostCategoryReference' {} a -> s {numberOfRules = a} :: CostCategoryReference)

-- | The unique identifier for your Cost Category.
costCategoryReference_costCategoryArn :: Lens.Lens' CostCategoryReference (Core.Maybe Core.Text)
costCategoryReference_costCategoryArn = Lens.lens (\CostCategoryReference' {costCategoryArn} -> costCategoryArn) (\s@CostCategoryReference' {} a -> s {costCategoryArn = a} :: CostCategoryReference)

-- | A list of unique cost category values in a specific cost category.
costCategoryReference_values :: Lens.Lens' CostCategoryReference (Core.Maybe [Core.Text])
costCategoryReference_values = Lens.lens (\CostCategoryReference' {values} -> values) (\s@CostCategoryReference' {} a -> s {values = a} :: CostCategoryReference) Core.. Lens.mapping Lens._Coerce

-- | The list of processing statuses for Cost Management products for a
-- specific cost category.
costCategoryReference_processingStatus :: Lens.Lens' CostCategoryReference (Core.Maybe [CostCategoryProcessingStatus])
costCategoryReference_processingStatus = Lens.lens (\CostCategoryReference' {processingStatus} -> processingStatus) (\s@CostCategoryReference' {} a -> s {processingStatus = a} :: CostCategoryReference) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
costCategoryReference_name :: Lens.Lens' CostCategoryReference (Core.Maybe Core.Text)
costCategoryReference_name = Lens.lens (\CostCategoryReference' {name} -> name) (\s@CostCategoryReference' {} a -> s {name = a} :: CostCategoryReference)

-- | The Cost Category\'s effective start date.
costCategoryReference_effectiveStart :: Lens.Lens' CostCategoryReference (Core.Maybe Core.Text)
costCategoryReference_effectiveStart = Lens.lens (\CostCategoryReference' {effectiveStart} -> effectiveStart) (\s@CostCategoryReference' {} a -> s {effectiveStart = a} :: CostCategoryReference)

-- | The Cost Category\'s effective end date.
costCategoryReference_effectiveEnd :: Lens.Lens' CostCategoryReference (Core.Maybe Core.Text)
costCategoryReference_effectiveEnd = Lens.lens (\CostCategoryReference' {effectiveEnd} -> effectiveEnd) (\s@CostCategoryReference' {} a -> s {effectiveEnd = a} :: CostCategoryReference)

instance Core.FromJSON CostCategoryReference where
  parseJSON =
    Core.withObject
      "CostCategoryReference"
      ( \x ->
          CostCategoryReference'
            Core.<$> (x Core..:? "NumberOfRules")
            Core.<*> (x Core..:? "CostCategoryArn")
            Core.<*> (x Core..:? "Values" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ProcessingStatus" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "EffectiveStart")
            Core.<*> (x Core..:? "EffectiveEnd")
      )

instance Core.Hashable CostCategoryReference

instance Core.NFData CostCategoryReference
