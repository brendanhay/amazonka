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
-- Module      : Amazonka.CostExplorer.Types.CostCategoryReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategoryReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostCategoryProcessingStatus
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A reference to a Cost Category containing only enough information to
-- identify the Cost Category.
--
-- You can use this information to retrieve the full Cost Category
-- information using @DescribeCostCategory@.
--
-- /See:/ 'newCostCategoryReference' smart constructor.
data CostCategoryReference = CostCategoryReference'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Prelude.Maybe Prelude.Text,
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The Cost Category\'s effective end date.
    effectiveEnd :: Prelude.Maybe Prelude.Text,
    -- | The Cost Category\'s effective start date.
    effectiveStart :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of rules that are associated with a specific Cost Category.
    numberOfRules :: Prelude.Maybe Prelude.Natural,
    -- | The list of processing statuses for Cost Management products for a
    -- specific cost category.
    processingStatus :: Prelude.Maybe [CostCategoryProcessingStatus],
    -- | A list of unique cost category values in a specific cost category.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostCategoryReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costCategoryArn', 'costCategoryReference_costCategoryArn' - The unique identifier for your Cost Category.
--
-- 'defaultValue', 'costCategoryReference_defaultValue' - Undocumented member.
--
-- 'effectiveEnd', 'costCategoryReference_effectiveEnd' - The Cost Category\'s effective end date.
--
-- 'effectiveStart', 'costCategoryReference_effectiveStart' - The Cost Category\'s effective start date.
--
-- 'name', 'costCategoryReference_name' - Undocumented member.
--
-- 'numberOfRules', 'costCategoryReference_numberOfRules' - The number of rules that are associated with a specific Cost Category.
--
-- 'processingStatus', 'costCategoryReference_processingStatus' - The list of processing statuses for Cost Management products for a
-- specific cost category.
--
-- 'values', 'costCategoryReference_values' - A list of unique cost category values in a specific cost category.
newCostCategoryReference ::
  CostCategoryReference
newCostCategoryReference =
  CostCategoryReference'
    { costCategoryArn =
        Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      effectiveEnd = Prelude.Nothing,
      effectiveStart = Prelude.Nothing,
      name = Prelude.Nothing,
      numberOfRules = Prelude.Nothing,
      processingStatus = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The unique identifier for your Cost Category.
costCategoryReference_costCategoryArn :: Lens.Lens' CostCategoryReference (Prelude.Maybe Prelude.Text)
costCategoryReference_costCategoryArn = Lens.lens (\CostCategoryReference' {costCategoryArn} -> costCategoryArn) (\s@CostCategoryReference' {} a -> s {costCategoryArn = a} :: CostCategoryReference)

-- | Undocumented member.
costCategoryReference_defaultValue :: Lens.Lens' CostCategoryReference (Prelude.Maybe Prelude.Text)
costCategoryReference_defaultValue = Lens.lens (\CostCategoryReference' {defaultValue} -> defaultValue) (\s@CostCategoryReference' {} a -> s {defaultValue = a} :: CostCategoryReference)

-- | The Cost Category\'s effective end date.
costCategoryReference_effectiveEnd :: Lens.Lens' CostCategoryReference (Prelude.Maybe Prelude.Text)
costCategoryReference_effectiveEnd = Lens.lens (\CostCategoryReference' {effectiveEnd} -> effectiveEnd) (\s@CostCategoryReference' {} a -> s {effectiveEnd = a} :: CostCategoryReference)

-- | The Cost Category\'s effective start date.
costCategoryReference_effectiveStart :: Lens.Lens' CostCategoryReference (Prelude.Maybe Prelude.Text)
costCategoryReference_effectiveStart = Lens.lens (\CostCategoryReference' {effectiveStart} -> effectiveStart) (\s@CostCategoryReference' {} a -> s {effectiveStart = a} :: CostCategoryReference)

-- | Undocumented member.
costCategoryReference_name :: Lens.Lens' CostCategoryReference (Prelude.Maybe Prelude.Text)
costCategoryReference_name = Lens.lens (\CostCategoryReference' {name} -> name) (\s@CostCategoryReference' {} a -> s {name = a} :: CostCategoryReference)

-- | The number of rules that are associated with a specific Cost Category.
costCategoryReference_numberOfRules :: Lens.Lens' CostCategoryReference (Prelude.Maybe Prelude.Natural)
costCategoryReference_numberOfRules = Lens.lens (\CostCategoryReference' {numberOfRules} -> numberOfRules) (\s@CostCategoryReference' {} a -> s {numberOfRules = a} :: CostCategoryReference)

-- | The list of processing statuses for Cost Management products for a
-- specific cost category.
costCategoryReference_processingStatus :: Lens.Lens' CostCategoryReference (Prelude.Maybe [CostCategoryProcessingStatus])
costCategoryReference_processingStatus = Lens.lens (\CostCategoryReference' {processingStatus} -> processingStatus) (\s@CostCategoryReference' {} a -> s {processingStatus = a} :: CostCategoryReference) Prelude.. Lens.mapping Lens.coerced

-- | A list of unique cost category values in a specific cost category.
costCategoryReference_values :: Lens.Lens' CostCategoryReference (Prelude.Maybe [Prelude.Text])
costCategoryReference_values = Lens.lens (\CostCategoryReference' {values} -> values) (\s@CostCategoryReference' {} a -> s {values = a} :: CostCategoryReference) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CostCategoryReference where
  parseJSON =
    Data.withObject
      "CostCategoryReference"
      ( \x ->
          CostCategoryReference'
            Prelude.<$> (x Data..:? "CostCategoryArn")
            Prelude.<*> (x Data..:? "DefaultValue")
            Prelude.<*> (x Data..:? "EffectiveEnd")
            Prelude.<*> (x Data..:? "EffectiveStart")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NumberOfRules")
            Prelude.<*> ( x Data..:? "ProcessingStatus"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CostCategoryReference where
  hashWithSalt _salt CostCategoryReference' {..} =
    _salt `Prelude.hashWithSalt` costCategoryArn
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` effectiveEnd
      `Prelude.hashWithSalt` effectiveStart
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` numberOfRules
      `Prelude.hashWithSalt` processingStatus
      `Prelude.hashWithSalt` values

instance Prelude.NFData CostCategoryReference where
  rnf CostCategoryReference' {..} =
    Prelude.rnf costCategoryArn
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf effectiveEnd
      `Prelude.seq` Prelude.rnf effectiveStart
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberOfRules
      `Prelude.seq` Prelude.rnf processingStatus
      `Prelude.seq` Prelude.rnf values
