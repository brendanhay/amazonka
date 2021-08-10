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
-- Module      : Network.AWS.CostExplorer.Types.CostCategory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategory where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
import Network.AWS.CostExplorer.Types.CostCategoryRule
import Network.AWS.CostExplorer.Types.CostCategoryRuleVersion
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The structure of Cost Categories. This includes detailed metadata and
-- the set of rules for the @CostCategory@ object.
--
-- /See:/ 'newCostCategory' smart constructor.
data CostCategory = CostCategory'
  { -- | The list of processing statuses for Cost Management products for a
    -- specific cost category.
    processingStatus :: Prelude.Maybe [CostCategoryProcessingStatus],
    -- | The Cost Category\'s effective end date.
    effectiveEnd :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for your Cost Category.
    costCategoryArn :: Prelude.Text,
    -- | The Cost Category\'s effective start date.
    effectiveStart :: Prelude.Text,
    name :: Prelude.Text,
    ruleVersion :: CostCategoryRuleVersion,
    -- | Rules are processed in order. If there are multiple rules that match the
    -- line item, then the first rule to match is used to determine that Cost
    -- Category value.
    rules :: Prelude.NonEmpty CostCategoryRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostCategory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingStatus', 'costCategory_processingStatus' - The list of processing statuses for Cost Management products for a
-- specific cost category.
--
-- 'effectiveEnd', 'costCategory_effectiveEnd' - The Cost Category\'s effective end date.
--
-- 'costCategoryArn', 'costCategory_costCategoryArn' - The unique identifier for your Cost Category.
--
-- 'effectiveStart', 'costCategory_effectiveStart' - The Cost Category\'s effective start date.
--
-- 'name', 'costCategory_name' - Undocumented member.
--
-- 'ruleVersion', 'costCategory_ruleVersion' - Undocumented member.
--
-- 'rules', 'costCategory_rules' - Rules are processed in order. If there are multiple rules that match the
-- line item, then the first rule to match is used to determine that Cost
-- Category value.
newCostCategory ::
  -- | 'costCategoryArn'
  Prelude.Text ->
  -- | 'effectiveStart'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'ruleVersion'
  CostCategoryRuleVersion ->
  -- | 'rules'
  Prelude.NonEmpty CostCategoryRule ->
  CostCategory
newCostCategory
  pCostCategoryArn_
  pEffectiveStart_
  pName_
  pRuleVersion_
  pRules_ =
    CostCategory'
      { processingStatus = Prelude.Nothing,
        effectiveEnd = Prelude.Nothing,
        costCategoryArn = pCostCategoryArn_,
        effectiveStart = pEffectiveStart_,
        name = pName_,
        ruleVersion = pRuleVersion_,
        rules = Lens._Coerce Lens.# pRules_
      }

-- | The list of processing statuses for Cost Management products for a
-- specific cost category.
costCategory_processingStatus :: Lens.Lens' CostCategory (Prelude.Maybe [CostCategoryProcessingStatus])
costCategory_processingStatus = Lens.lens (\CostCategory' {processingStatus} -> processingStatus) (\s@CostCategory' {} a -> s {processingStatus = a} :: CostCategory) Prelude.. Lens.mapping Lens._Coerce

-- | The Cost Category\'s effective end date.
costCategory_effectiveEnd :: Lens.Lens' CostCategory (Prelude.Maybe Prelude.Text)
costCategory_effectiveEnd = Lens.lens (\CostCategory' {effectiveEnd} -> effectiveEnd) (\s@CostCategory' {} a -> s {effectiveEnd = a} :: CostCategory)

-- | The unique identifier for your Cost Category.
costCategory_costCategoryArn :: Lens.Lens' CostCategory Prelude.Text
costCategory_costCategoryArn = Lens.lens (\CostCategory' {costCategoryArn} -> costCategoryArn) (\s@CostCategory' {} a -> s {costCategoryArn = a} :: CostCategory)

-- | The Cost Category\'s effective start date.
costCategory_effectiveStart :: Lens.Lens' CostCategory Prelude.Text
costCategory_effectiveStart = Lens.lens (\CostCategory' {effectiveStart} -> effectiveStart) (\s@CostCategory' {} a -> s {effectiveStart = a} :: CostCategory)

-- | Undocumented member.
costCategory_name :: Lens.Lens' CostCategory Prelude.Text
costCategory_name = Lens.lens (\CostCategory' {name} -> name) (\s@CostCategory' {} a -> s {name = a} :: CostCategory)

-- | Undocumented member.
costCategory_ruleVersion :: Lens.Lens' CostCategory CostCategoryRuleVersion
costCategory_ruleVersion = Lens.lens (\CostCategory' {ruleVersion} -> ruleVersion) (\s@CostCategory' {} a -> s {ruleVersion = a} :: CostCategory)

-- | Rules are processed in order. If there are multiple rules that match the
-- line item, then the first rule to match is used to determine that Cost
-- Category value.
costCategory_rules :: Lens.Lens' CostCategory (Prelude.NonEmpty CostCategoryRule)
costCategory_rules = Lens.lens (\CostCategory' {rules} -> rules) (\s@CostCategory' {} a -> s {rules = a} :: CostCategory) Prelude.. Lens._Coerce

instance Core.FromJSON CostCategory where
  parseJSON =
    Core.withObject
      "CostCategory"
      ( \x ->
          CostCategory'
            Prelude.<$> ( x Core..:? "ProcessingStatus"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EffectiveEnd")
            Prelude.<*> (x Core..: "CostCategoryArn")
            Prelude.<*> (x Core..: "EffectiveStart")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "RuleVersion")
            Prelude.<*> (x Core..: "Rules")
      )

instance Prelude.Hashable CostCategory

instance Prelude.NFData CostCategory
