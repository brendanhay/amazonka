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
-- Module      : Amazonka.CostExplorer.Types.CostCategorySplitChargeRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategorySplitChargeRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostCategorySplitChargeMethod
import Amazonka.CostExplorer.Types.CostCategorySplitChargeRuleParameter
import qualified Amazonka.Prelude as Prelude

-- | Use the split charge rule to split the cost of one Cost Category value
-- across several other target values.
--
-- /See:/ 'newCostCategorySplitChargeRule' smart constructor.
data CostCategorySplitChargeRule = CostCategorySplitChargeRule'
  { -- | The parameters for a split charge method. This is only required for the
    -- @FIXED@ method.
    parameters :: Prelude.Maybe (Prelude.NonEmpty CostCategorySplitChargeRuleParameter),
    -- | The Cost Category value that you want to split. That value can\'t be
    -- used as a source or a target in other split charge rules. To indicate
    -- uncategorized costs, you can use an empty string as the source.
    source :: Prelude.Text,
    -- | The Cost Category values that you want to split costs across. These
    -- values can\'t be used as a source in other split charge rules.
    targets :: Prelude.NonEmpty Prelude.Text,
    -- | The method that\'s used to define how to split your source costs across
    -- your targets.
    --
    -- @Proportional@ - Allocates charges across your targets based on the
    -- proportional weighted cost of each target.
    --
    -- @Fixed@ - Allocates charges across your targets based on your defined
    -- allocation percentage.
    --
    -- >@Even@ - Allocates costs evenly across all targets.
    method :: CostCategorySplitChargeMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostCategorySplitChargeRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'costCategorySplitChargeRule_parameters' - The parameters for a split charge method. This is only required for the
-- @FIXED@ method.
--
-- 'source', 'costCategorySplitChargeRule_source' - The Cost Category value that you want to split. That value can\'t be
-- used as a source or a target in other split charge rules. To indicate
-- uncategorized costs, you can use an empty string as the source.
--
-- 'targets', 'costCategorySplitChargeRule_targets' - The Cost Category values that you want to split costs across. These
-- values can\'t be used as a source in other split charge rules.
--
-- 'method', 'costCategorySplitChargeRule_method' - The method that\'s used to define how to split your source costs across
-- your targets.
--
-- @Proportional@ - Allocates charges across your targets based on the
-- proportional weighted cost of each target.
--
-- @Fixed@ - Allocates charges across your targets based on your defined
-- allocation percentage.
--
-- >@Even@ - Allocates costs evenly across all targets.
newCostCategorySplitChargeRule ::
  -- | 'source'
  Prelude.Text ->
  -- | 'targets'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'method'
  CostCategorySplitChargeMethod ->
  CostCategorySplitChargeRule
newCostCategorySplitChargeRule
  pSource_
  pTargets_
  pMethod_ =
    CostCategorySplitChargeRule'
      { parameters =
          Prelude.Nothing,
        source = pSource_,
        targets = Lens.coerced Lens.# pTargets_,
        method = pMethod_
      }

-- | The parameters for a split charge method. This is only required for the
-- @FIXED@ method.
costCategorySplitChargeRule_parameters :: Lens.Lens' CostCategorySplitChargeRule (Prelude.Maybe (Prelude.NonEmpty CostCategorySplitChargeRuleParameter))
costCategorySplitChargeRule_parameters = Lens.lens (\CostCategorySplitChargeRule' {parameters} -> parameters) (\s@CostCategorySplitChargeRule' {} a -> s {parameters = a} :: CostCategorySplitChargeRule) Prelude.. Lens.mapping Lens.coerced

-- | The Cost Category value that you want to split. That value can\'t be
-- used as a source or a target in other split charge rules. To indicate
-- uncategorized costs, you can use an empty string as the source.
costCategorySplitChargeRule_source :: Lens.Lens' CostCategorySplitChargeRule Prelude.Text
costCategorySplitChargeRule_source = Lens.lens (\CostCategorySplitChargeRule' {source} -> source) (\s@CostCategorySplitChargeRule' {} a -> s {source = a} :: CostCategorySplitChargeRule)

-- | The Cost Category values that you want to split costs across. These
-- values can\'t be used as a source in other split charge rules.
costCategorySplitChargeRule_targets :: Lens.Lens' CostCategorySplitChargeRule (Prelude.NonEmpty Prelude.Text)
costCategorySplitChargeRule_targets = Lens.lens (\CostCategorySplitChargeRule' {targets} -> targets) (\s@CostCategorySplitChargeRule' {} a -> s {targets = a} :: CostCategorySplitChargeRule) Prelude.. Lens.coerced

-- | The method that\'s used to define how to split your source costs across
-- your targets.
--
-- @Proportional@ - Allocates charges across your targets based on the
-- proportional weighted cost of each target.
--
-- @Fixed@ - Allocates charges across your targets based on your defined
-- allocation percentage.
--
-- >@Even@ - Allocates costs evenly across all targets.
costCategorySplitChargeRule_method :: Lens.Lens' CostCategorySplitChargeRule CostCategorySplitChargeMethod
costCategorySplitChargeRule_method = Lens.lens (\CostCategorySplitChargeRule' {method} -> method) (\s@CostCategorySplitChargeRule' {} a -> s {method = a} :: CostCategorySplitChargeRule)

instance Core.FromJSON CostCategorySplitChargeRule where
  parseJSON =
    Core.withObject
      "CostCategorySplitChargeRule"
      ( \x ->
          CostCategorySplitChargeRule'
            Prelude.<$> (x Core..:? "Parameters")
            Prelude.<*> (x Core..: "Source")
            Prelude.<*> (x Core..: "Targets")
            Prelude.<*> (x Core..: "Method")
      )

instance Prelude.Hashable CostCategorySplitChargeRule where
  hashWithSalt _salt CostCategorySplitChargeRule' {..} =
    _salt `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` method

instance Prelude.NFData CostCategorySplitChargeRule where
  rnf CostCategorySplitChargeRule' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf method

instance Core.ToJSON CostCategorySplitChargeRule where
  toJSON CostCategorySplitChargeRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("Source" Core..= source),
            Prelude.Just ("Targets" Core..= targets),
            Prelude.Just ("Method" Core..= method)
          ]
      )
