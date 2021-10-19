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
-- Module      : Network.AWS.CostExplorer.Types.CostCategorySplitChargeRuleParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategorySplitChargeRuleParameter where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.CostCategorySplitChargeRuleParameterType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The parameters for a split charge method.
--
-- /See:/ 'newCostCategorySplitChargeRuleParameter' smart constructor.
data CostCategorySplitChargeRuleParameter = CostCategorySplitChargeRuleParameter'
  { -- | The parameter type.
    type' :: CostCategorySplitChargeRuleParameterType,
    -- | The parameter values.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostCategorySplitChargeRuleParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'costCategorySplitChargeRuleParameter_type' - The parameter type.
--
-- 'values', 'costCategorySplitChargeRuleParameter_values' - The parameter values.
newCostCategorySplitChargeRuleParameter ::
  -- | 'type''
  CostCategorySplitChargeRuleParameterType ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  CostCategorySplitChargeRuleParameter
newCostCategorySplitChargeRuleParameter
  pType_
  pValues_ =
    CostCategorySplitChargeRuleParameter'
      { type' =
          pType_,
        values = Lens.coerced Lens.# pValues_
      }

-- | The parameter type.
costCategorySplitChargeRuleParameter_type :: Lens.Lens' CostCategorySplitChargeRuleParameter CostCategorySplitChargeRuleParameterType
costCategorySplitChargeRuleParameter_type = Lens.lens (\CostCategorySplitChargeRuleParameter' {type'} -> type') (\s@CostCategorySplitChargeRuleParameter' {} a -> s {type' = a} :: CostCategorySplitChargeRuleParameter)

-- | The parameter values.
costCategorySplitChargeRuleParameter_values :: Lens.Lens' CostCategorySplitChargeRuleParameter (Prelude.NonEmpty Prelude.Text)
costCategorySplitChargeRuleParameter_values = Lens.lens (\CostCategorySplitChargeRuleParameter' {values} -> values) (\s@CostCategorySplitChargeRuleParameter' {} a -> s {values = a} :: CostCategorySplitChargeRuleParameter) Prelude.. Lens.coerced

instance
  Core.FromJSON
    CostCategorySplitChargeRuleParameter
  where
  parseJSON =
    Core.withObject
      "CostCategorySplitChargeRuleParameter"
      ( \x ->
          CostCategorySplitChargeRuleParameter'
            Prelude.<$> (x Core..: "Type") Prelude.<*> (x Core..: "Values")
      )

instance
  Prelude.Hashable
    CostCategorySplitChargeRuleParameter

instance
  Prelude.NFData
    CostCategorySplitChargeRuleParameter

instance
  Core.ToJSON
    CostCategorySplitChargeRuleParameter
  where
  toJSON CostCategorySplitChargeRuleParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("Values" Core..= values)
          ]
      )
