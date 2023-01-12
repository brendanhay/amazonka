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
-- Module      : Amazonka.CostExplorer.Types.CostCategorySplitChargeRuleParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategorySplitChargeRuleParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostCategorySplitChargeRuleParameterType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  Data.FromJSON
    CostCategorySplitChargeRuleParameter
  where
  parseJSON =
    Data.withObject
      "CostCategorySplitChargeRuleParameter"
      ( \x ->
          CostCategorySplitChargeRuleParameter'
            Prelude.<$> (x Data..: "Type") Prelude.<*> (x Data..: "Values")
      )

instance
  Prelude.Hashable
    CostCategorySplitChargeRuleParameter
  where
  hashWithSalt
    _salt
    CostCategorySplitChargeRuleParameter' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    CostCategorySplitChargeRuleParameter
  where
  rnf CostCategorySplitChargeRuleParameter' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf values

instance
  Data.ToJSON
    CostCategorySplitChargeRuleParameter
  where
  toJSON CostCategorySplitChargeRuleParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Values" Data..= values)
          ]
      )
