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
-- Module      : Amazonka.Evidently.Types.VariationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.VariationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.VariableValue
import qualified Amazonka.Prelude as Prelude

-- | This structure contains the name and variation value of one variation of
-- a feature.
--
-- /See:/ 'newVariationConfig' smart constructor.
data VariationConfig = VariationConfig'
  { -- | The name of the variation.
    name :: Prelude.Text,
    -- | The value assigned to this variation.
    value :: VariableValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'variationConfig_name' - The name of the variation.
--
-- 'value', 'variationConfig_value' - The value assigned to this variation.
newVariationConfig ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  VariableValue ->
  VariationConfig
newVariationConfig pName_ pValue_ =
  VariationConfig' {name = pName_, value = pValue_}

-- | The name of the variation.
variationConfig_name :: Lens.Lens' VariationConfig Prelude.Text
variationConfig_name = Lens.lens (\VariationConfig' {name} -> name) (\s@VariationConfig' {} a -> s {name = a} :: VariationConfig)

-- | The value assigned to this variation.
variationConfig_value :: Lens.Lens' VariationConfig VariableValue
variationConfig_value = Lens.lens (\VariationConfig' {value} -> value) (\s@VariationConfig' {} a -> s {value = a} :: VariationConfig)

instance Prelude.Hashable VariationConfig where
  hashWithSalt _salt VariationConfig' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData VariationConfig where
  rnf VariationConfig' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON VariationConfig where
  toJSON VariationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("value" Data..= value)
          ]
      )
