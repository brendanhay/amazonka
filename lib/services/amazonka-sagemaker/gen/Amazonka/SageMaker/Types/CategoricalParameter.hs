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
-- Module      : Amazonka.SageMaker.Types.CategoricalParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CategoricalParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Environment parameters you want to benchmark your load test against.
--
-- /See:/ 'newCategoricalParameter' smart constructor.
data CategoricalParameter = CategoricalParameter'
  { -- | The Name of the environment variable.
    name :: Prelude.Text,
    -- | The list of values you can pass.
    value :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoricalParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'categoricalParameter_name' - The Name of the environment variable.
--
-- 'value', 'categoricalParameter_value' - The list of values you can pass.
newCategoricalParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.NonEmpty Prelude.Text ->
  CategoricalParameter
newCategoricalParameter pName_ pValue_ =
  CategoricalParameter'
    { name = pName_,
      value = Lens.coerced Lens.# pValue_
    }

-- | The Name of the environment variable.
categoricalParameter_name :: Lens.Lens' CategoricalParameter Prelude.Text
categoricalParameter_name = Lens.lens (\CategoricalParameter' {name} -> name) (\s@CategoricalParameter' {} a -> s {name = a} :: CategoricalParameter)

-- | The list of values you can pass.
categoricalParameter_value :: Lens.Lens' CategoricalParameter (Prelude.NonEmpty Prelude.Text)
categoricalParameter_value = Lens.lens (\CategoricalParameter' {value} -> value) (\s@CategoricalParameter' {} a -> s {value = a} :: CategoricalParameter) Prelude.. Lens.coerced

instance Data.FromJSON CategoricalParameter where
  parseJSON =
    Data.withObject
      "CategoricalParameter"
      ( \x ->
          CategoricalParameter'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable CategoricalParameter where
  hashWithSalt _salt CategoricalParameter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData CategoricalParameter where
  rnf CategoricalParameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON CategoricalParameter where
  toJSON CategoricalParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
