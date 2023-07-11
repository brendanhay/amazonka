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
-- Module      : Amazonka.Personalize.Types.CategoricalHyperParameterRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.CategoricalHyperParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the name and range of a categorical hyperparameter.
--
-- /See:/ 'newCategoricalHyperParameterRange' smart constructor.
data CategoricalHyperParameterRange = CategoricalHyperParameterRange'
  { -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of the categories for the hyperparameter.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoricalHyperParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'categoricalHyperParameterRange_name' - The name of the hyperparameter.
--
-- 'values', 'categoricalHyperParameterRange_values' - A list of the categories for the hyperparameter.
newCategoricalHyperParameterRange ::
  CategoricalHyperParameterRange
newCategoricalHyperParameterRange =
  CategoricalHyperParameterRange'
    { name =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the hyperparameter.
categoricalHyperParameterRange_name :: Lens.Lens' CategoricalHyperParameterRange (Prelude.Maybe Prelude.Text)
categoricalHyperParameterRange_name = Lens.lens (\CategoricalHyperParameterRange' {name} -> name) (\s@CategoricalHyperParameterRange' {} a -> s {name = a} :: CategoricalHyperParameterRange)

-- | A list of the categories for the hyperparameter.
categoricalHyperParameterRange_values :: Lens.Lens' CategoricalHyperParameterRange (Prelude.Maybe [Prelude.Text])
categoricalHyperParameterRange_values = Lens.lens (\CategoricalHyperParameterRange' {values} -> values) (\s@CategoricalHyperParameterRange' {} a -> s {values = a} :: CategoricalHyperParameterRange) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CategoricalHyperParameterRange where
  parseJSON =
    Data.withObject
      "CategoricalHyperParameterRange"
      ( \x ->
          CategoricalHyperParameterRange'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    CategoricalHyperParameterRange
  where
  hashWithSalt
    _salt
    CategoricalHyperParameterRange' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    CategoricalHyperParameterRange
  where
  rnf CategoricalHyperParameterRange' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON CategoricalHyperParameterRange where
  toJSON CategoricalHyperParameterRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
