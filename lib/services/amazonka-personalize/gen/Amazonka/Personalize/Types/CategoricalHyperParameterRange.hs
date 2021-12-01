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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.CategoricalHyperParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the name and range of a categorical hyperparameter.
--
-- /See:/ 'newCategoricalHyperParameterRange' smart constructor.
data CategoricalHyperParameterRange = CategoricalHyperParameterRange'
  { -- | A list of the categories for the hyperparameter.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text
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
-- 'values', 'categoricalHyperParameterRange_values' - A list of the categories for the hyperparameter.
--
-- 'name', 'categoricalHyperParameterRange_name' - The name of the hyperparameter.
newCategoricalHyperParameterRange ::
  CategoricalHyperParameterRange
newCategoricalHyperParameterRange =
  CategoricalHyperParameterRange'
    { values =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A list of the categories for the hyperparameter.
categoricalHyperParameterRange_values :: Lens.Lens' CategoricalHyperParameterRange (Prelude.Maybe [Prelude.Text])
categoricalHyperParameterRange_values = Lens.lens (\CategoricalHyperParameterRange' {values} -> values) (\s@CategoricalHyperParameterRange' {} a -> s {values = a} :: CategoricalHyperParameterRange) Prelude.. Lens.mapping Lens.coerced

-- | The name of the hyperparameter.
categoricalHyperParameterRange_name :: Lens.Lens' CategoricalHyperParameterRange (Prelude.Maybe Prelude.Text)
categoricalHyperParameterRange_name = Lens.lens (\CategoricalHyperParameterRange' {name} -> name) (\s@CategoricalHyperParameterRange' {} a -> s {name = a} :: CategoricalHyperParameterRange)

instance Core.FromJSON CategoricalHyperParameterRange where
  parseJSON =
    Core.withObject
      "CategoricalHyperParameterRange"
      ( \x ->
          CategoricalHyperParameterRange'
            Prelude.<$> (x Core..:? "values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
      )

instance
  Prelude.Hashable
    CategoricalHyperParameterRange
  where
  hashWithSalt
    salt'
    CategoricalHyperParameterRange' {..} =
      salt' `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    CategoricalHyperParameterRange
  where
  rnf CategoricalHyperParameterRange' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf name

instance Core.ToJSON CategoricalHyperParameterRange where
  toJSON CategoricalHyperParameterRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("name" Core..=) Prelude.<$> name
          ]
      )
