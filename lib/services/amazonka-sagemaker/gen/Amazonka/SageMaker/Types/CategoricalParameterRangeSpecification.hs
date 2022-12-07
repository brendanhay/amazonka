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
-- Module      : Amazonka.SageMaker.Types.CategoricalParameterRangeSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CategoricalParameterRangeSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the possible values for a categorical hyperparameter.
--
-- /See:/ 'newCategoricalParameterRangeSpecification' smart constructor.
data CategoricalParameterRangeSpecification = CategoricalParameterRangeSpecification'
  { -- | The allowed categories for the hyperparameter.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoricalParameterRangeSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'categoricalParameterRangeSpecification_values' - The allowed categories for the hyperparameter.
newCategoricalParameterRangeSpecification ::
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  CategoricalParameterRangeSpecification
newCategoricalParameterRangeSpecification pValues_ =
  CategoricalParameterRangeSpecification'
    { values =
        Lens.coerced Lens.# pValues_
    }

-- | The allowed categories for the hyperparameter.
categoricalParameterRangeSpecification_values :: Lens.Lens' CategoricalParameterRangeSpecification (Prelude.NonEmpty Prelude.Text)
categoricalParameterRangeSpecification_values = Lens.lens (\CategoricalParameterRangeSpecification' {values} -> values) (\s@CategoricalParameterRangeSpecification' {} a -> s {values = a} :: CategoricalParameterRangeSpecification) Prelude.. Lens.coerced

instance
  Data.FromJSON
    CategoricalParameterRangeSpecification
  where
  parseJSON =
    Data.withObject
      "CategoricalParameterRangeSpecification"
      ( \x ->
          CategoricalParameterRangeSpecification'
            Prelude.<$> (x Data..: "Values")
      )

instance
  Prelude.Hashable
    CategoricalParameterRangeSpecification
  where
  hashWithSalt
    _salt
    CategoricalParameterRangeSpecification' {..} =
      _salt `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    CategoricalParameterRangeSpecification
  where
  rnf CategoricalParameterRangeSpecification' {..} =
    Prelude.rnf values

instance
  Data.ToJSON
    CategoricalParameterRangeSpecification
  where
  toJSON CategoricalParameterRangeSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Values" Data..= values)]
      )
