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
-- Module      : Amazonka.Personalize.Types.DefaultCategoricalHyperParameterRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DefaultCategoricalHyperParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the name and default range of a categorical hyperparameter and
-- whether the hyperparameter is tunable. A tunable hyperparameter can have
-- its value determined during hyperparameter optimization (HPO).
--
-- /See:/ 'newDefaultCategoricalHyperParameterRange' smart constructor.
data DefaultCategoricalHyperParameterRange = DefaultCategoricalHyperParameterRange'
  { -- | Whether the hyperparameter is tunable.
    isTunable :: Prelude.Maybe Prelude.Bool,
    -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of the categories for the hyperparameter.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultCategoricalHyperParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTunable', 'defaultCategoricalHyperParameterRange_isTunable' - Whether the hyperparameter is tunable.
--
-- 'name', 'defaultCategoricalHyperParameterRange_name' - The name of the hyperparameter.
--
-- 'values', 'defaultCategoricalHyperParameterRange_values' - A list of the categories for the hyperparameter.
newDefaultCategoricalHyperParameterRange ::
  DefaultCategoricalHyperParameterRange
newDefaultCategoricalHyperParameterRange =
  DefaultCategoricalHyperParameterRange'
    { isTunable =
        Prelude.Nothing,
      name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | Whether the hyperparameter is tunable.
defaultCategoricalHyperParameterRange_isTunable :: Lens.Lens' DefaultCategoricalHyperParameterRange (Prelude.Maybe Prelude.Bool)
defaultCategoricalHyperParameterRange_isTunable = Lens.lens (\DefaultCategoricalHyperParameterRange' {isTunable} -> isTunable) (\s@DefaultCategoricalHyperParameterRange' {} a -> s {isTunable = a} :: DefaultCategoricalHyperParameterRange)

-- | The name of the hyperparameter.
defaultCategoricalHyperParameterRange_name :: Lens.Lens' DefaultCategoricalHyperParameterRange (Prelude.Maybe Prelude.Text)
defaultCategoricalHyperParameterRange_name = Lens.lens (\DefaultCategoricalHyperParameterRange' {name} -> name) (\s@DefaultCategoricalHyperParameterRange' {} a -> s {name = a} :: DefaultCategoricalHyperParameterRange)

-- | A list of the categories for the hyperparameter.
defaultCategoricalHyperParameterRange_values :: Lens.Lens' DefaultCategoricalHyperParameterRange (Prelude.Maybe [Prelude.Text])
defaultCategoricalHyperParameterRange_values = Lens.lens (\DefaultCategoricalHyperParameterRange' {values} -> values) (\s@DefaultCategoricalHyperParameterRange' {} a -> s {values = a} :: DefaultCategoricalHyperParameterRange) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    DefaultCategoricalHyperParameterRange
  where
  parseJSON =
    Data.withObject
      "DefaultCategoricalHyperParameterRange"
      ( \x ->
          DefaultCategoricalHyperParameterRange'
            Prelude.<$> (x Data..:? "isTunable")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    DefaultCategoricalHyperParameterRange
  where
  hashWithSalt
    _salt
    DefaultCategoricalHyperParameterRange' {..} =
      _salt `Prelude.hashWithSalt` isTunable
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    DefaultCategoricalHyperParameterRange
  where
  rnf DefaultCategoricalHyperParameterRange' {..} =
    Prelude.rnf isTunable
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf values
