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
-- Module      : Amazonka.Forecast.Types.ParameterRanges
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ParameterRanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.CategoricalParameterRange
import Amazonka.Forecast.Types.ContinuousParameterRange
import Amazonka.Forecast.Types.IntegerParameterRange
import qualified Amazonka.Prelude as Prelude

-- | Specifies the categorical, continuous, and integer hyperparameters, and
-- their ranges of tunable values. The range of tunable values determines
-- which values that a hyperparameter tuning job can choose for the
-- specified hyperparameter. This object is part of the
-- HyperParameterTuningJobConfig object.
--
-- /See:/ 'newParameterRanges' smart constructor.
data ParameterRanges = ParameterRanges'
  { -- | Specifies the tunable range for each categorical hyperparameter.
    categoricalParameterRanges :: Prelude.Maybe (Prelude.NonEmpty CategoricalParameterRange),
    -- | Specifies the tunable range for each continuous hyperparameter.
    continuousParameterRanges :: Prelude.Maybe (Prelude.NonEmpty ContinuousParameterRange),
    -- | Specifies the tunable range for each integer hyperparameter.
    integerParameterRanges :: Prelude.Maybe (Prelude.NonEmpty IntegerParameterRange)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterRanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoricalParameterRanges', 'parameterRanges_categoricalParameterRanges' - Specifies the tunable range for each categorical hyperparameter.
--
-- 'continuousParameterRanges', 'parameterRanges_continuousParameterRanges' - Specifies the tunable range for each continuous hyperparameter.
--
-- 'integerParameterRanges', 'parameterRanges_integerParameterRanges' - Specifies the tunable range for each integer hyperparameter.
newParameterRanges ::
  ParameterRanges
newParameterRanges =
  ParameterRanges'
    { categoricalParameterRanges =
        Prelude.Nothing,
      continuousParameterRanges = Prelude.Nothing,
      integerParameterRanges = Prelude.Nothing
    }

-- | Specifies the tunable range for each categorical hyperparameter.
parameterRanges_categoricalParameterRanges :: Lens.Lens' ParameterRanges (Prelude.Maybe (Prelude.NonEmpty CategoricalParameterRange))
parameterRanges_categoricalParameterRanges = Lens.lens (\ParameterRanges' {categoricalParameterRanges} -> categoricalParameterRanges) (\s@ParameterRanges' {} a -> s {categoricalParameterRanges = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the tunable range for each continuous hyperparameter.
parameterRanges_continuousParameterRanges :: Lens.Lens' ParameterRanges (Prelude.Maybe (Prelude.NonEmpty ContinuousParameterRange))
parameterRanges_continuousParameterRanges = Lens.lens (\ParameterRanges' {continuousParameterRanges} -> continuousParameterRanges) (\s@ParameterRanges' {} a -> s {continuousParameterRanges = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the tunable range for each integer hyperparameter.
parameterRanges_integerParameterRanges :: Lens.Lens' ParameterRanges (Prelude.Maybe (Prelude.NonEmpty IntegerParameterRange))
parameterRanges_integerParameterRanges = Lens.lens (\ParameterRanges' {integerParameterRanges} -> integerParameterRanges) (\s@ParameterRanges' {} a -> s {integerParameterRanges = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ParameterRanges where
  parseJSON =
    Data.withObject
      "ParameterRanges"
      ( \x ->
          ParameterRanges'
            Prelude.<$> (x Data..:? "CategoricalParameterRanges")
            Prelude.<*> (x Data..:? "ContinuousParameterRanges")
            Prelude.<*> (x Data..:? "IntegerParameterRanges")
      )

instance Prelude.Hashable ParameterRanges where
  hashWithSalt _salt ParameterRanges' {..} =
    _salt
      `Prelude.hashWithSalt` categoricalParameterRanges
      `Prelude.hashWithSalt` continuousParameterRanges
      `Prelude.hashWithSalt` integerParameterRanges

instance Prelude.NFData ParameterRanges where
  rnf ParameterRanges' {..} =
    Prelude.rnf categoricalParameterRanges
      `Prelude.seq` Prelude.rnf continuousParameterRanges
      `Prelude.seq` Prelude.rnf integerParameterRanges

instance Data.ToJSON ParameterRanges where
  toJSON ParameterRanges' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoricalParameterRanges" Data..=)
              Prelude.<$> categoricalParameterRanges,
            ("ContinuousParameterRanges" Data..=)
              Prelude.<$> continuousParameterRanges,
            ("IntegerParameterRanges" Data..=)
              Prelude.<$> integerParameterRanges
          ]
      )
