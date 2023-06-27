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
-- Module      : Amazonka.SageMaker.Types.ParameterRanges
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ParameterRanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoParameter
import Amazonka.SageMaker.Types.CategoricalParameterRange
import Amazonka.SageMaker.Types.ContinuousParameterRange
import Amazonka.SageMaker.Types.IntegerParameterRange

-- | Specifies ranges of integer, continuous, and categorical hyperparameters
-- that a hyperparameter tuning job searches. The hyperparameter tuning job
-- launches training jobs with hyperparameter values within these ranges to
-- find the combination of values that result in the training job with the
-- best performance as measured by the objective metric of the
-- hyperparameter tuning job.
--
-- The maximum number of items specified for @Array Members@ refers to the
-- maximum number of hyperparameters for each range and also the maximum
-- for the hyperparameter tuning job itself. That is, the sum of the number
-- of hyperparameters for all the ranges can\'t exceed the maximum number
-- specified.
--
-- /See:/ 'newParameterRanges' smart constructor.
data ParameterRanges = ParameterRanges'
  { -- | A list containing hyperparameter names and example values to be used by
    -- Autotune to determine optimal ranges for your tuning job.
    autoParameters :: Prelude.Maybe [AutoParameter],
    -- | The array of
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CategoricalParameterRange.html CategoricalParameterRange>
    -- objects that specify ranges of categorical hyperparameters that a
    -- hyperparameter tuning job searches.
    categoricalParameterRanges :: Prelude.Maybe [CategoricalParameterRange],
    -- | The array of
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ContinuousParameterRange.html ContinuousParameterRange>
    -- objects that specify ranges of continuous hyperparameters that a
    -- hyperparameter tuning job searches.
    continuousParameterRanges :: Prelude.Maybe [ContinuousParameterRange],
    -- | The array of
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_IntegerParameterRange.html IntegerParameterRange>
    -- objects that specify ranges of integer hyperparameters that a
    -- hyperparameter tuning job searches.
    integerParameterRanges :: Prelude.Maybe [IntegerParameterRange]
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
-- 'autoParameters', 'parameterRanges_autoParameters' - A list containing hyperparameter names and example values to be used by
-- Autotune to determine optimal ranges for your tuning job.
--
-- 'categoricalParameterRanges', 'parameterRanges_categoricalParameterRanges' - The array of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CategoricalParameterRange.html CategoricalParameterRange>
-- objects that specify ranges of categorical hyperparameters that a
-- hyperparameter tuning job searches.
--
-- 'continuousParameterRanges', 'parameterRanges_continuousParameterRanges' - The array of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ContinuousParameterRange.html ContinuousParameterRange>
-- objects that specify ranges of continuous hyperparameters that a
-- hyperparameter tuning job searches.
--
-- 'integerParameterRanges', 'parameterRanges_integerParameterRanges' - The array of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_IntegerParameterRange.html IntegerParameterRange>
-- objects that specify ranges of integer hyperparameters that a
-- hyperparameter tuning job searches.
newParameterRanges ::
  ParameterRanges
newParameterRanges =
  ParameterRanges'
    { autoParameters = Prelude.Nothing,
      categoricalParameterRanges = Prelude.Nothing,
      continuousParameterRanges = Prelude.Nothing,
      integerParameterRanges = Prelude.Nothing
    }

-- | A list containing hyperparameter names and example values to be used by
-- Autotune to determine optimal ranges for your tuning job.
parameterRanges_autoParameters :: Lens.Lens' ParameterRanges (Prelude.Maybe [AutoParameter])
parameterRanges_autoParameters = Lens.lens (\ParameterRanges' {autoParameters} -> autoParameters) (\s@ParameterRanges' {} a -> s {autoParameters = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The array of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CategoricalParameterRange.html CategoricalParameterRange>
-- objects that specify ranges of categorical hyperparameters that a
-- hyperparameter tuning job searches.
parameterRanges_categoricalParameterRanges :: Lens.Lens' ParameterRanges (Prelude.Maybe [CategoricalParameterRange])
parameterRanges_categoricalParameterRanges = Lens.lens (\ParameterRanges' {categoricalParameterRanges} -> categoricalParameterRanges) (\s@ParameterRanges' {} a -> s {categoricalParameterRanges = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The array of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ContinuousParameterRange.html ContinuousParameterRange>
-- objects that specify ranges of continuous hyperparameters that a
-- hyperparameter tuning job searches.
parameterRanges_continuousParameterRanges :: Lens.Lens' ParameterRanges (Prelude.Maybe [ContinuousParameterRange])
parameterRanges_continuousParameterRanges = Lens.lens (\ParameterRanges' {continuousParameterRanges} -> continuousParameterRanges) (\s@ParameterRanges' {} a -> s {continuousParameterRanges = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The array of
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_IntegerParameterRange.html IntegerParameterRange>
-- objects that specify ranges of integer hyperparameters that a
-- hyperparameter tuning job searches.
parameterRanges_integerParameterRanges :: Lens.Lens' ParameterRanges (Prelude.Maybe [IntegerParameterRange])
parameterRanges_integerParameterRanges = Lens.lens (\ParameterRanges' {integerParameterRanges} -> integerParameterRanges) (\s@ParameterRanges' {} a -> s {integerParameterRanges = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ParameterRanges where
  parseJSON =
    Data.withObject
      "ParameterRanges"
      ( \x ->
          ParameterRanges'
            Prelude.<$> (x Data..:? "AutoParameters" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "CategoricalParameterRanges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ContinuousParameterRanges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "IntegerParameterRanges"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ParameterRanges where
  hashWithSalt _salt ParameterRanges' {..} =
    _salt
      `Prelude.hashWithSalt` autoParameters
      `Prelude.hashWithSalt` categoricalParameterRanges
      `Prelude.hashWithSalt` continuousParameterRanges
      `Prelude.hashWithSalt` integerParameterRanges

instance Prelude.NFData ParameterRanges where
  rnf ParameterRanges' {..} =
    Prelude.rnf autoParameters
      `Prelude.seq` Prelude.rnf categoricalParameterRanges
      `Prelude.seq` Prelude.rnf continuousParameterRanges
      `Prelude.seq` Prelude.rnf integerParameterRanges

instance Data.ToJSON ParameterRanges where
  toJSON ParameterRanges' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoParameters" Data..=)
              Prelude.<$> autoParameters,
            ("CategoricalParameterRanges" Data..=)
              Prelude.<$> categoricalParameterRanges,
            ("ContinuousParameterRanges" Data..=)
              Prelude.<$> continuousParameterRanges,
            ("IntegerParameterRanges" Data..=)
              Prelude.<$> integerParameterRanges
          ]
      )
