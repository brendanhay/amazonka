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
-- Module      : Amazonka.SageMaker.Types.ParameterRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CategoricalParameterRangeSpecification
import Amazonka.SageMaker.Types.ContinuousParameterRangeSpecification
import Amazonka.SageMaker.Types.IntegerParameterRangeSpecification

-- | Defines the possible values for categorical, continuous, and integer
-- hyperparameters to be used by an algorithm.
--
-- /See:/ 'newParameterRange' smart constructor.
data ParameterRange = ParameterRange'
  { -- | A @CategoricalParameterRangeSpecification@ object that defines the
    -- possible values for a categorical hyperparameter.
    categoricalParameterRangeSpecification :: Prelude.Maybe CategoricalParameterRangeSpecification,
    -- | A @IntegerParameterRangeSpecification@ object that defines the possible
    -- values for an integer hyperparameter.
    integerParameterRangeSpecification :: Prelude.Maybe IntegerParameterRangeSpecification,
    -- | A @ContinuousParameterRangeSpecification@ object that defines the
    -- possible values for a continuous hyperparameter.
    continuousParameterRangeSpecification :: Prelude.Maybe ContinuousParameterRangeSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoricalParameterRangeSpecification', 'parameterRange_categoricalParameterRangeSpecification' - A @CategoricalParameterRangeSpecification@ object that defines the
-- possible values for a categorical hyperparameter.
--
-- 'integerParameterRangeSpecification', 'parameterRange_integerParameterRangeSpecification' - A @IntegerParameterRangeSpecification@ object that defines the possible
-- values for an integer hyperparameter.
--
-- 'continuousParameterRangeSpecification', 'parameterRange_continuousParameterRangeSpecification' - A @ContinuousParameterRangeSpecification@ object that defines the
-- possible values for a continuous hyperparameter.
newParameterRange ::
  ParameterRange
newParameterRange =
  ParameterRange'
    { categoricalParameterRangeSpecification =
        Prelude.Nothing,
      integerParameterRangeSpecification = Prelude.Nothing,
      continuousParameterRangeSpecification =
        Prelude.Nothing
    }

-- | A @CategoricalParameterRangeSpecification@ object that defines the
-- possible values for a categorical hyperparameter.
parameterRange_categoricalParameterRangeSpecification :: Lens.Lens' ParameterRange (Prelude.Maybe CategoricalParameterRangeSpecification)
parameterRange_categoricalParameterRangeSpecification = Lens.lens (\ParameterRange' {categoricalParameterRangeSpecification} -> categoricalParameterRangeSpecification) (\s@ParameterRange' {} a -> s {categoricalParameterRangeSpecification = a} :: ParameterRange)

-- | A @IntegerParameterRangeSpecification@ object that defines the possible
-- values for an integer hyperparameter.
parameterRange_integerParameterRangeSpecification :: Lens.Lens' ParameterRange (Prelude.Maybe IntegerParameterRangeSpecification)
parameterRange_integerParameterRangeSpecification = Lens.lens (\ParameterRange' {integerParameterRangeSpecification} -> integerParameterRangeSpecification) (\s@ParameterRange' {} a -> s {integerParameterRangeSpecification = a} :: ParameterRange)

-- | A @ContinuousParameterRangeSpecification@ object that defines the
-- possible values for a continuous hyperparameter.
parameterRange_continuousParameterRangeSpecification :: Lens.Lens' ParameterRange (Prelude.Maybe ContinuousParameterRangeSpecification)
parameterRange_continuousParameterRangeSpecification = Lens.lens (\ParameterRange' {continuousParameterRangeSpecification} -> continuousParameterRangeSpecification) (\s@ParameterRange' {} a -> s {continuousParameterRangeSpecification = a} :: ParameterRange)

instance Core.FromJSON ParameterRange where
  parseJSON =
    Core.withObject
      "ParameterRange"
      ( \x ->
          ParameterRange'
            Prelude.<$> (x Core..:? "CategoricalParameterRangeSpecification")
            Prelude.<*> (x Core..:? "IntegerParameterRangeSpecification")
            Prelude.<*> (x Core..:? "ContinuousParameterRangeSpecification")
      )

instance Prelude.Hashable ParameterRange where
  hashWithSalt salt' ParameterRange' {..} =
    salt'
      `Prelude.hashWithSalt` continuousParameterRangeSpecification
      `Prelude.hashWithSalt` integerParameterRangeSpecification
      `Prelude.hashWithSalt` categoricalParameterRangeSpecification

instance Prelude.NFData ParameterRange where
  rnf ParameterRange' {..} =
    Prelude.rnf categoricalParameterRangeSpecification
      `Prelude.seq` Prelude.rnf continuousParameterRangeSpecification
      `Prelude.seq` Prelude.rnf integerParameterRangeSpecification

instance Core.ToJSON ParameterRange where
  toJSON ParameterRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CategoricalParameterRangeSpecification" Core..=)
              Prelude.<$> categoricalParameterRangeSpecification,
            ("IntegerParameterRangeSpecification" Core..=)
              Prelude.<$> integerParameterRangeSpecification,
            ("ContinuousParameterRangeSpecification" Core..=)
              Prelude.<$> continuousParameterRangeSpecification
          ]
      )
