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
-- Module      : Network.AWS.SageMaker.Types.ParameterRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParameterRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
import Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
import Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification

-- | Defines the possible values for categorical, continuous, and integer
-- hyperparameters to be used by an algorithm.
--
-- /See:/ 'newParameterRange' smart constructor.
data ParameterRange = ParameterRange'
  { -- | A @ContinuousParameterRangeSpecification@ object that defines the
    -- possible values for a continuous hyperparameter.
    continuousParameterRangeSpecification :: Core.Maybe ContinuousParameterRangeSpecification,
    -- | A @IntegerParameterRangeSpecification@ object that defines the possible
    -- values for an integer hyperparameter.
    integerParameterRangeSpecification :: Core.Maybe IntegerParameterRangeSpecification,
    -- | A @CategoricalParameterRangeSpecification@ object that defines the
    -- possible values for a categorical hyperparameter.
    categoricalParameterRangeSpecification :: Core.Maybe CategoricalParameterRangeSpecification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousParameterRangeSpecification', 'parameterRange_continuousParameterRangeSpecification' - A @ContinuousParameterRangeSpecification@ object that defines the
-- possible values for a continuous hyperparameter.
--
-- 'integerParameterRangeSpecification', 'parameterRange_integerParameterRangeSpecification' - A @IntegerParameterRangeSpecification@ object that defines the possible
-- values for an integer hyperparameter.
--
-- 'categoricalParameterRangeSpecification', 'parameterRange_categoricalParameterRangeSpecification' - A @CategoricalParameterRangeSpecification@ object that defines the
-- possible values for a categorical hyperparameter.
newParameterRange ::
  ParameterRange
newParameterRange =
  ParameterRange'
    { continuousParameterRangeSpecification =
        Core.Nothing,
      integerParameterRangeSpecification = Core.Nothing,
      categoricalParameterRangeSpecification =
        Core.Nothing
    }

-- | A @ContinuousParameterRangeSpecification@ object that defines the
-- possible values for a continuous hyperparameter.
parameterRange_continuousParameterRangeSpecification :: Lens.Lens' ParameterRange (Core.Maybe ContinuousParameterRangeSpecification)
parameterRange_continuousParameterRangeSpecification = Lens.lens (\ParameterRange' {continuousParameterRangeSpecification} -> continuousParameterRangeSpecification) (\s@ParameterRange' {} a -> s {continuousParameterRangeSpecification = a} :: ParameterRange)

-- | A @IntegerParameterRangeSpecification@ object that defines the possible
-- values for an integer hyperparameter.
parameterRange_integerParameterRangeSpecification :: Lens.Lens' ParameterRange (Core.Maybe IntegerParameterRangeSpecification)
parameterRange_integerParameterRangeSpecification = Lens.lens (\ParameterRange' {integerParameterRangeSpecification} -> integerParameterRangeSpecification) (\s@ParameterRange' {} a -> s {integerParameterRangeSpecification = a} :: ParameterRange)

-- | A @CategoricalParameterRangeSpecification@ object that defines the
-- possible values for a categorical hyperparameter.
parameterRange_categoricalParameterRangeSpecification :: Lens.Lens' ParameterRange (Core.Maybe CategoricalParameterRangeSpecification)
parameterRange_categoricalParameterRangeSpecification = Lens.lens (\ParameterRange' {categoricalParameterRangeSpecification} -> categoricalParameterRangeSpecification) (\s@ParameterRange' {} a -> s {categoricalParameterRangeSpecification = a} :: ParameterRange)

instance Core.FromJSON ParameterRange where
  parseJSON =
    Core.withObject
      "ParameterRange"
      ( \x ->
          ParameterRange'
            Core.<$> (x Core..:? "ContinuousParameterRangeSpecification")
            Core.<*> (x Core..:? "IntegerParameterRangeSpecification")
            Core.<*> ( x
                         Core..:? "CategoricalParameterRangeSpecification"
                     )
      )

instance Core.Hashable ParameterRange

instance Core.NFData ParameterRange

instance Core.ToJSON ParameterRange where
  toJSON ParameterRange' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContinuousParameterRangeSpecification" Core..=)
              Core.<$> continuousParameterRangeSpecification,
            ("IntegerParameterRangeSpecification" Core..=)
              Core.<$> integerParameterRangeSpecification,
            ("CategoricalParameterRangeSpecification" Core..=)
              Core.<$> categoricalParameterRangeSpecification
          ]
      )
