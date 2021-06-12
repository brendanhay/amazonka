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
-- Module      : Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines the possible values for an integer hyperparameter.
--
-- /See:/ 'newIntegerParameterRangeSpecification' smart constructor.
data IntegerParameterRangeSpecification = IntegerParameterRangeSpecification'
  { -- | The minimum integer value allowed.
    minValue :: Core.Text,
    -- | The maximum integer value allowed.
    maxValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IntegerParameterRangeSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minValue', 'integerParameterRangeSpecification_minValue' - The minimum integer value allowed.
--
-- 'maxValue', 'integerParameterRangeSpecification_maxValue' - The maximum integer value allowed.
newIntegerParameterRangeSpecification ::
  -- | 'minValue'
  Core.Text ->
  -- | 'maxValue'
  Core.Text ->
  IntegerParameterRangeSpecification
newIntegerParameterRangeSpecification
  pMinValue_
  pMaxValue_ =
    IntegerParameterRangeSpecification'
      { minValue =
          pMinValue_,
        maxValue = pMaxValue_
      }

-- | The minimum integer value allowed.
integerParameterRangeSpecification_minValue :: Lens.Lens' IntegerParameterRangeSpecification Core.Text
integerParameterRangeSpecification_minValue = Lens.lens (\IntegerParameterRangeSpecification' {minValue} -> minValue) (\s@IntegerParameterRangeSpecification' {} a -> s {minValue = a} :: IntegerParameterRangeSpecification)

-- | The maximum integer value allowed.
integerParameterRangeSpecification_maxValue :: Lens.Lens' IntegerParameterRangeSpecification Core.Text
integerParameterRangeSpecification_maxValue = Lens.lens (\IntegerParameterRangeSpecification' {maxValue} -> maxValue) (\s@IntegerParameterRangeSpecification' {} a -> s {maxValue = a} :: IntegerParameterRangeSpecification)

instance
  Core.FromJSON
    IntegerParameterRangeSpecification
  where
  parseJSON =
    Core.withObject
      "IntegerParameterRangeSpecification"
      ( \x ->
          IntegerParameterRangeSpecification'
            Core.<$> (x Core..: "MinValue")
            Core.<*> (x Core..: "MaxValue")
      )

instance
  Core.Hashable
    IntegerParameterRangeSpecification

instance
  Core.NFData
    IntegerParameterRangeSpecification

instance
  Core.ToJSON
    IntegerParameterRangeSpecification
  where
  toJSON IntegerParameterRangeSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MinValue" Core..= minValue),
            Core.Just ("MaxValue" Core..= maxValue)
          ]
      )
