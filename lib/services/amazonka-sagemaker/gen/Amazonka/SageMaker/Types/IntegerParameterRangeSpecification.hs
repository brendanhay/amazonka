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
-- Module      : Amazonka.SageMaker.Types.IntegerParameterRangeSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.IntegerParameterRangeSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the possible values for an integer hyperparameter.
--
-- /See:/ 'newIntegerParameterRangeSpecification' smart constructor.
data IntegerParameterRangeSpecification = IntegerParameterRangeSpecification'
  { -- | The minimum integer value allowed.
    minValue :: Prelude.Text,
    -- | The maximum integer value allowed.
    maxValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'maxValue'
  Prelude.Text ->
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
integerParameterRangeSpecification_minValue :: Lens.Lens' IntegerParameterRangeSpecification Prelude.Text
integerParameterRangeSpecification_minValue = Lens.lens (\IntegerParameterRangeSpecification' {minValue} -> minValue) (\s@IntegerParameterRangeSpecification' {} a -> s {minValue = a} :: IntegerParameterRangeSpecification)

-- | The maximum integer value allowed.
integerParameterRangeSpecification_maxValue :: Lens.Lens' IntegerParameterRangeSpecification Prelude.Text
integerParameterRangeSpecification_maxValue = Lens.lens (\IntegerParameterRangeSpecification' {maxValue} -> maxValue) (\s@IntegerParameterRangeSpecification' {} a -> s {maxValue = a} :: IntegerParameterRangeSpecification)

instance
  Data.FromJSON
    IntegerParameterRangeSpecification
  where
  parseJSON =
    Data.withObject
      "IntegerParameterRangeSpecification"
      ( \x ->
          IntegerParameterRangeSpecification'
            Prelude.<$> (x Data..: "MinValue")
            Prelude.<*> (x Data..: "MaxValue")
      )

instance
  Prelude.Hashable
    IntegerParameterRangeSpecification
  where
  hashWithSalt
    _salt
    IntegerParameterRangeSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` minValue
        `Prelude.hashWithSalt` maxValue

instance
  Prelude.NFData
    IntegerParameterRangeSpecification
  where
  rnf IntegerParameterRangeSpecification' {..} =
    Prelude.rnf minValue `Prelude.seq`
      Prelude.rnf maxValue

instance
  Data.ToJSON
    IntegerParameterRangeSpecification
  where
  toJSON IntegerParameterRangeSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MinValue" Data..= minValue),
            Prelude.Just ("MaxValue" Data..= maxValue)
          ]
      )
