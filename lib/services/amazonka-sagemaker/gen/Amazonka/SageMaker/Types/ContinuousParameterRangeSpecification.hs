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
-- Module      : Amazonka.SageMaker.Types.ContinuousParameterRangeSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ContinuousParameterRangeSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the possible values for a continuous hyperparameter.
--
-- /See:/ 'newContinuousParameterRangeSpecification' smart constructor.
data ContinuousParameterRangeSpecification = ContinuousParameterRangeSpecification'
  { -- | The minimum floating-point value allowed.
    minValue :: Prelude.Text,
    -- | The maximum floating-point value allowed.
    maxValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousParameterRangeSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minValue', 'continuousParameterRangeSpecification_minValue' - The minimum floating-point value allowed.
--
-- 'maxValue', 'continuousParameterRangeSpecification_maxValue' - The maximum floating-point value allowed.
newContinuousParameterRangeSpecification ::
  -- | 'minValue'
  Prelude.Text ->
  -- | 'maxValue'
  Prelude.Text ->
  ContinuousParameterRangeSpecification
newContinuousParameterRangeSpecification
  pMinValue_
  pMaxValue_ =
    ContinuousParameterRangeSpecification'
      { minValue =
          pMinValue_,
        maxValue = pMaxValue_
      }

-- | The minimum floating-point value allowed.
continuousParameterRangeSpecification_minValue :: Lens.Lens' ContinuousParameterRangeSpecification Prelude.Text
continuousParameterRangeSpecification_minValue = Lens.lens (\ContinuousParameterRangeSpecification' {minValue} -> minValue) (\s@ContinuousParameterRangeSpecification' {} a -> s {minValue = a} :: ContinuousParameterRangeSpecification)

-- | The maximum floating-point value allowed.
continuousParameterRangeSpecification_maxValue :: Lens.Lens' ContinuousParameterRangeSpecification Prelude.Text
continuousParameterRangeSpecification_maxValue = Lens.lens (\ContinuousParameterRangeSpecification' {maxValue} -> maxValue) (\s@ContinuousParameterRangeSpecification' {} a -> s {maxValue = a} :: ContinuousParameterRangeSpecification)

instance
  Data.FromJSON
    ContinuousParameterRangeSpecification
  where
  parseJSON =
    Data.withObject
      "ContinuousParameterRangeSpecification"
      ( \x ->
          ContinuousParameterRangeSpecification'
            Prelude.<$> (x Data..: "MinValue")
            Prelude.<*> (x Data..: "MaxValue")
      )

instance
  Prelude.Hashable
    ContinuousParameterRangeSpecification
  where
  hashWithSalt
    _salt
    ContinuousParameterRangeSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` minValue
        `Prelude.hashWithSalt` maxValue

instance
  Prelude.NFData
    ContinuousParameterRangeSpecification
  where
  rnf ContinuousParameterRangeSpecification' {..} =
    Prelude.rnf minValue `Prelude.seq`
      Prelude.rnf maxValue

instance
  Data.ToJSON
    ContinuousParameterRangeSpecification
  where
  toJSON ContinuousParameterRangeSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MinValue" Data..= minValue),
            Prelude.Just ("MaxValue" Data..= maxValue)
          ]
      )
