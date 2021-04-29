{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines the possible values for a continuous hyperparameter.
--
-- /See:/ 'newContinuousParameterRangeSpecification' smart constructor.
data ContinuousParameterRangeSpecification = ContinuousParameterRangeSpecification'
  { -- | The minimum floating-point value allowed.
    minValue :: Prelude.Text,
    -- | The maximum floating-point value allowed.
    maxValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    ContinuousParameterRangeSpecification
  where
  parseJSON =
    Prelude.withObject
      "ContinuousParameterRangeSpecification"
      ( \x ->
          ContinuousParameterRangeSpecification'
            Prelude.<$> (x Prelude..: "MinValue")
            Prelude.<*> (x Prelude..: "MaxValue")
      )

instance
  Prelude.Hashable
    ContinuousParameterRangeSpecification

instance
  Prelude.NFData
    ContinuousParameterRangeSpecification

instance
  Prelude.ToJSON
    ContinuousParameterRangeSpecification
  where
  toJSON ContinuousParameterRangeSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MinValue" Prelude..= minValue),
            Prelude.Just ("MaxValue" Prelude..= maxValue)
          ]
      )
