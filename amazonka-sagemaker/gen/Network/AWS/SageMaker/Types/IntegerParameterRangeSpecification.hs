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
-- Module      : Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines the possible values for an integer hyperparameter.
--
-- /See:/ 'newIntegerParameterRangeSpecification' smart constructor.
data IntegerParameterRangeSpecification = IntegerParameterRangeSpecification'
  { -- | The minimum integer value allowed.
    minValue :: Prelude.Text,
    -- | The maximum integer value allowed.
    maxValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    IntegerParameterRangeSpecification
  where
  parseJSON =
    Prelude.withObject
      "IntegerParameterRangeSpecification"
      ( \x ->
          IntegerParameterRangeSpecification'
            Prelude.<$> (x Prelude..: "MinValue")
            Prelude.<*> (x Prelude..: "MaxValue")
      )

instance
  Prelude.Hashable
    IntegerParameterRangeSpecification

instance
  Prelude.NFData
    IntegerParameterRangeSpecification

instance
  Prelude.ToJSON
    IntegerParameterRangeSpecification
  where
  toJSON IntegerParameterRangeSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MinValue" Prelude..= minValue),
            Prelude.Just ("MaxValue" Prelude..= maxValue)
          ]
      )
