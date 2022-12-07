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
-- Module      : Amazonka.IoTEvents.Types.SimpleRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.SimpleRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.ComparisonOperator
import qualified Amazonka.Prelude as Prelude

-- | A rule that compares an input property value to a threshold value with a
-- comparison operator.
--
-- /See:/ 'newSimpleRule' smart constructor.
data SimpleRule = SimpleRule'
  { -- | The value on the left side of the comparison operator. You can specify
    -- an AWS IoT Events input attribute as an input property.
    inputProperty :: Prelude.Text,
    -- | The comparison operator.
    comparisonOperator :: ComparisonOperator,
    -- | The value on the right side of the comparison operator. You can enter a
    -- number or specify an AWS IoT Events input attribute.
    threshold :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimpleRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputProperty', 'simpleRule_inputProperty' - The value on the left side of the comparison operator. You can specify
-- an AWS IoT Events input attribute as an input property.
--
-- 'comparisonOperator', 'simpleRule_comparisonOperator' - The comparison operator.
--
-- 'threshold', 'simpleRule_threshold' - The value on the right side of the comparison operator. You can enter a
-- number or specify an AWS IoT Events input attribute.
newSimpleRule ::
  -- | 'inputProperty'
  Prelude.Text ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'threshold'
  Prelude.Text ->
  SimpleRule
newSimpleRule
  pInputProperty_
  pComparisonOperator_
  pThreshold_ =
    SimpleRule'
      { inputProperty = pInputProperty_,
        comparisonOperator = pComparisonOperator_,
        threshold = pThreshold_
      }

-- | The value on the left side of the comparison operator. You can specify
-- an AWS IoT Events input attribute as an input property.
simpleRule_inputProperty :: Lens.Lens' SimpleRule Prelude.Text
simpleRule_inputProperty = Lens.lens (\SimpleRule' {inputProperty} -> inputProperty) (\s@SimpleRule' {} a -> s {inputProperty = a} :: SimpleRule)

-- | The comparison operator.
simpleRule_comparisonOperator :: Lens.Lens' SimpleRule ComparisonOperator
simpleRule_comparisonOperator = Lens.lens (\SimpleRule' {comparisonOperator} -> comparisonOperator) (\s@SimpleRule' {} a -> s {comparisonOperator = a} :: SimpleRule)

-- | The value on the right side of the comparison operator. You can enter a
-- number or specify an AWS IoT Events input attribute.
simpleRule_threshold :: Lens.Lens' SimpleRule Prelude.Text
simpleRule_threshold = Lens.lens (\SimpleRule' {threshold} -> threshold) (\s@SimpleRule' {} a -> s {threshold = a} :: SimpleRule)

instance Data.FromJSON SimpleRule where
  parseJSON =
    Data.withObject
      "SimpleRule"
      ( \x ->
          SimpleRule'
            Prelude.<$> (x Data..: "inputProperty")
            Prelude.<*> (x Data..: "comparisonOperator")
            Prelude.<*> (x Data..: "threshold")
      )

instance Prelude.Hashable SimpleRule where
  hashWithSalt _salt SimpleRule' {..} =
    _salt `Prelude.hashWithSalt` inputProperty
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData SimpleRule where
  rnf SimpleRule' {..} =
    Prelude.rnf inputProperty
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf threshold

instance Data.ToJSON SimpleRule where
  toJSON SimpleRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("inputProperty" Data..= inputProperty),
            Prelude.Just
              ("comparisonOperator" Data..= comparisonOperator),
            Prelude.Just ("threshold" Data..= threshold)
          ]
      )
