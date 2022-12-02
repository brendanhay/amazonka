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
-- Module      : Amazonka.Forecast.Types.TimeSeriesCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.TimeSeriesCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.Condition
import qualified Amazonka.Prelude as Prelude

-- | Creates a subset of items within an attribute that are modified. For
-- example, you can use this operation to create a subset of items that
-- cost $5 or less. To do this, you specify @\"AttributeName\": \"price\"@,
-- @\"AttributeValue\": \"5\"@, and @\"Condition\": \"LESS_THAN\"@. Pair
-- this operation with the Action operation within the
-- CreateWhatIfForecastRequest$TimeSeriesTransformations operation to
-- define how the attribute is modified.
--
-- /See:/ 'newTimeSeriesCondition' smart constructor.
data TimeSeriesCondition = TimeSeriesCondition'
  { -- | The item_id, dimension name, IM name, or timestamp that you are
    -- modifying.
    attributeName :: Prelude.Text,
    -- | The value that is applied for the chosen @Condition@.
    attributeValue :: Prelude.Text,
    -- | The condition to apply. Valid values are @EQUALS@, @NOT_EQUALS@,
    -- @LESS_THAN@ and @GREATER_THAN@.
    condition :: Condition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'timeSeriesCondition_attributeName' - The item_id, dimension name, IM name, or timestamp that you are
-- modifying.
--
-- 'attributeValue', 'timeSeriesCondition_attributeValue' - The value that is applied for the chosen @Condition@.
--
-- 'condition', 'timeSeriesCondition_condition' - The condition to apply. Valid values are @EQUALS@, @NOT_EQUALS@,
-- @LESS_THAN@ and @GREATER_THAN@.
newTimeSeriesCondition ::
  -- | 'attributeName'
  Prelude.Text ->
  -- | 'attributeValue'
  Prelude.Text ->
  -- | 'condition'
  Condition ->
  TimeSeriesCondition
newTimeSeriesCondition
  pAttributeName_
  pAttributeValue_
  pCondition_ =
    TimeSeriesCondition'
      { attributeName =
          pAttributeName_,
        attributeValue = pAttributeValue_,
        condition = pCondition_
      }

-- | The item_id, dimension name, IM name, or timestamp that you are
-- modifying.
timeSeriesCondition_attributeName :: Lens.Lens' TimeSeriesCondition Prelude.Text
timeSeriesCondition_attributeName = Lens.lens (\TimeSeriesCondition' {attributeName} -> attributeName) (\s@TimeSeriesCondition' {} a -> s {attributeName = a} :: TimeSeriesCondition)

-- | The value that is applied for the chosen @Condition@.
timeSeriesCondition_attributeValue :: Lens.Lens' TimeSeriesCondition Prelude.Text
timeSeriesCondition_attributeValue = Lens.lens (\TimeSeriesCondition' {attributeValue} -> attributeValue) (\s@TimeSeriesCondition' {} a -> s {attributeValue = a} :: TimeSeriesCondition)

-- | The condition to apply. Valid values are @EQUALS@, @NOT_EQUALS@,
-- @LESS_THAN@ and @GREATER_THAN@.
timeSeriesCondition_condition :: Lens.Lens' TimeSeriesCondition Condition
timeSeriesCondition_condition = Lens.lens (\TimeSeriesCondition' {condition} -> condition) (\s@TimeSeriesCondition' {} a -> s {condition = a} :: TimeSeriesCondition)

instance Data.FromJSON TimeSeriesCondition where
  parseJSON =
    Data.withObject
      "TimeSeriesCondition"
      ( \x ->
          TimeSeriesCondition'
            Prelude.<$> (x Data..: "AttributeName")
            Prelude.<*> (x Data..: "AttributeValue")
            Prelude.<*> (x Data..: "Condition")
      )

instance Prelude.Hashable TimeSeriesCondition where
  hashWithSalt _salt TimeSeriesCondition' {..} =
    _salt `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` attributeValue
      `Prelude.hashWithSalt` condition

instance Prelude.NFData TimeSeriesCondition where
  rnf TimeSeriesCondition' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf attributeValue
      `Prelude.seq` Prelude.rnf condition

instance Data.ToJSON TimeSeriesCondition where
  toJSON TimeSeriesCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributeName" Data..= attributeName),
            Prelude.Just
              ("AttributeValue" Data..= attributeValue),
            Prelude.Just ("Condition" Data..= condition)
          ]
      )
