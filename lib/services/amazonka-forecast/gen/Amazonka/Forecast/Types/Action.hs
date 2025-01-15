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
-- Module      : Amazonka.Forecast.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.Operation
import qualified Amazonka.Prelude as Prelude

-- | Defines the modifications that you are making to an attribute for a
-- what-if forecast. For example, you can use this operation to create a
-- what-if forecast that investigates a 10% off sale on all shoes. To do
-- this, you specify @\"AttributeName\": \"shoes\"@,
-- @\"Operation\": \"MULTIPLY\"@, and @\"Value\": \"0.90\"@. Pair this
-- operation with the TimeSeriesCondition operation within the
-- CreateWhatIfForecastRequest$TimeSeriesTransformations operation to
-- define a subset of attribute items that are modified.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | The related time series that you are modifying. This value is case
    -- insensitive.
    attributeName :: Prelude.Text,
    -- | The operation that is applied to the provided attribute. Operations
    -- include:
    --
    -- -   @ADD@ - adds @Value@ to all rows of @AttributeName@.
    --
    -- -   @SUBTRACT@ - subtracts @Value@ from all rows of @AttributeName@.
    --
    -- -   @MULTIPLY@ - multiplies all rows of @AttributeName@ by @Value@.
    --
    -- -   @DIVIDE@ - divides all rows of @AttributeName@ by @Value@.
    operation :: Operation,
    -- | The value that is applied for the chosen @Operation@.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'action_attributeName' - The related time series that you are modifying. This value is case
-- insensitive.
--
-- 'operation', 'action_operation' - The operation that is applied to the provided attribute. Operations
-- include:
--
-- -   @ADD@ - adds @Value@ to all rows of @AttributeName@.
--
-- -   @SUBTRACT@ - subtracts @Value@ from all rows of @AttributeName@.
--
-- -   @MULTIPLY@ - multiplies all rows of @AttributeName@ by @Value@.
--
-- -   @DIVIDE@ - divides all rows of @AttributeName@ by @Value@.
--
-- 'value', 'action_value' - The value that is applied for the chosen @Operation@.
newAction ::
  -- | 'attributeName'
  Prelude.Text ->
  -- | 'operation'
  Operation ->
  -- | 'value'
  Prelude.Double ->
  Action
newAction pAttributeName_ pOperation_ pValue_ =
  Action'
    { attributeName = pAttributeName_,
      operation = pOperation_,
      value = pValue_
    }

-- | The related time series that you are modifying. This value is case
-- insensitive.
action_attributeName :: Lens.Lens' Action Prelude.Text
action_attributeName = Lens.lens (\Action' {attributeName} -> attributeName) (\s@Action' {} a -> s {attributeName = a} :: Action)

-- | The operation that is applied to the provided attribute. Operations
-- include:
--
-- -   @ADD@ - adds @Value@ to all rows of @AttributeName@.
--
-- -   @SUBTRACT@ - subtracts @Value@ from all rows of @AttributeName@.
--
-- -   @MULTIPLY@ - multiplies all rows of @AttributeName@ by @Value@.
--
-- -   @DIVIDE@ - divides all rows of @AttributeName@ by @Value@.
action_operation :: Lens.Lens' Action Operation
action_operation = Lens.lens (\Action' {operation} -> operation) (\s@Action' {} a -> s {operation = a} :: Action)

-- | The value that is applied for the chosen @Operation@.
action_value :: Lens.Lens' Action Prelude.Double
action_value = Lens.lens (\Action' {value} -> value) (\s@Action' {} a -> s {value = a} :: Action)

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Data..: "AttributeName")
            Prelude.<*> (x Data..: "Operation")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` value

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf attributeName `Prelude.seq`
      Prelude.rnf operation `Prelude.seq`
        Prelude.rnf value

instance Data.ToJSON Action where
  toJSON Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributeName" Data..= attributeName),
            Prelude.Just ("Operation" Data..= operation),
            Prelude.Just ("Value" Data..= value)
          ]
      )
