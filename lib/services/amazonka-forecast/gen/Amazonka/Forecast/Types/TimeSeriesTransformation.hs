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
-- Module      : Amazonka.Forecast.Types.TimeSeriesTransformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.TimeSeriesTransformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.Action
import Amazonka.Forecast.Types.TimeSeriesCondition
import qualified Amazonka.Prelude as Prelude

-- | A transformation function is a pair of operations that select and modify
-- the rows in a related time series. You select the rows that you want
-- with a condition operation and you modify the rows with a transformation
-- operation. All conditions are joined with an AND operation, meaning that
-- all conditions must be true for the transformation to be applied.
-- Transformations are applied in the order that they are listed.
--
-- /See:/ 'newTimeSeriesTransformation' smart constructor.
data TimeSeriesTransformation = TimeSeriesTransformation'
  { -- | An array of actions that define a time series and how it is transformed.
    -- These transformations create a new time series that is used for the
    -- what-if analysis.
    action :: Prelude.Maybe Action,
    -- | An array of conditions that define which members of the related time
    -- series are transformed.
    timeSeriesConditions :: Prelude.Maybe [TimeSeriesCondition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesTransformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'timeSeriesTransformation_action' - An array of actions that define a time series and how it is transformed.
-- These transformations create a new time series that is used for the
-- what-if analysis.
--
-- 'timeSeriesConditions', 'timeSeriesTransformation_timeSeriesConditions' - An array of conditions that define which members of the related time
-- series are transformed.
newTimeSeriesTransformation ::
  TimeSeriesTransformation
newTimeSeriesTransformation =
  TimeSeriesTransformation'
    { action = Prelude.Nothing,
      timeSeriesConditions = Prelude.Nothing
    }

-- | An array of actions that define a time series and how it is transformed.
-- These transformations create a new time series that is used for the
-- what-if analysis.
timeSeriesTransformation_action :: Lens.Lens' TimeSeriesTransformation (Prelude.Maybe Action)
timeSeriesTransformation_action = Lens.lens (\TimeSeriesTransformation' {action} -> action) (\s@TimeSeriesTransformation' {} a -> s {action = a} :: TimeSeriesTransformation)

-- | An array of conditions that define which members of the related time
-- series are transformed.
timeSeriesTransformation_timeSeriesConditions :: Lens.Lens' TimeSeriesTransformation (Prelude.Maybe [TimeSeriesCondition])
timeSeriesTransformation_timeSeriesConditions = Lens.lens (\TimeSeriesTransformation' {timeSeriesConditions} -> timeSeriesConditions) (\s@TimeSeriesTransformation' {} a -> s {timeSeriesConditions = a} :: TimeSeriesTransformation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TimeSeriesTransformation where
  parseJSON =
    Data.withObject
      "TimeSeriesTransformation"
      ( \x ->
          TimeSeriesTransformation'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> ( x Data..:? "TimeSeriesConditions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TimeSeriesTransformation where
  hashWithSalt _salt TimeSeriesTransformation' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` timeSeriesConditions

instance Prelude.NFData TimeSeriesTransformation where
  rnf TimeSeriesTransformation' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf timeSeriesConditions

instance Data.ToJSON TimeSeriesTransformation where
  toJSON TimeSeriesTransformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("TimeSeriesConditions" Data..=)
              Prelude.<$> timeSeriesConditions
          ]
      )
