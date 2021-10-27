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
-- Module      : Network.AWS.Forecast.Types.EvaluationParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Types.EvaluationParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Parameters that define how to split a dataset into training data and
-- testing data, and the number of iterations to perform. These parameters
-- are specified in the predefined algorithms but you can override them in
-- the CreatePredictor request.
--
-- /See:/ 'newEvaluationParameters' smart constructor.
data EvaluationParameters = EvaluationParameters'
  { -- | The point from the end of the dataset where you want to split the data
    -- for model training and testing (evaluation). Specify the value as the
    -- number of data points. The default is the value of the forecast horizon.
    -- @BackTestWindowOffset@ can be used to mimic a past virtual forecast
    -- start date. This value must be greater than or equal to the forecast
    -- horizon and less than half of the TARGET_TIME_SERIES dataset length.
    --
    -- @ForecastHorizon@ \<= @BackTestWindowOffset@ \< 1\/2 *
    -- TARGET_TIME_SERIES dataset length
    backTestWindowOffset :: Prelude.Maybe Prelude.Int,
    -- | The number of times to split the input data. The default is 1. Valid
    -- values are 1 through 5.
    numberOfBacktestWindows :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backTestWindowOffset', 'evaluationParameters_backTestWindowOffset' - The point from the end of the dataset where you want to split the data
-- for model training and testing (evaluation). Specify the value as the
-- number of data points. The default is the value of the forecast horizon.
-- @BackTestWindowOffset@ can be used to mimic a past virtual forecast
-- start date. This value must be greater than or equal to the forecast
-- horizon and less than half of the TARGET_TIME_SERIES dataset length.
--
-- @ForecastHorizon@ \<= @BackTestWindowOffset@ \< 1\/2 *
-- TARGET_TIME_SERIES dataset length
--
-- 'numberOfBacktestWindows', 'evaluationParameters_numberOfBacktestWindows' - The number of times to split the input data. The default is 1. Valid
-- values are 1 through 5.
newEvaluationParameters ::
  EvaluationParameters
newEvaluationParameters =
  EvaluationParameters'
    { backTestWindowOffset =
        Prelude.Nothing,
      numberOfBacktestWindows = Prelude.Nothing
    }

-- | The point from the end of the dataset where you want to split the data
-- for model training and testing (evaluation). Specify the value as the
-- number of data points. The default is the value of the forecast horizon.
-- @BackTestWindowOffset@ can be used to mimic a past virtual forecast
-- start date. This value must be greater than or equal to the forecast
-- horizon and less than half of the TARGET_TIME_SERIES dataset length.
--
-- @ForecastHorizon@ \<= @BackTestWindowOffset@ \< 1\/2 *
-- TARGET_TIME_SERIES dataset length
evaluationParameters_backTestWindowOffset :: Lens.Lens' EvaluationParameters (Prelude.Maybe Prelude.Int)
evaluationParameters_backTestWindowOffset = Lens.lens (\EvaluationParameters' {backTestWindowOffset} -> backTestWindowOffset) (\s@EvaluationParameters' {} a -> s {backTestWindowOffset = a} :: EvaluationParameters)

-- | The number of times to split the input data. The default is 1. Valid
-- values are 1 through 5.
evaluationParameters_numberOfBacktestWindows :: Lens.Lens' EvaluationParameters (Prelude.Maybe Prelude.Int)
evaluationParameters_numberOfBacktestWindows = Lens.lens (\EvaluationParameters' {numberOfBacktestWindows} -> numberOfBacktestWindows) (\s@EvaluationParameters' {} a -> s {numberOfBacktestWindows = a} :: EvaluationParameters)

instance Core.FromJSON EvaluationParameters where
  parseJSON =
    Core.withObject
      "EvaluationParameters"
      ( \x ->
          EvaluationParameters'
            Prelude.<$> (x Core..:? "BackTestWindowOffset")
            Prelude.<*> (x Core..:? "NumberOfBacktestWindows")
      )

instance Prelude.Hashable EvaluationParameters

instance Prelude.NFData EvaluationParameters

instance Core.ToJSON EvaluationParameters where
  toJSON EvaluationParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BackTestWindowOffset" Core..=)
              Prelude.<$> backTestWindowOffset,
            ("NumberOfBacktestWindows" Core..=)
              Prelude.<$> numberOfBacktestWindows
          ]
      )
