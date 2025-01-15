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
-- Module      : Amazonka.Forecast.Types.PredictorExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.PredictorExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.TestWindowSummary
import qualified Amazonka.Prelude as Prelude

-- | The algorithm used to perform a backtest and the status of those tests.
--
-- /See:/ 'newPredictorExecution' smart constructor.
data PredictorExecution = PredictorExecution'
  { -- | The ARN of the algorithm used to test the predictor.
    algorithmArn :: Prelude.Maybe Prelude.Text,
    -- | An array of test windows used to evaluate the algorithm. The
    -- @NumberOfBacktestWindows@ from the object determines the number of
    -- windows in the array.
    testWindows :: Prelude.Maybe [TestWindowSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictorExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmArn', 'predictorExecution_algorithmArn' - The ARN of the algorithm used to test the predictor.
--
-- 'testWindows', 'predictorExecution_testWindows' - An array of test windows used to evaluate the algorithm. The
-- @NumberOfBacktestWindows@ from the object determines the number of
-- windows in the array.
newPredictorExecution ::
  PredictorExecution
newPredictorExecution =
  PredictorExecution'
    { algorithmArn = Prelude.Nothing,
      testWindows = Prelude.Nothing
    }

-- | The ARN of the algorithm used to test the predictor.
predictorExecution_algorithmArn :: Lens.Lens' PredictorExecution (Prelude.Maybe Prelude.Text)
predictorExecution_algorithmArn = Lens.lens (\PredictorExecution' {algorithmArn} -> algorithmArn) (\s@PredictorExecution' {} a -> s {algorithmArn = a} :: PredictorExecution)

-- | An array of test windows used to evaluate the algorithm. The
-- @NumberOfBacktestWindows@ from the object determines the number of
-- windows in the array.
predictorExecution_testWindows :: Lens.Lens' PredictorExecution (Prelude.Maybe [TestWindowSummary])
predictorExecution_testWindows = Lens.lens (\PredictorExecution' {testWindows} -> testWindows) (\s@PredictorExecution' {} a -> s {testWindows = a} :: PredictorExecution) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PredictorExecution where
  parseJSON =
    Data.withObject
      "PredictorExecution"
      ( \x ->
          PredictorExecution'
            Prelude.<$> (x Data..:? "AlgorithmArn")
            Prelude.<*> (x Data..:? "TestWindows" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PredictorExecution where
  hashWithSalt _salt PredictorExecution' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmArn
      `Prelude.hashWithSalt` testWindows

instance Prelude.NFData PredictorExecution where
  rnf PredictorExecution' {..} =
    Prelude.rnf algorithmArn `Prelude.seq`
      Prelude.rnf testWindows
