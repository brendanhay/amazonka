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
-- Module      : Amazonka.Forecast.Types.PredictorEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.PredictorEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about a predictor event, such as a retraining.
--
-- /See:/ 'newPredictorEvent' smart constructor.
data PredictorEvent = PredictorEvent'
  { -- | The timestamp for when the event occurred.
    datetime :: Prelude.Maybe Data.POSIX,
    -- | The type of event. For example, @Retrain@. A retraining event denotes
    -- the timepoint when a predictor was retrained. Any monitor results from
    -- before the @Datetime@ are from the previous predictor. Any new metrics
    -- are for the newly retrained predictor.
    detail :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictorEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datetime', 'predictorEvent_datetime' - The timestamp for when the event occurred.
--
-- 'detail', 'predictorEvent_detail' - The type of event. For example, @Retrain@. A retraining event denotes
-- the timepoint when a predictor was retrained. Any monitor results from
-- before the @Datetime@ are from the previous predictor. Any new metrics
-- are for the newly retrained predictor.
newPredictorEvent ::
  PredictorEvent
newPredictorEvent =
  PredictorEvent'
    { datetime = Prelude.Nothing,
      detail = Prelude.Nothing
    }

-- | The timestamp for when the event occurred.
predictorEvent_datetime :: Lens.Lens' PredictorEvent (Prelude.Maybe Prelude.UTCTime)
predictorEvent_datetime = Lens.lens (\PredictorEvent' {datetime} -> datetime) (\s@PredictorEvent' {} a -> s {datetime = a} :: PredictorEvent) Prelude.. Lens.mapping Data._Time

-- | The type of event. For example, @Retrain@. A retraining event denotes
-- the timepoint when a predictor was retrained. Any monitor results from
-- before the @Datetime@ are from the previous predictor. Any new metrics
-- are for the newly retrained predictor.
predictorEvent_detail :: Lens.Lens' PredictorEvent (Prelude.Maybe Prelude.Text)
predictorEvent_detail = Lens.lens (\PredictorEvent' {detail} -> detail) (\s@PredictorEvent' {} a -> s {detail = a} :: PredictorEvent)

instance Data.FromJSON PredictorEvent where
  parseJSON =
    Data.withObject
      "PredictorEvent"
      ( \x ->
          PredictorEvent'
            Prelude.<$> (x Data..:? "Datetime")
            Prelude.<*> (x Data..:? "Detail")
      )

instance Prelude.Hashable PredictorEvent where
  hashWithSalt _salt PredictorEvent' {..} =
    _salt
      `Prelude.hashWithSalt` datetime
      `Prelude.hashWithSalt` detail

instance Prelude.NFData PredictorEvent where
  rnf PredictorEvent' {..} =
    Prelude.rnf datetime
      `Prelude.seq` Prelude.rnf detail
