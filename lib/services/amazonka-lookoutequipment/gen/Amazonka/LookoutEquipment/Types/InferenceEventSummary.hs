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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceEventSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceEventSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the specific inference event, including start
-- and end time, diagnostics information, event duration and so on.
--
-- /See:/ 'newInferenceEventSummary' smart constructor.
data InferenceEventSummary = InferenceEventSummary'
  { -- | An array which specifies the names and values of all sensors
    -- contributing to an inference event.
    diagnostics :: Prelude.Maybe Prelude.Text,
    -- | Indicates the size of an inference event in seconds.
    eventDurationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Indicates the ending time of an inference event.
    eventEndTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates the starting time of an inference event.
    eventStartTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the inference scheduler being used for
    -- the inference event.
    inferenceSchedulerArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the inference scheduler being used for the inference events.
    inferenceSchedulerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceEventSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diagnostics', 'inferenceEventSummary_diagnostics' - An array which specifies the names and values of all sensors
-- contributing to an inference event.
--
-- 'eventDurationInSeconds', 'inferenceEventSummary_eventDurationInSeconds' - Indicates the size of an inference event in seconds.
--
-- 'eventEndTime', 'inferenceEventSummary_eventEndTime' - Indicates the ending time of an inference event.
--
-- 'eventStartTime', 'inferenceEventSummary_eventStartTime' - Indicates the starting time of an inference event.
--
-- 'inferenceSchedulerArn', 'inferenceEventSummary_inferenceSchedulerArn' - The Amazon Resource Name (ARN) of the inference scheduler being used for
-- the inference event.
--
-- 'inferenceSchedulerName', 'inferenceEventSummary_inferenceSchedulerName' - The name of the inference scheduler being used for the inference events.
newInferenceEventSummary ::
  InferenceEventSummary
newInferenceEventSummary =
  InferenceEventSummary'
    { diagnostics =
        Prelude.Nothing,
      eventDurationInSeconds = Prelude.Nothing,
      eventEndTime = Prelude.Nothing,
      eventStartTime = Prelude.Nothing,
      inferenceSchedulerArn = Prelude.Nothing,
      inferenceSchedulerName = Prelude.Nothing
    }

-- | An array which specifies the names and values of all sensors
-- contributing to an inference event.
inferenceEventSummary_diagnostics :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.Text)
inferenceEventSummary_diagnostics = Lens.lens (\InferenceEventSummary' {diagnostics} -> diagnostics) (\s@InferenceEventSummary' {} a -> s {diagnostics = a} :: InferenceEventSummary)

-- | Indicates the size of an inference event in seconds.
inferenceEventSummary_eventDurationInSeconds :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.Natural)
inferenceEventSummary_eventDurationInSeconds = Lens.lens (\InferenceEventSummary' {eventDurationInSeconds} -> eventDurationInSeconds) (\s@InferenceEventSummary' {} a -> s {eventDurationInSeconds = a} :: InferenceEventSummary)

-- | Indicates the ending time of an inference event.
inferenceEventSummary_eventEndTime :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.UTCTime)
inferenceEventSummary_eventEndTime = Lens.lens (\InferenceEventSummary' {eventEndTime} -> eventEndTime) (\s@InferenceEventSummary' {} a -> s {eventEndTime = a} :: InferenceEventSummary) Prelude.. Lens.mapping Data._Time

-- | Indicates the starting time of an inference event.
inferenceEventSummary_eventStartTime :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.UTCTime)
inferenceEventSummary_eventStartTime = Lens.lens (\InferenceEventSummary' {eventStartTime} -> eventStartTime) (\s@InferenceEventSummary' {} a -> s {eventStartTime = a} :: InferenceEventSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the inference scheduler being used for
-- the inference event.
inferenceEventSummary_inferenceSchedulerArn :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.Text)
inferenceEventSummary_inferenceSchedulerArn = Lens.lens (\InferenceEventSummary' {inferenceSchedulerArn} -> inferenceSchedulerArn) (\s@InferenceEventSummary' {} a -> s {inferenceSchedulerArn = a} :: InferenceEventSummary)

-- | The name of the inference scheduler being used for the inference events.
inferenceEventSummary_inferenceSchedulerName :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.Text)
inferenceEventSummary_inferenceSchedulerName = Lens.lens (\InferenceEventSummary' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@InferenceEventSummary' {} a -> s {inferenceSchedulerName = a} :: InferenceEventSummary)

instance Data.FromJSON InferenceEventSummary where
  parseJSON =
    Data.withObject
      "InferenceEventSummary"
      ( \x ->
          InferenceEventSummary'
            Prelude.<$> (x Data..:? "Diagnostics")
            Prelude.<*> (x Data..:? "EventDurationInSeconds")
            Prelude.<*> (x Data..:? "EventEndTime")
            Prelude.<*> (x Data..:? "EventStartTime")
            Prelude.<*> (x Data..:? "InferenceSchedulerArn")
            Prelude.<*> (x Data..:? "InferenceSchedulerName")
      )

instance Prelude.Hashable InferenceEventSummary where
  hashWithSalt _salt InferenceEventSummary' {..} =
    _salt
      `Prelude.hashWithSalt` diagnostics
      `Prelude.hashWithSalt` eventDurationInSeconds
      `Prelude.hashWithSalt` eventEndTime
      `Prelude.hashWithSalt` eventStartTime
      `Prelude.hashWithSalt` inferenceSchedulerArn
      `Prelude.hashWithSalt` inferenceSchedulerName

instance Prelude.NFData InferenceEventSummary where
  rnf InferenceEventSummary' {..} =
    Prelude.rnf diagnostics
      `Prelude.seq` Prelude.rnf eventDurationInSeconds
      `Prelude.seq` Prelude.rnf eventEndTime
      `Prelude.seq` Prelude.rnf eventStartTime
      `Prelude.seq` Prelude.rnf inferenceSchedulerArn
      `Prelude.seq` Prelude.rnf inferenceSchedulerName
