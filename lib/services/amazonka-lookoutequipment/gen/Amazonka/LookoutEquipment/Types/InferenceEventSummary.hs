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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceEventSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the specific inference event, including start
-- and end time, diagnostics information, event duration and so on.
--
-- /See:/ 'newInferenceEventSummary' smart constructor.
data InferenceEventSummary = InferenceEventSummary'
  { -- | The name of the inference scheduler being used for the inference events.
    inferenceSchedulerName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the ending time of an inference event.
    eventEndTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates the size of an inference event in seconds.
    eventDurationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | An array which specifies the names and values of all sensors
    -- contributing to an inference event.
    diagnostics :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the inference scheduler being used for
    -- the inference event.
    inferenceSchedulerArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the starting time of an inference event.
    eventStartTime :: Prelude.Maybe Core.POSIX
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
-- 'inferenceSchedulerName', 'inferenceEventSummary_inferenceSchedulerName' - The name of the inference scheduler being used for the inference events.
--
-- 'eventEndTime', 'inferenceEventSummary_eventEndTime' - Indicates the ending time of an inference event.
--
-- 'eventDurationInSeconds', 'inferenceEventSummary_eventDurationInSeconds' - Indicates the size of an inference event in seconds.
--
-- 'diagnostics', 'inferenceEventSummary_diagnostics' - An array which specifies the names and values of all sensors
-- contributing to an inference event.
--
-- 'inferenceSchedulerArn', 'inferenceEventSummary_inferenceSchedulerArn' - The Amazon Resource Name (ARN) of the inference scheduler being used for
-- the inference event.
--
-- 'eventStartTime', 'inferenceEventSummary_eventStartTime' - Indicates the starting time of an inference event.
newInferenceEventSummary ::
  InferenceEventSummary
newInferenceEventSummary =
  InferenceEventSummary'
    { inferenceSchedulerName =
        Prelude.Nothing,
      eventEndTime = Prelude.Nothing,
      eventDurationInSeconds = Prelude.Nothing,
      diagnostics = Prelude.Nothing,
      inferenceSchedulerArn = Prelude.Nothing,
      eventStartTime = Prelude.Nothing
    }

-- | The name of the inference scheduler being used for the inference events.
inferenceEventSummary_inferenceSchedulerName :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.Text)
inferenceEventSummary_inferenceSchedulerName = Lens.lens (\InferenceEventSummary' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@InferenceEventSummary' {} a -> s {inferenceSchedulerName = a} :: InferenceEventSummary)

-- | Indicates the ending time of an inference event.
inferenceEventSummary_eventEndTime :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.UTCTime)
inferenceEventSummary_eventEndTime = Lens.lens (\InferenceEventSummary' {eventEndTime} -> eventEndTime) (\s@InferenceEventSummary' {} a -> s {eventEndTime = a} :: InferenceEventSummary) Prelude.. Lens.mapping Core._Time

-- | Indicates the size of an inference event in seconds.
inferenceEventSummary_eventDurationInSeconds :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.Natural)
inferenceEventSummary_eventDurationInSeconds = Lens.lens (\InferenceEventSummary' {eventDurationInSeconds} -> eventDurationInSeconds) (\s@InferenceEventSummary' {} a -> s {eventDurationInSeconds = a} :: InferenceEventSummary)

-- | An array which specifies the names and values of all sensors
-- contributing to an inference event.
inferenceEventSummary_diagnostics :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.Text)
inferenceEventSummary_diagnostics = Lens.lens (\InferenceEventSummary' {diagnostics} -> diagnostics) (\s@InferenceEventSummary' {} a -> s {diagnostics = a} :: InferenceEventSummary)

-- | The Amazon Resource Name (ARN) of the inference scheduler being used for
-- the inference event.
inferenceEventSummary_inferenceSchedulerArn :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.Text)
inferenceEventSummary_inferenceSchedulerArn = Lens.lens (\InferenceEventSummary' {inferenceSchedulerArn} -> inferenceSchedulerArn) (\s@InferenceEventSummary' {} a -> s {inferenceSchedulerArn = a} :: InferenceEventSummary)

-- | Indicates the starting time of an inference event.
inferenceEventSummary_eventStartTime :: Lens.Lens' InferenceEventSummary (Prelude.Maybe Prelude.UTCTime)
inferenceEventSummary_eventStartTime = Lens.lens (\InferenceEventSummary' {eventStartTime} -> eventStartTime) (\s@InferenceEventSummary' {} a -> s {eventStartTime = a} :: InferenceEventSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON InferenceEventSummary where
  parseJSON =
    Core.withObject
      "InferenceEventSummary"
      ( \x ->
          InferenceEventSummary'
            Prelude.<$> (x Core..:? "InferenceSchedulerName")
            Prelude.<*> (x Core..:? "EventEndTime")
            Prelude.<*> (x Core..:? "EventDurationInSeconds")
            Prelude.<*> (x Core..:? "Diagnostics")
            Prelude.<*> (x Core..:? "InferenceSchedulerArn")
            Prelude.<*> (x Core..:? "EventStartTime")
      )

instance Prelude.Hashable InferenceEventSummary where
  hashWithSalt _salt InferenceEventSummary' {..} =
    _salt `Prelude.hashWithSalt` inferenceSchedulerName
      `Prelude.hashWithSalt` eventEndTime
      `Prelude.hashWithSalt` eventDurationInSeconds
      `Prelude.hashWithSalt` diagnostics
      `Prelude.hashWithSalt` inferenceSchedulerArn
      `Prelude.hashWithSalt` eventStartTime

instance Prelude.NFData InferenceEventSummary where
  rnf InferenceEventSummary' {..} =
    Prelude.rnf inferenceSchedulerName
      `Prelude.seq` Prelude.rnf eventEndTime
      `Prelude.seq` Prelude.rnf eventDurationInSeconds
      `Prelude.seq` Prelude.rnf diagnostics
      `Prelude.seq` Prelude.rnf inferenceSchedulerArn
      `Prelude.seq` Prelude.rnf eventStartTime
