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
-- Module      : Amazonka.FraudDetector.Types.EventPredictionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EventPredictionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the summary of an event prediction.
--
-- /See:/ 'newEventPredictionSummary' smart constructor.
data EventPredictionSummary = EventPredictionSummary'
  { -- | The detector ID.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The detector version ID.
    detectorVersionId :: Prelude.Maybe Prelude.Text,
    -- | The event ID.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the event.
    eventTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The event type.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the prediction was generated.
    predictionTimestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventPredictionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'eventPredictionSummary_detectorId' - The detector ID.
--
-- 'detectorVersionId', 'eventPredictionSummary_detectorVersionId' - The detector version ID.
--
-- 'eventId', 'eventPredictionSummary_eventId' - The event ID.
--
-- 'eventTimestamp', 'eventPredictionSummary_eventTimestamp' - The timestamp of the event.
--
-- 'eventTypeName', 'eventPredictionSummary_eventTypeName' - The event type.
--
-- 'predictionTimestamp', 'eventPredictionSummary_predictionTimestamp' - The timestamp when the prediction was generated.
newEventPredictionSummary ::
  EventPredictionSummary
newEventPredictionSummary =
  EventPredictionSummary'
    { detectorId =
        Prelude.Nothing,
      detectorVersionId = Prelude.Nothing,
      eventId = Prelude.Nothing,
      eventTimestamp = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      predictionTimestamp = Prelude.Nothing
    }

-- | The detector ID.
eventPredictionSummary_detectorId :: Lens.Lens' EventPredictionSummary (Prelude.Maybe Prelude.Text)
eventPredictionSummary_detectorId = Lens.lens (\EventPredictionSummary' {detectorId} -> detectorId) (\s@EventPredictionSummary' {} a -> s {detectorId = a} :: EventPredictionSummary)

-- | The detector version ID.
eventPredictionSummary_detectorVersionId :: Lens.Lens' EventPredictionSummary (Prelude.Maybe Prelude.Text)
eventPredictionSummary_detectorVersionId = Lens.lens (\EventPredictionSummary' {detectorVersionId} -> detectorVersionId) (\s@EventPredictionSummary' {} a -> s {detectorVersionId = a} :: EventPredictionSummary)

-- | The event ID.
eventPredictionSummary_eventId :: Lens.Lens' EventPredictionSummary (Prelude.Maybe Prelude.Text)
eventPredictionSummary_eventId = Lens.lens (\EventPredictionSummary' {eventId} -> eventId) (\s@EventPredictionSummary' {} a -> s {eventId = a} :: EventPredictionSummary)

-- | The timestamp of the event.
eventPredictionSummary_eventTimestamp :: Lens.Lens' EventPredictionSummary (Prelude.Maybe Prelude.Text)
eventPredictionSummary_eventTimestamp = Lens.lens (\EventPredictionSummary' {eventTimestamp} -> eventTimestamp) (\s@EventPredictionSummary' {} a -> s {eventTimestamp = a} :: EventPredictionSummary)

-- | The event type.
eventPredictionSummary_eventTypeName :: Lens.Lens' EventPredictionSummary (Prelude.Maybe Prelude.Text)
eventPredictionSummary_eventTypeName = Lens.lens (\EventPredictionSummary' {eventTypeName} -> eventTypeName) (\s@EventPredictionSummary' {} a -> s {eventTypeName = a} :: EventPredictionSummary)

-- | The timestamp when the prediction was generated.
eventPredictionSummary_predictionTimestamp :: Lens.Lens' EventPredictionSummary (Prelude.Maybe Prelude.Text)
eventPredictionSummary_predictionTimestamp = Lens.lens (\EventPredictionSummary' {predictionTimestamp} -> predictionTimestamp) (\s@EventPredictionSummary' {} a -> s {predictionTimestamp = a} :: EventPredictionSummary)

instance Data.FromJSON EventPredictionSummary where
  parseJSON =
    Data.withObject
      "EventPredictionSummary"
      ( \x ->
          EventPredictionSummary'
            Prelude.<$> (x Data..:? "detectorId")
            Prelude.<*> (x Data..:? "detectorVersionId")
            Prelude.<*> (x Data..:? "eventId")
            Prelude.<*> (x Data..:? "eventTimestamp")
            Prelude.<*> (x Data..:? "eventTypeName")
            Prelude.<*> (x Data..:? "predictionTimestamp")
      )

instance Prelude.Hashable EventPredictionSummary where
  hashWithSalt _salt EventPredictionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` detectorVersionId
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventTimestamp
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` predictionTimestamp

instance Prelude.NFData EventPredictionSummary where
  rnf EventPredictionSummary' {..} =
    Prelude.rnf detectorId `Prelude.seq`
      Prelude.rnf detectorVersionId `Prelude.seq`
        Prelude.rnf eventId `Prelude.seq`
          Prelude.rnf eventTimestamp `Prelude.seq`
            Prelude.rnf eventTypeName `Prelude.seq`
              Prelude.rnf predictionTimestamp
