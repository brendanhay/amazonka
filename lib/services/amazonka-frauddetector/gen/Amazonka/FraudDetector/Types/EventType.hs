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
-- Module      : Amazonka.FraudDetector.Types.EventType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EventType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.EventIngestion
import Amazonka.FraudDetector.Types.IngestedEventStatistics
import qualified Amazonka.Prelude as Prelude

-- | The event type details.
--
-- /See:/ 'newEventType' smart constructor.
data EventType = EventType'
  { -- | The event type name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the event type was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The event type entity types.
    entityTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The entity type ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The event type description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the event type was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | Data about the stored events.
    ingestedEventStatistics :: Prelude.Maybe IngestedEventStatistics,
    -- | The event type labels.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | If @Enabled@, Amazon Fraud Detector stores event data when you generate
    -- a prediction and uses that data to update calculated variables in near
    -- real-time. Amazon Fraud Detector uses this data, known as
    -- @INGESTED_EVENTS@, to train your model and improve fraud predictions.
    eventIngestion :: Prelude.Maybe EventIngestion,
    -- | The event type event variables.
    eventVariables :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eventType_name' - The event type name.
--
-- 'createdTime', 'eventType_createdTime' - Timestamp of when the event type was created.
--
-- 'entityTypes', 'eventType_entityTypes' - The event type entity types.
--
-- 'arn', 'eventType_arn' - The entity type ARN.
--
-- 'description', 'eventType_description' - The event type description.
--
-- 'lastUpdatedTime', 'eventType_lastUpdatedTime' - Timestamp of when the event type was last updated.
--
-- 'ingestedEventStatistics', 'eventType_ingestedEventStatistics' - Data about the stored events.
--
-- 'labels', 'eventType_labels' - The event type labels.
--
-- 'eventIngestion', 'eventType_eventIngestion' - If @Enabled@, Amazon Fraud Detector stores event data when you generate
-- a prediction and uses that data to update calculated variables in near
-- real-time. Amazon Fraud Detector uses this data, known as
-- @INGESTED_EVENTS@, to train your model and improve fraud predictions.
--
-- 'eventVariables', 'eventType_eventVariables' - The event type event variables.
newEventType ::
  EventType
newEventType =
  EventType'
    { name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      entityTypes = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      ingestedEventStatistics = Prelude.Nothing,
      labels = Prelude.Nothing,
      eventIngestion = Prelude.Nothing,
      eventVariables = Prelude.Nothing
    }

-- | The event type name.
eventType_name :: Lens.Lens' EventType (Prelude.Maybe Prelude.Text)
eventType_name = Lens.lens (\EventType' {name} -> name) (\s@EventType' {} a -> s {name = a} :: EventType)

-- | Timestamp of when the event type was created.
eventType_createdTime :: Lens.Lens' EventType (Prelude.Maybe Prelude.Text)
eventType_createdTime = Lens.lens (\EventType' {createdTime} -> createdTime) (\s@EventType' {} a -> s {createdTime = a} :: EventType)

-- | The event type entity types.
eventType_entityTypes :: Lens.Lens' EventType (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventType_entityTypes = Lens.lens (\EventType' {entityTypes} -> entityTypes) (\s@EventType' {} a -> s {entityTypes = a} :: EventType) Prelude.. Lens.mapping Lens.coerced

-- | The entity type ARN.
eventType_arn :: Lens.Lens' EventType (Prelude.Maybe Prelude.Text)
eventType_arn = Lens.lens (\EventType' {arn} -> arn) (\s@EventType' {} a -> s {arn = a} :: EventType)

-- | The event type description.
eventType_description :: Lens.Lens' EventType (Prelude.Maybe Prelude.Text)
eventType_description = Lens.lens (\EventType' {description} -> description) (\s@EventType' {} a -> s {description = a} :: EventType)

-- | Timestamp of when the event type was last updated.
eventType_lastUpdatedTime :: Lens.Lens' EventType (Prelude.Maybe Prelude.Text)
eventType_lastUpdatedTime = Lens.lens (\EventType' {lastUpdatedTime} -> lastUpdatedTime) (\s@EventType' {} a -> s {lastUpdatedTime = a} :: EventType)

-- | Data about the stored events.
eventType_ingestedEventStatistics :: Lens.Lens' EventType (Prelude.Maybe IngestedEventStatistics)
eventType_ingestedEventStatistics = Lens.lens (\EventType' {ingestedEventStatistics} -> ingestedEventStatistics) (\s@EventType' {} a -> s {ingestedEventStatistics = a} :: EventType)

-- | The event type labels.
eventType_labels :: Lens.Lens' EventType (Prelude.Maybe [Prelude.Text])
eventType_labels = Lens.lens (\EventType' {labels} -> labels) (\s@EventType' {} a -> s {labels = a} :: EventType) Prelude.. Lens.mapping Lens.coerced

-- | If @Enabled@, Amazon Fraud Detector stores event data when you generate
-- a prediction and uses that data to update calculated variables in near
-- real-time. Amazon Fraud Detector uses this data, known as
-- @INGESTED_EVENTS@, to train your model and improve fraud predictions.
eventType_eventIngestion :: Lens.Lens' EventType (Prelude.Maybe EventIngestion)
eventType_eventIngestion = Lens.lens (\EventType' {eventIngestion} -> eventIngestion) (\s@EventType' {} a -> s {eventIngestion = a} :: EventType)

-- | The event type event variables.
eventType_eventVariables :: Lens.Lens' EventType (Prelude.Maybe [Prelude.Text])
eventType_eventVariables = Lens.lens (\EventType' {eventVariables} -> eventVariables) (\s@EventType' {} a -> s {eventVariables = a} :: EventType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EventType where
  parseJSON =
    Data.withObject
      "EventType"
      ( \x ->
          EventType'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "entityTypes")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "ingestedEventStatistics")
            Prelude.<*> (x Data..:? "labels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "eventIngestion")
            Prelude.<*> ( x Data..:? "eventVariables"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EventType where
  hashWithSalt _salt EventType' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` entityTypes
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` ingestedEventStatistics
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` eventIngestion
      `Prelude.hashWithSalt` eventVariables

instance Prelude.NFData EventType where
  rnf EventType' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf entityTypes
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf ingestedEventStatistics
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf eventIngestion
      `Prelude.seq` Prelude.rnf eventVariables
