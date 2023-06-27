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
-- Module      : Amazonka.InternetMonitor.Types.HealthEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.HealthEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types.HealthEventImpactType
import Amazonka.InternetMonitor.Types.HealthEventStatus
import Amazonka.InternetMonitor.Types.ImpactedLocation
import qualified Amazonka.Prelude as Prelude

-- | Information about a health event created in a monitor in Amazon
-- CloudWatch Internet Monitor.
--
-- /See:/ 'newHealthEvent' smart constructor.
data HealthEvent = HealthEvent'
  { -- | When the health event was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The time when a health event ended. If the health event is still active,
    -- then the end time is not set.
    endedAt :: Prelude.Maybe Data.ISO8601,
    -- | The impact on global traffic monitored by this monitor for this health
    -- event.
    percentOfTotalTrafficImpacted :: Prelude.Maybe Prelude.Double,
    -- | The Amazon Resource Name (ARN) of the event.
    eventArn :: Prelude.Text,
    -- | The internally generated identifier of a specific network traffic
    -- impairment health event.
    eventId :: Prelude.Text,
    -- | When a health event started.
    startedAt :: Data.ISO8601,
    -- | When the health event was last updated.
    lastUpdatedAt :: Data.ISO8601,
    -- | The locations impacted by the health event.
    impactedLocations :: [ImpactedLocation],
    -- | Health event list member.
    status :: HealthEventStatus,
    -- | The type of impairment for a health event.
    impactType :: HealthEventImpactType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HealthEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'healthEvent_createdAt' - When the health event was created.
--
-- 'endedAt', 'healthEvent_endedAt' - The time when a health event ended. If the health event is still active,
-- then the end time is not set.
--
-- 'percentOfTotalTrafficImpacted', 'healthEvent_percentOfTotalTrafficImpacted' - The impact on global traffic monitored by this monitor for this health
-- event.
--
-- 'eventArn', 'healthEvent_eventArn' - The Amazon Resource Name (ARN) of the event.
--
-- 'eventId', 'healthEvent_eventId' - The internally generated identifier of a specific network traffic
-- impairment health event.
--
-- 'startedAt', 'healthEvent_startedAt' - When a health event started.
--
-- 'lastUpdatedAt', 'healthEvent_lastUpdatedAt' - When the health event was last updated.
--
-- 'impactedLocations', 'healthEvent_impactedLocations' - The locations impacted by the health event.
--
-- 'status', 'healthEvent_status' - Health event list member.
--
-- 'impactType', 'healthEvent_impactType' - The type of impairment for a health event.
newHealthEvent ::
  -- | 'eventArn'
  Prelude.Text ->
  -- | 'eventId'
  Prelude.Text ->
  -- | 'startedAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  -- | 'status'
  HealthEventStatus ->
  -- | 'impactType'
  HealthEventImpactType ->
  HealthEvent
newHealthEvent
  pEventArn_
  pEventId_
  pStartedAt_
  pLastUpdatedAt_
  pStatus_
  pImpactType_ =
    HealthEvent'
      { createdAt = Prelude.Nothing,
        endedAt = Prelude.Nothing,
        percentOfTotalTrafficImpacted = Prelude.Nothing,
        eventArn = pEventArn_,
        eventId = pEventId_,
        startedAt = Data._Time Lens.# pStartedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_,
        impactedLocations = Prelude.mempty,
        status = pStatus_,
        impactType = pImpactType_
      }

-- | When the health event was created.
healthEvent_createdAt :: Lens.Lens' HealthEvent (Prelude.Maybe Prelude.UTCTime)
healthEvent_createdAt = Lens.lens (\HealthEvent' {createdAt} -> createdAt) (\s@HealthEvent' {} a -> s {createdAt = a} :: HealthEvent) Prelude.. Lens.mapping Data._Time

-- | The time when a health event ended. If the health event is still active,
-- then the end time is not set.
healthEvent_endedAt :: Lens.Lens' HealthEvent (Prelude.Maybe Prelude.UTCTime)
healthEvent_endedAt = Lens.lens (\HealthEvent' {endedAt} -> endedAt) (\s@HealthEvent' {} a -> s {endedAt = a} :: HealthEvent) Prelude.. Lens.mapping Data._Time

-- | The impact on global traffic monitored by this monitor for this health
-- event.
healthEvent_percentOfTotalTrafficImpacted :: Lens.Lens' HealthEvent (Prelude.Maybe Prelude.Double)
healthEvent_percentOfTotalTrafficImpacted = Lens.lens (\HealthEvent' {percentOfTotalTrafficImpacted} -> percentOfTotalTrafficImpacted) (\s@HealthEvent' {} a -> s {percentOfTotalTrafficImpacted = a} :: HealthEvent)

-- | The Amazon Resource Name (ARN) of the event.
healthEvent_eventArn :: Lens.Lens' HealthEvent Prelude.Text
healthEvent_eventArn = Lens.lens (\HealthEvent' {eventArn} -> eventArn) (\s@HealthEvent' {} a -> s {eventArn = a} :: HealthEvent)

-- | The internally generated identifier of a specific network traffic
-- impairment health event.
healthEvent_eventId :: Lens.Lens' HealthEvent Prelude.Text
healthEvent_eventId = Lens.lens (\HealthEvent' {eventId} -> eventId) (\s@HealthEvent' {} a -> s {eventId = a} :: HealthEvent)

-- | When a health event started.
healthEvent_startedAt :: Lens.Lens' HealthEvent Prelude.UTCTime
healthEvent_startedAt = Lens.lens (\HealthEvent' {startedAt} -> startedAt) (\s@HealthEvent' {} a -> s {startedAt = a} :: HealthEvent) Prelude.. Data._Time

-- | When the health event was last updated.
healthEvent_lastUpdatedAt :: Lens.Lens' HealthEvent Prelude.UTCTime
healthEvent_lastUpdatedAt = Lens.lens (\HealthEvent' {lastUpdatedAt} -> lastUpdatedAt) (\s@HealthEvent' {} a -> s {lastUpdatedAt = a} :: HealthEvent) Prelude.. Data._Time

-- | The locations impacted by the health event.
healthEvent_impactedLocations :: Lens.Lens' HealthEvent [ImpactedLocation]
healthEvent_impactedLocations = Lens.lens (\HealthEvent' {impactedLocations} -> impactedLocations) (\s@HealthEvent' {} a -> s {impactedLocations = a} :: HealthEvent) Prelude.. Lens.coerced

-- | Health event list member.
healthEvent_status :: Lens.Lens' HealthEvent HealthEventStatus
healthEvent_status = Lens.lens (\HealthEvent' {status} -> status) (\s@HealthEvent' {} a -> s {status = a} :: HealthEvent)

-- | The type of impairment for a health event.
healthEvent_impactType :: Lens.Lens' HealthEvent HealthEventImpactType
healthEvent_impactType = Lens.lens (\HealthEvent' {impactType} -> impactType) (\s@HealthEvent' {} a -> s {impactType = a} :: HealthEvent)

instance Data.FromJSON HealthEvent where
  parseJSON =
    Data.withObject
      "HealthEvent"
      ( \x ->
          HealthEvent'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "EndedAt")
            Prelude.<*> (x Data..:? "PercentOfTotalTrafficImpacted")
            Prelude.<*> (x Data..: "EventArn")
            Prelude.<*> (x Data..: "EventId")
            Prelude.<*> (x Data..: "StartedAt")
            Prelude.<*> (x Data..: "LastUpdatedAt")
            Prelude.<*> ( x
                            Data..:? "ImpactedLocations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "ImpactType")
      )

instance Prelude.Hashable HealthEvent where
  hashWithSalt _salt HealthEvent' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` endedAt
      `Prelude.hashWithSalt` percentOfTotalTrafficImpacted
      `Prelude.hashWithSalt` eventArn
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` impactedLocations
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` impactType

instance Prelude.NFData HealthEvent where
  rnf HealthEvent' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf endedAt
      `Prelude.seq` Prelude.rnf percentOfTotalTrafficImpacted
      `Prelude.seq` Prelude.rnf eventArn
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf impactedLocations
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf impactType
