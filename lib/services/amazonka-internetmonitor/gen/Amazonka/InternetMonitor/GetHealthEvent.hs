{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.InternetMonitor.GetHealthEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information the Amazon CloudWatch Internet Monitor has created and
-- stored about a health event for a specified monitor. This information
-- includes the impacted locations, and all of the information related to
-- the event by location.
--
-- The information returned includes the performance, availability, and
-- round-trip time impact, information about the network providers, the
-- event type, and so on.
--
-- Information rolled up at the global traffic level is also returned,
-- including the impact type and total traffic impact.
module Amazonka.InternetMonitor.GetHealthEvent
  ( -- * Creating a Request
    GetHealthEvent (..),
    newGetHealthEvent,

    -- * Request Lenses
    getHealthEvent_monitorName,
    getHealthEvent_eventId,

    -- * Destructuring the Response
    GetHealthEventResponse (..),
    newGetHealthEventResponse,

    -- * Response Lenses
    getHealthEventResponse_createdAt,
    getHealthEventResponse_endedAt,
    getHealthEventResponse_percentOfTotalTrafficImpacted,
    getHealthEventResponse_httpStatus,
    getHealthEventResponse_eventArn,
    getHealthEventResponse_eventId,
    getHealthEventResponse_startedAt,
    getHealthEventResponse_lastUpdatedAt,
    getHealthEventResponse_impactedLocations,
    getHealthEventResponse_status,
    getHealthEventResponse_impactType,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHealthEvent' smart constructor.
data GetHealthEvent = GetHealthEvent'
  { -- | The name of the monitor.
    monitorName :: Prelude.Text,
    -- | The internally generated identifier of a health event. Because @EventID@
    -- contains the forward slash (“\/”) character, you must URL-encode the
    -- @EventID@ field in the request URL.
    eventId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHealthEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitorName', 'getHealthEvent_monitorName' - The name of the monitor.
--
-- 'eventId', 'getHealthEvent_eventId' - The internally generated identifier of a health event. Because @EventID@
-- contains the forward slash (“\/”) character, you must URL-encode the
-- @EventID@ field in the request URL.
newGetHealthEvent ::
  -- | 'monitorName'
  Prelude.Text ->
  -- | 'eventId'
  Prelude.Text ->
  GetHealthEvent
newGetHealthEvent pMonitorName_ pEventId_ =
  GetHealthEvent'
    { monitorName = pMonitorName_,
      eventId = pEventId_
    }

-- | The name of the monitor.
getHealthEvent_monitorName :: Lens.Lens' GetHealthEvent Prelude.Text
getHealthEvent_monitorName = Lens.lens (\GetHealthEvent' {monitorName} -> monitorName) (\s@GetHealthEvent' {} a -> s {monitorName = a} :: GetHealthEvent)

-- | The internally generated identifier of a health event. Because @EventID@
-- contains the forward slash (“\/”) character, you must URL-encode the
-- @EventID@ field in the request URL.
getHealthEvent_eventId :: Lens.Lens' GetHealthEvent Prelude.Text
getHealthEvent_eventId = Lens.lens (\GetHealthEvent' {eventId} -> eventId) (\s@GetHealthEvent' {} a -> s {eventId = a} :: GetHealthEvent)

instance Core.AWSRequest GetHealthEvent where
  type
    AWSResponse GetHealthEvent =
      GetHealthEventResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHealthEventResponse'
            Prelude.<$> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "EndedAt")
            Prelude.<*> (x Data..?> "PercentOfTotalTrafficImpacted")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EventArn")
            Prelude.<*> (x Data..:> "EventId")
            Prelude.<*> (x Data..:> "StartedAt")
            Prelude.<*> (x Data..:> "LastUpdatedAt")
            Prelude.<*> ( x
                            Data..?> "ImpactedLocations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "Status")
            Prelude.<*> (x Data..:> "ImpactType")
      )

instance Prelude.Hashable GetHealthEvent where
  hashWithSalt _salt GetHealthEvent' {..} =
    _salt
      `Prelude.hashWithSalt` monitorName
      `Prelude.hashWithSalt` eventId

instance Prelude.NFData GetHealthEvent where
  rnf GetHealthEvent' {..} =
    Prelude.rnf monitorName
      `Prelude.seq` Prelude.rnf eventId

instance Data.ToHeaders GetHealthEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetHealthEvent where
  toPath GetHealthEvent' {..} =
    Prelude.mconcat
      [ "/v20210603/Monitors/",
        Data.toBS monitorName,
        "/HealthEvents/",
        Data.toBS eventId
      ]

instance Data.ToQuery GetHealthEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetHealthEventResponse' smart constructor.
data GetHealthEventResponse = GetHealthEventResponse'
  { -- | The time when a health event was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The time when a health event was resolved. If the health event is still
    -- active, the end time is not set.
    endedAt :: Prelude.Maybe Data.ISO8601,
    -- | The impact on total traffic that a health event has.
    percentOfTotalTrafficImpacted :: Prelude.Maybe Prelude.Double,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the event.
    eventArn :: Prelude.Text,
    -- | The internally generated identifier of a health event.
    eventId :: Prelude.Text,
    -- | The time when a health event started.
    startedAt :: Data.ISO8601,
    -- | The time when a health event was last updated or recalculated.
    lastUpdatedAt :: Data.ISO8601,
    -- | The locations affected by a health event.
    impactedLocations :: [ImpactedLocation],
    -- | The status of a health event.
    status :: HealthEventStatus,
    -- | The type of impairment of a specific health event.
    impactType :: HealthEventImpactType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHealthEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'getHealthEventResponse_createdAt' - The time when a health event was created.
--
-- 'endedAt', 'getHealthEventResponse_endedAt' - The time when a health event was resolved. If the health event is still
-- active, the end time is not set.
--
-- 'percentOfTotalTrafficImpacted', 'getHealthEventResponse_percentOfTotalTrafficImpacted' - The impact on total traffic that a health event has.
--
-- 'httpStatus', 'getHealthEventResponse_httpStatus' - The response's http status code.
--
-- 'eventArn', 'getHealthEventResponse_eventArn' - The Amazon Resource Name (ARN) of the event.
--
-- 'eventId', 'getHealthEventResponse_eventId' - The internally generated identifier of a health event.
--
-- 'startedAt', 'getHealthEventResponse_startedAt' - The time when a health event started.
--
-- 'lastUpdatedAt', 'getHealthEventResponse_lastUpdatedAt' - The time when a health event was last updated or recalculated.
--
-- 'impactedLocations', 'getHealthEventResponse_impactedLocations' - The locations affected by a health event.
--
-- 'status', 'getHealthEventResponse_status' - The status of a health event.
--
-- 'impactType', 'getHealthEventResponse_impactType' - The type of impairment of a specific health event.
newGetHealthEventResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
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
  GetHealthEventResponse
newGetHealthEventResponse
  pHttpStatus_
  pEventArn_
  pEventId_
  pStartedAt_
  pLastUpdatedAt_
  pStatus_
  pImpactType_ =
    GetHealthEventResponse'
      { createdAt =
          Prelude.Nothing,
        endedAt = Prelude.Nothing,
        percentOfTotalTrafficImpacted = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        eventArn = pEventArn_,
        eventId = pEventId_,
        startedAt = Data._Time Lens.# pStartedAt_,
        lastUpdatedAt = Data._Time Lens.# pLastUpdatedAt_,
        impactedLocations = Prelude.mempty,
        status = pStatus_,
        impactType = pImpactType_
      }

-- | The time when a health event was created.
getHealthEventResponse_createdAt :: Lens.Lens' GetHealthEventResponse (Prelude.Maybe Prelude.UTCTime)
getHealthEventResponse_createdAt = Lens.lens (\GetHealthEventResponse' {createdAt} -> createdAt) (\s@GetHealthEventResponse' {} a -> s {createdAt = a} :: GetHealthEventResponse) Prelude.. Lens.mapping Data._Time

-- | The time when a health event was resolved. If the health event is still
-- active, the end time is not set.
getHealthEventResponse_endedAt :: Lens.Lens' GetHealthEventResponse (Prelude.Maybe Prelude.UTCTime)
getHealthEventResponse_endedAt = Lens.lens (\GetHealthEventResponse' {endedAt} -> endedAt) (\s@GetHealthEventResponse' {} a -> s {endedAt = a} :: GetHealthEventResponse) Prelude.. Lens.mapping Data._Time

-- | The impact on total traffic that a health event has.
getHealthEventResponse_percentOfTotalTrafficImpacted :: Lens.Lens' GetHealthEventResponse (Prelude.Maybe Prelude.Double)
getHealthEventResponse_percentOfTotalTrafficImpacted = Lens.lens (\GetHealthEventResponse' {percentOfTotalTrafficImpacted} -> percentOfTotalTrafficImpacted) (\s@GetHealthEventResponse' {} a -> s {percentOfTotalTrafficImpacted = a} :: GetHealthEventResponse)

-- | The response's http status code.
getHealthEventResponse_httpStatus :: Lens.Lens' GetHealthEventResponse Prelude.Int
getHealthEventResponse_httpStatus = Lens.lens (\GetHealthEventResponse' {httpStatus} -> httpStatus) (\s@GetHealthEventResponse' {} a -> s {httpStatus = a} :: GetHealthEventResponse)

-- | The Amazon Resource Name (ARN) of the event.
getHealthEventResponse_eventArn :: Lens.Lens' GetHealthEventResponse Prelude.Text
getHealthEventResponse_eventArn = Lens.lens (\GetHealthEventResponse' {eventArn} -> eventArn) (\s@GetHealthEventResponse' {} a -> s {eventArn = a} :: GetHealthEventResponse)

-- | The internally generated identifier of a health event.
getHealthEventResponse_eventId :: Lens.Lens' GetHealthEventResponse Prelude.Text
getHealthEventResponse_eventId = Lens.lens (\GetHealthEventResponse' {eventId} -> eventId) (\s@GetHealthEventResponse' {} a -> s {eventId = a} :: GetHealthEventResponse)

-- | The time when a health event started.
getHealthEventResponse_startedAt :: Lens.Lens' GetHealthEventResponse Prelude.UTCTime
getHealthEventResponse_startedAt = Lens.lens (\GetHealthEventResponse' {startedAt} -> startedAt) (\s@GetHealthEventResponse' {} a -> s {startedAt = a} :: GetHealthEventResponse) Prelude.. Data._Time

-- | The time when a health event was last updated or recalculated.
getHealthEventResponse_lastUpdatedAt :: Lens.Lens' GetHealthEventResponse Prelude.UTCTime
getHealthEventResponse_lastUpdatedAt = Lens.lens (\GetHealthEventResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetHealthEventResponse' {} a -> s {lastUpdatedAt = a} :: GetHealthEventResponse) Prelude.. Data._Time

-- | The locations affected by a health event.
getHealthEventResponse_impactedLocations :: Lens.Lens' GetHealthEventResponse [ImpactedLocation]
getHealthEventResponse_impactedLocations = Lens.lens (\GetHealthEventResponse' {impactedLocations} -> impactedLocations) (\s@GetHealthEventResponse' {} a -> s {impactedLocations = a} :: GetHealthEventResponse) Prelude.. Lens.coerced

-- | The status of a health event.
getHealthEventResponse_status :: Lens.Lens' GetHealthEventResponse HealthEventStatus
getHealthEventResponse_status = Lens.lens (\GetHealthEventResponse' {status} -> status) (\s@GetHealthEventResponse' {} a -> s {status = a} :: GetHealthEventResponse)

-- | The type of impairment of a specific health event.
getHealthEventResponse_impactType :: Lens.Lens' GetHealthEventResponse HealthEventImpactType
getHealthEventResponse_impactType = Lens.lens (\GetHealthEventResponse' {impactType} -> impactType) (\s@GetHealthEventResponse' {} a -> s {impactType = a} :: GetHealthEventResponse)

instance Prelude.NFData GetHealthEventResponse where
  rnf GetHealthEventResponse' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf endedAt
      `Prelude.seq` Prelude.rnf percentOfTotalTrafficImpacted
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf eventArn
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf impactedLocations
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf impactType
