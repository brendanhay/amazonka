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
-- Module      : Amazonka.Evidently.PutProjectEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends performance events to Evidently. These events can be used to
-- evaluate a launch or an experiment.
module Amazonka.Evidently.PutProjectEvents
  ( -- * Creating a Request
    PutProjectEvents (..),
    newPutProjectEvents,

    -- * Request Lenses
    putProjectEvents_events,
    putProjectEvents_project,

    -- * Destructuring the Response
    PutProjectEventsResponse (..),
    newPutProjectEventsResponse,

    -- * Response Lenses
    putProjectEventsResponse_eventResults,
    putProjectEventsResponse_failedEventCount,
    putProjectEventsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutProjectEvents' smart constructor.
data PutProjectEvents = PutProjectEvents'
  { -- | An array of event structures that contain the performance data that is
    -- being sent to Evidently.
    events :: [Event],
    -- | The name or ARN of the project to write the events to.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProjectEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'putProjectEvents_events' - An array of event structures that contain the performance data that is
-- being sent to Evidently.
--
-- 'project', 'putProjectEvents_project' - The name or ARN of the project to write the events to.
newPutProjectEvents ::
  -- | 'project'
  Prelude.Text ->
  PutProjectEvents
newPutProjectEvents pProject_ =
  PutProjectEvents'
    { events = Prelude.mempty,
      project = pProject_
    }

-- | An array of event structures that contain the performance data that is
-- being sent to Evidently.
putProjectEvents_events :: Lens.Lens' PutProjectEvents [Event]
putProjectEvents_events = Lens.lens (\PutProjectEvents' {events} -> events) (\s@PutProjectEvents' {} a -> s {events = a} :: PutProjectEvents) Prelude.. Lens.coerced

-- | The name or ARN of the project to write the events to.
putProjectEvents_project :: Lens.Lens' PutProjectEvents Prelude.Text
putProjectEvents_project = Lens.lens (\PutProjectEvents' {project} -> project) (\s@PutProjectEvents' {} a -> s {project = a} :: PutProjectEvents)

instance Core.AWSRequest PutProjectEvents where
  type
    AWSResponse PutProjectEvents =
      PutProjectEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutProjectEventsResponse'
            Prelude.<$> (x Data..?> "eventResults" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "failedEventCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutProjectEvents where
  hashWithSalt _salt PutProjectEvents' {..} =
    _salt `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` project

instance Prelude.NFData PutProjectEvents where
  rnf PutProjectEvents' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders PutProjectEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutProjectEvents where
  toJSON PutProjectEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("events" Data..= events)]
      )

instance Data.ToPath PutProjectEvents where
  toPath PutProjectEvents' {..} =
    Prelude.mconcat
      ["/events/projects/", Data.toBS project]

instance Data.ToQuery PutProjectEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutProjectEventsResponse' smart constructor.
data PutProjectEventsResponse = PutProjectEventsResponse'
  { -- | A structure that contains Evidently\'s response to the sent events,
    -- including an event ID and error codes, if any.
    eventResults :: Prelude.Maybe [PutProjectEventsResultEntry],
    -- | The number of events in the operation that could not be used by
    -- Evidently.
    failedEventCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProjectEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventResults', 'putProjectEventsResponse_eventResults' - A structure that contains Evidently\'s response to the sent events,
-- including an event ID and error codes, if any.
--
-- 'failedEventCount', 'putProjectEventsResponse_failedEventCount' - The number of events in the operation that could not be used by
-- Evidently.
--
-- 'httpStatus', 'putProjectEventsResponse_httpStatus' - The response's http status code.
newPutProjectEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutProjectEventsResponse
newPutProjectEventsResponse pHttpStatus_ =
  PutProjectEventsResponse'
    { eventResults =
        Prelude.Nothing,
      failedEventCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains Evidently\'s response to the sent events,
-- including an event ID and error codes, if any.
putProjectEventsResponse_eventResults :: Lens.Lens' PutProjectEventsResponse (Prelude.Maybe [PutProjectEventsResultEntry])
putProjectEventsResponse_eventResults = Lens.lens (\PutProjectEventsResponse' {eventResults} -> eventResults) (\s@PutProjectEventsResponse' {} a -> s {eventResults = a} :: PutProjectEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of events in the operation that could not be used by
-- Evidently.
putProjectEventsResponse_failedEventCount :: Lens.Lens' PutProjectEventsResponse (Prelude.Maybe Prelude.Int)
putProjectEventsResponse_failedEventCount = Lens.lens (\PutProjectEventsResponse' {failedEventCount} -> failedEventCount) (\s@PutProjectEventsResponse' {} a -> s {failedEventCount = a} :: PutProjectEventsResponse)

-- | The response's http status code.
putProjectEventsResponse_httpStatus :: Lens.Lens' PutProjectEventsResponse Prelude.Int
putProjectEventsResponse_httpStatus = Lens.lens (\PutProjectEventsResponse' {httpStatus} -> httpStatus) (\s@PutProjectEventsResponse' {} a -> s {httpStatus = a} :: PutProjectEventsResponse)

instance Prelude.NFData PutProjectEventsResponse where
  rnf PutProjectEventsResponse' {..} =
    Prelude.rnf eventResults
      `Prelude.seq` Prelude.rnf failedEventCount
      `Prelude.seq` Prelude.rnf httpStatus
