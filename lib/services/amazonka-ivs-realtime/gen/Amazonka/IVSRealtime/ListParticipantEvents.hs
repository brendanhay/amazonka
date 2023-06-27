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
-- Module      : Amazonka.IVSRealtime.ListParticipantEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists events for a specified participant that occurred during a
-- specified stage session.
module Amazonka.IVSRealtime.ListParticipantEvents
  ( -- * Creating a Request
    ListParticipantEvents (..),
    newListParticipantEvents,

    -- * Request Lenses
    listParticipantEvents_maxResults,
    listParticipantEvents_nextToken,
    listParticipantEvents_participantId,
    listParticipantEvents_sessionId,
    listParticipantEvents_stageArn,

    -- * Destructuring the Response
    ListParticipantEventsResponse (..),
    newListParticipantEventsResponse,

    -- * Response Lenses
    listParticipantEventsResponse_nextToken,
    listParticipantEventsResponse_httpStatus,
    listParticipantEventsResponse_events,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListParticipantEvents' smart constructor.
data ListParticipantEvents = ListParticipantEvents'
  { -- | Maximum number of results to return. Default: 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The first participant to retrieve. This is used for pagination; see the
    -- @nextToken@ response field.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier for this participant. This is assigned by IVS and
    -- returned by CreateParticipantToken.
    participantId :: Prelude.Text,
    -- | ID of a session within the stage.
    sessionId :: Prelude.Text,
    -- | Stage ARN.
    stageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListParticipantEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listParticipantEvents_maxResults' - Maximum number of results to return. Default: 50.
--
-- 'nextToken', 'listParticipantEvents_nextToken' - The first participant to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
--
-- 'participantId', 'listParticipantEvents_participantId' - Unique identifier for this participant. This is assigned by IVS and
-- returned by CreateParticipantToken.
--
-- 'sessionId', 'listParticipantEvents_sessionId' - ID of a session within the stage.
--
-- 'stageArn', 'listParticipantEvents_stageArn' - Stage ARN.
newListParticipantEvents ::
  -- | 'participantId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'stageArn'
  Prelude.Text ->
  ListParticipantEvents
newListParticipantEvents
  pParticipantId_
  pSessionId_
  pStageArn_ =
    ListParticipantEvents'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        participantId = pParticipantId_,
        sessionId = pSessionId_,
        stageArn = pStageArn_
      }

-- | Maximum number of results to return. Default: 50.
listParticipantEvents_maxResults :: Lens.Lens' ListParticipantEvents (Prelude.Maybe Prelude.Natural)
listParticipantEvents_maxResults = Lens.lens (\ListParticipantEvents' {maxResults} -> maxResults) (\s@ListParticipantEvents' {} a -> s {maxResults = a} :: ListParticipantEvents)

-- | The first participant to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
listParticipantEvents_nextToken :: Lens.Lens' ListParticipantEvents (Prelude.Maybe Prelude.Text)
listParticipantEvents_nextToken = Lens.lens (\ListParticipantEvents' {nextToken} -> nextToken) (\s@ListParticipantEvents' {} a -> s {nextToken = a} :: ListParticipantEvents)

-- | Unique identifier for this participant. This is assigned by IVS and
-- returned by CreateParticipantToken.
listParticipantEvents_participantId :: Lens.Lens' ListParticipantEvents Prelude.Text
listParticipantEvents_participantId = Lens.lens (\ListParticipantEvents' {participantId} -> participantId) (\s@ListParticipantEvents' {} a -> s {participantId = a} :: ListParticipantEvents)

-- | ID of a session within the stage.
listParticipantEvents_sessionId :: Lens.Lens' ListParticipantEvents Prelude.Text
listParticipantEvents_sessionId = Lens.lens (\ListParticipantEvents' {sessionId} -> sessionId) (\s@ListParticipantEvents' {} a -> s {sessionId = a} :: ListParticipantEvents)

-- | Stage ARN.
listParticipantEvents_stageArn :: Lens.Lens' ListParticipantEvents Prelude.Text
listParticipantEvents_stageArn = Lens.lens (\ListParticipantEvents' {stageArn} -> stageArn) (\s@ListParticipantEvents' {} a -> s {stageArn = a} :: ListParticipantEvents)

instance Core.AWSRequest ListParticipantEvents where
  type
    AWSResponse ListParticipantEvents =
      ListParticipantEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListParticipantEventsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "events" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListParticipantEvents where
  hashWithSalt _salt ListParticipantEvents' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` participantId
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` stageArn

instance Prelude.NFData ListParticipantEvents where
  rnf ListParticipantEvents' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf stageArn

instance Data.ToHeaders ListParticipantEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListParticipantEvents where
  toJSON ListParticipantEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("participantId" Data..= participantId),
            Prelude.Just ("sessionId" Data..= sessionId),
            Prelude.Just ("stageArn" Data..= stageArn)
          ]
      )

instance Data.ToPath ListParticipantEvents where
  toPath = Prelude.const "/ListParticipantEvents"

instance Data.ToQuery ListParticipantEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListParticipantEventsResponse' smart constructor.
data ListParticipantEventsResponse = ListParticipantEventsResponse'
  { -- | If there are more rooms than @maxResults@, use @nextToken@ in the
    -- request to get the next set.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of the matching events.
    events :: [Event]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListParticipantEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listParticipantEventsResponse_nextToken' - If there are more rooms than @maxResults@, use @nextToken@ in the
-- request to get the next set.
--
-- 'httpStatus', 'listParticipantEventsResponse_httpStatus' - The response's http status code.
--
-- 'events', 'listParticipantEventsResponse_events' - List of the matching events.
newListParticipantEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListParticipantEventsResponse
newListParticipantEventsResponse pHttpStatus_ =
  ListParticipantEventsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      events = Prelude.mempty
    }

-- | If there are more rooms than @maxResults@, use @nextToken@ in the
-- request to get the next set.
listParticipantEventsResponse_nextToken :: Lens.Lens' ListParticipantEventsResponse (Prelude.Maybe Prelude.Text)
listParticipantEventsResponse_nextToken = Lens.lens (\ListParticipantEventsResponse' {nextToken} -> nextToken) (\s@ListParticipantEventsResponse' {} a -> s {nextToken = a} :: ListParticipantEventsResponse)

-- | The response's http status code.
listParticipantEventsResponse_httpStatus :: Lens.Lens' ListParticipantEventsResponse Prelude.Int
listParticipantEventsResponse_httpStatus = Lens.lens (\ListParticipantEventsResponse' {httpStatus} -> httpStatus) (\s@ListParticipantEventsResponse' {} a -> s {httpStatus = a} :: ListParticipantEventsResponse)

-- | List of the matching events.
listParticipantEventsResponse_events :: Lens.Lens' ListParticipantEventsResponse [Event]
listParticipantEventsResponse_events = Lens.lens (\ListParticipantEventsResponse' {events} -> events) (\s@ListParticipantEventsResponse' {} a -> s {events = a} :: ListParticipantEventsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListParticipantEventsResponse where
  rnf ListParticipantEventsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf events
