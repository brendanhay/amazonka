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
-- Module      : Amazonka.IVSRealtime.ListParticipants
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all participants in a specified stage session.
module Amazonka.IVSRealtime.ListParticipants
  ( -- * Creating a Request
    ListParticipants (..),
    newListParticipants,

    -- * Request Lenses
    listParticipants_filterByPublished,
    listParticipants_filterByState,
    listParticipants_filterByUserId,
    listParticipants_maxResults,
    listParticipants_nextToken,
    listParticipants_sessionId,
    listParticipants_stageArn,

    -- * Destructuring the Response
    ListParticipantsResponse (..),
    newListParticipantsResponse,

    -- * Response Lenses
    listParticipantsResponse_nextToken,
    listParticipantsResponse_httpStatus,
    listParticipantsResponse_participants,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListParticipants' smart constructor.
data ListParticipants = ListParticipants'
  { -- | Filters the response list to only show participants who published during
    -- the stage session. Only one of @filterByUserId@, @filterByPublished@, or
    -- @filterByState@ can be provided per request.
    filterByPublished :: Prelude.Maybe Prelude.Bool,
    -- | Filters the response list to only show participants in the specified
    -- state. Only one of @filterByUserId@, @filterByPublished@, or
    -- @filterByState@ can be provided per request.
    filterByState :: Prelude.Maybe ParticipantState,
    -- | Filters the response list to match the specified user ID. Only one of
    -- @filterByUserId@, @filterByPublished@, or @filterByState@ can be
    -- provided per request. A @userId@ is a customer-assigned name to help
    -- identify the token; this can be used to link a participant to a user in
    -- the customer’s own systems.
    filterByUserId :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to return. Default: 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The first participant to retrieve. This is used for pagination; see the
    -- @nextToken@ response field.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | ID of the session within the stage.
    sessionId :: Prelude.Text,
    -- | Stage ARN.
    stageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListParticipants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterByPublished', 'listParticipants_filterByPublished' - Filters the response list to only show participants who published during
-- the stage session. Only one of @filterByUserId@, @filterByPublished@, or
-- @filterByState@ can be provided per request.
--
-- 'filterByState', 'listParticipants_filterByState' - Filters the response list to only show participants in the specified
-- state. Only one of @filterByUserId@, @filterByPublished@, or
-- @filterByState@ can be provided per request.
--
-- 'filterByUserId', 'listParticipants_filterByUserId' - Filters the response list to match the specified user ID. Only one of
-- @filterByUserId@, @filterByPublished@, or @filterByState@ can be
-- provided per request. A @userId@ is a customer-assigned name to help
-- identify the token; this can be used to link a participant to a user in
-- the customer’s own systems.
--
-- 'maxResults', 'listParticipants_maxResults' - Maximum number of results to return. Default: 50.
--
-- 'nextToken', 'listParticipants_nextToken' - The first participant to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
--
-- 'sessionId', 'listParticipants_sessionId' - ID of the session within the stage.
--
-- 'stageArn', 'listParticipants_stageArn' - Stage ARN.
newListParticipants ::
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'stageArn'
  Prelude.Text ->
  ListParticipants
newListParticipants pSessionId_ pStageArn_ =
  ListParticipants'
    { filterByPublished =
        Prelude.Nothing,
      filterByState = Prelude.Nothing,
      filterByUserId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sessionId = pSessionId_,
      stageArn = pStageArn_
    }

-- | Filters the response list to only show participants who published during
-- the stage session. Only one of @filterByUserId@, @filterByPublished@, or
-- @filterByState@ can be provided per request.
listParticipants_filterByPublished :: Lens.Lens' ListParticipants (Prelude.Maybe Prelude.Bool)
listParticipants_filterByPublished = Lens.lens (\ListParticipants' {filterByPublished} -> filterByPublished) (\s@ListParticipants' {} a -> s {filterByPublished = a} :: ListParticipants)

-- | Filters the response list to only show participants in the specified
-- state. Only one of @filterByUserId@, @filterByPublished@, or
-- @filterByState@ can be provided per request.
listParticipants_filterByState :: Lens.Lens' ListParticipants (Prelude.Maybe ParticipantState)
listParticipants_filterByState = Lens.lens (\ListParticipants' {filterByState} -> filterByState) (\s@ListParticipants' {} a -> s {filterByState = a} :: ListParticipants)

-- | Filters the response list to match the specified user ID. Only one of
-- @filterByUserId@, @filterByPublished@, or @filterByState@ can be
-- provided per request. A @userId@ is a customer-assigned name to help
-- identify the token; this can be used to link a participant to a user in
-- the customer’s own systems.
listParticipants_filterByUserId :: Lens.Lens' ListParticipants (Prelude.Maybe Prelude.Text)
listParticipants_filterByUserId = Lens.lens (\ListParticipants' {filterByUserId} -> filterByUserId) (\s@ListParticipants' {} a -> s {filterByUserId = a} :: ListParticipants)

-- | Maximum number of results to return. Default: 50.
listParticipants_maxResults :: Lens.Lens' ListParticipants (Prelude.Maybe Prelude.Natural)
listParticipants_maxResults = Lens.lens (\ListParticipants' {maxResults} -> maxResults) (\s@ListParticipants' {} a -> s {maxResults = a} :: ListParticipants)

-- | The first participant to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
listParticipants_nextToken :: Lens.Lens' ListParticipants (Prelude.Maybe Prelude.Text)
listParticipants_nextToken = Lens.lens (\ListParticipants' {nextToken} -> nextToken) (\s@ListParticipants' {} a -> s {nextToken = a} :: ListParticipants)

-- | ID of the session within the stage.
listParticipants_sessionId :: Lens.Lens' ListParticipants Prelude.Text
listParticipants_sessionId = Lens.lens (\ListParticipants' {sessionId} -> sessionId) (\s@ListParticipants' {} a -> s {sessionId = a} :: ListParticipants)

-- | Stage ARN.
listParticipants_stageArn :: Lens.Lens' ListParticipants Prelude.Text
listParticipants_stageArn = Lens.lens (\ListParticipants' {stageArn} -> stageArn) (\s@ListParticipants' {} a -> s {stageArn = a} :: ListParticipants)

instance Core.AWSRequest ListParticipants where
  type
    AWSResponse ListParticipants =
      ListParticipantsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListParticipantsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "participants" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListParticipants where
  hashWithSalt _salt ListParticipants' {..} =
    _salt
      `Prelude.hashWithSalt` filterByPublished
      `Prelude.hashWithSalt` filterByState
      `Prelude.hashWithSalt` filterByUserId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` stageArn

instance Prelude.NFData ListParticipants where
  rnf ListParticipants' {..} =
    Prelude.rnf filterByPublished
      `Prelude.seq` Prelude.rnf filterByState
      `Prelude.seq` Prelude.rnf filterByUserId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf stageArn

instance Data.ToHeaders ListParticipants where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListParticipants where
  toJSON ListParticipants' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterByPublished" Data..=)
              Prelude.<$> filterByPublished,
            ("filterByState" Data..=) Prelude.<$> filterByState,
            ("filterByUserId" Data..=)
              Prelude.<$> filterByUserId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("sessionId" Data..= sessionId),
            Prelude.Just ("stageArn" Data..= stageArn)
          ]
      )

instance Data.ToPath ListParticipants where
  toPath = Prelude.const "/ListParticipants"

instance Data.ToQuery ListParticipants where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListParticipantsResponse' smart constructor.
data ListParticipantsResponse = ListParticipantsResponse'
  { -- | If there are more rooms than @maxResults@, use @nextToken@ in the
    -- request to get the next set.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of the matching participants (summary information only).
    participants :: [ParticipantSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListParticipantsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listParticipantsResponse_nextToken' - If there are more rooms than @maxResults@, use @nextToken@ in the
-- request to get the next set.
--
-- 'httpStatus', 'listParticipantsResponse_httpStatus' - The response's http status code.
--
-- 'participants', 'listParticipantsResponse_participants' - List of the matching participants (summary information only).
newListParticipantsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListParticipantsResponse
newListParticipantsResponse pHttpStatus_ =
  ListParticipantsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      participants = Prelude.mempty
    }

-- | If there are more rooms than @maxResults@, use @nextToken@ in the
-- request to get the next set.
listParticipantsResponse_nextToken :: Lens.Lens' ListParticipantsResponse (Prelude.Maybe Prelude.Text)
listParticipantsResponse_nextToken = Lens.lens (\ListParticipantsResponse' {nextToken} -> nextToken) (\s@ListParticipantsResponse' {} a -> s {nextToken = a} :: ListParticipantsResponse)

-- | The response's http status code.
listParticipantsResponse_httpStatus :: Lens.Lens' ListParticipantsResponse Prelude.Int
listParticipantsResponse_httpStatus = Lens.lens (\ListParticipantsResponse' {httpStatus} -> httpStatus) (\s@ListParticipantsResponse' {} a -> s {httpStatus = a} :: ListParticipantsResponse)

-- | List of the matching participants (summary information only).
listParticipantsResponse_participants :: Lens.Lens' ListParticipantsResponse [ParticipantSummary]
listParticipantsResponse_participants = Lens.lens (\ListParticipantsResponse' {participants} -> participants) (\s@ListParticipantsResponse' {} a -> s {participants = a} :: ListParticipantsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListParticipantsResponse where
  rnf ListParticipantsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf participants
