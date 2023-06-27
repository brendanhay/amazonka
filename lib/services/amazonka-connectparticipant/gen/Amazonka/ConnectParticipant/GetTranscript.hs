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
-- Module      : Amazonka.ConnectParticipant.GetTranscript
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a transcript of the session, including details about any
-- attachments. For information about accessing past chat contact
-- transcripts for a persistent chat, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/chat-persistence.html Enable persistent chat>.
--
-- @ConnectionToken@ is used for invoking this API instead of
-- @ParticipantToken@.
--
-- The Amazon Connect Participant Service APIs do not use
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 authentication>.
module Amazonka.ConnectParticipant.GetTranscript
  ( -- * Creating a Request
    GetTranscript (..),
    newGetTranscript,

    -- * Request Lenses
    getTranscript_contactId,
    getTranscript_maxResults,
    getTranscript_nextToken,
    getTranscript_scanDirection,
    getTranscript_sortOrder,
    getTranscript_startPosition,
    getTranscript_connectionToken,

    -- * Destructuring the Response
    GetTranscriptResponse (..),
    newGetTranscriptResponse,

    -- * Response Lenses
    getTranscriptResponse_initialContactId,
    getTranscriptResponse_nextToken,
    getTranscriptResponse_transcript,
    getTranscriptResponse_httpStatus,
  )
where

import Amazonka.ConnectParticipant.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTranscript' smart constructor.
data GetTranscript = GetTranscript'
  { -- | The contactId from the current contact chain for which transcript is
    -- needed.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the page. Default: 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token. Use the value returned previously in the next
    -- subsequent request to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The direction from StartPosition from which to retrieve message.
    -- Default: BACKWARD when no StartPosition is provided, FORWARD with
    -- StartPosition.
    scanDirection :: Prelude.Maybe ScanDirection,
    -- | The sort order for the records. Default: DESCENDING.
    sortOrder :: Prelude.Maybe SortKey,
    -- | A filtering option for where to start.
    startPosition :: Prelude.Maybe StartPosition,
    -- | The authentication token associated with the participant\'s connection.
    connectionToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTranscript' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'getTranscript_contactId' - The contactId from the current contact chain for which transcript is
-- needed.
--
-- 'maxResults', 'getTranscript_maxResults' - The maximum number of results to return in the page. Default: 10.
--
-- 'nextToken', 'getTranscript_nextToken' - The pagination token. Use the value returned previously in the next
-- subsequent request to retrieve the next set of results.
--
-- 'scanDirection', 'getTranscript_scanDirection' - The direction from StartPosition from which to retrieve message.
-- Default: BACKWARD when no StartPosition is provided, FORWARD with
-- StartPosition.
--
-- 'sortOrder', 'getTranscript_sortOrder' - The sort order for the records. Default: DESCENDING.
--
-- 'startPosition', 'getTranscript_startPosition' - A filtering option for where to start.
--
-- 'connectionToken', 'getTranscript_connectionToken' - The authentication token associated with the participant\'s connection.
newGetTranscript ::
  -- | 'connectionToken'
  Prelude.Text ->
  GetTranscript
newGetTranscript pConnectionToken_ =
  GetTranscript'
    { contactId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      scanDirection = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      startPosition = Prelude.Nothing,
      connectionToken = pConnectionToken_
    }

-- | The contactId from the current contact chain for which transcript is
-- needed.
getTranscript_contactId :: Lens.Lens' GetTranscript (Prelude.Maybe Prelude.Text)
getTranscript_contactId = Lens.lens (\GetTranscript' {contactId} -> contactId) (\s@GetTranscript' {} a -> s {contactId = a} :: GetTranscript)

-- | The maximum number of results to return in the page. Default: 10.
getTranscript_maxResults :: Lens.Lens' GetTranscript (Prelude.Maybe Prelude.Natural)
getTranscript_maxResults = Lens.lens (\GetTranscript' {maxResults} -> maxResults) (\s@GetTranscript' {} a -> s {maxResults = a} :: GetTranscript)

-- | The pagination token. Use the value returned previously in the next
-- subsequent request to retrieve the next set of results.
getTranscript_nextToken :: Lens.Lens' GetTranscript (Prelude.Maybe Prelude.Text)
getTranscript_nextToken = Lens.lens (\GetTranscript' {nextToken} -> nextToken) (\s@GetTranscript' {} a -> s {nextToken = a} :: GetTranscript)

-- | The direction from StartPosition from which to retrieve message.
-- Default: BACKWARD when no StartPosition is provided, FORWARD with
-- StartPosition.
getTranscript_scanDirection :: Lens.Lens' GetTranscript (Prelude.Maybe ScanDirection)
getTranscript_scanDirection = Lens.lens (\GetTranscript' {scanDirection} -> scanDirection) (\s@GetTranscript' {} a -> s {scanDirection = a} :: GetTranscript)

-- | The sort order for the records. Default: DESCENDING.
getTranscript_sortOrder :: Lens.Lens' GetTranscript (Prelude.Maybe SortKey)
getTranscript_sortOrder = Lens.lens (\GetTranscript' {sortOrder} -> sortOrder) (\s@GetTranscript' {} a -> s {sortOrder = a} :: GetTranscript)

-- | A filtering option for where to start.
getTranscript_startPosition :: Lens.Lens' GetTranscript (Prelude.Maybe StartPosition)
getTranscript_startPosition = Lens.lens (\GetTranscript' {startPosition} -> startPosition) (\s@GetTranscript' {} a -> s {startPosition = a} :: GetTranscript)

-- | The authentication token associated with the participant\'s connection.
getTranscript_connectionToken :: Lens.Lens' GetTranscript Prelude.Text
getTranscript_connectionToken = Lens.lens (\GetTranscript' {connectionToken} -> connectionToken) (\s@GetTranscript' {} a -> s {connectionToken = a} :: GetTranscript)

instance Core.AWSRequest GetTranscript where
  type
    AWSResponse GetTranscript =
      GetTranscriptResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTranscriptResponse'
            Prelude.<$> (x Data..?> "InitialContactId")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Transcript" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTranscript where
  hashWithSalt _salt GetTranscript' {..} =
    _salt
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` scanDirection
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` startPosition
      `Prelude.hashWithSalt` connectionToken

instance Prelude.NFData GetTranscript where
  rnf GetTranscript' {..} =
    Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scanDirection
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf startPosition
      `Prelude.seq` Prelude.rnf connectionToken

instance Data.ToHeaders GetTranscript where
  toHeaders GetTranscript' {..} =
    Prelude.mconcat
      [ "X-Amz-Bearer" Data.=# connectionToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON GetTranscript where
  toJSON GetTranscript' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContactId" Data..=) Prelude.<$> contactId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ScanDirection" Data..=) Prelude.<$> scanDirection,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StartPosition" Data..=) Prelude.<$> startPosition
          ]
      )

instance Data.ToPath GetTranscript where
  toPath = Prelude.const "/participant/transcript"

instance Data.ToQuery GetTranscript where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTranscriptResponse' smart constructor.
data GetTranscriptResponse = GetTranscriptResponse'
  { -- | The initial contact ID for the contact.
    initialContactId :: Prelude.Maybe Prelude.Text,
    -- | The pagination token. Use the value returned previously in the next
    -- subsequent request to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of messages in the session.
    transcript :: Prelude.Maybe [Item],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTranscriptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialContactId', 'getTranscriptResponse_initialContactId' - The initial contact ID for the contact.
--
-- 'nextToken', 'getTranscriptResponse_nextToken' - The pagination token. Use the value returned previously in the next
-- subsequent request to retrieve the next set of results.
--
-- 'transcript', 'getTranscriptResponse_transcript' - The list of messages in the session.
--
-- 'httpStatus', 'getTranscriptResponse_httpStatus' - The response's http status code.
newGetTranscriptResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTranscriptResponse
newGetTranscriptResponse pHttpStatus_ =
  GetTranscriptResponse'
    { initialContactId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      transcript = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The initial contact ID for the contact.
getTranscriptResponse_initialContactId :: Lens.Lens' GetTranscriptResponse (Prelude.Maybe Prelude.Text)
getTranscriptResponse_initialContactId = Lens.lens (\GetTranscriptResponse' {initialContactId} -> initialContactId) (\s@GetTranscriptResponse' {} a -> s {initialContactId = a} :: GetTranscriptResponse)

-- | The pagination token. Use the value returned previously in the next
-- subsequent request to retrieve the next set of results.
getTranscriptResponse_nextToken :: Lens.Lens' GetTranscriptResponse (Prelude.Maybe Prelude.Text)
getTranscriptResponse_nextToken = Lens.lens (\GetTranscriptResponse' {nextToken} -> nextToken) (\s@GetTranscriptResponse' {} a -> s {nextToken = a} :: GetTranscriptResponse)

-- | The list of messages in the session.
getTranscriptResponse_transcript :: Lens.Lens' GetTranscriptResponse (Prelude.Maybe [Item])
getTranscriptResponse_transcript = Lens.lens (\GetTranscriptResponse' {transcript} -> transcript) (\s@GetTranscriptResponse' {} a -> s {transcript = a} :: GetTranscriptResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTranscriptResponse_httpStatus :: Lens.Lens' GetTranscriptResponse Prelude.Int
getTranscriptResponse_httpStatus = Lens.lens (\GetTranscriptResponse' {httpStatus} -> httpStatus) (\s@GetTranscriptResponse' {} a -> s {httpStatus = a} :: GetTranscriptResponse)

instance Prelude.NFData GetTranscriptResponse where
  rnf GetTranscriptResponse' {..} =
    Prelude.rnf initialContactId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transcript
      `Prelude.seq` Prelude.rnf httpStatus
