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
-- Module      : Amazonka.CustomerProfiles.GetEventStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified event stream in a specific
-- domain.
module Amazonka.CustomerProfiles.GetEventStream
  ( -- * Creating a Request
    GetEventStream (..),
    newGetEventStream,

    -- * Request Lenses
    getEventStream_domainName,
    getEventStream_eventStreamName,

    -- * Destructuring the Response
    GetEventStreamResponse (..),
    newGetEventStreamResponse,

    -- * Response Lenses
    getEventStreamResponse_stoppedSince,
    getEventStreamResponse_tags,
    getEventStreamResponse_httpStatus,
    getEventStreamResponse_domainName,
    getEventStreamResponse_eventStreamArn,
    getEventStreamResponse_createdAt,
    getEventStreamResponse_state,
    getEventStreamResponse_destinationDetails,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventStream' smart constructor.
data GetEventStream = GetEventStream'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The name of the event stream provided during create operations.
    eventStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getEventStream_domainName' - The unique name of the domain.
--
-- 'eventStreamName', 'getEventStream_eventStreamName' - The name of the event stream provided during create operations.
newGetEventStream ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'eventStreamName'
  Prelude.Text ->
  GetEventStream
newGetEventStream pDomainName_ pEventStreamName_ =
  GetEventStream'
    { domainName = pDomainName_,
      eventStreamName = pEventStreamName_
    }

-- | The unique name of the domain.
getEventStream_domainName :: Lens.Lens' GetEventStream Prelude.Text
getEventStream_domainName = Lens.lens (\GetEventStream' {domainName} -> domainName) (\s@GetEventStream' {} a -> s {domainName = a} :: GetEventStream)

-- | The name of the event stream provided during create operations.
getEventStream_eventStreamName :: Lens.Lens' GetEventStream Prelude.Text
getEventStream_eventStreamName = Lens.lens (\GetEventStream' {eventStreamName} -> eventStreamName) (\s@GetEventStream' {} a -> s {eventStreamName = a} :: GetEventStream)

instance Core.AWSRequest GetEventStream where
  type
    AWSResponse GetEventStream =
      GetEventStreamResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventStreamResponse'
            Prelude.<$> (x Data..?> "StoppedSince")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainName")
            Prelude.<*> (x Data..:> "EventStreamArn")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "State")
            Prelude.<*> (x Data..:> "DestinationDetails")
      )

instance Prelude.Hashable GetEventStream where
  hashWithSalt _salt GetEventStream' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` eventStreamName

instance Prelude.NFData GetEventStream where
  rnf GetEventStream' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf eventStreamName

instance Data.ToHeaders GetEventStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEventStream where
  toPath GetEventStream' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/event-streams/",
        Data.toBS eventStreamName
      ]

instance Data.ToQuery GetEventStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventStreamResponse' smart constructor.
data GetEventStreamResponse = GetEventStreamResponse'
  { -- | The timestamp when the @State@ changed to @STOPPED@.
    stoppedSince :: Prelude.Maybe Data.POSIX,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | A unique identifier for the event stream.
    eventStreamArn :: Prelude.Text,
    -- | The timestamp of when the export was created.
    createdAt :: Data.POSIX,
    -- | The operational state of destination stream for export.
    state :: EventStreamState,
    -- | Details regarding the Kinesis stream.
    destinationDetails :: EventStreamDestinationDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stoppedSince', 'getEventStreamResponse_stoppedSince' - The timestamp when the @State@ changed to @STOPPED@.
--
-- 'tags', 'getEventStreamResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'getEventStreamResponse_httpStatus' - The response's http status code.
--
-- 'domainName', 'getEventStreamResponse_domainName' - The unique name of the domain.
--
-- 'eventStreamArn', 'getEventStreamResponse_eventStreamArn' - A unique identifier for the event stream.
--
-- 'createdAt', 'getEventStreamResponse_createdAt' - The timestamp of when the export was created.
--
-- 'state', 'getEventStreamResponse_state' - The operational state of destination stream for export.
--
-- 'destinationDetails', 'getEventStreamResponse_destinationDetails' - Details regarding the Kinesis stream.
newGetEventStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'eventStreamArn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'state'
  EventStreamState ->
  -- | 'destinationDetails'
  EventStreamDestinationDetails ->
  GetEventStreamResponse
newGetEventStreamResponse
  pHttpStatus_
  pDomainName_
  pEventStreamArn_
  pCreatedAt_
  pState_
  pDestinationDetails_ =
    GetEventStreamResponse'
      { stoppedSince =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainName = pDomainName_,
        eventStreamArn = pEventStreamArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        state = pState_,
        destinationDetails = pDestinationDetails_
      }

-- | The timestamp when the @State@ changed to @STOPPED@.
getEventStreamResponse_stoppedSince :: Lens.Lens' GetEventStreamResponse (Prelude.Maybe Prelude.UTCTime)
getEventStreamResponse_stoppedSince = Lens.lens (\GetEventStreamResponse' {stoppedSince} -> stoppedSince) (\s@GetEventStreamResponse' {} a -> s {stoppedSince = a} :: GetEventStreamResponse) Prelude.. Lens.mapping Data._Time

-- | The tags used to organize, track, or control access for this resource.
getEventStreamResponse_tags :: Lens.Lens' GetEventStreamResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getEventStreamResponse_tags = Lens.lens (\GetEventStreamResponse' {tags} -> tags) (\s@GetEventStreamResponse' {} a -> s {tags = a} :: GetEventStreamResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEventStreamResponse_httpStatus :: Lens.Lens' GetEventStreamResponse Prelude.Int
getEventStreamResponse_httpStatus = Lens.lens (\GetEventStreamResponse' {httpStatus} -> httpStatus) (\s@GetEventStreamResponse' {} a -> s {httpStatus = a} :: GetEventStreamResponse)

-- | The unique name of the domain.
getEventStreamResponse_domainName :: Lens.Lens' GetEventStreamResponse Prelude.Text
getEventStreamResponse_domainName = Lens.lens (\GetEventStreamResponse' {domainName} -> domainName) (\s@GetEventStreamResponse' {} a -> s {domainName = a} :: GetEventStreamResponse)

-- | A unique identifier for the event stream.
getEventStreamResponse_eventStreamArn :: Lens.Lens' GetEventStreamResponse Prelude.Text
getEventStreamResponse_eventStreamArn = Lens.lens (\GetEventStreamResponse' {eventStreamArn} -> eventStreamArn) (\s@GetEventStreamResponse' {} a -> s {eventStreamArn = a} :: GetEventStreamResponse)

-- | The timestamp of when the export was created.
getEventStreamResponse_createdAt :: Lens.Lens' GetEventStreamResponse Prelude.UTCTime
getEventStreamResponse_createdAt = Lens.lens (\GetEventStreamResponse' {createdAt} -> createdAt) (\s@GetEventStreamResponse' {} a -> s {createdAt = a} :: GetEventStreamResponse) Prelude.. Data._Time

-- | The operational state of destination stream for export.
getEventStreamResponse_state :: Lens.Lens' GetEventStreamResponse EventStreamState
getEventStreamResponse_state = Lens.lens (\GetEventStreamResponse' {state} -> state) (\s@GetEventStreamResponse' {} a -> s {state = a} :: GetEventStreamResponse)

-- | Details regarding the Kinesis stream.
getEventStreamResponse_destinationDetails :: Lens.Lens' GetEventStreamResponse EventStreamDestinationDetails
getEventStreamResponse_destinationDetails = Lens.lens (\GetEventStreamResponse' {destinationDetails} -> destinationDetails) (\s@GetEventStreamResponse' {} a -> s {destinationDetails = a} :: GetEventStreamResponse)

instance Prelude.NFData GetEventStreamResponse where
  rnf GetEventStreamResponse' {..} =
    Prelude.rnf stoppedSince
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf eventStreamArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf destinationDetails
