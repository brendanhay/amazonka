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
-- Module      : Amazonka.CustomerProfiles.CreateEventStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an event stream, which is a subscription to real-time events,
-- such as when profiles are created and updated through Amazon Connect
-- Customer Profiles.
--
-- Each event stream can be associated with only one Kinesis Data Stream
-- destination in the same region and Amazon Web Services account as the
-- customer profiles domain
module Amazonka.CustomerProfiles.CreateEventStream
  ( -- * Creating a Request
    CreateEventStream (..),
    newCreateEventStream,

    -- * Request Lenses
    createEventStream_tags,
    createEventStream_domainName,
    createEventStream_uri,
    createEventStream_eventStreamName,

    -- * Destructuring the Response
    CreateEventStreamResponse (..),
    newCreateEventStreamResponse,

    -- * Response Lenses
    createEventStreamResponse_tags,
    createEventStreamResponse_httpStatus,
    createEventStreamResponse_eventStreamArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEventStream' smart constructor.
data CreateEventStream = CreateEventStream'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The StreamARN of the destination to deliver profile events to. For
    -- example, arn:aws:kinesis:region:account-id:stream\/stream-name
    uri :: Prelude.Text,
    -- | The name of the event stream.
    eventStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEventStream_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'domainName', 'createEventStream_domainName' - The unique name of the domain.
--
-- 'uri', 'createEventStream_uri' - The StreamARN of the destination to deliver profile events to. For
-- example, arn:aws:kinesis:region:account-id:stream\/stream-name
--
-- 'eventStreamName', 'createEventStream_eventStreamName' - The name of the event stream.
newCreateEventStream ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'uri'
  Prelude.Text ->
  -- | 'eventStreamName'
  Prelude.Text ->
  CreateEventStream
newCreateEventStream
  pDomainName_
  pUri_
  pEventStreamName_ =
    CreateEventStream'
      { tags = Prelude.Nothing,
        domainName = pDomainName_,
        uri = pUri_,
        eventStreamName = pEventStreamName_
      }

-- | The tags used to organize, track, or control access for this resource.
createEventStream_tags :: Lens.Lens' CreateEventStream (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEventStream_tags = Lens.lens (\CreateEventStream' {tags} -> tags) (\s@CreateEventStream' {} a -> s {tags = a} :: CreateEventStream) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the domain.
createEventStream_domainName :: Lens.Lens' CreateEventStream Prelude.Text
createEventStream_domainName = Lens.lens (\CreateEventStream' {domainName} -> domainName) (\s@CreateEventStream' {} a -> s {domainName = a} :: CreateEventStream)

-- | The StreamARN of the destination to deliver profile events to. For
-- example, arn:aws:kinesis:region:account-id:stream\/stream-name
createEventStream_uri :: Lens.Lens' CreateEventStream Prelude.Text
createEventStream_uri = Lens.lens (\CreateEventStream' {uri} -> uri) (\s@CreateEventStream' {} a -> s {uri = a} :: CreateEventStream)

-- | The name of the event stream.
createEventStream_eventStreamName :: Lens.Lens' CreateEventStream Prelude.Text
createEventStream_eventStreamName = Lens.lens (\CreateEventStream' {eventStreamName} -> eventStreamName) (\s@CreateEventStream' {} a -> s {eventStreamName = a} :: CreateEventStream)

instance Core.AWSRequest CreateEventStream where
  type
    AWSResponse CreateEventStream =
      CreateEventStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventStreamResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "EventStreamArn")
      )

instance Prelude.Hashable CreateEventStream where
  hashWithSalt _salt CreateEventStream' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` eventStreamName

instance Prelude.NFData CreateEventStream where
  rnf CreateEventStream' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf eventStreamName

instance Data.ToHeaders CreateEventStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEventStream where
  toJSON CreateEventStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Uri" Data..= uri)
          ]
      )

instance Data.ToPath CreateEventStream where
  toPath CreateEventStream' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/event-streams/",
        Data.toBS eventStreamName
      ]

instance Data.ToQuery CreateEventStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEventStreamResponse' smart constructor.
data CreateEventStreamResponse = CreateEventStreamResponse'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the event stream.
    eventStreamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEventStreamResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'createEventStreamResponse_httpStatus' - The response's http status code.
--
-- 'eventStreamArn', 'createEventStreamResponse_eventStreamArn' - A unique identifier for the event stream.
newCreateEventStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'eventStreamArn'
  Prelude.Text ->
  CreateEventStreamResponse
newCreateEventStreamResponse
  pHttpStatus_
  pEventStreamArn_ =
    CreateEventStreamResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        eventStreamArn = pEventStreamArn_
      }

-- | The tags used to organize, track, or control access for this resource.
createEventStreamResponse_tags :: Lens.Lens' CreateEventStreamResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEventStreamResponse_tags = Lens.lens (\CreateEventStreamResponse' {tags} -> tags) (\s@CreateEventStreamResponse' {} a -> s {tags = a} :: CreateEventStreamResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createEventStreamResponse_httpStatus :: Lens.Lens' CreateEventStreamResponse Prelude.Int
createEventStreamResponse_httpStatus = Lens.lens (\CreateEventStreamResponse' {httpStatus} -> httpStatus) (\s@CreateEventStreamResponse' {} a -> s {httpStatus = a} :: CreateEventStreamResponse)

-- | A unique identifier for the event stream.
createEventStreamResponse_eventStreamArn :: Lens.Lens' CreateEventStreamResponse Prelude.Text
createEventStreamResponse_eventStreamArn = Lens.lens (\CreateEventStreamResponse' {eventStreamArn} -> eventStreamArn) (\s@CreateEventStreamResponse' {} a -> s {eventStreamArn = a} :: CreateEventStreamResponse)

instance Prelude.NFData CreateEventStreamResponse where
  rnf CreateEventStreamResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf eventStreamArn
