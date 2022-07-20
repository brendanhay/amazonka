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
-- Module      : Amazonka.Pinpoint.PutEventStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event stream for an application or updates the settings of
-- an existing event stream for an application.
module Amazonka.Pinpoint.PutEventStream
  ( -- * Creating a Request
    PutEventStream (..),
    newPutEventStream,

    -- * Request Lenses
    putEventStream_applicationId,
    putEventStream_writeEventStream,

    -- * Destructuring the Response
    PutEventStreamResponse (..),
    newPutEventStreamResponse,

    -- * Response Lenses
    putEventStreamResponse_httpStatus,
    putEventStreamResponse_eventStream,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutEventStream' smart constructor.
data PutEventStream = PutEventStream'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    writeEventStream :: WriteEventStream
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'putEventStream_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'writeEventStream', 'putEventStream_writeEventStream' - Undocumented member.
newPutEventStream ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'writeEventStream'
  WriteEventStream ->
  PutEventStream
newPutEventStream pApplicationId_ pWriteEventStream_ =
  PutEventStream'
    { applicationId = pApplicationId_,
      writeEventStream = pWriteEventStream_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
putEventStream_applicationId :: Lens.Lens' PutEventStream Prelude.Text
putEventStream_applicationId = Lens.lens (\PutEventStream' {applicationId} -> applicationId) (\s@PutEventStream' {} a -> s {applicationId = a} :: PutEventStream)

-- | Undocumented member.
putEventStream_writeEventStream :: Lens.Lens' PutEventStream WriteEventStream
putEventStream_writeEventStream = Lens.lens (\PutEventStream' {writeEventStream} -> writeEventStream) (\s@PutEventStream' {} a -> s {writeEventStream = a} :: PutEventStream)

instance Core.AWSRequest PutEventStream where
  type
    AWSResponse PutEventStream =
      PutEventStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable PutEventStream where
  hashWithSalt _salt PutEventStream' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` writeEventStream

instance Prelude.NFData PutEventStream where
  rnf PutEventStream' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf writeEventStream

instance Core.ToHeaders PutEventStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutEventStream where
  toJSON PutEventStream' {..} =
    Core.toJSON writeEventStream

instance Core.ToPath PutEventStream where
  toPath PutEventStream' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/eventstream"
      ]

instance Core.ToQuery PutEventStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutEventStreamResponse' smart constructor.
data PutEventStreamResponse = PutEventStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    eventStream :: EventStream
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEventStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putEventStreamResponse_httpStatus' - The response's http status code.
--
-- 'eventStream', 'putEventStreamResponse_eventStream' - Undocumented member.
newPutEventStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'eventStream'
  EventStream ->
  PutEventStreamResponse
newPutEventStreamResponse pHttpStatus_ pEventStream_ =
  PutEventStreamResponse'
    { httpStatus = pHttpStatus_,
      eventStream = pEventStream_
    }

-- | The response's http status code.
putEventStreamResponse_httpStatus :: Lens.Lens' PutEventStreamResponse Prelude.Int
putEventStreamResponse_httpStatus = Lens.lens (\PutEventStreamResponse' {httpStatus} -> httpStatus) (\s@PutEventStreamResponse' {} a -> s {httpStatus = a} :: PutEventStreamResponse)

-- | Undocumented member.
putEventStreamResponse_eventStream :: Lens.Lens' PutEventStreamResponse EventStream
putEventStreamResponse_eventStream = Lens.lens (\PutEventStreamResponse' {eventStream} -> eventStream) (\s@PutEventStreamResponse' {} a -> s {eventStream = a} :: PutEventStreamResponse)

instance Prelude.NFData PutEventStreamResponse where
  rnf PutEventStreamResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf eventStream
