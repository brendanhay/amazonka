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
-- Module      : Amazonka.Pinpoint.GetEventStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the event stream settings for an
-- application.
module Amazonka.Pinpoint.GetEventStream
  ( -- * Creating a Request
    GetEventStream (..),
    newGetEventStream,

    -- * Request Lenses
    getEventStream_applicationId,

    -- * Destructuring the Response
    GetEventStreamResponse (..),
    newGetEventStreamResponse,

    -- * Response Lenses
    getEventStreamResponse_httpStatus,
    getEventStreamResponse_eventStream,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventStream' smart constructor.
data GetEventStream = GetEventStream'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
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
-- 'applicationId', 'getEventStream_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetEventStream ::
  -- | 'applicationId'
  Prelude.Text ->
  GetEventStream
newGetEventStream pApplicationId_ =
  GetEventStream' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getEventStream_applicationId :: Lens.Lens' GetEventStream Prelude.Text
getEventStream_applicationId = Lens.lens (\GetEventStream' {applicationId} -> applicationId) (\s@GetEventStream' {} a -> s {applicationId = a} :: GetEventStream)

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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetEventStream where
  hashWithSalt _salt GetEventStream' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetEventStream where
  rnf GetEventStream' {..} = Prelude.rnf applicationId

instance Core.ToHeaders GetEventStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetEventStream where
  toPath GetEventStream' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/eventstream"
      ]

instance Core.ToQuery GetEventStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventStreamResponse' smart constructor.
data GetEventStreamResponse = GetEventStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    eventStream :: EventStream
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
-- 'httpStatus', 'getEventStreamResponse_httpStatus' - The response's http status code.
--
-- 'eventStream', 'getEventStreamResponse_eventStream' - Undocumented member.
newGetEventStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'eventStream'
  EventStream ->
  GetEventStreamResponse
newGetEventStreamResponse pHttpStatus_ pEventStream_ =
  GetEventStreamResponse'
    { httpStatus = pHttpStatus_,
      eventStream = pEventStream_
    }

-- | The response's http status code.
getEventStreamResponse_httpStatus :: Lens.Lens' GetEventStreamResponse Prelude.Int
getEventStreamResponse_httpStatus = Lens.lens (\GetEventStreamResponse' {httpStatus} -> httpStatus) (\s@GetEventStreamResponse' {} a -> s {httpStatus = a} :: GetEventStreamResponse)

-- | Undocumented member.
getEventStreamResponse_eventStream :: Lens.Lens' GetEventStreamResponse EventStream
getEventStreamResponse_eventStream = Lens.lens (\GetEventStreamResponse' {eventStream} -> eventStream) (\s@GetEventStreamResponse' {} a -> s {eventStream = a} :: GetEventStreamResponse)

instance Prelude.NFData GetEventStreamResponse where
  rnf GetEventStreamResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf eventStream
