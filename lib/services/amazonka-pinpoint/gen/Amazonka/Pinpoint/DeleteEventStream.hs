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
-- Module      : Amazonka.Pinpoint.DeleteEventStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the event stream for an application.
module Amazonka.Pinpoint.DeleteEventStream
  ( -- * Creating a Request
    DeleteEventStream (..),
    newDeleteEventStream,

    -- * Request Lenses
    deleteEventStream_applicationId,

    -- * Destructuring the Response
    DeleteEventStreamResponse (..),
    newDeleteEventStreamResponse,

    -- * Response Lenses
    deleteEventStreamResponse_httpStatus,
    deleteEventStreamResponse_eventStream,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEventStream' smart constructor.
data DeleteEventStream = DeleteEventStream'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteEventStream_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteEventStream ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteEventStream
newDeleteEventStream pApplicationId_ =
  DeleteEventStream' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteEventStream_applicationId :: Lens.Lens' DeleteEventStream Prelude.Text
deleteEventStream_applicationId = Lens.lens (\DeleteEventStream' {applicationId} -> applicationId) (\s@DeleteEventStream' {} a -> s {applicationId = a} :: DeleteEventStream)

instance Core.AWSRequest DeleteEventStream where
  type
    AWSResponse DeleteEventStream =
      DeleteEventStreamResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEventStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteEventStream where
  hashWithSalt _salt DeleteEventStream' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteEventStream where
  rnf DeleteEventStream' {..} =
    Prelude.rnf applicationId

instance Data.ToHeaders DeleteEventStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEventStream where
  toPath DeleteEventStream' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/eventstream"
      ]

instance Data.ToQuery DeleteEventStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventStreamResponse' smart constructor.
data DeleteEventStreamResponse = DeleteEventStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    eventStream :: EventStream
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEventStreamResponse_httpStatus' - The response's http status code.
--
-- 'eventStream', 'deleteEventStreamResponse_eventStream' - Undocumented member.
newDeleteEventStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'eventStream'
  EventStream ->
  DeleteEventStreamResponse
newDeleteEventStreamResponse
  pHttpStatus_
  pEventStream_ =
    DeleteEventStreamResponse'
      { httpStatus =
          pHttpStatus_,
        eventStream = pEventStream_
      }

-- | The response's http status code.
deleteEventStreamResponse_httpStatus :: Lens.Lens' DeleteEventStreamResponse Prelude.Int
deleteEventStreamResponse_httpStatus = Lens.lens (\DeleteEventStreamResponse' {httpStatus} -> httpStatus) (\s@DeleteEventStreamResponse' {} a -> s {httpStatus = a} :: DeleteEventStreamResponse)

-- | Undocumented member.
deleteEventStreamResponse_eventStream :: Lens.Lens' DeleteEventStreamResponse EventStream
deleteEventStreamResponse_eventStream = Lens.lens (\DeleteEventStreamResponse' {eventStream} -> eventStream) (\s@DeleteEventStreamResponse' {} a -> s {eventStream = a} :: DeleteEventStreamResponse)

instance Prelude.NFData DeleteEventStreamResponse where
  rnf DeleteEventStreamResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf eventStream
