{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.DeleteEventStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the event stream for an application.
module Network.AWS.Pinpoint.DeleteEventStream
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEventStream' smart constructor.
data DeleteEventStream = DeleteEventStream'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteEventStream where
  type Rs DeleteEventStream = DeleteEventStreamResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEventStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteEventStream

instance Prelude.NFData DeleteEventStream

instance Prelude.ToHeaders DeleteEventStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteEventStream where
  toPath DeleteEventStream' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Prelude.toBS applicationId,
        "/eventstream"
      ]

instance Prelude.ToQuery DeleteEventStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventStreamResponse' smart constructor.
data DeleteEventStreamResponse = DeleteEventStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    eventStream :: EventStream
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteEventStreamResponse
