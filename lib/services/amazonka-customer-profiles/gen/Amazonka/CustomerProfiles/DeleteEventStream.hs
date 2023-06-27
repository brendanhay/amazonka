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
-- Module      : Amazonka.CustomerProfiles.DeleteEventStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables and deletes the specified event stream.
module Amazonka.CustomerProfiles.DeleteEventStream
  ( -- * Creating a Request
    DeleteEventStream (..),
    newDeleteEventStream,

    -- * Request Lenses
    deleteEventStream_domainName,
    deleteEventStream_eventStreamName,

    -- * Destructuring the Response
    DeleteEventStreamResponse (..),
    newDeleteEventStreamResponse,

    -- * Response Lenses
    deleteEventStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEventStream' smart constructor.
data DeleteEventStream = DeleteEventStream'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The name of the event stream
    eventStreamName :: Prelude.Text
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
-- 'domainName', 'deleteEventStream_domainName' - The unique name of the domain.
--
-- 'eventStreamName', 'deleteEventStream_eventStreamName' - The name of the event stream
newDeleteEventStream ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'eventStreamName'
  Prelude.Text ->
  DeleteEventStream
newDeleteEventStream pDomainName_ pEventStreamName_ =
  DeleteEventStream'
    { domainName = pDomainName_,
      eventStreamName = pEventStreamName_
    }

-- | The unique name of the domain.
deleteEventStream_domainName :: Lens.Lens' DeleteEventStream Prelude.Text
deleteEventStream_domainName = Lens.lens (\DeleteEventStream' {domainName} -> domainName) (\s@DeleteEventStream' {} a -> s {domainName = a} :: DeleteEventStream)

-- | The name of the event stream
deleteEventStream_eventStreamName :: Lens.Lens' DeleteEventStream Prelude.Text
deleteEventStream_eventStreamName = Lens.lens (\DeleteEventStream' {eventStreamName} -> eventStreamName) (\s@DeleteEventStream' {} a -> s {eventStreamName = a} :: DeleteEventStream)

instance Core.AWSRequest DeleteEventStream where
  type
    AWSResponse DeleteEventStream =
      DeleteEventStreamResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEventStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEventStream where
  hashWithSalt _salt DeleteEventStream' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` eventStreamName

instance Prelude.NFData DeleteEventStream where
  rnf DeleteEventStream' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf eventStreamName

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
      [ "/domains/",
        Data.toBS domainName,
        "/event-streams/",
        Data.toBS eventStreamName
      ]

instance Data.ToQuery DeleteEventStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventStreamResponse' smart constructor.
data DeleteEventStreamResponse = DeleteEventStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
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
newDeleteEventStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEventStreamResponse
newDeleteEventStreamResponse pHttpStatus_ =
  DeleteEventStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEventStreamResponse_httpStatus :: Lens.Lens' DeleteEventStreamResponse Prelude.Int
deleteEventStreamResponse_httpStatus = Lens.lens (\DeleteEventStreamResponse' {httpStatus} -> httpStatus) (\s@DeleteEventStreamResponse' {} a -> s {httpStatus = a} :: DeleteEventStreamResponse)

instance Prelude.NFData DeleteEventStreamResponse where
  rnf DeleteEventStreamResponse' {..} =
    Prelude.rnf httpStatus
