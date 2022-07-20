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
-- Module      : Amazonka.Pinpoint.PutEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event to record for endpoints, or creates or updates
-- endpoint data that existing events are associated with.
module Amazonka.Pinpoint.PutEvents
  ( -- * Creating a Request
    PutEvents (..),
    newPutEvents,

    -- * Request Lenses
    putEvents_applicationId,
    putEvents_eventsRequest,

    -- * Destructuring the Response
    PutEventsResponse (..),
    newPutEventsResponse,

    -- * Response Lenses
    putEventsResponse_httpStatus,
    putEventsResponse_eventsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutEvents' smart constructor.
data PutEvents = PutEvents'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    eventsRequest :: EventsRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'putEvents_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'eventsRequest', 'putEvents_eventsRequest' - Undocumented member.
newPutEvents ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'eventsRequest'
  EventsRequest ->
  PutEvents
newPutEvents pApplicationId_ pEventsRequest_ =
  PutEvents'
    { applicationId = pApplicationId_,
      eventsRequest = pEventsRequest_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
putEvents_applicationId :: Lens.Lens' PutEvents Prelude.Text
putEvents_applicationId = Lens.lens (\PutEvents' {applicationId} -> applicationId) (\s@PutEvents' {} a -> s {applicationId = a} :: PutEvents)

-- | Undocumented member.
putEvents_eventsRequest :: Lens.Lens' PutEvents EventsRequest
putEvents_eventsRequest = Lens.lens (\PutEvents' {eventsRequest} -> eventsRequest) (\s@PutEvents' {} a -> s {eventsRequest = a} :: PutEvents)

instance Core.AWSRequest PutEvents where
  type AWSResponse PutEvents = PutEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable PutEvents where
  hashWithSalt _salt PutEvents' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` eventsRequest

instance Prelude.NFData PutEvents where
  rnf PutEvents' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf eventsRequest

instance Core.ToHeaders PutEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutEvents where
  toJSON PutEvents' {..} = Core.toJSON eventsRequest

instance Core.ToPath PutEvents where
  toPath PutEvents' {..} =
    Prelude.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/events"]

instance Core.ToQuery PutEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutEventsResponse' smart constructor.
data PutEventsResponse = PutEventsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    eventsResponse :: EventsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putEventsResponse_httpStatus' - The response's http status code.
--
-- 'eventsResponse', 'putEventsResponse_eventsResponse' - Undocumented member.
newPutEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'eventsResponse'
  EventsResponse ->
  PutEventsResponse
newPutEventsResponse pHttpStatus_ pEventsResponse_ =
  PutEventsResponse'
    { httpStatus = pHttpStatus_,
      eventsResponse = pEventsResponse_
    }

-- | The response's http status code.
putEventsResponse_httpStatus :: Lens.Lens' PutEventsResponse Prelude.Int
putEventsResponse_httpStatus = Lens.lens (\PutEventsResponse' {httpStatus} -> httpStatus) (\s@PutEventsResponse' {} a -> s {httpStatus = a} :: PutEventsResponse)

-- | Undocumented member.
putEventsResponse_eventsResponse :: Lens.Lens' PutEventsResponse EventsResponse
putEventsResponse_eventsResponse = Lens.lens (\PutEventsResponse' {eventsResponse} -> eventsResponse) (\s@PutEventsResponse' {} a -> s {eventsResponse = a} :: PutEventsResponse)

instance Prelude.NFData PutEventsResponse where
  rnf PutEventsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf eventsResponse
