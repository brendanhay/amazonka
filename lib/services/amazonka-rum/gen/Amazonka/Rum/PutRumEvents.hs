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
-- Module      : Amazonka.Rum.PutRumEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends telemetry events about your application performance and user
-- behavior to CloudWatch RUM. The code snippet that RUM generates for you
-- to add to your application includes @PutRumEvents@ operations to send
-- this data to RUM.
--
-- Each @PutRumEvents@ operation can send a batch of events from one user
-- session.
module Amazonka.Rum.PutRumEvents
  ( -- * Creating a Request
    PutRumEvents (..),
    newPutRumEvents,

    -- * Request Lenses
    putRumEvents_appMonitorDetails,
    putRumEvents_batchId,
    putRumEvents_id,
    putRumEvents_rumEvents,
    putRumEvents_userDetails,

    -- * Destructuring the Response
    PutRumEventsResponse (..),
    newPutRumEventsResponse,

    -- * Response Lenses
    putRumEventsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Rum.Types

-- | /See:/ 'newPutRumEvents' smart constructor.
data PutRumEvents = PutRumEvents'
  { -- | A structure that contains information about the app monitor that
    -- collected this telemetry information.
    appMonitorDetails :: AppMonitorDetails,
    -- | A unique identifier for this batch of RUM event data.
    batchId :: Prelude.Text,
    -- | The ID of the app monitor that is sending this data.
    id :: Prelude.Text,
    -- | An array of structures that contain the telemetry event data.
    rumEvents :: [RumEvent],
    -- | A structure that contains information about the user session that this
    -- batch of events was collected from.
    userDetails :: UserDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRumEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appMonitorDetails', 'putRumEvents_appMonitorDetails' - A structure that contains information about the app monitor that
-- collected this telemetry information.
--
-- 'batchId', 'putRumEvents_batchId' - A unique identifier for this batch of RUM event data.
--
-- 'id', 'putRumEvents_id' - The ID of the app monitor that is sending this data.
--
-- 'rumEvents', 'putRumEvents_rumEvents' - An array of structures that contain the telemetry event data.
--
-- 'userDetails', 'putRumEvents_userDetails' - A structure that contains information about the user session that this
-- batch of events was collected from.
newPutRumEvents ::
  -- | 'appMonitorDetails'
  AppMonitorDetails ->
  -- | 'batchId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'userDetails'
  UserDetails ->
  PutRumEvents
newPutRumEvents
  pAppMonitorDetails_
  pBatchId_
  pId_
  pUserDetails_ =
    PutRumEvents'
      { appMonitorDetails =
          pAppMonitorDetails_,
        batchId = pBatchId_,
        id = pId_,
        rumEvents = Prelude.mempty,
        userDetails = pUserDetails_
      }

-- | A structure that contains information about the app monitor that
-- collected this telemetry information.
putRumEvents_appMonitorDetails :: Lens.Lens' PutRumEvents AppMonitorDetails
putRumEvents_appMonitorDetails = Lens.lens (\PutRumEvents' {appMonitorDetails} -> appMonitorDetails) (\s@PutRumEvents' {} a -> s {appMonitorDetails = a} :: PutRumEvents)

-- | A unique identifier for this batch of RUM event data.
putRumEvents_batchId :: Lens.Lens' PutRumEvents Prelude.Text
putRumEvents_batchId = Lens.lens (\PutRumEvents' {batchId} -> batchId) (\s@PutRumEvents' {} a -> s {batchId = a} :: PutRumEvents)

-- | The ID of the app monitor that is sending this data.
putRumEvents_id :: Lens.Lens' PutRumEvents Prelude.Text
putRumEvents_id = Lens.lens (\PutRumEvents' {id} -> id) (\s@PutRumEvents' {} a -> s {id = a} :: PutRumEvents)

-- | An array of structures that contain the telemetry event data.
putRumEvents_rumEvents :: Lens.Lens' PutRumEvents [RumEvent]
putRumEvents_rumEvents = Lens.lens (\PutRumEvents' {rumEvents} -> rumEvents) (\s@PutRumEvents' {} a -> s {rumEvents = a} :: PutRumEvents) Prelude.. Lens.coerced

-- | A structure that contains information about the user session that this
-- batch of events was collected from.
putRumEvents_userDetails :: Lens.Lens' PutRumEvents UserDetails
putRumEvents_userDetails = Lens.lens (\PutRumEvents' {userDetails} -> userDetails) (\s@PutRumEvents' {} a -> s {userDetails = a} :: PutRumEvents)

instance Core.AWSRequest PutRumEvents where
  type AWSResponse PutRumEvents = PutRumEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRumEventsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRumEvents where
  hashWithSalt _salt PutRumEvents' {..} =
    _salt `Prelude.hashWithSalt` appMonitorDetails
      `Prelude.hashWithSalt` batchId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` rumEvents
      `Prelude.hashWithSalt` userDetails

instance Prelude.NFData PutRumEvents where
  rnf PutRumEvents' {..} =
    Prelude.rnf appMonitorDetails
      `Prelude.seq` Prelude.rnf batchId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf rumEvents
      `Prelude.seq` Prelude.rnf userDetails

instance Data.ToHeaders PutRumEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRumEvents where
  toJSON PutRumEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AppMonitorDetails" Data..= appMonitorDetails),
            Prelude.Just ("BatchId" Data..= batchId),
            Prelude.Just ("RumEvents" Data..= rumEvents),
            Prelude.Just ("UserDetails" Data..= userDetails)
          ]
      )

instance Data.ToPath PutRumEvents where
  toPath PutRumEvents' {..} =
    Prelude.mconcat
      ["/appmonitors/", Data.toBS id, "/"]

instance Data.ToQuery PutRumEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRumEventsResponse' smart constructor.
data PutRumEventsResponse = PutRumEventsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRumEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRumEventsResponse_httpStatus' - The response's http status code.
newPutRumEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRumEventsResponse
newPutRumEventsResponse pHttpStatus_ =
  PutRumEventsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putRumEventsResponse_httpStatus :: Lens.Lens' PutRumEventsResponse Prelude.Int
putRumEventsResponse_httpStatus = Lens.lens (\PutRumEventsResponse' {httpStatus} -> httpStatus) (\s@PutRumEventsResponse' {} a -> s {httpStatus = a} :: PutRumEventsResponse)

instance Prelude.NFData PutRumEventsResponse where
  rnf PutRumEventsResponse' {..} =
    Prelude.rnf httpStatus
