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
-- Module      : Amazonka.FraudDetector.GetDeleteEventsByEventTypeStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of a @DeleteEventsByEventType@ action.
module Amazonka.FraudDetector.GetDeleteEventsByEventTypeStatus
  ( -- * Creating a Request
    GetDeleteEventsByEventTypeStatus (..),
    newGetDeleteEventsByEventTypeStatus,

    -- * Request Lenses
    getDeleteEventsByEventTypeStatus_eventTypeName,

    -- * Destructuring the Response
    GetDeleteEventsByEventTypeStatusResponse (..),
    newGetDeleteEventsByEventTypeStatusResponse,

    -- * Response Lenses
    getDeleteEventsByEventTypeStatusResponse_eventTypeName,
    getDeleteEventsByEventTypeStatusResponse_eventsDeletionStatus,
    getDeleteEventsByEventTypeStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDeleteEventsByEventTypeStatus' smart constructor.
data GetDeleteEventsByEventTypeStatus = GetDeleteEventsByEventTypeStatus'
  { -- | Name of event type for which to get the deletion status.
    eventTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeleteEventsByEventTypeStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTypeName', 'getDeleteEventsByEventTypeStatus_eventTypeName' - Name of event type for which to get the deletion status.
newGetDeleteEventsByEventTypeStatus ::
  -- | 'eventTypeName'
  Prelude.Text ->
  GetDeleteEventsByEventTypeStatus
newGetDeleteEventsByEventTypeStatus pEventTypeName_ =
  GetDeleteEventsByEventTypeStatus'
    { eventTypeName =
        pEventTypeName_
    }

-- | Name of event type for which to get the deletion status.
getDeleteEventsByEventTypeStatus_eventTypeName :: Lens.Lens' GetDeleteEventsByEventTypeStatus Prelude.Text
getDeleteEventsByEventTypeStatus_eventTypeName = Lens.lens (\GetDeleteEventsByEventTypeStatus' {eventTypeName} -> eventTypeName) (\s@GetDeleteEventsByEventTypeStatus' {} a -> s {eventTypeName = a} :: GetDeleteEventsByEventTypeStatus)

instance
  Core.AWSRequest
    GetDeleteEventsByEventTypeStatus
  where
  type
    AWSResponse GetDeleteEventsByEventTypeStatus =
      GetDeleteEventsByEventTypeStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeleteEventsByEventTypeStatusResponse'
            Prelude.<$> (x Data..?> "eventTypeName")
            Prelude.<*> (x Data..?> "eventsDeletionStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDeleteEventsByEventTypeStatus
  where
  hashWithSalt
    _salt
    GetDeleteEventsByEventTypeStatus' {..} =
      _salt `Prelude.hashWithSalt` eventTypeName

instance
  Prelude.NFData
    GetDeleteEventsByEventTypeStatus
  where
  rnf GetDeleteEventsByEventTypeStatus' {..} =
    Prelude.rnf eventTypeName

instance
  Data.ToHeaders
    GetDeleteEventsByEventTypeStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetDeleteEventsByEventTypeStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDeleteEventsByEventTypeStatus where
  toJSON GetDeleteEventsByEventTypeStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("eventTypeName" Data..= eventTypeName)
          ]
      )

instance Data.ToPath GetDeleteEventsByEventTypeStatus where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetDeleteEventsByEventTypeStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeleteEventsByEventTypeStatusResponse' smart constructor.
data GetDeleteEventsByEventTypeStatusResponse = GetDeleteEventsByEventTypeStatusResponse'
  { -- | The event type name.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | The deletion status.
    eventsDeletionStatus :: Prelude.Maybe AsyncJobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeleteEventsByEventTypeStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTypeName', 'getDeleteEventsByEventTypeStatusResponse_eventTypeName' - The event type name.
--
-- 'eventsDeletionStatus', 'getDeleteEventsByEventTypeStatusResponse_eventsDeletionStatus' - The deletion status.
--
-- 'httpStatus', 'getDeleteEventsByEventTypeStatusResponse_httpStatus' - The response's http status code.
newGetDeleteEventsByEventTypeStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeleteEventsByEventTypeStatusResponse
newGetDeleteEventsByEventTypeStatusResponse
  pHttpStatus_ =
    GetDeleteEventsByEventTypeStatusResponse'
      { eventTypeName =
          Prelude.Nothing,
        eventsDeletionStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The event type name.
getDeleteEventsByEventTypeStatusResponse_eventTypeName :: Lens.Lens' GetDeleteEventsByEventTypeStatusResponse (Prelude.Maybe Prelude.Text)
getDeleteEventsByEventTypeStatusResponse_eventTypeName = Lens.lens (\GetDeleteEventsByEventTypeStatusResponse' {eventTypeName} -> eventTypeName) (\s@GetDeleteEventsByEventTypeStatusResponse' {} a -> s {eventTypeName = a} :: GetDeleteEventsByEventTypeStatusResponse)

-- | The deletion status.
getDeleteEventsByEventTypeStatusResponse_eventsDeletionStatus :: Lens.Lens' GetDeleteEventsByEventTypeStatusResponse (Prelude.Maybe AsyncJobStatus)
getDeleteEventsByEventTypeStatusResponse_eventsDeletionStatus = Lens.lens (\GetDeleteEventsByEventTypeStatusResponse' {eventsDeletionStatus} -> eventsDeletionStatus) (\s@GetDeleteEventsByEventTypeStatusResponse' {} a -> s {eventsDeletionStatus = a} :: GetDeleteEventsByEventTypeStatusResponse)

-- | The response's http status code.
getDeleteEventsByEventTypeStatusResponse_httpStatus :: Lens.Lens' GetDeleteEventsByEventTypeStatusResponse Prelude.Int
getDeleteEventsByEventTypeStatusResponse_httpStatus = Lens.lens (\GetDeleteEventsByEventTypeStatusResponse' {httpStatus} -> httpStatus) (\s@GetDeleteEventsByEventTypeStatusResponse' {} a -> s {httpStatus = a} :: GetDeleteEventsByEventTypeStatusResponse)

instance
  Prelude.NFData
    GetDeleteEventsByEventTypeStatusResponse
  where
  rnf GetDeleteEventsByEventTypeStatusResponse' {..} =
    Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf eventsDeletionStatus
      `Prelude.seq` Prelude.rnf httpStatus
