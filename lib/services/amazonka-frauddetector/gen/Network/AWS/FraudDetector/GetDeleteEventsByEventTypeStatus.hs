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
-- Module      : Network.AWS.FraudDetector.GetDeleteEventsByEventTypeStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of a @DeleteEventsByEventType@ action.
module Network.AWS.FraudDetector.GetDeleteEventsByEventTypeStatus
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

import qualified Network.AWS.Core as Core
import Network.AWS.FraudDetector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeleteEventsByEventTypeStatusResponse'
            Prelude.<$> (x Core..?> "eventTypeName")
            Prelude.<*> (x Core..?> "eventsDeletionStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDeleteEventsByEventTypeStatus

instance
  Prelude.NFData
    GetDeleteEventsByEventTypeStatus

instance
  Core.ToHeaders
    GetDeleteEventsByEventTypeStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetDeleteEventsByEventTypeStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDeleteEventsByEventTypeStatus where
  toJSON GetDeleteEventsByEventTypeStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("eventTypeName" Core..= eventTypeName)
          ]
      )

instance Core.ToPath GetDeleteEventsByEventTypeStatus where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
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
