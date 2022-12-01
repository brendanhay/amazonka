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
-- Module      : Amazonka.FraudDetector.DeleteEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified event.
--
-- When you delete an event, Amazon Fraud Detector permanently deletes that
-- event and the event data is no longer stored in Amazon Fraud Detector.
module Amazonka.FraudDetector.DeleteEvent
  ( -- * Creating a Request
    DeleteEvent (..),
    newDeleteEvent,

    -- * Request Lenses
    deleteEvent_deleteAuditHistory,
    deleteEvent_eventId,
    deleteEvent_eventTypeName,

    -- * Destructuring the Response
    DeleteEventResponse (..),
    newDeleteEventResponse,

    -- * Response Lenses
    deleteEventResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEvent' smart constructor.
data DeleteEvent = DeleteEvent'
  { -- | Specifies whether or not to delete any predictions associated with the
    -- event.
    deleteAuditHistory :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the event to delete.
    eventId :: Prelude.Text,
    -- | The name of the event type.
    eventTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteAuditHistory', 'deleteEvent_deleteAuditHistory' - Specifies whether or not to delete any predictions associated with the
-- event.
--
-- 'eventId', 'deleteEvent_eventId' - The ID of the event to delete.
--
-- 'eventTypeName', 'deleteEvent_eventTypeName' - The name of the event type.
newDeleteEvent ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'eventTypeName'
  Prelude.Text ->
  DeleteEvent
newDeleteEvent pEventId_ pEventTypeName_ =
  DeleteEvent'
    { deleteAuditHistory = Prelude.Nothing,
      eventId = pEventId_,
      eventTypeName = pEventTypeName_
    }

-- | Specifies whether or not to delete any predictions associated with the
-- event.
deleteEvent_deleteAuditHistory :: Lens.Lens' DeleteEvent (Prelude.Maybe Prelude.Bool)
deleteEvent_deleteAuditHistory = Lens.lens (\DeleteEvent' {deleteAuditHistory} -> deleteAuditHistory) (\s@DeleteEvent' {} a -> s {deleteAuditHistory = a} :: DeleteEvent)

-- | The ID of the event to delete.
deleteEvent_eventId :: Lens.Lens' DeleteEvent Prelude.Text
deleteEvent_eventId = Lens.lens (\DeleteEvent' {eventId} -> eventId) (\s@DeleteEvent' {} a -> s {eventId = a} :: DeleteEvent)

-- | The name of the event type.
deleteEvent_eventTypeName :: Lens.Lens' DeleteEvent Prelude.Text
deleteEvent_eventTypeName = Lens.lens (\DeleteEvent' {eventTypeName} -> eventTypeName) (\s@DeleteEvent' {} a -> s {eventTypeName = a} :: DeleteEvent)

instance Core.AWSRequest DeleteEvent where
  type AWSResponse DeleteEvent = DeleteEventResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEventResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEvent where
  hashWithSalt _salt DeleteEvent' {..} =
    _salt `Prelude.hashWithSalt` deleteAuditHistory
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventTypeName

instance Prelude.NFData DeleteEvent where
  rnf DeleteEvent' {..} =
    Prelude.rnf deleteAuditHistory
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventTypeName

instance Core.ToHeaders DeleteEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.DeleteEvent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteEvent where
  toJSON DeleteEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("deleteAuditHistory" Core..=)
              Prelude.<$> deleteAuditHistory,
            Prelude.Just ("eventId" Core..= eventId),
            Prelude.Just
              ("eventTypeName" Core..= eventTypeName)
          ]
      )

instance Core.ToPath DeleteEvent where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventResponse' smart constructor.
data DeleteEventResponse = DeleteEventResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEventResponse_httpStatus' - The response's http status code.
newDeleteEventResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEventResponse
newDeleteEventResponse pHttpStatus_ =
  DeleteEventResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteEventResponse_httpStatus :: Lens.Lens' DeleteEventResponse Prelude.Int
deleteEventResponse_httpStatus = Lens.lens (\DeleteEventResponse' {httpStatus} -> httpStatus) (\s@DeleteEventResponse' {} a -> s {httpStatus = a} :: DeleteEventResponse)

instance Prelude.NFData DeleteEventResponse where
  rnf DeleteEventResponse' {..} = Prelude.rnf httpStatus
