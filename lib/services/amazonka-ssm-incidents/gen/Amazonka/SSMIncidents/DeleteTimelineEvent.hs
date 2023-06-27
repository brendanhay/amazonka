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
-- Module      : Amazonka.SSMIncidents.DeleteTimelineEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a timeline event from an incident.
module Amazonka.SSMIncidents.DeleteTimelineEvent
  ( -- * Creating a Request
    DeleteTimelineEvent (..),
    newDeleteTimelineEvent,

    -- * Request Lenses
    deleteTimelineEvent_eventId,
    deleteTimelineEvent_incidentRecordArn,

    -- * Destructuring the Response
    DeleteTimelineEventResponse (..),
    newDeleteTimelineEventResponse,

    -- * Response Lenses
    deleteTimelineEventResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newDeleteTimelineEvent' smart constructor.
data DeleteTimelineEvent = DeleteTimelineEvent'
  { -- | The ID of the event to update. You can use @ListTimelineEvents@ to find
    -- an event\'s ID.
    eventId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the incident that includes the
    -- timeline event.
    incidentRecordArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTimelineEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'deleteTimelineEvent_eventId' - The ID of the event to update. You can use @ListTimelineEvents@ to find
-- an event\'s ID.
--
-- 'incidentRecordArn', 'deleteTimelineEvent_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident that includes the
-- timeline event.
newDeleteTimelineEvent ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'incidentRecordArn'
  Prelude.Text ->
  DeleteTimelineEvent
newDeleteTimelineEvent pEventId_ pIncidentRecordArn_ =
  DeleteTimelineEvent'
    { eventId = pEventId_,
      incidentRecordArn = pIncidentRecordArn_
    }

-- | The ID of the event to update. You can use @ListTimelineEvents@ to find
-- an event\'s ID.
deleteTimelineEvent_eventId :: Lens.Lens' DeleteTimelineEvent Prelude.Text
deleteTimelineEvent_eventId = Lens.lens (\DeleteTimelineEvent' {eventId} -> eventId) (\s@DeleteTimelineEvent' {} a -> s {eventId = a} :: DeleteTimelineEvent)

-- | The Amazon Resource Name (ARN) of the incident that includes the
-- timeline event.
deleteTimelineEvent_incidentRecordArn :: Lens.Lens' DeleteTimelineEvent Prelude.Text
deleteTimelineEvent_incidentRecordArn = Lens.lens (\DeleteTimelineEvent' {incidentRecordArn} -> incidentRecordArn) (\s@DeleteTimelineEvent' {} a -> s {incidentRecordArn = a} :: DeleteTimelineEvent)

instance Core.AWSRequest DeleteTimelineEvent where
  type
    AWSResponse DeleteTimelineEvent =
      DeleteTimelineEventResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTimelineEventResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTimelineEvent where
  hashWithSalt _salt DeleteTimelineEvent' {..} =
    _salt
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` incidentRecordArn

instance Prelude.NFData DeleteTimelineEvent where
  rnf DeleteTimelineEvent' {..} =
    Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf incidentRecordArn

instance Data.ToHeaders DeleteTimelineEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTimelineEvent where
  toJSON DeleteTimelineEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("eventId" Data..= eventId),
            Prelude.Just
              ("incidentRecordArn" Data..= incidentRecordArn)
          ]
      )

instance Data.ToPath DeleteTimelineEvent where
  toPath = Prelude.const "/deleteTimelineEvent"

instance Data.ToQuery DeleteTimelineEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTimelineEventResponse' smart constructor.
data DeleteTimelineEventResponse = DeleteTimelineEventResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTimelineEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTimelineEventResponse_httpStatus' - The response's http status code.
newDeleteTimelineEventResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTimelineEventResponse
newDeleteTimelineEventResponse pHttpStatus_ =
  DeleteTimelineEventResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTimelineEventResponse_httpStatus :: Lens.Lens' DeleteTimelineEventResponse Prelude.Int
deleteTimelineEventResponse_httpStatus = Lens.lens (\DeleteTimelineEventResponse' {httpStatus} -> httpStatus) (\s@DeleteTimelineEventResponse' {} a -> s {httpStatus = a} :: DeleteTimelineEventResponse)

instance Prelude.NFData DeleteTimelineEventResponse where
  rnf DeleteTimelineEventResponse' {..} =
    Prelude.rnf httpStatus
