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
-- Module      : Network.AWS.SSMIncidents.DeleteTimelineEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a timeline event from an incident.
module Network.AWS.SSMIncidents.DeleteTimelineEvent
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSMIncidents.Types

-- | /See:/ 'newDeleteTimelineEvent' smart constructor.
data DeleteTimelineEvent = DeleteTimelineEvent'
  { -- | The ID of the event you are updating. You can find this by using
    -- @ListTimelineEvents@.
    eventId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the incident that the event is part
    -- of.
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
-- 'eventId', 'deleteTimelineEvent_eventId' - The ID of the event you are updating. You can find this by using
-- @ListTimelineEvents@.
--
-- 'incidentRecordArn', 'deleteTimelineEvent_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident that the event is part
-- of.
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

-- | The ID of the event you are updating. You can find this by using
-- @ListTimelineEvents@.
deleteTimelineEvent_eventId :: Lens.Lens' DeleteTimelineEvent Prelude.Text
deleteTimelineEvent_eventId = Lens.lens (\DeleteTimelineEvent' {eventId} -> eventId) (\s@DeleteTimelineEvent' {} a -> s {eventId = a} :: DeleteTimelineEvent)

-- | The Amazon Resource Name (ARN) of the incident that the event is part
-- of.
deleteTimelineEvent_incidentRecordArn :: Lens.Lens' DeleteTimelineEvent Prelude.Text
deleteTimelineEvent_incidentRecordArn = Lens.lens (\DeleteTimelineEvent' {incidentRecordArn} -> incidentRecordArn) (\s@DeleteTimelineEvent' {} a -> s {incidentRecordArn = a} :: DeleteTimelineEvent)

instance Core.AWSRequest DeleteTimelineEvent where
  type
    AWSResponse DeleteTimelineEvent =
      DeleteTimelineEventResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTimelineEventResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTimelineEvent

instance Prelude.NFData DeleteTimelineEvent

instance Core.ToHeaders DeleteTimelineEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteTimelineEvent where
  toJSON DeleteTimelineEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("eventId" Core..= eventId),
            Prelude.Just
              ("incidentRecordArn" Core..= incidentRecordArn)
          ]
      )

instance Core.ToPath DeleteTimelineEvent where
  toPath = Prelude.const "/deleteTimelineEvent"

instance Core.ToQuery DeleteTimelineEvent where
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

instance Prelude.NFData DeleteTimelineEventResponse
