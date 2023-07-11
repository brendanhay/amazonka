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
-- Module      : Amazonka.ChimeSdkMeetings.UpdateAttendeeCapabilities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The capabilties that you want to update.
--
-- You use the capabilities with a set of values that control what the
-- capabilities can do, such as @SendReceive@ data. For more information
-- about those values, see .
--
-- When using capabilities, be aware of these corner cases:
--
-- -   You can\'t set @content@ capabilities to @SendReceive@ or @Receive@
--     unless you also set @video@ capabilities to @SendReceive@ or
--     @Receive@. If you don\'t set the @video@ capability to receive, the
--     response will contain an HTTP 400 Bad Request status code. However,
--     you can set your @video@ capability to receive and you set your
--     @content@ capability to not receive.
--
-- -   When you change an @audio@ capability from @None@ or @Receive@ to
--     @Send@ or @SendReceive@ , and if the attendee left their microphone
--     unmuted, audio will flow from the attendee to the other meeting
--     participants.
--
-- -   When you change a @video@ or @content@ capability from @None@ or
--     @Receive@ to @Send@ or @SendReceive@ , and if the attendee turned on
--     their video or content streams, remote attendess can receive those
--     streams, but only after media renegotiation between the client and
--     the Amazon Chime back-end server.
module Amazonka.ChimeSdkMeetings.UpdateAttendeeCapabilities
  ( -- * Creating a Request
    UpdateAttendeeCapabilities (..),
    newUpdateAttendeeCapabilities,

    -- * Request Lenses
    updateAttendeeCapabilities_meetingId,
    updateAttendeeCapabilities_attendeeId,
    updateAttendeeCapabilities_capabilities,

    -- * Destructuring the Response
    UpdateAttendeeCapabilitiesResponse (..),
    newUpdateAttendeeCapabilitiesResponse,

    -- * Response Lenses
    updateAttendeeCapabilitiesResponse_attendee,
    updateAttendeeCapabilitiesResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMeetings.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAttendeeCapabilities' smart constructor.
data UpdateAttendeeCapabilities = UpdateAttendeeCapabilities'
  { -- | The ID of the meeting associated with the update request.
    meetingId :: Prelude.Text,
    -- | The ID of the attendee associated with the update request.
    attendeeId :: Prelude.Text,
    -- | The capabilties that you want to update.
    capabilities :: AttendeeCapabilities
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAttendeeCapabilities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'updateAttendeeCapabilities_meetingId' - The ID of the meeting associated with the update request.
--
-- 'attendeeId', 'updateAttendeeCapabilities_attendeeId' - The ID of the attendee associated with the update request.
--
-- 'capabilities', 'updateAttendeeCapabilities_capabilities' - The capabilties that you want to update.
newUpdateAttendeeCapabilities ::
  -- | 'meetingId'
  Prelude.Text ->
  -- | 'attendeeId'
  Prelude.Text ->
  -- | 'capabilities'
  AttendeeCapabilities ->
  UpdateAttendeeCapabilities
newUpdateAttendeeCapabilities
  pMeetingId_
  pAttendeeId_
  pCapabilities_ =
    UpdateAttendeeCapabilities'
      { meetingId =
          pMeetingId_,
        attendeeId = pAttendeeId_,
        capabilities = pCapabilities_
      }

-- | The ID of the meeting associated with the update request.
updateAttendeeCapabilities_meetingId :: Lens.Lens' UpdateAttendeeCapabilities Prelude.Text
updateAttendeeCapabilities_meetingId = Lens.lens (\UpdateAttendeeCapabilities' {meetingId} -> meetingId) (\s@UpdateAttendeeCapabilities' {} a -> s {meetingId = a} :: UpdateAttendeeCapabilities)

-- | The ID of the attendee associated with the update request.
updateAttendeeCapabilities_attendeeId :: Lens.Lens' UpdateAttendeeCapabilities Prelude.Text
updateAttendeeCapabilities_attendeeId = Lens.lens (\UpdateAttendeeCapabilities' {attendeeId} -> attendeeId) (\s@UpdateAttendeeCapabilities' {} a -> s {attendeeId = a} :: UpdateAttendeeCapabilities)

-- | The capabilties that you want to update.
updateAttendeeCapabilities_capabilities :: Lens.Lens' UpdateAttendeeCapabilities AttendeeCapabilities
updateAttendeeCapabilities_capabilities = Lens.lens (\UpdateAttendeeCapabilities' {capabilities} -> capabilities) (\s@UpdateAttendeeCapabilities' {} a -> s {capabilities = a} :: UpdateAttendeeCapabilities)

instance Core.AWSRequest UpdateAttendeeCapabilities where
  type
    AWSResponse UpdateAttendeeCapabilities =
      UpdateAttendeeCapabilitiesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAttendeeCapabilitiesResponse'
            Prelude.<$> (x Data..?> "Attendee")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAttendeeCapabilities where
  hashWithSalt _salt UpdateAttendeeCapabilities' {..} =
    _salt
      `Prelude.hashWithSalt` meetingId
      `Prelude.hashWithSalt` attendeeId
      `Prelude.hashWithSalt` capabilities

instance Prelude.NFData UpdateAttendeeCapabilities where
  rnf UpdateAttendeeCapabilities' {..} =
    Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf attendeeId
      `Prelude.seq` Prelude.rnf capabilities

instance Data.ToHeaders UpdateAttendeeCapabilities where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateAttendeeCapabilities where
  toJSON UpdateAttendeeCapabilities' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Capabilities" Data..= capabilities)]
      )

instance Data.ToPath UpdateAttendeeCapabilities where
  toPath UpdateAttendeeCapabilities' {..} =
    Prelude.mconcat
      [ "/meetings/",
        Data.toBS meetingId,
        "/attendees/",
        Data.toBS attendeeId,
        "/capabilities"
      ]

instance Data.ToQuery UpdateAttendeeCapabilities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAttendeeCapabilitiesResponse' smart constructor.
data UpdateAttendeeCapabilitiesResponse = UpdateAttendeeCapabilitiesResponse'
  { -- | The updated attendee data.
    attendee :: Prelude.Maybe Attendee,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAttendeeCapabilitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attendee', 'updateAttendeeCapabilitiesResponse_attendee' - The updated attendee data.
--
-- 'httpStatus', 'updateAttendeeCapabilitiesResponse_httpStatus' - The response's http status code.
newUpdateAttendeeCapabilitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAttendeeCapabilitiesResponse
newUpdateAttendeeCapabilitiesResponse pHttpStatus_ =
  UpdateAttendeeCapabilitiesResponse'
    { attendee =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated attendee data.
updateAttendeeCapabilitiesResponse_attendee :: Lens.Lens' UpdateAttendeeCapabilitiesResponse (Prelude.Maybe Attendee)
updateAttendeeCapabilitiesResponse_attendee = Lens.lens (\UpdateAttendeeCapabilitiesResponse' {attendee} -> attendee) (\s@UpdateAttendeeCapabilitiesResponse' {} a -> s {attendee = a} :: UpdateAttendeeCapabilitiesResponse)

-- | The response's http status code.
updateAttendeeCapabilitiesResponse_httpStatus :: Lens.Lens' UpdateAttendeeCapabilitiesResponse Prelude.Int
updateAttendeeCapabilitiesResponse_httpStatus = Lens.lens (\UpdateAttendeeCapabilitiesResponse' {httpStatus} -> httpStatus) (\s@UpdateAttendeeCapabilitiesResponse' {} a -> s {httpStatus = a} :: UpdateAttendeeCapabilitiesResponse)

instance
  Prelude.NFData
    UpdateAttendeeCapabilitiesResponse
  where
  rnf UpdateAttendeeCapabilitiesResponse' {..} =
    Prelude.rnf attendee
      `Prelude.seq` Prelude.rnf httpStatus
