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
-- Module      : Amazonka.ChimeSdkMeetings.CreateAttendee
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new attendee for an active Amazon Chime SDK meeting. For more
-- information about the Amazon Chime SDK, see
-- <https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html Using the Amazon Chime SDK>
-- in the /Amazon Chime Developer Guide/.
module Amazonka.ChimeSdkMeetings.CreateAttendee
  ( -- * Creating a Request
    CreateAttendee (..),
    newCreateAttendee,

    -- * Request Lenses
    createAttendee_capabilities,
    createAttendee_meetingId,
    createAttendee_externalUserId,

    -- * Destructuring the Response
    CreateAttendeeResponse (..),
    newCreateAttendeeResponse,

    -- * Response Lenses
    createAttendeeResponse_attendee,
    createAttendeeResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMeetings.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAttendee' smart constructor.
data CreateAttendee = CreateAttendee'
  { -- | The capabilities (@audio@, @video@, or @content@) that you want to grant
    -- an attendee. If you don\'t specify capabilities, all users have send and
    -- receive capabilities on all media channels by default.
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
    capabilities :: Prelude.Maybe AttendeeCapabilities,
    -- | The unique ID of the meeting.
    meetingId :: Prelude.Text,
    -- | The Amazon Chime SDK external user ID. An idempotency token. Links the
    -- attendee to an identity managed by a builder application.
    externalUserId :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAttendee' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilities', 'createAttendee_capabilities' - The capabilities (@audio@, @video@, or @content@) that you want to grant
-- an attendee. If you don\'t specify capabilities, all users have send and
-- receive capabilities on all media channels by default.
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
--
-- 'meetingId', 'createAttendee_meetingId' - The unique ID of the meeting.
--
-- 'externalUserId', 'createAttendee_externalUserId' - The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
newCreateAttendee ::
  -- | 'meetingId'
  Prelude.Text ->
  -- | 'externalUserId'
  Prelude.Text ->
  CreateAttendee
newCreateAttendee pMeetingId_ pExternalUserId_ =
  CreateAttendee'
    { capabilities = Prelude.Nothing,
      meetingId = pMeetingId_,
      externalUserId =
        Core._Sensitive Lens.# pExternalUserId_
    }

-- | The capabilities (@audio@, @video@, or @content@) that you want to grant
-- an attendee. If you don\'t specify capabilities, all users have send and
-- receive capabilities on all media channels by default.
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
createAttendee_capabilities :: Lens.Lens' CreateAttendee (Prelude.Maybe AttendeeCapabilities)
createAttendee_capabilities = Lens.lens (\CreateAttendee' {capabilities} -> capabilities) (\s@CreateAttendee' {} a -> s {capabilities = a} :: CreateAttendee)

-- | The unique ID of the meeting.
createAttendee_meetingId :: Lens.Lens' CreateAttendee Prelude.Text
createAttendee_meetingId = Lens.lens (\CreateAttendee' {meetingId} -> meetingId) (\s@CreateAttendee' {} a -> s {meetingId = a} :: CreateAttendee)

-- | The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
createAttendee_externalUserId :: Lens.Lens' CreateAttendee Prelude.Text
createAttendee_externalUserId = Lens.lens (\CreateAttendee' {externalUserId} -> externalUserId) (\s@CreateAttendee' {} a -> s {externalUserId = a} :: CreateAttendee) Prelude.. Core._Sensitive

instance Core.AWSRequest CreateAttendee where
  type
    AWSResponse CreateAttendee =
      CreateAttendeeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAttendeeResponse'
            Prelude.<$> (x Core..?> "Attendee")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAttendee where
  hashWithSalt _salt CreateAttendee' {..} =
    _salt `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` meetingId
      `Prelude.hashWithSalt` externalUserId

instance Prelude.NFData CreateAttendee where
  rnf CreateAttendee' {..} =
    Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf externalUserId

instance Core.ToHeaders CreateAttendee where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateAttendee where
  toJSON CreateAttendee' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Capabilities" Core..=) Prelude.<$> capabilities,
            Prelude.Just
              ("ExternalUserId" Core..= externalUserId)
          ]
      )

instance Core.ToPath CreateAttendee where
  toPath CreateAttendee' {..} =
    Prelude.mconcat
      ["/meetings/", Core.toBS meetingId, "/attendees"]

instance Core.ToQuery CreateAttendee where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAttendeeResponse' smart constructor.
data CreateAttendeeResponse = CreateAttendeeResponse'
  { -- | The attendee information, including attendee ID and join token.
    attendee :: Prelude.Maybe Attendee,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAttendeeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attendee', 'createAttendeeResponse_attendee' - The attendee information, including attendee ID and join token.
--
-- 'httpStatus', 'createAttendeeResponse_httpStatus' - The response's http status code.
newCreateAttendeeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAttendeeResponse
newCreateAttendeeResponse pHttpStatus_ =
  CreateAttendeeResponse'
    { attendee = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attendee information, including attendee ID and join token.
createAttendeeResponse_attendee :: Lens.Lens' CreateAttendeeResponse (Prelude.Maybe Attendee)
createAttendeeResponse_attendee = Lens.lens (\CreateAttendeeResponse' {attendee} -> attendee) (\s@CreateAttendeeResponse' {} a -> s {attendee = a} :: CreateAttendeeResponse)

-- | The response's http status code.
createAttendeeResponse_httpStatus :: Lens.Lens' CreateAttendeeResponse Prelude.Int
createAttendeeResponse_httpStatus = Lens.lens (\CreateAttendeeResponse' {httpStatus} -> httpStatus) (\s@CreateAttendeeResponse' {} a -> s {httpStatus = a} :: CreateAttendeeResponse)

instance Prelude.NFData CreateAttendeeResponse where
  rnf CreateAttendeeResponse' {..} =
    Prelude.rnf attendee
      `Prelude.seq` Prelude.rnf httpStatus
