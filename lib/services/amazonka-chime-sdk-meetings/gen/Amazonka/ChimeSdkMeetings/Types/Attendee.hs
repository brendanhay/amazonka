{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkMeetings.Types.Attendee
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.Attendee where

import Amazonka.ChimeSdkMeetings.Types.AttendeeCapabilities
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Amazon Chime SDK meeting attendee. Includes a unique @AttendeeId@ and
-- @JoinToken@. The @JoinToken@ allows a client to authenticate and join as
-- the specified attendee. The @JoinToken@ expires when the meeting ends,
-- or when DeleteAttendee is called. After that, the attendee is unable to
-- join the meeting.
--
-- We recommend securely transferring each @JoinToken@ from your server
-- application to the client so that no other client has access to the
-- token except for the one authorized to represent the attendee.
--
-- /See:/ 'newAttendee' smart constructor.
data Attendee = Attendee'
  { -- | The Amazon Chime SDK attendee ID.
    attendeeId :: Prelude.Maybe Prelude.Text,
    -- | The capabilities assigned to an attendee: audio, video, or content.
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
    -- | The Amazon Chime SDK external user ID. An idempotency token. Links the
    -- attendee to an identity managed by a builder application.
    externalUserId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The join token used by the Amazon Chime SDK attendee.
    joinToken :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attendee' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attendeeId', 'attendee_attendeeId' - The Amazon Chime SDK attendee ID.
--
-- 'capabilities', 'attendee_capabilities' - The capabilities assigned to an attendee: audio, video, or content.
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
-- 'externalUserId', 'attendee_externalUserId' - The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
--
-- 'joinToken', 'attendee_joinToken' - The join token used by the Amazon Chime SDK attendee.
newAttendee ::
  Attendee
newAttendee =
  Attendee'
    { attendeeId = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      externalUserId = Prelude.Nothing,
      joinToken = Prelude.Nothing
    }

-- | The Amazon Chime SDK attendee ID.
attendee_attendeeId :: Lens.Lens' Attendee (Prelude.Maybe Prelude.Text)
attendee_attendeeId = Lens.lens (\Attendee' {attendeeId} -> attendeeId) (\s@Attendee' {} a -> s {attendeeId = a} :: Attendee)

-- | The capabilities assigned to an attendee: audio, video, or content.
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
attendee_capabilities :: Lens.Lens' Attendee (Prelude.Maybe AttendeeCapabilities)
attendee_capabilities = Lens.lens (\Attendee' {capabilities} -> capabilities) (\s@Attendee' {} a -> s {capabilities = a} :: Attendee)

-- | The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
attendee_externalUserId :: Lens.Lens' Attendee (Prelude.Maybe Prelude.Text)
attendee_externalUserId = Lens.lens (\Attendee' {externalUserId} -> externalUserId) (\s@Attendee' {} a -> s {externalUserId = a} :: Attendee) Prelude.. Lens.mapping Data._Sensitive

-- | The join token used by the Amazon Chime SDK attendee.
attendee_joinToken :: Lens.Lens' Attendee (Prelude.Maybe Prelude.Text)
attendee_joinToken = Lens.lens (\Attendee' {joinToken} -> joinToken) (\s@Attendee' {} a -> s {joinToken = a} :: Attendee) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Attendee where
  parseJSON =
    Data.withObject
      "Attendee"
      ( \x ->
          Attendee'
            Prelude.<$> (x Data..:? "AttendeeId")
            Prelude.<*> (x Data..:? "Capabilities")
            Prelude.<*> (x Data..:? "ExternalUserId")
            Prelude.<*> (x Data..:? "JoinToken")
      )

instance Prelude.Hashable Attendee where
  hashWithSalt _salt Attendee' {..} =
    _salt
      `Prelude.hashWithSalt` attendeeId
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` externalUserId
      `Prelude.hashWithSalt` joinToken

instance Prelude.NFData Attendee where
  rnf Attendee' {..} =
    Prelude.rnf attendeeId
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf externalUserId
      `Prelude.seq` Prelude.rnf joinToken
