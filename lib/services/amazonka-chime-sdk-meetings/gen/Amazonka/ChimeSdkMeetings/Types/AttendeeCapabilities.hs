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
-- Module      : Amazonka.ChimeSdkMeetings.Types.AttendeeCapabilities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.AttendeeCapabilities where

import Amazonka.ChimeSdkMeetings.Types.MediaCapabilities
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The media capabilities of an attendee: audio, video, or content.
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
-- /See:/ 'newAttendeeCapabilities' smart constructor.
data AttendeeCapabilities = AttendeeCapabilities'
  { -- | The audio capability assigned to an attendee.
    audio :: MediaCapabilities,
    -- | The video capability assigned to an attendee.
    video :: MediaCapabilities,
    -- | The content capability assigned to an attendee.
    content :: MediaCapabilities
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttendeeCapabilities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audio', 'attendeeCapabilities_audio' - The audio capability assigned to an attendee.
--
-- 'video', 'attendeeCapabilities_video' - The video capability assigned to an attendee.
--
-- 'content', 'attendeeCapabilities_content' - The content capability assigned to an attendee.
newAttendeeCapabilities ::
  -- | 'audio'
  MediaCapabilities ->
  -- | 'video'
  MediaCapabilities ->
  -- | 'content'
  MediaCapabilities ->
  AttendeeCapabilities
newAttendeeCapabilities pAudio_ pVideo_ pContent_ =
  AttendeeCapabilities'
    { audio = pAudio_,
      video = pVideo_,
      content = pContent_
    }

-- | The audio capability assigned to an attendee.
attendeeCapabilities_audio :: Lens.Lens' AttendeeCapabilities MediaCapabilities
attendeeCapabilities_audio = Lens.lens (\AttendeeCapabilities' {audio} -> audio) (\s@AttendeeCapabilities' {} a -> s {audio = a} :: AttendeeCapabilities)

-- | The video capability assigned to an attendee.
attendeeCapabilities_video :: Lens.Lens' AttendeeCapabilities MediaCapabilities
attendeeCapabilities_video = Lens.lens (\AttendeeCapabilities' {video} -> video) (\s@AttendeeCapabilities' {} a -> s {video = a} :: AttendeeCapabilities)

-- | The content capability assigned to an attendee.
attendeeCapabilities_content :: Lens.Lens' AttendeeCapabilities MediaCapabilities
attendeeCapabilities_content = Lens.lens (\AttendeeCapabilities' {content} -> content) (\s@AttendeeCapabilities' {} a -> s {content = a} :: AttendeeCapabilities)

instance Data.FromJSON AttendeeCapabilities where
  parseJSON =
    Data.withObject
      "AttendeeCapabilities"
      ( \x ->
          AttendeeCapabilities'
            Prelude.<$> (x Data..: "Audio")
            Prelude.<*> (x Data..: "Video")
            Prelude.<*> (x Data..: "Content")
      )

instance Prelude.Hashable AttendeeCapabilities where
  hashWithSalt _salt AttendeeCapabilities' {..} =
    _salt `Prelude.hashWithSalt` audio
      `Prelude.hashWithSalt` video
      `Prelude.hashWithSalt` content

instance Prelude.NFData AttendeeCapabilities where
  rnf AttendeeCapabilities' {..} =
    Prelude.rnf audio
      `Prelude.seq` Prelude.rnf video
      `Prelude.seq` Prelude.rnf content

instance Data.ToJSON AttendeeCapabilities where
  toJSON AttendeeCapabilities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Audio" Data..= audio),
            Prelude.Just ("Video" Data..= video),
            Prelude.Just ("Content" Data..= content)
          ]
      )
