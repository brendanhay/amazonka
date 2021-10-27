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
-- Module      : Network.AWS.LexV2Runtime.Types.PlaybackInterruptionEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.PlaybackInterruptionEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.PlaybackInterruptionReason
import qualified Network.AWS.Prelude as Prelude

-- | Event sent from Amazon Lex V2 to indicate to the client application
-- should stop playback of audio. For example, if the client is playing a
-- prompt that asks for the user\'s telephone number, the user might start
-- to say the phone number before the prompt is complete. Amazon Lex V2
-- sends this event to the client application to indicate that the user is
-- responding and that Amazon Lex V2 is processing their input.
--
-- /See:/ 'newPlaybackInterruptionEvent' smart constructor.
data PlaybackInterruptionEvent = PlaybackInterruptionEvent'
  { -- | The identifier of the event that contained the audio, DTMF, or text that
    -- caused the interruption.
    causedByEventId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the type of user input that Amazon Lex V2 detected.
    eventReason :: Prelude.Maybe PlaybackInterruptionReason,
    -- | A unique identifier of the event sent by Amazon Lex V2. The identifier
    -- is in the form @RESPONSE-N@, where N is a number starting with one and
    -- incremented for each event sent by Amazon Lex V2 in the current session.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlaybackInterruptionEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'causedByEventId', 'playbackInterruptionEvent_causedByEventId' - The identifier of the event that contained the audio, DTMF, or text that
-- caused the interruption.
--
-- 'eventReason', 'playbackInterruptionEvent_eventReason' - Indicates the type of user input that Amazon Lex V2 detected.
--
-- 'eventId', 'playbackInterruptionEvent_eventId' - A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
newPlaybackInterruptionEvent ::
  PlaybackInterruptionEvent
newPlaybackInterruptionEvent =
  PlaybackInterruptionEvent'
    { causedByEventId =
        Prelude.Nothing,
      eventReason = Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | The identifier of the event that contained the audio, DTMF, or text that
-- caused the interruption.
playbackInterruptionEvent_causedByEventId :: Lens.Lens' PlaybackInterruptionEvent (Prelude.Maybe Prelude.Text)
playbackInterruptionEvent_causedByEventId = Lens.lens (\PlaybackInterruptionEvent' {causedByEventId} -> causedByEventId) (\s@PlaybackInterruptionEvent' {} a -> s {causedByEventId = a} :: PlaybackInterruptionEvent)

-- | Indicates the type of user input that Amazon Lex V2 detected.
playbackInterruptionEvent_eventReason :: Lens.Lens' PlaybackInterruptionEvent (Prelude.Maybe PlaybackInterruptionReason)
playbackInterruptionEvent_eventReason = Lens.lens (\PlaybackInterruptionEvent' {eventReason} -> eventReason) (\s@PlaybackInterruptionEvent' {} a -> s {eventReason = a} :: PlaybackInterruptionEvent)

-- | A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
playbackInterruptionEvent_eventId :: Lens.Lens' PlaybackInterruptionEvent (Prelude.Maybe Prelude.Text)
playbackInterruptionEvent_eventId = Lens.lens (\PlaybackInterruptionEvent' {eventId} -> eventId) (\s@PlaybackInterruptionEvent' {} a -> s {eventId = a} :: PlaybackInterruptionEvent)

instance Core.FromJSON PlaybackInterruptionEvent where
  parseJSON =
    Core.withObject
      "PlaybackInterruptionEvent"
      ( \x ->
          PlaybackInterruptionEvent'
            Prelude.<$> (x Core..:? "causedByEventId")
            Prelude.<*> (x Core..:? "eventReason")
            Prelude.<*> (x Core..:? "eventId")
      )

instance Prelude.Hashable PlaybackInterruptionEvent

instance Prelude.NFData PlaybackInterruptionEvent
