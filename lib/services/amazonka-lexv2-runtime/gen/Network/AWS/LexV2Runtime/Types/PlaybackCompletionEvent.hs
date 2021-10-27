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
-- Module      : Network.AWS.LexV2Runtime.Types.PlaybackCompletionEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.PlaybackCompletionEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Event sent from the client application to Amazon Lex V2 to indicate that
-- playback of audio is complete and that Amazon Lex V2 should start
-- processing the user\'s input.
--
-- /See:/ 'newPlaybackCompletionEvent' smart constructor.
data PlaybackCompletionEvent = PlaybackCompletionEvent'
  { -- | A timestamp set by the client of the date and time that the event was
    -- sent to Amazon Lex V2.
    clientTimestampMillis :: Prelude.Maybe Prelude.Integer,
    -- | A unique identifier that your application assigns to the event. You can
    -- use this to identify events in logs.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlaybackCompletionEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientTimestampMillis', 'playbackCompletionEvent_clientTimestampMillis' - A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
--
-- 'eventId', 'playbackCompletionEvent_eventId' - A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
newPlaybackCompletionEvent ::
  PlaybackCompletionEvent
newPlaybackCompletionEvent =
  PlaybackCompletionEvent'
    { clientTimestampMillis =
        Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
playbackCompletionEvent_clientTimestampMillis :: Lens.Lens' PlaybackCompletionEvent (Prelude.Maybe Prelude.Integer)
playbackCompletionEvent_clientTimestampMillis = Lens.lens (\PlaybackCompletionEvent' {clientTimestampMillis} -> clientTimestampMillis) (\s@PlaybackCompletionEvent' {} a -> s {clientTimestampMillis = a} :: PlaybackCompletionEvent)

-- | A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
playbackCompletionEvent_eventId :: Lens.Lens' PlaybackCompletionEvent (Prelude.Maybe Prelude.Text)
playbackCompletionEvent_eventId = Lens.lens (\PlaybackCompletionEvent' {eventId} -> eventId) (\s@PlaybackCompletionEvent' {} a -> s {eventId = a} :: PlaybackCompletionEvent)

instance Prelude.Hashable PlaybackCompletionEvent

instance Prelude.NFData PlaybackCompletionEvent

instance Core.ToJSON PlaybackCompletionEvent where
  toJSON PlaybackCompletionEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientTimestampMillis" Core..=)
              Prelude.<$> clientTimestampMillis,
            ("eventId" Core..=) Prelude.<$> eventId
          ]
      )
