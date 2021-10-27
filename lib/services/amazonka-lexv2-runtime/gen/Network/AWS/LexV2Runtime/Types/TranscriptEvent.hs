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
-- Module      : Network.AWS.LexV2Runtime.Types.TranscriptEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.TranscriptEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Event sent from Amazon Lex V2 to your client application that contains a
-- transcript of voice audio.
--
-- /See:/ 'newTranscriptEvent' smart constructor.
data TranscriptEvent = TranscriptEvent'
  { -- | The transcript of the voice audio from the user.
    transcript :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier of the event sent by Amazon Lex V2. The identifier
    -- is in the form @RESPONSE-N@, where N is a number starting with one and
    -- incremented for each event sent by Amazon Lex V2 in the current session.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranscriptEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcript', 'transcriptEvent_transcript' - The transcript of the voice audio from the user.
--
-- 'eventId', 'transcriptEvent_eventId' - A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
newTranscriptEvent ::
  TranscriptEvent
newTranscriptEvent =
  TranscriptEvent'
    { transcript = Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | The transcript of the voice audio from the user.
transcriptEvent_transcript :: Lens.Lens' TranscriptEvent (Prelude.Maybe Prelude.Text)
transcriptEvent_transcript = Lens.lens (\TranscriptEvent' {transcript} -> transcript) (\s@TranscriptEvent' {} a -> s {transcript = a} :: TranscriptEvent)

-- | A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
transcriptEvent_eventId :: Lens.Lens' TranscriptEvent (Prelude.Maybe Prelude.Text)
transcriptEvent_eventId = Lens.lens (\TranscriptEvent' {eventId} -> eventId) (\s@TranscriptEvent' {} a -> s {eventId = a} :: TranscriptEvent)

instance Core.FromJSON TranscriptEvent where
  parseJSON =
    Core.withObject
      "TranscriptEvent"
      ( \x ->
          TranscriptEvent'
            Prelude.<$> (x Core..:? "transcript")
            Prelude.<*> (x Core..:? "eventId")
      )

instance Prelude.Hashable TranscriptEvent

instance Prelude.NFData TranscriptEvent
