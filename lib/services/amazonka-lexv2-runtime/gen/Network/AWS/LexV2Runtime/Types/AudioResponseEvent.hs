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
-- Module      : Network.AWS.LexV2Runtime.Types.AudioResponseEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.AudioResponseEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An event sent from Amazon Lex V2 to your client application containing
-- audio to play to the user.
--
-- /See:/ 'newAudioResponseEvent' smart constructor.
data AudioResponseEvent = AudioResponseEvent'
  { -- | A chunk of the audio to play.
    audioChunk :: Prelude.Maybe Core.Base64,
    -- | The encoding of the audio chunk. This is the same as the encoding
    -- configure in the @contentType@ field of the @ConfigurationEvent@.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier of the event sent by Amazon Lex V2. The identifier
    -- is in the form @RESPONSE-N@, where N is a number starting with one and
    -- incremented for each event sent by Amazon Lex V2 in the current session.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioResponseEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioChunk', 'audioResponseEvent_audioChunk' - A chunk of the audio to play.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'contentType', 'audioResponseEvent_contentType' - The encoding of the audio chunk. This is the same as the encoding
-- configure in the @contentType@ field of the @ConfigurationEvent@.
--
-- 'eventId', 'audioResponseEvent_eventId' - A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
newAudioResponseEvent ::
  AudioResponseEvent
newAudioResponseEvent =
  AudioResponseEvent'
    { audioChunk = Prelude.Nothing,
      contentType = Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | A chunk of the audio to play.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
audioResponseEvent_audioChunk :: Lens.Lens' AudioResponseEvent (Prelude.Maybe Prelude.ByteString)
audioResponseEvent_audioChunk = Lens.lens (\AudioResponseEvent' {audioChunk} -> audioChunk) (\s@AudioResponseEvent' {} a -> s {audioChunk = a} :: AudioResponseEvent) Prelude.. Lens.mapping Core._Base64

-- | The encoding of the audio chunk. This is the same as the encoding
-- configure in the @contentType@ field of the @ConfigurationEvent@.
audioResponseEvent_contentType :: Lens.Lens' AudioResponseEvent (Prelude.Maybe Prelude.Text)
audioResponseEvent_contentType = Lens.lens (\AudioResponseEvent' {contentType} -> contentType) (\s@AudioResponseEvent' {} a -> s {contentType = a} :: AudioResponseEvent)

-- | A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
audioResponseEvent_eventId :: Lens.Lens' AudioResponseEvent (Prelude.Maybe Prelude.Text)
audioResponseEvent_eventId = Lens.lens (\AudioResponseEvent' {eventId} -> eventId) (\s@AudioResponseEvent' {} a -> s {eventId = a} :: AudioResponseEvent)

instance Core.FromJSON AudioResponseEvent where
  parseJSON =
    Core.withObject
      "AudioResponseEvent"
      ( \x ->
          AudioResponseEvent'
            Prelude.<$> (x Core..:? "audioChunk")
            Prelude.<*> (x Core..:? "contentType")
            Prelude.<*> (x Core..:? "eventId")
      )

instance Prelude.Hashable AudioResponseEvent

instance Prelude.NFData AudioResponseEvent
