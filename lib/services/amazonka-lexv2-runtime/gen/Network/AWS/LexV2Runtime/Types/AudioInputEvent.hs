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
-- Module      : Network.AWS.LexV2Runtime.Types.AudioInputEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.AudioInputEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a chunk of audio sent from the client application to Amazon
-- Lex V2. The audio is all or part of an utterance from the user.
--
-- Amazon Lex V2 accumulates audio chunks until it recognizes a natural
-- pause in speech before processing the input.
--
-- /See:/ 'newAudioInputEvent' smart constructor.
data AudioInputEvent = AudioInputEvent'
  { -- | An encoded stream of audio.
    audioChunk :: Prelude.Maybe Core.Base64,
    -- | A timestamp set by the client of the date and time that the event was
    -- sent to Amazon Lex V2.
    clientTimestampMillis :: Prelude.Maybe Prelude.Integer,
    -- | A unique identifier that your application assigns to the event. You can
    -- use this to identify events in logs.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The encoding used for the audio chunk. You must use 8 KHz PCM 16-bit
    -- mono-channel little-endian format. The value of the field should be:
    --
    -- @audio\/lpcm; sample-rate=8000; sample-size-bits=16; channel-count=1; is-big-endian=false@
    contentType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioInputEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioChunk', 'audioInputEvent_audioChunk' - An encoded stream of audio.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'clientTimestampMillis', 'audioInputEvent_clientTimestampMillis' - A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
--
-- 'eventId', 'audioInputEvent_eventId' - A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
--
-- 'contentType', 'audioInputEvent_contentType' - The encoding used for the audio chunk. You must use 8 KHz PCM 16-bit
-- mono-channel little-endian format. The value of the field should be:
--
-- @audio\/lpcm; sample-rate=8000; sample-size-bits=16; channel-count=1; is-big-endian=false@
newAudioInputEvent ::
  -- | 'contentType'
  Prelude.Text ->
  AudioInputEvent
newAudioInputEvent pContentType_ =
  AudioInputEvent'
    { audioChunk = Prelude.Nothing,
      clientTimestampMillis = Prelude.Nothing,
      eventId = Prelude.Nothing,
      contentType = pContentType_
    }

-- | An encoded stream of audio.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
audioInputEvent_audioChunk :: Lens.Lens' AudioInputEvent (Prelude.Maybe Prelude.ByteString)
audioInputEvent_audioChunk = Lens.lens (\AudioInputEvent' {audioChunk} -> audioChunk) (\s@AudioInputEvent' {} a -> s {audioChunk = a} :: AudioInputEvent) Prelude.. Lens.mapping Core._Base64

-- | A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
audioInputEvent_clientTimestampMillis :: Lens.Lens' AudioInputEvent (Prelude.Maybe Prelude.Integer)
audioInputEvent_clientTimestampMillis = Lens.lens (\AudioInputEvent' {clientTimestampMillis} -> clientTimestampMillis) (\s@AudioInputEvent' {} a -> s {clientTimestampMillis = a} :: AudioInputEvent)

-- | A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
audioInputEvent_eventId :: Lens.Lens' AudioInputEvent (Prelude.Maybe Prelude.Text)
audioInputEvent_eventId = Lens.lens (\AudioInputEvent' {eventId} -> eventId) (\s@AudioInputEvent' {} a -> s {eventId = a} :: AudioInputEvent)

-- | The encoding used for the audio chunk. You must use 8 KHz PCM 16-bit
-- mono-channel little-endian format. The value of the field should be:
--
-- @audio\/lpcm; sample-rate=8000; sample-size-bits=16; channel-count=1; is-big-endian=false@
audioInputEvent_contentType :: Lens.Lens' AudioInputEvent Prelude.Text
audioInputEvent_contentType = Lens.lens (\AudioInputEvent' {contentType} -> contentType) (\s@AudioInputEvent' {} a -> s {contentType = a} :: AudioInputEvent)

instance Prelude.Hashable AudioInputEvent

instance Prelude.NFData AudioInputEvent

instance Core.ToJSON AudioInputEvent where
  toJSON AudioInputEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("audioChunk" Core..=) Prelude.<$> audioChunk,
            ("clientTimestampMillis" Core..=)
              Prelude.<$> clientTimestampMillis,
            ("eventId" Core..=) Prelude.<$> eventId,
            Prelude.Just ("contentType" Core..= contentType)
          ]
      )
