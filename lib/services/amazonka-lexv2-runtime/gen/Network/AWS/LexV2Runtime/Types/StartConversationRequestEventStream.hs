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
-- Module      : Network.AWS.LexV2Runtime.Types.StartConversationRequestEventStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.StartConversationRequestEventStream where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.AudioInputEvent
import Network.AWS.LexV2Runtime.Types.ConfigurationEvent
import Network.AWS.LexV2Runtime.Types.DTMFInputEvent
import Network.AWS.LexV2Runtime.Types.DisconnectionEvent
import Network.AWS.LexV2Runtime.Types.PlaybackCompletionEvent
import Network.AWS.LexV2Runtime.Types.TextInputEvent
import qualified Network.AWS.Prelude as Prelude

-- | Represents a stream of events between your application and Amazon Lex
-- V2.
--
-- /See:/ 'newStartConversationRequestEventStream' smart constructor.
data StartConversationRequestEventStream = StartConversationRequestEventStream'
  { -- | Event sent from the client application to Amazon Lex V2 to indicate that
    -- it has finished playing audio and that Amazon Lex V2 should start
    -- listening for user input.
    playbackCompletionEvent :: Prelude.Maybe PlaybackCompletionEvent,
    -- | DTMF information sent to Amazon Lex V2 by your application. Amazon Lex
    -- V2 accumulates the DMTF information from when the user sends the first
    -- character and ends
    --
    -- -   when there\'s a pause longer that the value configured for the end
    --     timeout.
    --
    -- -   when there\'s a digit that is the configured end character.
    --
    -- -   when Amazon Lex V2 accumulates characters equal to the maximum DTMF
    --     character configuration.
    dTMFInputEvent :: Prelude.Maybe DTMFInputEvent,
    -- | Configuration information sent from your client application to Amazon
    -- Lex V2
    configurationEvent :: Prelude.Maybe ConfigurationEvent,
    -- | Event sent from the client application to indicate to Amazon Lex V2 that
    -- the conversation is over.
    disconnectionEvent :: Prelude.Maybe DisconnectionEvent,
    -- | Speech audio sent from your client application to Amazon Lex V2. Audio
    -- starts accumulating when Amazon Lex V2 identifies a voice and continues
    -- until a natural pause in the speech is found before processing.
    audioInputEvent :: Prelude.Maybe AudioInputEvent,
    -- | Text sent from your client application to Amazon Lex V2. Each
    -- @TextInputEvent@ is processed individually.
    textInputEvent :: Prelude.Maybe TextInputEvent
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConversationRequestEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playbackCompletionEvent', 'startConversationRequestEventStream_playbackCompletionEvent' - Event sent from the client application to Amazon Lex V2 to indicate that
-- it has finished playing audio and that Amazon Lex V2 should start
-- listening for user input.
--
-- 'dTMFInputEvent', 'startConversationRequestEventStream_dTMFInputEvent' - DTMF information sent to Amazon Lex V2 by your application. Amazon Lex
-- V2 accumulates the DMTF information from when the user sends the first
-- character and ends
--
-- -   when there\'s a pause longer that the value configured for the end
--     timeout.
--
-- -   when there\'s a digit that is the configured end character.
--
-- -   when Amazon Lex V2 accumulates characters equal to the maximum DTMF
--     character configuration.
--
-- 'configurationEvent', 'startConversationRequestEventStream_configurationEvent' - Configuration information sent from your client application to Amazon
-- Lex V2
--
-- 'disconnectionEvent', 'startConversationRequestEventStream_disconnectionEvent' - Event sent from the client application to indicate to Amazon Lex V2 that
-- the conversation is over.
--
-- 'audioInputEvent', 'startConversationRequestEventStream_audioInputEvent' - Speech audio sent from your client application to Amazon Lex V2. Audio
-- starts accumulating when Amazon Lex V2 identifies a voice and continues
-- until a natural pause in the speech is found before processing.
--
-- 'textInputEvent', 'startConversationRequestEventStream_textInputEvent' - Text sent from your client application to Amazon Lex V2. Each
-- @TextInputEvent@ is processed individually.
newStartConversationRequestEventStream ::
  StartConversationRequestEventStream
newStartConversationRequestEventStream =
  StartConversationRequestEventStream'
    { playbackCompletionEvent =
        Prelude.Nothing,
      dTMFInputEvent = Prelude.Nothing,
      configurationEvent = Prelude.Nothing,
      disconnectionEvent = Prelude.Nothing,
      audioInputEvent = Prelude.Nothing,
      textInputEvent = Prelude.Nothing
    }

-- | Event sent from the client application to Amazon Lex V2 to indicate that
-- it has finished playing audio and that Amazon Lex V2 should start
-- listening for user input.
startConversationRequestEventStream_playbackCompletionEvent :: Lens.Lens' StartConversationRequestEventStream (Prelude.Maybe PlaybackCompletionEvent)
startConversationRequestEventStream_playbackCompletionEvent = Lens.lens (\StartConversationRequestEventStream' {playbackCompletionEvent} -> playbackCompletionEvent) (\s@StartConversationRequestEventStream' {} a -> s {playbackCompletionEvent = a} :: StartConversationRequestEventStream)

-- | DTMF information sent to Amazon Lex V2 by your application. Amazon Lex
-- V2 accumulates the DMTF information from when the user sends the first
-- character and ends
--
-- -   when there\'s a pause longer that the value configured for the end
--     timeout.
--
-- -   when there\'s a digit that is the configured end character.
--
-- -   when Amazon Lex V2 accumulates characters equal to the maximum DTMF
--     character configuration.
startConversationRequestEventStream_dTMFInputEvent :: Lens.Lens' StartConversationRequestEventStream (Prelude.Maybe DTMFInputEvent)
startConversationRequestEventStream_dTMFInputEvent = Lens.lens (\StartConversationRequestEventStream' {dTMFInputEvent} -> dTMFInputEvent) (\s@StartConversationRequestEventStream' {} a -> s {dTMFInputEvent = a} :: StartConversationRequestEventStream)

-- | Configuration information sent from your client application to Amazon
-- Lex V2
startConversationRequestEventStream_configurationEvent :: Lens.Lens' StartConversationRequestEventStream (Prelude.Maybe ConfigurationEvent)
startConversationRequestEventStream_configurationEvent = Lens.lens (\StartConversationRequestEventStream' {configurationEvent} -> configurationEvent) (\s@StartConversationRequestEventStream' {} a -> s {configurationEvent = a} :: StartConversationRequestEventStream)

-- | Event sent from the client application to indicate to Amazon Lex V2 that
-- the conversation is over.
startConversationRequestEventStream_disconnectionEvent :: Lens.Lens' StartConversationRequestEventStream (Prelude.Maybe DisconnectionEvent)
startConversationRequestEventStream_disconnectionEvent = Lens.lens (\StartConversationRequestEventStream' {disconnectionEvent} -> disconnectionEvent) (\s@StartConversationRequestEventStream' {} a -> s {disconnectionEvent = a} :: StartConversationRequestEventStream)

-- | Speech audio sent from your client application to Amazon Lex V2. Audio
-- starts accumulating when Amazon Lex V2 identifies a voice and continues
-- until a natural pause in the speech is found before processing.
startConversationRequestEventStream_audioInputEvent :: Lens.Lens' StartConversationRequestEventStream (Prelude.Maybe AudioInputEvent)
startConversationRequestEventStream_audioInputEvent = Lens.lens (\StartConversationRequestEventStream' {audioInputEvent} -> audioInputEvent) (\s@StartConversationRequestEventStream' {} a -> s {audioInputEvent = a} :: StartConversationRequestEventStream)

-- | Text sent from your client application to Amazon Lex V2. Each
-- @TextInputEvent@ is processed individually.
startConversationRequestEventStream_textInputEvent :: Lens.Lens' StartConversationRequestEventStream (Prelude.Maybe TextInputEvent)
startConversationRequestEventStream_textInputEvent = Lens.lens (\StartConversationRequestEventStream' {textInputEvent} -> textInputEvent) (\s@StartConversationRequestEventStream' {} a -> s {textInputEvent = a} :: StartConversationRequestEventStream)

instance
  Prelude.Hashable
    StartConversationRequestEventStream

instance
  Prelude.NFData
    StartConversationRequestEventStream

instance
  Core.ToJSON
    StartConversationRequestEventStream
  where
  toJSON StartConversationRequestEventStream' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PlaybackCompletionEvent" Core..=)
              Prelude.<$> playbackCompletionEvent,
            ("DTMFInputEvent" Core..=)
              Prelude.<$> dTMFInputEvent,
            ("ConfigurationEvent" Core..=)
              Prelude.<$> configurationEvent,
            ("DisconnectionEvent" Core..=)
              Prelude.<$> disconnectionEvent,
            ("AudioInputEvent" Core..=)
              Prelude.<$> audioInputEvent,
            ("TextInputEvent" Core..=)
              Prelude.<$> textInputEvent
          ]
      )
