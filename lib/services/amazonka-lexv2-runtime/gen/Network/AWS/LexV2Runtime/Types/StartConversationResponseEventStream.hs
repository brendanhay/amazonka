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
-- Module      : Network.AWS.LexV2Runtime.Types.StartConversationResponseEventStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.StartConversationResponseEventStream where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.AccessDeniedException
import Network.AWS.LexV2Runtime.Types.AudioResponseEvent
import Network.AWS.LexV2Runtime.Types.BadGatewayException
import Network.AWS.LexV2Runtime.Types.ConflictException
import Network.AWS.LexV2Runtime.Types.DependencyFailedException
import Network.AWS.LexV2Runtime.Types.HeartbeatEvent
import Network.AWS.LexV2Runtime.Types.IntentResultEvent
import Network.AWS.LexV2Runtime.Types.InternalServerException
import Network.AWS.LexV2Runtime.Types.PlaybackInterruptionEvent
import Network.AWS.LexV2Runtime.Types.ResourceNotFoundException
import Network.AWS.LexV2Runtime.Types.TextResponseEvent
import Network.AWS.LexV2Runtime.Types.ThrottlingException
import Network.AWS.LexV2Runtime.Types.TranscriptEvent
import Network.AWS.LexV2Runtime.Types.ValidationException
import qualified Network.AWS.Prelude as Prelude

-- | Represents a stream of events between Amazon Lex V2 and your
-- application.
--
-- /See:/ 'newStartConversationResponseEventStream' smart constructor.
data StartConversationResponseEventStream = StartConversationResponseEventStream'
  { -- | Exception thrown when one or more parameters could not be validated. The
    -- @message@ contains the name of the field that isn\'t valid.
    validationException :: Prelude.Maybe ValidationException,
    -- | Exception thrown when the credentials passed with the request are
    -- invalid or expired. Also thrown when the credentials in the request do
    -- not have permission to access the @StartConversation@ operation.
    accessDeniedException :: Prelude.Maybe AccessDeniedException,
    transcriptEvent :: Prelude.Maybe TranscriptEvent,
    textResponseEvent :: Prelude.Maybe TextResponseEvent,
    dependencyFailedException :: Prelude.Maybe DependencyFailedException,
    -- | Event sent from Amazon Lex V2 to the client application containing the
    -- current state of the conversation between the user and Amazon Lex V2.
    intentResultEvent :: Prelude.Maybe IntentResultEvent,
    -- | Exception thrown when two clients are using the same AWS account, Amazon
    -- Lex V2 bot, and session ID.
    conflictException :: Prelude.Maybe ConflictException,
    playbackInterruptionEvent :: Prelude.Maybe PlaybackInterruptionEvent,
    heartbeatEvent :: Prelude.Maybe HeartbeatEvent,
    -- | Exception thrown when your application exceeds the maximum number of
    -- concurrent requests.
    throttlingException :: Prelude.Maybe ThrottlingException,
    -- | An error occurred with Amazon Lex V2.
    internalServerException :: Prelude.Maybe InternalServerException,
    badGatewayException :: Prelude.Maybe BadGatewayException,
    -- | Exception thrown if one of the input parameters points to a resource
    -- that does not exist. For example, if the bot ID specified does not
    -- exist.
    resourceNotFoundException :: Prelude.Maybe ResourceNotFoundException,
    audioResponseEvent :: Prelude.Maybe AudioResponseEvent
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConversationResponseEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationException', 'startConversationResponseEventStream_validationException' - Exception thrown when one or more parameters could not be validated. The
-- @message@ contains the name of the field that isn\'t valid.
--
-- 'accessDeniedException', 'startConversationResponseEventStream_accessDeniedException' - Exception thrown when the credentials passed with the request are
-- invalid or expired. Also thrown when the credentials in the request do
-- not have permission to access the @StartConversation@ operation.
--
-- 'transcriptEvent', 'startConversationResponseEventStream_transcriptEvent' - Undocumented member.
--
-- 'textResponseEvent', 'startConversationResponseEventStream_textResponseEvent' - Undocumented member.
--
-- 'dependencyFailedException', 'startConversationResponseEventStream_dependencyFailedException' - Undocumented member.
--
-- 'intentResultEvent', 'startConversationResponseEventStream_intentResultEvent' - Event sent from Amazon Lex V2 to the client application containing the
-- current state of the conversation between the user and Amazon Lex V2.
--
-- 'conflictException', 'startConversationResponseEventStream_conflictException' - Exception thrown when two clients are using the same AWS account, Amazon
-- Lex V2 bot, and session ID.
--
-- 'playbackInterruptionEvent', 'startConversationResponseEventStream_playbackInterruptionEvent' - Undocumented member.
--
-- 'heartbeatEvent', 'startConversationResponseEventStream_heartbeatEvent' - Undocumented member.
--
-- 'throttlingException', 'startConversationResponseEventStream_throttlingException' - Exception thrown when your application exceeds the maximum number of
-- concurrent requests.
--
-- 'internalServerException', 'startConversationResponseEventStream_internalServerException' - An error occurred with Amazon Lex V2.
--
-- 'badGatewayException', 'startConversationResponseEventStream_badGatewayException' - Undocumented member.
--
-- 'resourceNotFoundException', 'startConversationResponseEventStream_resourceNotFoundException' - Exception thrown if one of the input parameters points to a resource
-- that does not exist. For example, if the bot ID specified does not
-- exist.
--
-- 'audioResponseEvent', 'startConversationResponseEventStream_audioResponseEvent' - Undocumented member.
newStartConversationResponseEventStream ::
  StartConversationResponseEventStream
newStartConversationResponseEventStream =
  StartConversationResponseEventStream'
    { validationException =
        Prelude.Nothing,
      accessDeniedException =
        Prelude.Nothing,
      transcriptEvent = Prelude.Nothing,
      textResponseEvent = Prelude.Nothing,
      dependencyFailedException =
        Prelude.Nothing,
      intentResultEvent = Prelude.Nothing,
      conflictException = Prelude.Nothing,
      playbackInterruptionEvent =
        Prelude.Nothing,
      heartbeatEvent = Prelude.Nothing,
      throttlingException = Prelude.Nothing,
      internalServerException =
        Prelude.Nothing,
      badGatewayException = Prelude.Nothing,
      resourceNotFoundException =
        Prelude.Nothing,
      audioResponseEvent = Prelude.Nothing
    }

-- | Exception thrown when one or more parameters could not be validated. The
-- @message@ contains the name of the field that isn\'t valid.
startConversationResponseEventStream_validationException :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe ValidationException)
startConversationResponseEventStream_validationException = Lens.lens (\StartConversationResponseEventStream' {validationException} -> validationException) (\s@StartConversationResponseEventStream' {} a -> s {validationException = a} :: StartConversationResponseEventStream)

-- | Exception thrown when the credentials passed with the request are
-- invalid or expired. Also thrown when the credentials in the request do
-- not have permission to access the @StartConversation@ operation.
startConversationResponseEventStream_accessDeniedException :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe AccessDeniedException)
startConversationResponseEventStream_accessDeniedException = Lens.lens (\StartConversationResponseEventStream' {accessDeniedException} -> accessDeniedException) (\s@StartConversationResponseEventStream' {} a -> s {accessDeniedException = a} :: StartConversationResponseEventStream)

-- | Undocumented member.
startConversationResponseEventStream_transcriptEvent :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe TranscriptEvent)
startConversationResponseEventStream_transcriptEvent = Lens.lens (\StartConversationResponseEventStream' {transcriptEvent} -> transcriptEvent) (\s@StartConversationResponseEventStream' {} a -> s {transcriptEvent = a} :: StartConversationResponseEventStream)

-- | Undocumented member.
startConversationResponseEventStream_textResponseEvent :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe TextResponseEvent)
startConversationResponseEventStream_textResponseEvent = Lens.lens (\StartConversationResponseEventStream' {textResponseEvent} -> textResponseEvent) (\s@StartConversationResponseEventStream' {} a -> s {textResponseEvent = a} :: StartConversationResponseEventStream)

-- | Undocumented member.
startConversationResponseEventStream_dependencyFailedException :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe DependencyFailedException)
startConversationResponseEventStream_dependencyFailedException = Lens.lens (\StartConversationResponseEventStream' {dependencyFailedException} -> dependencyFailedException) (\s@StartConversationResponseEventStream' {} a -> s {dependencyFailedException = a} :: StartConversationResponseEventStream)

-- | Event sent from Amazon Lex V2 to the client application containing the
-- current state of the conversation between the user and Amazon Lex V2.
startConversationResponseEventStream_intentResultEvent :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe IntentResultEvent)
startConversationResponseEventStream_intentResultEvent = Lens.lens (\StartConversationResponseEventStream' {intentResultEvent} -> intentResultEvent) (\s@StartConversationResponseEventStream' {} a -> s {intentResultEvent = a} :: StartConversationResponseEventStream)

-- | Exception thrown when two clients are using the same AWS account, Amazon
-- Lex V2 bot, and session ID.
startConversationResponseEventStream_conflictException :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe ConflictException)
startConversationResponseEventStream_conflictException = Lens.lens (\StartConversationResponseEventStream' {conflictException} -> conflictException) (\s@StartConversationResponseEventStream' {} a -> s {conflictException = a} :: StartConversationResponseEventStream)

-- | Undocumented member.
startConversationResponseEventStream_playbackInterruptionEvent :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe PlaybackInterruptionEvent)
startConversationResponseEventStream_playbackInterruptionEvent = Lens.lens (\StartConversationResponseEventStream' {playbackInterruptionEvent} -> playbackInterruptionEvent) (\s@StartConversationResponseEventStream' {} a -> s {playbackInterruptionEvent = a} :: StartConversationResponseEventStream)

-- | Undocumented member.
startConversationResponseEventStream_heartbeatEvent :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe HeartbeatEvent)
startConversationResponseEventStream_heartbeatEvent = Lens.lens (\StartConversationResponseEventStream' {heartbeatEvent} -> heartbeatEvent) (\s@StartConversationResponseEventStream' {} a -> s {heartbeatEvent = a} :: StartConversationResponseEventStream)

-- | Exception thrown when your application exceeds the maximum number of
-- concurrent requests.
startConversationResponseEventStream_throttlingException :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe ThrottlingException)
startConversationResponseEventStream_throttlingException = Lens.lens (\StartConversationResponseEventStream' {throttlingException} -> throttlingException) (\s@StartConversationResponseEventStream' {} a -> s {throttlingException = a} :: StartConversationResponseEventStream)

-- | An error occurred with Amazon Lex V2.
startConversationResponseEventStream_internalServerException :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe InternalServerException)
startConversationResponseEventStream_internalServerException = Lens.lens (\StartConversationResponseEventStream' {internalServerException} -> internalServerException) (\s@StartConversationResponseEventStream' {} a -> s {internalServerException = a} :: StartConversationResponseEventStream)

-- | Undocumented member.
startConversationResponseEventStream_badGatewayException :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe BadGatewayException)
startConversationResponseEventStream_badGatewayException = Lens.lens (\StartConversationResponseEventStream' {badGatewayException} -> badGatewayException) (\s@StartConversationResponseEventStream' {} a -> s {badGatewayException = a} :: StartConversationResponseEventStream)

-- | Exception thrown if one of the input parameters points to a resource
-- that does not exist. For example, if the bot ID specified does not
-- exist.
startConversationResponseEventStream_resourceNotFoundException :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe ResourceNotFoundException)
startConversationResponseEventStream_resourceNotFoundException = Lens.lens (\StartConversationResponseEventStream' {resourceNotFoundException} -> resourceNotFoundException) (\s@StartConversationResponseEventStream' {} a -> s {resourceNotFoundException = a} :: StartConversationResponseEventStream)

-- | Undocumented member.
startConversationResponseEventStream_audioResponseEvent :: Lens.Lens' StartConversationResponseEventStream (Prelude.Maybe AudioResponseEvent)
startConversationResponseEventStream_audioResponseEvent = Lens.lens (\StartConversationResponseEventStream' {audioResponseEvent} -> audioResponseEvent) (\s@StartConversationResponseEventStream' {} a -> s {audioResponseEvent = a} :: StartConversationResponseEventStream)

instance
  Core.FromJSON
    StartConversationResponseEventStream
  where
  parseJSON =
    Core.withObject
      "StartConversationResponseEventStream"
      ( \x ->
          StartConversationResponseEventStream'
            Prelude.<$> (x Core..:? "ValidationException")
            Prelude.<*> (x Core..:? "AccessDeniedException")
            Prelude.<*> (x Core..:? "TranscriptEvent")
            Prelude.<*> (x Core..:? "TextResponseEvent")
            Prelude.<*> (x Core..:? "DependencyFailedException")
            Prelude.<*> (x Core..:? "IntentResultEvent")
            Prelude.<*> (x Core..:? "ConflictException")
            Prelude.<*> (x Core..:? "PlaybackInterruptionEvent")
            Prelude.<*> (x Core..:? "HeartbeatEvent")
            Prelude.<*> (x Core..:? "ThrottlingException")
            Prelude.<*> (x Core..:? "InternalServerException")
            Prelude.<*> (x Core..:? "BadGatewayException")
            Prelude.<*> (x Core..:? "ResourceNotFoundException")
            Prelude.<*> (x Core..:? "AudioResponseEvent")
      )

instance
  Prelude.Hashable
    StartConversationResponseEventStream

instance
  Prelude.NFData
    StartConversationResponseEventStream
