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
-- Module      : Network.AWS.LexV2Runtime.StartConversation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an HTTP\/2 bidirectional event stream that enables you to send
-- audio, text, or DTMF input in real time. After your application starts a
-- conversation, users send input to Amazon Lex V2 as a stream of events.
-- Amazon Lex V2 processes the incoming events and responds with streaming
-- text or audio events.
--
-- Audio input must be in the following format:
-- @audio\/lpcm sample-rate=8000 sample-size-bits=16 channel-count=1; is-big-endian=false@.
--
-- If the optional post-fulfillment response is specified, the messages are
-- returned as follows. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_PostFulfillmentStatusSpecification.html PostFulfillmentStatusSpecification>.
--
-- -   __Success message__ - Returned if the Lambda function completes
--     successfully and the intent state is fulfilled or ready fulfillment
--     if the message is present.
--
-- -   __Failed message__ - The failed message is returned if the Lambda
--     function throws an exception or if the Lambda function returns a
--     failed intent state without a message.
--
-- -   __Timeout message__ - If you don\'t configure a timeout message and
--     a timeout, and the Lambda function doesn\'t return within 30
--     seconds, the timeout message is returned. If you configure a
--     timeout, the timeout message is returned when the period times out.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/streaming-progress.html#progress-complete.html Completion message>.
--
-- If the optional update message is configured, it is played at the
-- specified frequency while the Lambda function is running and the update
-- message state is active. If the fulfillment update message is not
-- active, the Lambda function runs with a 30 second timeout.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/streaming-progress.html#progress-update.html Update message>
--
-- The @StartConversation@ operation is supported only in the following
-- SDKs:
--
-- -   <https://docs.aws.amazon.com/goto/SdkForCpp/runtime.lex.v2-2020-08-07/StartConversation AWS SDK for C++>
--
-- -   <https://docs.aws.amazon.com/goto/SdkForJavaV2/runtime.lex.v2-2020-08-07/StartConversation AWS SDK for Java V2>
--
-- -   <https://docs.aws.amazon.com/goto/SdkForRubyV3/runtime.lex.v2-2020-08-07/StartConversation AWS SDK for Ruby V3>
module Network.AWS.LexV2Runtime.StartConversation
  ( -- * Creating a Request
    StartConversation (..),
    newStartConversation,

    -- * Request Lenses
    startConversation_conversationMode,
    startConversation_botId,
    startConversation_botAliasId,
    startConversation_localeId,
    startConversation_requestEventStream,
    startConversation_sessionId,

    -- * Destructuring the Response
    StartConversationResponse (..),
    newStartConversationResponse,

    -- * Response Lenses
    startConversationResponse_responseEventStream,
    startConversationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartConversation' smart constructor.
data StartConversation = StartConversation'
  { -- | The conversation type that you are using the Amazon Lex V2. If the
    -- conversation mode is @AUDIO@ you can send both audio and DTMF
    -- information. If the mode is @TEXT@ you can only send text.
    conversationMode :: Prelude.Maybe ConversationMode,
    -- | The identifier of the bot to process the request.
    botId :: Prelude.Text,
    -- | The alias identifier in use for the bot that processes the request.
    botAliasId :: Prelude.Text,
    -- | The locale where the session is in use.
    localeId :: Prelude.Text,
    -- | Represents the stream of events to Amazon Lex V2 from your application.
    -- The events are encoded as HTTP\/2 data frames.
    requestEventStream :: StartConversationRequestEventStream,
    -- | The identifier of the user session that is having the conversation.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConversation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversationMode', 'startConversation_conversationMode' - The conversation type that you are using the Amazon Lex V2. If the
-- conversation mode is @AUDIO@ you can send both audio and DTMF
-- information. If the mode is @TEXT@ you can only send text.
--
-- 'botId', 'startConversation_botId' - The identifier of the bot to process the request.
--
-- 'botAliasId', 'startConversation_botAliasId' - The alias identifier in use for the bot that processes the request.
--
-- 'localeId', 'startConversation_localeId' - The locale where the session is in use.
--
-- 'requestEventStream', 'startConversation_requestEventStream' - Represents the stream of events to Amazon Lex V2 from your application.
-- The events are encoded as HTTP\/2 data frames.
--
-- 'sessionId', 'startConversation_sessionId' - The identifier of the user session that is having the conversation.
newStartConversation ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'requestEventStream'
  StartConversationRequestEventStream ->
  -- | 'sessionId'
  Prelude.Text ->
  StartConversation
newStartConversation
  pBotId_
  pBotAliasId_
  pLocaleId_
  pRequestEventStream_
  pSessionId_ =
    StartConversation'
      { conversationMode =
          Prelude.Nothing,
        botId = pBotId_,
        botAliasId = pBotAliasId_,
        localeId = pLocaleId_,
        requestEventStream = pRequestEventStream_,
        sessionId = pSessionId_
      }

-- | The conversation type that you are using the Amazon Lex V2. If the
-- conversation mode is @AUDIO@ you can send both audio and DTMF
-- information. If the mode is @TEXT@ you can only send text.
startConversation_conversationMode :: Lens.Lens' StartConversation (Prelude.Maybe ConversationMode)
startConversation_conversationMode = Lens.lens (\StartConversation' {conversationMode} -> conversationMode) (\s@StartConversation' {} a -> s {conversationMode = a} :: StartConversation)

-- | The identifier of the bot to process the request.
startConversation_botId :: Lens.Lens' StartConversation Prelude.Text
startConversation_botId = Lens.lens (\StartConversation' {botId} -> botId) (\s@StartConversation' {} a -> s {botId = a} :: StartConversation)

-- | The alias identifier in use for the bot that processes the request.
startConversation_botAliasId :: Lens.Lens' StartConversation Prelude.Text
startConversation_botAliasId = Lens.lens (\StartConversation' {botAliasId} -> botAliasId) (\s@StartConversation' {} a -> s {botAliasId = a} :: StartConversation)

-- | The locale where the session is in use.
startConversation_localeId :: Lens.Lens' StartConversation Prelude.Text
startConversation_localeId = Lens.lens (\StartConversation' {localeId} -> localeId) (\s@StartConversation' {} a -> s {localeId = a} :: StartConversation)

-- | Represents the stream of events to Amazon Lex V2 from your application.
-- The events are encoded as HTTP\/2 data frames.
startConversation_requestEventStream :: Lens.Lens' StartConversation StartConversationRequestEventStream
startConversation_requestEventStream = Lens.lens (\StartConversation' {requestEventStream} -> requestEventStream) (\s@StartConversation' {} a -> s {requestEventStream = a} :: StartConversation)

-- | The identifier of the user session that is having the conversation.
startConversation_sessionId :: Lens.Lens' StartConversation Prelude.Text
startConversation_sessionId = Lens.lens (\StartConversation' {sessionId} -> sessionId) (\s@StartConversation' {} a -> s {sessionId = a} :: StartConversation)

instance Core.AWSRequest StartConversation where
  type
    AWSResponse StartConversation =
      StartConversationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartConversationResponse'
            Prelude.<$> (Core.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartConversation

instance Prelude.NFData StartConversation

instance Core.ToHeaders StartConversation where
  toHeaders StartConversation' {..} =
    Prelude.mconcat
      [ "x-amz-lex-conversation-mode"
          Core.=# conversationMode,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON StartConversation where
  toJSON StartConversation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("requestEventStream" Core..= requestEventStream)
          ]
      )

instance Core.ToPath StartConversation where
  toPath StartConversation' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botAliases/",
        Core.toBS botAliasId,
        "/botLocales/",
        Core.toBS localeId,
        "/sessions/",
        Core.toBS sessionId,
        "/conversation"
      ]

instance Core.ToQuery StartConversation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartConversationResponse' smart constructor.
data StartConversationResponse = StartConversationResponse'
  { -- | Represents the stream of events from Amazon Lex V2 to your application.
    -- The events are encoded as HTTP\/2 data frames.
    responseEventStream :: Prelude.Maybe StartConversationResponseEventStream,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConversationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseEventStream', 'startConversationResponse_responseEventStream' - Represents the stream of events from Amazon Lex V2 to your application.
-- The events are encoded as HTTP\/2 data frames.
--
-- 'httpStatus', 'startConversationResponse_httpStatus' - The response's http status code.
newStartConversationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartConversationResponse
newStartConversationResponse pHttpStatus_ =
  StartConversationResponse'
    { responseEventStream =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the stream of events from Amazon Lex V2 to your application.
-- The events are encoded as HTTP\/2 data frames.
startConversationResponse_responseEventStream :: Lens.Lens' StartConversationResponse (Prelude.Maybe StartConversationResponseEventStream)
startConversationResponse_responseEventStream = Lens.lens (\StartConversationResponse' {responseEventStream} -> responseEventStream) (\s@StartConversationResponse' {} a -> s {responseEventStream = a} :: StartConversationResponse)

-- | The response's http status code.
startConversationResponse_httpStatus :: Lens.Lens' StartConversationResponse Prelude.Int
startConversationResponse_httpStatus = Lens.lens (\StartConversationResponse' {httpStatus} -> httpStatus) (\s@StartConversationResponse' {} a -> s {httpStatus = a} :: StartConversationResponse)

instance Prelude.NFData StartConversationResponse
