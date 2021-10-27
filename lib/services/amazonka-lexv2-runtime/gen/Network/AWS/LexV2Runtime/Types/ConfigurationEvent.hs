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
-- Module      : Network.AWS.LexV2Runtime.Types.ConfigurationEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.ConfigurationEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.Message
import Network.AWS.LexV2Runtime.Types.SessionState
import qualified Network.AWS.Prelude as Prelude

-- | The initial event sent from the application to Amazon Lex V2 to
-- configure the conversation, including session and request attributes and
-- the response content type.
--
-- /See:/ 'newConfigurationEvent' smart constructor.
data ConfigurationEvent = ConfigurationEvent'
  { -- | A list of messages to send to the user.
    welcomeMessages :: Prelude.Maybe [Message],
    -- | Determines whether Amazon Lex V2 should send audio responses to the
    -- client application.
    --
    -- Set this field to false when the client is operating in a playback mode
    -- where audio responses are played to the user. If the client isn\'t
    -- operating in playback mode, such as a text chat application, set this to
    -- true so that Amazon Lex V2 doesn\'t wait for the prompt to finish
    -- playing on the client.
    disablePlayback :: Prelude.Maybe Prelude.Bool,
    sessionState :: Prelude.Maybe SessionState,
    -- | Request-specific information passed between the client application and
    -- Amazon Lex V2.
    --
    -- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
    -- create any request attributes for prefix @x-amz-lex:@.
    requestAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A timestamp set by the client of the date and time that the event was
    -- sent to Amazon Lex V2.
    clientTimestampMillis :: Prelude.Maybe Prelude.Integer,
    -- | A unique identifier that your application assigns to the event. You can
    -- use this to identify events in logs.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The message that Amazon Lex V2 returns in the response can be either
    -- text or speech based on the @responseContentType@ value.
    --
    -- -   If the value is @text\/plain;charset=utf-8@, Amazon Lex V2 returns
    --     text in the response.
    --
    -- -   If the value begins with @audio\/@, Amazon Lex V2 returns speech in
    --     the response. Amazon Lex V2 uses Amazon Polly to generate the speech
    --     using the configuration that you specified in the
    --     @requestContentType@ parameter. For example, if you specify
    --     @audio\/mpeg@ as the value, Amazon Lex V2 returns speech in the MPEG
    --     format.
    --
    -- -   If the value is @audio\/pcm@, the speech returned is audio\/pcm in
    --     16-bit, little-endian format.
    --
    -- -   The following are the accepted values:
    --
    --     -   audio\/mpeg
    --
    --     -   audio\/ogg
    --
    --     -   audio\/pcm
    --
    --     -   audio\/* (defaults to mpeg)
    --
    --     -   text\/plain; charset=utf-8
    responseContentType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'welcomeMessages', 'configurationEvent_welcomeMessages' - A list of messages to send to the user.
--
-- 'disablePlayback', 'configurationEvent_disablePlayback' - Determines whether Amazon Lex V2 should send audio responses to the
-- client application.
--
-- Set this field to false when the client is operating in a playback mode
-- where audio responses are played to the user. If the client isn\'t
-- operating in playback mode, such as a text chat application, set this to
-- true so that Amazon Lex V2 doesn\'t wait for the prompt to finish
-- playing on the client.
--
-- 'sessionState', 'configurationEvent_sessionState' - Undocumented member.
--
-- 'requestAttributes', 'configurationEvent_requestAttributes' - Request-specific information passed between the client application and
-- Amazon Lex V2.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes for prefix @x-amz-lex:@.
--
-- 'clientTimestampMillis', 'configurationEvent_clientTimestampMillis' - A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
--
-- 'eventId', 'configurationEvent_eventId' - A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
--
-- 'responseContentType', 'configurationEvent_responseContentType' - The message that Amazon Lex V2 returns in the response can be either
-- text or speech based on the @responseContentType@ value.
--
-- -   If the value is @text\/plain;charset=utf-8@, Amazon Lex V2 returns
--     text in the response.
--
-- -   If the value begins with @audio\/@, Amazon Lex V2 returns speech in
--     the response. Amazon Lex V2 uses Amazon Polly to generate the speech
--     using the configuration that you specified in the
--     @requestContentType@ parameter. For example, if you specify
--     @audio\/mpeg@ as the value, Amazon Lex V2 returns speech in the MPEG
--     format.
--
-- -   If the value is @audio\/pcm@, the speech returned is audio\/pcm in
--     16-bit, little-endian format.
--
-- -   The following are the accepted values:
--
--     -   audio\/mpeg
--
--     -   audio\/ogg
--
--     -   audio\/pcm
--
--     -   audio\/* (defaults to mpeg)
--
--     -   text\/plain; charset=utf-8
newConfigurationEvent ::
  -- | 'responseContentType'
  Prelude.Text ->
  ConfigurationEvent
newConfigurationEvent pResponseContentType_ =
  ConfigurationEvent'
    { welcomeMessages =
        Prelude.Nothing,
      disablePlayback = Prelude.Nothing,
      sessionState = Prelude.Nothing,
      requestAttributes = Prelude.Nothing,
      clientTimestampMillis = Prelude.Nothing,
      eventId = Prelude.Nothing,
      responseContentType = pResponseContentType_
    }

-- | A list of messages to send to the user.
configurationEvent_welcomeMessages :: Lens.Lens' ConfigurationEvent (Prelude.Maybe [Message])
configurationEvent_welcomeMessages = Lens.lens (\ConfigurationEvent' {welcomeMessages} -> welcomeMessages) (\s@ConfigurationEvent' {} a -> s {welcomeMessages = a} :: ConfigurationEvent) Prelude.. Lens.mapping Lens.coerced

-- | Determines whether Amazon Lex V2 should send audio responses to the
-- client application.
--
-- Set this field to false when the client is operating in a playback mode
-- where audio responses are played to the user. If the client isn\'t
-- operating in playback mode, such as a text chat application, set this to
-- true so that Amazon Lex V2 doesn\'t wait for the prompt to finish
-- playing on the client.
configurationEvent_disablePlayback :: Lens.Lens' ConfigurationEvent (Prelude.Maybe Prelude.Bool)
configurationEvent_disablePlayback = Lens.lens (\ConfigurationEvent' {disablePlayback} -> disablePlayback) (\s@ConfigurationEvent' {} a -> s {disablePlayback = a} :: ConfigurationEvent)

-- | Undocumented member.
configurationEvent_sessionState :: Lens.Lens' ConfigurationEvent (Prelude.Maybe SessionState)
configurationEvent_sessionState = Lens.lens (\ConfigurationEvent' {sessionState} -> sessionState) (\s@ConfigurationEvent' {} a -> s {sessionState = a} :: ConfigurationEvent)

-- | Request-specific information passed between the client application and
-- Amazon Lex V2.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes for prefix @x-amz-lex:@.
configurationEvent_requestAttributes :: Lens.Lens' ConfigurationEvent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configurationEvent_requestAttributes = Lens.lens (\ConfigurationEvent' {requestAttributes} -> requestAttributes) (\s@ConfigurationEvent' {} a -> s {requestAttributes = a} :: ConfigurationEvent) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp set by the client of the date and time that the event was
-- sent to Amazon Lex V2.
configurationEvent_clientTimestampMillis :: Lens.Lens' ConfigurationEvent (Prelude.Maybe Prelude.Integer)
configurationEvent_clientTimestampMillis = Lens.lens (\ConfigurationEvent' {clientTimestampMillis} -> clientTimestampMillis) (\s@ConfigurationEvent' {} a -> s {clientTimestampMillis = a} :: ConfigurationEvent)

-- | A unique identifier that your application assigns to the event. You can
-- use this to identify events in logs.
configurationEvent_eventId :: Lens.Lens' ConfigurationEvent (Prelude.Maybe Prelude.Text)
configurationEvent_eventId = Lens.lens (\ConfigurationEvent' {eventId} -> eventId) (\s@ConfigurationEvent' {} a -> s {eventId = a} :: ConfigurationEvent)

-- | The message that Amazon Lex V2 returns in the response can be either
-- text or speech based on the @responseContentType@ value.
--
-- -   If the value is @text\/plain;charset=utf-8@, Amazon Lex V2 returns
--     text in the response.
--
-- -   If the value begins with @audio\/@, Amazon Lex V2 returns speech in
--     the response. Amazon Lex V2 uses Amazon Polly to generate the speech
--     using the configuration that you specified in the
--     @requestContentType@ parameter. For example, if you specify
--     @audio\/mpeg@ as the value, Amazon Lex V2 returns speech in the MPEG
--     format.
--
-- -   If the value is @audio\/pcm@, the speech returned is audio\/pcm in
--     16-bit, little-endian format.
--
-- -   The following are the accepted values:
--
--     -   audio\/mpeg
--
--     -   audio\/ogg
--
--     -   audio\/pcm
--
--     -   audio\/* (defaults to mpeg)
--
--     -   text\/plain; charset=utf-8
configurationEvent_responseContentType :: Lens.Lens' ConfigurationEvent Prelude.Text
configurationEvent_responseContentType = Lens.lens (\ConfigurationEvent' {responseContentType} -> responseContentType) (\s@ConfigurationEvent' {} a -> s {responseContentType = a} :: ConfigurationEvent)

instance Prelude.Hashable ConfigurationEvent

instance Prelude.NFData ConfigurationEvent

instance Core.ToJSON ConfigurationEvent where
  toJSON ConfigurationEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("welcomeMessages" Core..=)
              Prelude.<$> welcomeMessages,
            ("disablePlayback" Core..=)
              Prelude.<$> disablePlayback,
            ("sessionState" Core..=) Prelude.<$> sessionState,
            ("requestAttributes" Core..=)
              Prelude.<$> requestAttributes,
            ("clientTimestampMillis" Core..=)
              Prelude.<$> clientTimestampMillis,
            ("eventId" Core..=) Prelude.<$> eventId,
            Prelude.Just
              ("responseContentType" Core..= responseContentType)
          ]
      )
