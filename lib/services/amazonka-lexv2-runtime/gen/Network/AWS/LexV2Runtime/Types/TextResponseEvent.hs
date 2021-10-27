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
-- Module      : Network.AWS.LexV2Runtime.Types.TextResponseEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.TextResponseEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.Message
import qualified Network.AWS.Prelude as Prelude

-- | The event sent from Amazon Lex V2 to your application with text to
-- present to the user.
--
-- /See:/ 'newTextResponseEvent' smart constructor.
data TextResponseEvent = TextResponseEvent'
  { -- | A list of messages to send to the user. Messages are ordered based on
    -- the order that you returned the messages from your Lambda function or
    -- the order that the messages are defined in the bot.
    messages :: Prelude.Maybe [Message],
    -- | A unique identifier of the event sent by Amazon Lex V2. The identifier
    -- is in the form @RESPONSE-N@, where N is a number starting with one and
    -- incremented for each event sent by Amazon Lex V2 in the current session.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextResponseEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messages', 'textResponseEvent_messages' - A list of messages to send to the user. Messages are ordered based on
-- the order that you returned the messages from your Lambda function or
-- the order that the messages are defined in the bot.
--
-- 'eventId', 'textResponseEvent_eventId' - A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
newTextResponseEvent ::
  TextResponseEvent
newTextResponseEvent =
  TextResponseEvent'
    { messages = Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | A list of messages to send to the user. Messages are ordered based on
-- the order that you returned the messages from your Lambda function or
-- the order that the messages are defined in the bot.
textResponseEvent_messages :: Lens.Lens' TextResponseEvent (Prelude.Maybe [Message])
textResponseEvent_messages = Lens.lens (\TextResponseEvent' {messages} -> messages) (\s@TextResponseEvent' {} a -> s {messages = a} :: TextResponseEvent) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier of the event sent by Amazon Lex V2. The identifier
-- is in the form @RESPONSE-N@, where N is a number starting with one and
-- incremented for each event sent by Amazon Lex V2 in the current session.
textResponseEvent_eventId :: Lens.Lens' TextResponseEvent (Prelude.Maybe Prelude.Text)
textResponseEvent_eventId = Lens.lens (\TextResponseEvent' {eventId} -> eventId) (\s@TextResponseEvent' {} a -> s {eventId = a} :: TextResponseEvent)

instance Core.FromJSON TextResponseEvent where
  parseJSON =
    Core.withObject
      "TextResponseEvent"
      ( \x ->
          TextResponseEvent'
            Prelude.<$> (x Core..:? "messages" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "eventId")
      )

instance Prelude.Hashable TextResponseEvent

instance Prelude.NFData TextResponseEvent
