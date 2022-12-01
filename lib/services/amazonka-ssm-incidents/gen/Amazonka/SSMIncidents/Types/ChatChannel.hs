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
-- Module      : Amazonka.SSMIncidents.Types.ChatChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.ChatChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.EmptyChatChannel

-- | The Chatbot chat channel used for collaboration during an incident.
--
-- /See:/ 'newChatChannel' smart constructor.
data ChatChannel = ChatChannel'
  { -- | Used to remove the chat channel from an incident record or response
    -- plan.
    empty :: Prelude.Maybe EmptyChatChannel,
    -- | The Amazon SNS targets that Chatbot uses to notify the chat channel of
    -- updates to an incident. You can also make updates to the incident
    -- through the chat channel by using the Amazon SNS topics.
    chatbotSns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChatChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'empty', 'chatChannel_empty' - Used to remove the chat channel from an incident record or response
-- plan.
--
-- 'chatbotSns', 'chatChannel_chatbotSns' - The Amazon SNS targets that Chatbot uses to notify the chat channel of
-- updates to an incident. You can also make updates to the incident
-- through the chat channel by using the Amazon SNS topics.
newChatChannel ::
  ChatChannel
newChatChannel =
  ChatChannel'
    { empty = Prelude.Nothing,
      chatbotSns = Prelude.Nothing
    }

-- | Used to remove the chat channel from an incident record or response
-- plan.
chatChannel_empty :: Lens.Lens' ChatChannel (Prelude.Maybe EmptyChatChannel)
chatChannel_empty = Lens.lens (\ChatChannel' {empty} -> empty) (\s@ChatChannel' {} a -> s {empty = a} :: ChatChannel)

-- | The Amazon SNS targets that Chatbot uses to notify the chat channel of
-- updates to an incident. You can also make updates to the incident
-- through the chat channel by using the Amazon SNS topics.
chatChannel_chatbotSns :: Lens.Lens' ChatChannel (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
chatChannel_chatbotSns = Lens.lens (\ChatChannel' {chatbotSns} -> chatbotSns) (\s@ChatChannel' {} a -> s {chatbotSns = a} :: ChatChannel) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ChatChannel where
  parseJSON =
    Core.withObject
      "ChatChannel"
      ( \x ->
          ChatChannel'
            Prelude.<$> (x Core..:? "empty")
            Prelude.<*> (x Core..:? "chatbotSns")
      )

instance Prelude.Hashable ChatChannel where
  hashWithSalt _salt ChatChannel' {..} =
    _salt `Prelude.hashWithSalt` empty
      `Prelude.hashWithSalt` chatbotSns

instance Prelude.NFData ChatChannel where
  rnf ChatChannel' {..} =
    Prelude.rnf empty
      `Prelude.seq` Prelude.rnf chatbotSns

instance Core.ToJSON ChatChannel where
  toJSON ChatChannel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("empty" Core..=) Prelude.<$> empty,
            ("chatbotSns" Core..=) Prelude.<$> chatbotSns
          ]
      )
