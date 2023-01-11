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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.ChatChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.EmptyChatChannel

-- | The Chatbot chat channel used for collaboration during an incident.
--
-- /See:/ 'newChatChannel' smart constructor.
data ChatChannel = ChatChannel'
  { -- | The Amazon SNS targets that Chatbot uses to notify the chat channel of
    -- updates to an incident. You can also make updates to the incident
    -- through the chat channel by using the Amazon SNS topics.
    chatbotSns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Used to remove the chat channel from an incident record or response
    -- plan.
    empty :: Prelude.Maybe EmptyChatChannel
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
-- 'chatbotSns', 'chatChannel_chatbotSns' - The Amazon SNS targets that Chatbot uses to notify the chat channel of
-- updates to an incident. You can also make updates to the incident
-- through the chat channel by using the Amazon SNS topics.
--
-- 'empty', 'chatChannel_empty' - Used to remove the chat channel from an incident record or response
-- plan.
newChatChannel ::
  ChatChannel
newChatChannel =
  ChatChannel'
    { chatbotSns = Prelude.Nothing,
      empty = Prelude.Nothing
    }

-- | The Amazon SNS targets that Chatbot uses to notify the chat channel of
-- updates to an incident. You can also make updates to the incident
-- through the chat channel by using the Amazon SNS topics.
chatChannel_chatbotSns :: Lens.Lens' ChatChannel (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
chatChannel_chatbotSns = Lens.lens (\ChatChannel' {chatbotSns} -> chatbotSns) (\s@ChatChannel' {} a -> s {chatbotSns = a} :: ChatChannel) Prelude.. Lens.mapping Lens.coerced

-- | Used to remove the chat channel from an incident record or response
-- plan.
chatChannel_empty :: Lens.Lens' ChatChannel (Prelude.Maybe EmptyChatChannel)
chatChannel_empty = Lens.lens (\ChatChannel' {empty} -> empty) (\s@ChatChannel' {} a -> s {empty = a} :: ChatChannel)

instance Data.FromJSON ChatChannel where
  parseJSON =
    Data.withObject
      "ChatChannel"
      ( \x ->
          ChatChannel'
            Prelude.<$> (x Data..:? "chatbotSns")
            Prelude.<*> (x Data..:? "empty")
      )

instance Prelude.Hashable ChatChannel where
  hashWithSalt _salt ChatChannel' {..} =
    _salt `Prelude.hashWithSalt` chatbotSns
      `Prelude.hashWithSalt` empty

instance Prelude.NFData ChatChannel where
  rnf ChatChannel' {..} =
    Prelude.rnf chatbotSns
      `Prelude.seq` Prelude.rnf empty

instance Data.ToJSON ChatChannel where
  toJSON ChatChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("chatbotSns" Data..=) Prelude.<$> chatbotSns,
            ("empty" Data..=) Prelude.<$> empty
          ]
      )
