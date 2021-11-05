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
-- Module      : Network.AWS.SSMIncidents.Types.ChatChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Types.ChatChannel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSMIncidents.Types.EmptyChatChannel

-- | The AWS Chatbot chat channel used for collaboration during an incident.
--
-- /See:/ 'newChatChannel' smart constructor.
data ChatChannel = ChatChannel'
  { -- | Used to remove the chat channel from an incident record or response
    -- plan.
    empty :: Prelude.Maybe EmptyChatChannel,
    -- | The SNS targets that AWS Chatbot uses to notify the chat channel of
    -- updates to an incident. You can also make updates to the incident
    -- through the chat channel by using the SNS topics.
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
-- 'chatbotSns', 'chatChannel_chatbotSns' - The SNS targets that AWS Chatbot uses to notify the chat channel of
-- updates to an incident. You can also make updates to the incident
-- through the chat channel by using the SNS topics.
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

-- | The SNS targets that AWS Chatbot uses to notify the chat channel of
-- updates to an incident. You can also make updates to the incident
-- through the chat channel by using the SNS topics.
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

instance Prelude.Hashable ChatChannel

instance Prelude.NFData ChatChannel

instance Core.ToJSON ChatChannel where
  toJSON ChatChannel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("empty" Core..=) Prelude.<$> empty,
            ("chatbotSns" Core..=) Prelude.<$> chatbotSns
          ]
      )
