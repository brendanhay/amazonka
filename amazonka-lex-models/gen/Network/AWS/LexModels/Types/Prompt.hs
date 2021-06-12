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
-- Module      : Network.AWS.LexModels.Types.Prompt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Prompt where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Message

-- | Obtains information from the user. To define a prompt, provide one or
-- more messages and specify the number of attempts to get information from
-- the user. If you provide more than one message, Amazon Lex chooses one
-- of the messages to use to prompt the user. For more information, see
-- how-it-works.
--
-- /See:/ 'newPrompt' smart constructor.
data Prompt = Prompt'
  { -- | A response card. Amazon Lex uses this prompt at runtime, in the
    -- @PostText@ API response. It substitutes session attributes and slot
    -- values for placeholders in the response card. For more information, see
    -- ex-resp-card.
    responseCard :: Core.Maybe Core.Text,
    -- | An array of objects, each of which provides a message string and its
    -- type. You can specify the message string in plain text or in Speech
    -- Synthesis Markup Language (SSML).
    messages :: Core.NonEmpty Message,
    -- | The number of times to prompt the user for information.
    maxAttempts :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Prompt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseCard', 'prompt_responseCard' - A response card. Amazon Lex uses this prompt at runtime, in the
-- @PostText@ API response. It substitutes session attributes and slot
-- values for placeholders in the response card. For more information, see
-- ex-resp-card.
--
-- 'messages', 'prompt_messages' - An array of objects, each of which provides a message string and its
-- type. You can specify the message string in plain text or in Speech
-- Synthesis Markup Language (SSML).
--
-- 'maxAttempts', 'prompt_maxAttempts' - The number of times to prompt the user for information.
newPrompt ::
  -- | 'messages'
  Core.NonEmpty Message ->
  -- | 'maxAttempts'
  Core.Natural ->
  Prompt
newPrompt pMessages_ pMaxAttempts_ =
  Prompt'
    { responseCard = Core.Nothing,
      messages = Lens._Coerce Lens.# pMessages_,
      maxAttempts = pMaxAttempts_
    }

-- | A response card. Amazon Lex uses this prompt at runtime, in the
-- @PostText@ API response. It substitutes session attributes and slot
-- values for placeholders in the response card. For more information, see
-- ex-resp-card.
prompt_responseCard :: Lens.Lens' Prompt (Core.Maybe Core.Text)
prompt_responseCard = Lens.lens (\Prompt' {responseCard} -> responseCard) (\s@Prompt' {} a -> s {responseCard = a} :: Prompt)

-- | An array of objects, each of which provides a message string and its
-- type. You can specify the message string in plain text or in Speech
-- Synthesis Markup Language (SSML).
prompt_messages :: Lens.Lens' Prompt (Core.NonEmpty Message)
prompt_messages = Lens.lens (\Prompt' {messages} -> messages) (\s@Prompt' {} a -> s {messages = a} :: Prompt) Core.. Lens._Coerce

-- | The number of times to prompt the user for information.
prompt_maxAttempts :: Lens.Lens' Prompt Core.Natural
prompt_maxAttempts = Lens.lens (\Prompt' {maxAttempts} -> maxAttempts) (\s@Prompt' {} a -> s {maxAttempts = a} :: Prompt)

instance Core.FromJSON Prompt where
  parseJSON =
    Core.withObject
      "Prompt"
      ( \x ->
          Prompt'
            Core.<$> (x Core..:? "responseCard")
            Core.<*> (x Core..: "messages")
            Core.<*> (x Core..: "maxAttempts")
      )

instance Core.Hashable Prompt

instance Core.NFData Prompt

instance Core.ToJSON Prompt where
  toJSON Prompt' {..} =
    Core.object
      ( Core.catMaybes
          [ ("responseCard" Core..=) Core.<$> responseCard,
            Core.Just ("messages" Core..= messages),
            Core.Just ("maxAttempts" Core..= maxAttempts)
          ]
      )
