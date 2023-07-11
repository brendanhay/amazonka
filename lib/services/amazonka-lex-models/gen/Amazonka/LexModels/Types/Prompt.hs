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
-- Module      : Amazonka.LexModels.Types.Prompt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.Prompt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.Message
import qualified Amazonka.Prelude as Prelude

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
    responseCard :: Prelude.Maybe Prelude.Text,
    -- | An array of objects, each of which provides a message string and its
    -- type. You can specify the message string in plain text or in Speech
    -- Synthesis Markup Language (SSML).
    messages :: Prelude.NonEmpty Message,
    -- | The number of times to prompt the user for information.
    maxAttempts :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Message ->
  -- | 'maxAttempts'
  Prelude.Natural ->
  Prompt
newPrompt pMessages_ pMaxAttempts_ =
  Prompt'
    { responseCard = Prelude.Nothing,
      messages = Lens.coerced Lens.# pMessages_,
      maxAttempts = pMaxAttempts_
    }

-- | A response card. Amazon Lex uses this prompt at runtime, in the
-- @PostText@ API response. It substitutes session attributes and slot
-- values for placeholders in the response card. For more information, see
-- ex-resp-card.
prompt_responseCard :: Lens.Lens' Prompt (Prelude.Maybe Prelude.Text)
prompt_responseCard = Lens.lens (\Prompt' {responseCard} -> responseCard) (\s@Prompt' {} a -> s {responseCard = a} :: Prompt)

-- | An array of objects, each of which provides a message string and its
-- type. You can specify the message string in plain text or in Speech
-- Synthesis Markup Language (SSML).
prompt_messages :: Lens.Lens' Prompt (Prelude.NonEmpty Message)
prompt_messages = Lens.lens (\Prompt' {messages} -> messages) (\s@Prompt' {} a -> s {messages = a} :: Prompt) Prelude.. Lens.coerced

-- | The number of times to prompt the user for information.
prompt_maxAttempts :: Lens.Lens' Prompt Prelude.Natural
prompt_maxAttempts = Lens.lens (\Prompt' {maxAttempts} -> maxAttempts) (\s@Prompt' {} a -> s {maxAttempts = a} :: Prompt)

instance Data.FromJSON Prompt where
  parseJSON =
    Data.withObject
      "Prompt"
      ( \x ->
          Prompt'
            Prelude.<$> (x Data..:? "responseCard")
            Prelude.<*> (x Data..: "messages")
            Prelude.<*> (x Data..: "maxAttempts")
      )

instance Prelude.Hashable Prompt where
  hashWithSalt _salt Prompt' {..} =
    _salt
      `Prelude.hashWithSalt` responseCard
      `Prelude.hashWithSalt` messages
      `Prelude.hashWithSalt` maxAttempts

instance Prelude.NFData Prompt where
  rnf Prompt' {..} =
    Prelude.rnf responseCard
      `Prelude.seq` Prelude.rnf messages
      `Prelude.seq` Prelude.rnf maxAttempts

instance Data.ToJSON Prompt where
  toJSON Prompt' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("responseCard" Data..=) Prelude.<$> responseCard,
            Prelude.Just ("messages" Data..= messages),
            Prelude.Just ("maxAttempts" Data..= maxAttempts)
          ]
      )
