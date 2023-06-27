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
-- Module      : Amazonka.LexV2Models.Types.PromptSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.PromptSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.MessageGroup
import Amazonka.LexV2Models.Types.MessageSelectionStrategy
import Amazonka.LexV2Models.Types.PromptAttempt
import Amazonka.LexV2Models.Types.PromptAttemptSpecification
import qualified Amazonka.Prelude as Prelude

-- | Specifies a list of message groups that Amazon Lex sends to a user to
-- elicit a response.
--
-- /See:/ 'newPromptSpecification' smart constructor.
data PromptSpecification = PromptSpecification'
  { -- | Indicates whether the user can interrupt a speech prompt from the bot.
    allowInterrupt :: Prelude.Maybe Prelude.Bool,
    -- | Indicates how a message is selected from a message group among retries.
    messageSelectionStrategy :: Prelude.Maybe MessageSelectionStrategy,
    -- | Specifies the advanced settings on each attempt of the prompt.
    promptAttemptsSpecification :: Prelude.Maybe (Prelude.HashMap PromptAttempt PromptAttemptSpecification),
    -- | A collection of messages that Amazon Lex can send to the user. Amazon
    -- Lex chooses the actual message to send at runtime.
    messageGroups :: Prelude.NonEmpty MessageGroup,
    -- | The maximum number of times the bot tries to elicit a response from the
    -- user using this prompt.
    maxRetries :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromptSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowInterrupt', 'promptSpecification_allowInterrupt' - Indicates whether the user can interrupt a speech prompt from the bot.
--
-- 'messageSelectionStrategy', 'promptSpecification_messageSelectionStrategy' - Indicates how a message is selected from a message group among retries.
--
-- 'promptAttemptsSpecification', 'promptSpecification_promptAttemptsSpecification' - Specifies the advanced settings on each attempt of the prompt.
--
-- 'messageGroups', 'promptSpecification_messageGroups' - A collection of messages that Amazon Lex can send to the user. Amazon
-- Lex chooses the actual message to send at runtime.
--
-- 'maxRetries', 'promptSpecification_maxRetries' - The maximum number of times the bot tries to elicit a response from the
-- user using this prompt.
newPromptSpecification ::
  -- | 'messageGroups'
  Prelude.NonEmpty MessageGroup ->
  -- | 'maxRetries'
  Prelude.Natural ->
  PromptSpecification
newPromptSpecification pMessageGroups_ pMaxRetries_ =
  PromptSpecification'
    { allowInterrupt =
        Prelude.Nothing,
      messageSelectionStrategy = Prelude.Nothing,
      promptAttemptsSpecification = Prelude.Nothing,
      messageGroups = Lens.coerced Lens.# pMessageGroups_,
      maxRetries = pMaxRetries_
    }

-- | Indicates whether the user can interrupt a speech prompt from the bot.
promptSpecification_allowInterrupt :: Lens.Lens' PromptSpecification (Prelude.Maybe Prelude.Bool)
promptSpecification_allowInterrupt = Lens.lens (\PromptSpecification' {allowInterrupt} -> allowInterrupt) (\s@PromptSpecification' {} a -> s {allowInterrupt = a} :: PromptSpecification)

-- | Indicates how a message is selected from a message group among retries.
promptSpecification_messageSelectionStrategy :: Lens.Lens' PromptSpecification (Prelude.Maybe MessageSelectionStrategy)
promptSpecification_messageSelectionStrategy = Lens.lens (\PromptSpecification' {messageSelectionStrategy} -> messageSelectionStrategy) (\s@PromptSpecification' {} a -> s {messageSelectionStrategy = a} :: PromptSpecification)

-- | Specifies the advanced settings on each attempt of the prompt.
promptSpecification_promptAttemptsSpecification :: Lens.Lens' PromptSpecification (Prelude.Maybe (Prelude.HashMap PromptAttempt PromptAttemptSpecification))
promptSpecification_promptAttemptsSpecification = Lens.lens (\PromptSpecification' {promptAttemptsSpecification} -> promptAttemptsSpecification) (\s@PromptSpecification' {} a -> s {promptAttemptsSpecification = a} :: PromptSpecification) Prelude.. Lens.mapping Lens.coerced

-- | A collection of messages that Amazon Lex can send to the user. Amazon
-- Lex chooses the actual message to send at runtime.
promptSpecification_messageGroups :: Lens.Lens' PromptSpecification (Prelude.NonEmpty MessageGroup)
promptSpecification_messageGroups = Lens.lens (\PromptSpecification' {messageGroups} -> messageGroups) (\s@PromptSpecification' {} a -> s {messageGroups = a} :: PromptSpecification) Prelude.. Lens.coerced

-- | The maximum number of times the bot tries to elicit a response from the
-- user using this prompt.
promptSpecification_maxRetries :: Lens.Lens' PromptSpecification Prelude.Natural
promptSpecification_maxRetries = Lens.lens (\PromptSpecification' {maxRetries} -> maxRetries) (\s@PromptSpecification' {} a -> s {maxRetries = a} :: PromptSpecification)

instance Data.FromJSON PromptSpecification where
  parseJSON =
    Data.withObject
      "PromptSpecification"
      ( \x ->
          PromptSpecification'
            Prelude.<$> (x Data..:? "allowInterrupt")
            Prelude.<*> (x Data..:? "messageSelectionStrategy")
            Prelude.<*> ( x
                            Data..:? "promptAttemptsSpecification"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "messageGroups")
            Prelude.<*> (x Data..: "maxRetries")
      )

instance Prelude.Hashable PromptSpecification where
  hashWithSalt _salt PromptSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` allowInterrupt
      `Prelude.hashWithSalt` messageSelectionStrategy
      `Prelude.hashWithSalt` promptAttemptsSpecification
      `Prelude.hashWithSalt` messageGroups
      `Prelude.hashWithSalt` maxRetries

instance Prelude.NFData PromptSpecification where
  rnf PromptSpecification' {..} =
    Prelude.rnf allowInterrupt
      `Prelude.seq` Prelude.rnf messageSelectionStrategy
      `Prelude.seq` Prelude.rnf promptAttemptsSpecification
      `Prelude.seq` Prelude.rnf messageGroups
      `Prelude.seq` Prelude.rnf maxRetries

instance Data.ToJSON PromptSpecification where
  toJSON PromptSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowInterrupt" Data..=)
              Prelude.<$> allowInterrupt,
            ("messageSelectionStrategy" Data..=)
              Prelude.<$> messageSelectionStrategy,
            ("promptAttemptsSpecification" Data..=)
              Prelude.<$> promptAttemptsSpecification,
            Prelude.Just ("messageGroups" Data..= messageGroups),
            Prelude.Just ("maxRetries" Data..= maxRetries)
          ]
      )
