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
-- Module      : Amazonka.LexV2Models.Types.PromptAttemptSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.PromptAttemptSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.AllowedInputTypes
import Amazonka.LexV2Models.Types.AudioAndDTMFInputSpecification
import Amazonka.LexV2Models.Types.TextInputSpecification
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings on a prompt attempt.
--
-- /See:/ 'newPromptAttemptSpecification' smart constructor.
data PromptAttemptSpecification = PromptAttemptSpecification'
  { -- | Indicates whether the user can interrupt a speech prompt attempt from
    -- the bot.
    allowInterrupt :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the settings on audio and DTMF input.
    audioAndDTMFInputSpecification :: Prelude.Maybe AudioAndDTMFInputSpecification,
    -- | Specifies the settings on text input.
    textInputSpecification :: Prelude.Maybe TextInputSpecification,
    -- | Indicates the allowed input types of the prompt attempt.
    allowedInputTypes :: AllowedInputTypes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromptAttemptSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowInterrupt', 'promptAttemptSpecification_allowInterrupt' - Indicates whether the user can interrupt a speech prompt attempt from
-- the bot.
--
-- 'audioAndDTMFInputSpecification', 'promptAttemptSpecification_audioAndDTMFInputSpecification' - Specifies the settings on audio and DTMF input.
--
-- 'textInputSpecification', 'promptAttemptSpecification_textInputSpecification' - Specifies the settings on text input.
--
-- 'allowedInputTypes', 'promptAttemptSpecification_allowedInputTypes' - Indicates the allowed input types of the prompt attempt.
newPromptAttemptSpecification ::
  -- | 'allowedInputTypes'
  AllowedInputTypes ->
  PromptAttemptSpecification
newPromptAttemptSpecification pAllowedInputTypes_ =
  PromptAttemptSpecification'
    { allowInterrupt =
        Prelude.Nothing,
      audioAndDTMFInputSpecification =
        Prelude.Nothing,
      textInputSpecification = Prelude.Nothing,
      allowedInputTypes = pAllowedInputTypes_
    }

-- | Indicates whether the user can interrupt a speech prompt attempt from
-- the bot.
promptAttemptSpecification_allowInterrupt :: Lens.Lens' PromptAttemptSpecification (Prelude.Maybe Prelude.Bool)
promptAttemptSpecification_allowInterrupt = Lens.lens (\PromptAttemptSpecification' {allowInterrupt} -> allowInterrupt) (\s@PromptAttemptSpecification' {} a -> s {allowInterrupt = a} :: PromptAttemptSpecification)

-- | Specifies the settings on audio and DTMF input.
promptAttemptSpecification_audioAndDTMFInputSpecification :: Lens.Lens' PromptAttemptSpecification (Prelude.Maybe AudioAndDTMFInputSpecification)
promptAttemptSpecification_audioAndDTMFInputSpecification = Lens.lens (\PromptAttemptSpecification' {audioAndDTMFInputSpecification} -> audioAndDTMFInputSpecification) (\s@PromptAttemptSpecification' {} a -> s {audioAndDTMFInputSpecification = a} :: PromptAttemptSpecification)

-- | Specifies the settings on text input.
promptAttemptSpecification_textInputSpecification :: Lens.Lens' PromptAttemptSpecification (Prelude.Maybe TextInputSpecification)
promptAttemptSpecification_textInputSpecification = Lens.lens (\PromptAttemptSpecification' {textInputSpecification} -> textInputSpecification) (\s@PromptAttemptSpecification' {} a -> s {textInputSpecification = a} :: PromptAttemptSpecification)

-- | Indicates the allowed input types of the prompt attempt.
promptAttemptSpecification_allowedInputTypes :: Lens.Lens' PromptAttemptSpecification AllowedInputTypes
promptAttemptSpecification_allowedInputTypes = Lens.lens (\PromptAttemptSpecification' {allowedInputTypes} -> allowedInputTypes) (\s@PromptAttemptSpecification' {} a -> s {allowedInputTypes = a} :: PromptAttemptSpecification)

instance Data.FromJSON PromptAttemptSpecification where
  parseJSON =
    Data.withObject
      "PromptAttemptSpecification"
      ( \x ->
          PromptAttemptSpecification'
            Prelude.<$> (x Data..:? "allowInterrupt")
            Prelude.<*> (x Data..:? "audioAndDTMFInputSpecification")
            Prelude.<*> (x Data..:? "textInputSpecification")
            Prelude.<*> (x Data..: "allowedInputTypes")
      )

instance Prelude.Hashable PromptAttemptSpecification where
  hashWithSalt _salt PromptAttemptSpecification' {..} =
    _salt `Prelude.hashWithSalt` allowInterrupt
      `Prelude.hashWithSalt` audioAndDTMFInputSpecification
      `Prelude.hashWithSalt` textInputSpecification
      `Prelude.hashWithSalt` allowedInputTypes

instance Prelude.NFData PromptAttemptSpecification where
  rnf PromptAttemptSpecification' {..} =
    Prelude.rnf allowInterrupt
      `Prelude.seq` Prelude.rnf audioAndDTMFInputSpecification
      `Prelude.seq` Prelude.rnf textInputSpecification
      `Prelude.seq` Prelude.rnf allowedInputTypes

instance Data.ToJSON PromptAttemptSpecification where
  toJSON PromptAttemptSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowInterrupt" Data..=)
              Prelude.<$> allowInterrupt,
            ("audioAndDTMFInputSpecification" Data..=)
              Prelude.<$> audioAndDTMFInputSpecification,
            ("textInputSpecification" Data..=)
              Prelude.<$> textInputSpecification,
            Prelude.Just
              ("allowedInputTypes" Data..= allowedInputTypes)
          ]
      )
