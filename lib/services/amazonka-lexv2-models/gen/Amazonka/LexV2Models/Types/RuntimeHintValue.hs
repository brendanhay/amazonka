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
-- Module      : Amazonka.LexV2Models.Types.RuntimeHintValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.RuntimeHintValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the phrase that Amazon Lex should look for in the user\'s input
-- to the bot.
--
-- /See:/ 'newRuntimeHintValue' smart constructor.
data RuntimeHintValue = RuntimeHintValue'
  { -- | The phrase that Amazon Lex should look for in the user\'s input to the
    -- bot.
    phrase :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuntimeHintValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phrase', 'runtimeHintValue_phrase' - The phrase that Amazon Lex should look for in the user\'s input to the
-- bot.
newRuntimeHintValue ::
  -- | 'phrase'
  Prelude.Text ->
  RuntimeHintValue
newRuntimeHintValue pPhrase_ =
  RuntimeHintValue' {phrase = pPhrase_}

-- | The phrase that Amazon Lex should look for in the user\'s input to the
-- bot.
runtimeHintValue_phrase :: Lens.Lens' RuntimeHintValue Prelude.Text
runtimeHintValue_phrase = Lens.lens (\RuntimeHintValue' {phrase} -> phrase) (\s@RuntimeHintValue' {} a -> s {phrase = a} :: RuntimeHintValue)

instance Data.FromJSON RuntimeHintValue where
  parseJSON =
    Data.withObject
      "RuntimeHintValue"
      ( \x ->
          RuntimeHintValue' Prelude.<$> (x Data..: "phrase")
      )

instance Prelude.Hashable RuntimeHintValue where
  hashWithSalt _salt RuntimeHintValue' {..} =
    _salt `Prelude.hashWithSalt` phrase

instance Prelude.NFData RuntimeHintValue where
  rnf RuntimeHintValue' {..} = Prelude.rnf phrase
