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
-- Module      : Amazonka.Connect.Types.Prompt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Prompt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a prompt.
--
-- /See:/ 'newPrompt' smart constructor.
data Prompt = Prompt'
  { -- | The description of the prompt.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the prompt.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the prompt.
    promptARN :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the prompt.
    promptId :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'description', 'prompt_description' - The description of the prompt.
--
-- 'name', 'prompt_name' - The name of the prompt.
--
-- 'promptARN', 'prompt_promptARN' - The Amazon Resource Name (ARN) of the prompt.
--
-- 'promptId', 'prompt_promptId' - A unique identifier for the prompt.
--
-- 'tags', 'prompt_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
newPrompt ::
  Prompt
newPrompt =
  Prompt'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      promptARN = Prelude.Nothing,
      promptId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The description of the prompt.
prompt_description :: Lens.Lens' Prompt (Prelude.Maybe Prelude.Text)
prompt_description = Lens.lens (\Prompt' {description} -> description) (\s@Prompt' {} a -> s {description = a} :: Prompt)

-- | The name of the prompt.
prompt_name :: Lens.Lens' Prompt (Prelude.Maybe Prelude.Text)
prompt_name = Lens.lens (\Prompt' {name} -> name) (\s@Prompt' {} a -> s {name = a} :: Prompt)

-- | The Amazon Resource Name (ARN) of the prompt.
prompt_promptARN :: Lens.Lens' Prompt (Prelude.Maybe Prelude.Text)
prompt_promptARN = Lens.lens (\Prompt' {promptARN} -> promptARN) (\s@Prompt' {} a -> s {promptARN = a} :: Prompt)

-- | A unique identifier for the prompt.
prompt_promptId :: Lens.Lens' Prompt (Prelude.Maybe Prelude.Text)
prompt_promptId = Lens.lens (\Prompt' {promptId} -> promptId) (\s@Prompt' {} a -> s {promptId = a} :: Prompt)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
prompt_tags :: Lens.Lens' Prompt (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
prompt_tags = Lens.lens (\Prompt' {tags} -> tags) (\s@Prompt' {} a -> s {tags = a} :: Prompt) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Prompt where
  parseJSON =
    Data.withObject
      "Prompt"
      ( \x ->
          Prompt'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PromptARN")
            Prelude.<*> (x Data..:? "PromptId")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Prompt where
  hashWithSalt _salt Prompt' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` promptARN
      `Prelude.hashWithSalt` promptId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Prompt where
  rnf Prompt' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf promptARN
      `Prelude.seq` Prelude.rnf promptId
      `Prelude.seq` Prelude.rnf tags
