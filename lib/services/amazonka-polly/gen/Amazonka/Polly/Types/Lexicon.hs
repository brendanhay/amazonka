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
-- Module      : Amazonka.Polly.Types.Lexicon
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Types.Lexicon where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides lexicon name and lexicon content in string format. For more
-- information, see
-- <https://www.w3.org/TR/pronunciation-lexicon/ Pronunciation Lexicon Specification (PLS) Version 1.0>.
--
-- /See:/ 'newLexicon' smart constructor.
data Lexicon = Lexicon'
  { -- | Name of the lexicon.
    name :: Prelude.Maybe Prelude.Text,
    -- | Lexicon content in string format. The content of a lexicon must be in
    -- PLS format.
    content :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Lexicon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'lexicon_name' - Name of the lexicon.
--
-- 'content', 'lexicon_content' - Lexicon content in string format. The content of a lexicon must be in
-- PLS format.
newLexicon ::
  Lexicon
newLexicon =
  Lexicon'
    { name = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | Name of the lexicon.
lexicon_name :: Lens.Lens' Lexicon (Prelude.Maybe Prelude.Text)
lexicon_name = Lens.lens (\Lexicon' {name} -> name) (\s@Lexicon' {} a -> s {name = a} :: Lexicon)

-- | Lexicon content in string format. The content of a lexicon must be in
-- PLS format.
lexicon_content :: Lens.Lens' Lexicon (Prelude.Maybe Prelude.Text)
lexicon_content = Lens.lens (\Lexicon' {content} -> content) (\s@Lexicon' {} a -> s {content = a} :: Lexicon) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON Lexicon where
  parseJSON =
    Core.withObject
      "Lexicon"
      ( \x ->
          Lexicon'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Content")
      )

instance Prelude.Hashable Lexicon where
  hashWithSalt _salt Lexicon' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` content

instance Prelude.NFData Lexicon where
  rnf Lexicon' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf content
