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
-- Module      : Amazonka.Translate.Types.Term
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.Term where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The term being translated by the custom terminology.
--
-- /See:/ 'newTerm' smart constructor.
data Term = Term'
  { -- | The target text of the term being translated by the custom terminology.
    targetText :: Prelude.Maybe Prelude.Text,
    -- | The source text of the term being translated by the custom terminology.
    sourceText :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Term' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetText', 'term_targetText' - The target text of the term being translated by the custom terminology.
--
-- 'sourceText', 'term_sourceText' - The source text of the term being translated by the custom terminology.
newTerm ::
  Term
newTerm =
  Term'
    { targetText = Prelude.Nothing,
      sourceText = Prelude.Nothing
    }

-- | The target text of the term being translated by the custom terminology.
term_targetText :: Lens.Lens' Term (Prelude.Maybe Prelude.Text)
term_targetText = Lens.lens (\Term' {targetText} -> targetText) (\s@Term' {} a -> s {targetText = a} :: Term)

-- | The source text of the term being translated by the custom terminology.
term_sourceText :: Lens.Lens' Term (Prelude.Maybe Prelude.Text)
term_sourceText = Lens.lens (\Term' {sourceText} -> sourceText) (\s@Term' {} a -> s {sourceText = a} :: Term)

instance Data.FromJSON Term where
  parseJSON =
    Data.withObject
      "Term"
      ( \x ->
          Term'
            Prelude.<$> (x Data..:? "TargetText")
            Prelude.<*> (x Data..:? "SourceText")
      )

instance Prelude.Hashable Term where
  hashWithSalt _salt Term' {..} =
    _salt `Prelude.hashWithSalt` targetText
      `Prelude.hashWithSalt` sourceText

instance Prelude.NFData Term where
  rnf Term' {..} =
    Prelude.rnf targetText
      `Prelude.seq` Prelude.rnf sourceText
