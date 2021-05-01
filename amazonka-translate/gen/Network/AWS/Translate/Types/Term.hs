{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Translate.Types.Term
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.Term where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The term being translated by the custom terminology.
--
-- /See:/ 'newTerm' smart constructor.
data Term = Term'
  { -- | The target text of the term being translated by the custom terminology.
    targetText :: Prelude.Maybe Prelude.Text,
    -- | The source text of the term being translated by the custom terminology.
    sourceText :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Term where
  parseJSON =
    Prelude.withObject
      "Term"
      ( \x ->
          Term'
            Prelude.<$> (x Prelude..:? "TargetText")
            Prelude.<*> (x Prelude..:? "SourceText")
      )

instance Prelude.Hashable Term

instance Prelude.NFData Term
