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
-- Module      : Network.AWS.Polly.Types.Lexicon
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.Lexicon where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    content :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
lexicon_content = Lens.lens (\Lexicon' {content} -> content) (\s@Lexicon' {} a -> s {content = a} :: Lexicon) Prelude.. Lens.mapping Prelude._Sensitive

instance Prelude.FromJSON Lexicon where
  parseJSON =
    Prelude.withObject
      "Lexicon"
      ( \x ->
          Lexicon'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Content")
      )

instance Prelude.Hashable Lexicon

instance Prelude.NFData Lexicon
