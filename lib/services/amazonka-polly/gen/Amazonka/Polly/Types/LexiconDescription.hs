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
-- Module      : Amazonka.Polly.Types.LexiconDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Types.LexiconDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Polly.Types.LexiconAttributes
import qualified Amazonka.Prelude as Prelude

-- | Describes the content of the lexicon.
--
-- /See:/ 'newLexiconDescription' smart constructor.
data LexiconDescription = LexiconDescription'
  { -- | Name of the lexicon.
    name :: Prelude.Maybe Prelude.Text,
    -- | Provides lexicon metadata.
    attributes :: Prelude.Maybe LexiconAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LexiconDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'lexiconDescription_name' - Name of the lexicon.
--
-- 'attributes', 'lexiconDescription_attributes' - Provides lexicon metadata.
newLexiconDescription ::
  LexiconDescription
newLexiconDescription =
  LexiconDescription'
    { name = Prelude.Nothing,
      attributes = Prelude.Nothing
    }

-- | Name of the lexicon.
lexiconDescription_name :: Lens.Lens' LexiconDescription (Prelude.Maybe Prelude.Text)
lexiconDescription_name = Lens.lens (\LexiconDescription' {name} -> name) (\s@LexiconDescription' {} a -> s {name = a} :: LexiconDescription)

-- | Provides lexicon metadata.
lexiconDescription_attributes :: Lens.Lens' LexiconDescription (Prelude.Maybe LexiconAttributes)
lexiconDescription_attributes = Lens.lens (\LexiconDescription' {attributes} -> attributes) (\s@LexiconDescription' {} a -> s {attributes = a} :: LexiconDescription)

instance Core.FromJSON LexiconDescription where
  parseJSON =
    Core.withObject
      "LexiconDescription"
      ( \x ->
          LexiconDescription'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Attributes")
      )

instance Prelude.Hashable LexiconDescription where
  hashWithSalt _salt LexiconDescription' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData LexiconDescription where
  rnf LexiconDescription' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf attributes
