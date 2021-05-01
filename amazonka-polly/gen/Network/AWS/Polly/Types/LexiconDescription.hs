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
-- Module      : Network.AWS.Polly.Types.LexiconDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.LexiconDescription where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.LexiconAttributes
import qualified Network.AWS.Prelude as Prelude

-- | Describes the content of the lexicon.
--
-- /See:/ 'newLexiconDescription' smart constructor.
data LexiconDescription = LexiconDescription'
  { -- | Provides lexicon metadata.
    attributes :: Prelude.Maybe LexiconAttributes,
    -- | Name of the lexicon.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LexiconDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'lexiconDescription_attributes' - Provides lexicon metadata.
--
-- 'name', 'lexiconDescription_name' - Name of the lexicon.
newLexiconDescription ::
  LexiconDescription
newLexiconDescription =
  LexiconDescription'
    { attributes = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Provides lexicon metadata.
lexiconDescription_attributes :: Lens.Lens' LexiconDescription (Prelude.Maybe LexiconAttributes)
lexiconDescription_attributes = Lens.lens (\LexiconDescription' {attributes} -> attributes) (\s@LexiconDescription' {} a -> s {attributes = a} :: LexiconDescription)

-- | Name of the lexicon.
lexiconDescription_name :: Lens.Lens' LexiconDescription (Prelude.Maybe Prelude.Text)
lexiconDescription_name = Lens.lens (\LexiconDescription' {name} -> name) (\s@LexiconDescription' {} a -> s {name = a} :: LexiconDescription)

instance Prelude.FromJSON LexiconDescription where
  parseJSON =
    Prelude.withObject
      "LexiconDescription"
      ( \x ->
          LexiconDescription'
            Prelude.<$> (x Prelude..:? "Attributes")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable LexiconDescription

instance Prelude.NFData LexiconDescription
