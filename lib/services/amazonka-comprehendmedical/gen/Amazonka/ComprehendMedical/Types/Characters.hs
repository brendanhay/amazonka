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
-- Module      : Amazonka.ComprehendMedical.Types.Characters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.Characters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The number of characters in the input text to be analyzed.
--
-- /See:/ 'newCharacters' smart constructor.
data Characters = Characters'
  { -- | The number of characters present in the input text document as processed
    -- by Comprehend Medical.
    originalTextCharacters :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Characters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originalTextCharacters', 'characters_originalTextCharacters' - The number of characters present in the input text document as processed
-- by Comprehend Medical.
newCharacters ::
  Characters
newCharacters =
  Characters'
    { originalTextCharacters =
        Prelude.Nothing
    }

-- | The number of characters present in the input text document as processed
-- by Comprehend Medical.
characters_originalTextCharacters :: Lens.Lens' Characters (Prelude.Maybe Prelude.Int)
characters_originalTextCharacters = Lens.lens (\Characters' {originalTextCharacters} -> originalTextCharacters) (\s@Characters' {} a -> s {originalTextCharacters = a} :: Characters)

instance Data.FromJSON Characters where
  parseJSON =
    Data.withObject
      "Characters"
      ( \x ->
          Characters'
            Prelude.<$> (x Data..:? "OriginalTextCharacters")
      )

instance Prelude.Hashable Characters where
  hashWithSalt _salt Characters' {..} =
    _salt `Prelude.hashWithSalt` originalTextCharacters

instance Prelude.NFData Characters where
  rnf Characters' {..} =
    Prelude.rnf originalTextCharacters
