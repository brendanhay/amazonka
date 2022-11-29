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
-- Module      : Amazonka.Kendra.Types.SpellCorrectedQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SpellCorrectedQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.Correction
import qualified Amazonka.Prelude as Prelude

-- | A query with suggested spell corrections.
--
-- /See:/ 'newSpellCorrectedQuery' smart constructor.
data SpellCorrectedQuery = SpellCorrectedQuery'
  { -- | The query with the suggested spell corrections.
    suggestedQueryText :: Prelude.Maybe Prelude.Text,
    -- | The corrected misspelled word or words in a query.
    corrections :: Prelude.Maybe [Correction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpellCorrectedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suggestedQueryText', 'spellCorrectedQuery_suggestedQueryText' - The query with the suggested spell corrections.
--
-- 'corrections', 'spellCorrectedQuery_corrections' - The corrected misspelled word or words in a query.
newSpellCorrectedQuery ::
  SpellCorrectedQuery
newSpellCorrectedQuery =
  SpellCorrectedQuery'
    { suggestedQueryText =
        Prelude.Nothing,
      corrections = Prelude.Nothing
    }

-- | The query with the suggested spell corrections.
spellCorrectedQuery_suggestedQueryText :: Lens.Lens' SpellCorrectedQuery (Prelude.Maybe Prelude.Text)
spellCorrectedQuery_suggestedQueryText = Lens.lens (\SpellCorrectedQuery' {suggestedQueryText} -> suggestedQueryText) (\s@SpellCorrectedQuery' {} a -> s {suggestedQueryText = a} :: SpellCorrectedQuery)

-- | The corrected misspelled word or words in a query.
spellCorrectedQuery_corrections :: Lens.Lens' SpellCorrectedQuery (Prelude.Maybe [Correction])
spellCorrectedQuery_corrections = Lens.lens (\SpellCorrectedQuery' {corrections} -> corrections) (\s@SpellCorrectedQuery' {} a -> s {corrections = a} :: SpellCorrectedQuery) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SpellCorrectedQuery where
  parseJSON =
    Core.withObject
      "SpellCorrectedQuery"
      ( \x ->
          SpellCorrectedQuery'
            Prelude.<$> (x Core..:? "SuggestedQueryText")
            Prelude.<*> (x Core..:? "Corrections" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SpellCorrectedQuery where
  hashWithSalt _salt SpellCorrectedQuery' {..} =
    _salt `Prelude.hashWithSalt` suggestedQueryText
      `Prelude.hashWithSalt` corrections

instance Prelude.NFData SpellCorrectedQuery where
  rnf SpellCorrectedQuery' {..} =
    Prelude.rnf suggestedQueryText
      `Prelude.seq` Prelude.rnf corrections
