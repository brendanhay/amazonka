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
-- Module      : Amazonka.Kendra.Types.SpellCorrectionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SpellCorrectionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for suggested query spell
-- corrections.
--
-- Suggested spell corrections are based on words that appear in your
-- indexed documents and how closely a corrected word matches a misspelled
-- word.
--
-- This feature is designed with certain defaults or limits. For
-- information on the current limits and how to request more support for
-- some limits, see the
-- <https://docs.aws.amazon.com/kendra/latest/dg/query-spell-check.html Spell Checker documentation>.
--
-- /See:/ 'newSpellCorrectionConfiguration' smart constructor.
data SpellCorrectionConfiguration = SpellCorrectionConfiguration'
  { -- | @TRUE@ to suggest spell corrections for queries.
    includeQuerySpellCheckSuggestions :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpellCorrectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeQuerySpellCheckSuggestions', 'spellCorrectionConfiguration_includeQuerySpellCheckSuggestions' - @TRUE@ to suggest spell corrections for queries.
newSpellCorrectionConfiguration ::
  -- | 'includeQuerySpellCheckSuggestions'
  Prelude.Bool ->
  SpellCorrectionConfiguration
newSpellCorrectionConfiguration
  pIncludeQuerySpellCheckSuggestions_ =
    SpellCorrectionConfiguration'
      { includeQuerySpellCheckSuggestions =
          pIncludeQuerySpellCheckSuggestions_
      }

-- | @TRUE@ to suggest spell corrections for queries.
spellCorrectionConfiguration_includeQuerySpellCheckSuggestions :: Lens.Lens' SpellCorrectionConfiguration Prelude.Bool
spellCorrectionConfiguration_includeQuerySpellCheckSuggestions = Lens.lens (\SpellCorrectionConfiguration' {includeQuerySpellCheckSuggestions} -> includeQuerySpellCheckSuggestions) (\s@SpellCorrectionConfiguration' {} a -> s {includeQuerySpellCheckSuggestions = a} :: SpellCorrectionConfiguration)

instance
  Prelude.Hashable
    SpellCorrectionConfiguration
  where
  hashWithSalt _salt SpellCorrectionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` includeQuerySpellCheckSuggestions

instance Prelude.NFData SpellCorrectionConfiguration where
  rnf SpellCorrectionConfiguration' {..} =
    Prelude.rnf includeQuerySpellCheckSuggestions

instance Data.ToJSON SpellCorrectionConfiguration where
  toJSON SpellCorrectionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "IncludeQuerySpellCheckSuggestions"
                  Data..= includeQuerySpellCheckSuggestions
              )
          ]
      )
