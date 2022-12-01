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
-- Module      : Amazonka.CustomerProfiles.Types.Consolidation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.Consolidation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The matching criteria to be used during the auto-merging process.
--
-- /See:/ 'newConsolidation' smart constructor.
data Consolidation = Consolidation'
  { -- | A list of matching criteria.
    matchingAttributesList :: Prelude.NonEmpty (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Consolidation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchingAttributesList', 'consolidation_matchingAttributesList' - A list of matching criteria.
newConsolidation ::
  -- | 'matchingAttributesList'
  Prelude.NonEmpty (Prelude.NonEmpty Prelude.Text) ->
  Consolidation
newConsolidation pMatchingAttributesList_ =
  Consolidation'
    { matchingAttributesList =
        Lens.coerced Lens.# pMatchingAttributesList_
    }

-- | A list of matching criteria.
consolidation_matchingAttributesList :: Lens.Lens' Consolidation (Prelude.NonEmpty (Prelude.NonEmpty Prelude.Text))
consolidation_matchingAttributesList = Lens.lens (\Consolidation' {matchingAttributesList} -> matchingAttributesList) (\s@Consolidation' {} a -> s {matchingAttributesList = a} :: Consolidation) Prelude.. Lens.coerced

instance Core.FromJSON Consolidation where
  parseJSON =
    Core.withObject
      "Consolidation"
      ( \x ->
          Consolidation'
            Prelude.<$> (x Core..: "MatchingAttributesList")
      )

instance Prelude.Hashable Consolidation where
  hashWithSalt _salt Consolidation' {..} =
    _salt `Prelude.hashWithSalt` matchingAttributesList

instance Prelude.NFData Consolidation where
  rnf Consolidation' {..} =
    Prelude.rnf matchingAttributesList

instance Core.ToJSON Consolidation where
  toJSON Consolidation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MatchingAttributesList"
                  Core..= matchingAttributesList
              )
          ]
      )
