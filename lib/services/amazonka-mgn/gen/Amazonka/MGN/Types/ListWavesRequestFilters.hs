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
-- Module      : Amazonka.MGN.Types.ListWavesRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ListWavesRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Waves list filters.
--
-- /See:/ 'newListWavesRequestFilters' smart constructor.
data ListWavesRequestFilters = ListWavesRequestFilters'
  { -- | Filter waves list by archival status.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | Filter waves list by wave ID.
    waveIDs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWavesRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isArchived', 'listWavesRequestFilters_isArchived' - Filter waves list by archival status.
--
-- 'waveIDs', 'listWavesRequestFilters_waveIDs' - Filter waves list by wave ID.
newListWavesRequestFilters ::
  ListWavesRequestFilters
newListWavesRequestFilters =
  ListWavesRequestFilters'
    { isArchived =
        Prelude.Nothing,
      waveIDs = Prelude.Nothing
    }

-- | Filter waves list by archival status.
listWavesRequestFilters_isArchived :: Lens.Lens' ListWavesRequestFilters (Prelude.Maybe Prelude.Bool)
listWavesRequestFilters_isArchived = Lens.lens (\ListWavesRequestFilters' {isArchived} -> isArchived) (\s@ListWavesRequestFilters' {} a -> s {isArchived = a} :: ListWavesRequestFilters)

-- | Filter waves list by wave ID.
listWavesRequestFilters_waveIDs :: Lens.Lens' ListWavesRequestFilters (Prelude.Maybe [Prelude.Text])
listWavesRequestFilters_waveIDs = Lens.lens (\ListWavesRequestFilters' {waveIDs} -> waveIDs) (\s@ListWavesRequestFilters' {} a -> s {waveIDs = a} :: ListWavesRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ListWavesRequestFilters where
  hashWithSalt _salt ListWavesRequestFilters' {..} =
    _salt
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` waveIDs

instance Prelude.NFData ListWavesRequestFilters where
  rnf ListWavesRequestFilters' {..} =
    Prelude.rnf isArchived `Prelude.seq`
      Prelude.rnf waveIDs

instance Data.ToJSON ListWavesRequestFilters where
  toJSON ListWavesRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("isArchived" Data..=) Prelude.<$> isArchived,
            ("waveIDs" Data..=) Prelude.<$> waveIDs
          ]
      )
