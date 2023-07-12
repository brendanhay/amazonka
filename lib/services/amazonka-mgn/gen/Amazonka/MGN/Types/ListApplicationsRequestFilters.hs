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
-- Module      : Amazonka.MGN.Types.ListApplicationsRequestFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ListApplicationsRequestFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Applications list filters.
--
-- /See:/ 'newListApplicationsRequestFilters' smart constructor.
data ListApplicationsRequestFilters = ListApplicationsRequestFilters'
  { -- | Filter applications list by application ID.
    applicationIDs :: Prelude.Maybe [Prelude.Text],
    -- | Filter applications list by archival status.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | Filter applications list by wave ID.
    waveIDs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationsRequestFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationIDs', 'listApplicationsRequestFilters_applicationIDs' - Filter applications list by application ID.
--
-- 'isArchived', 'listApplicationsRequestFilters_isArchived' - Filter applications list by archival status.
--
-- 'waveIDs', 'listApplicationsRequestFilters_waveIDs' - Filter applications list by wave ID.
newListApplicationsRequestFilters ::
  ListApplicationsRequestFilters
newListApplicationsRequestFilters =
  ListApplicationsRequestFilters'
    { applicationIDs =
        Prelude.Nothing,
      isArchived = Prelude.Nothing,
      waveIDs = Prelude.Nothing
    }

-- | Filter applications list by application ID.
listApplicationsRequestFilters_applicationIDs :: Lens.Lens' ListApplicationsRequestFilters (Prelude.Maybe [Prelude.Text])
listApplicationsRequestFilters_applicationIDs = Lens.lens (\ListApplicationsRequestFilters' {applicationIDs} -> applicationIDs) (\s@ListApplicationsRequestFilters' {} a -> s {applicationIDs = a} :: ListApplicationsRequestFilters) Prelude.. Lens.mapping Lens.coerced

-- | Filter applications list by archival status.
listApplicationsRequestFilters_isArchived :: Lens.Lens' ListApplicationsRequestFilters (Prelude.Maybe Prelude.Bool)
listApplicationsRequestFilters_isArchived = Lens.lens (\ListApplicationsRequestFilters' {isArchived} -> isArchived) (\s@ListApplicationsRequestFilters' {} a -> s {isArchived = a} :: ListApplicationsRequestFilters)

-- | Filter applications list by wave ID.
listApplicationsRequestFilters_waveIDs :: Lens.Lens' ListApplicationsRequestFilters (Prelude.Maybe [Prelude.Text])
listApplicationsRequestFilters_waveIDs = Lens.lens (\ListApplicationsRequestFilters' {waveIDs} -> waveIDs) (\s@ListApplicationsRequestFilters' {} a -> s {waveIDs = a} :: ListApplicationsRequestFilters) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    ListApplicationsRequestFilters
  where
  hashWithSalt
    _salt
    ListApplicationsRequestFilters' {..} =
      _salt
        `Prelude.hashWithSalt` applicationIDs
        `Prelude.hashWithSalt` isArchived
        `Prelude.hashWithSalt` waveIDs

instance
  Prelude.NFData
    ListApplicationsRequestFilters
  where
  rnf ListApplicationsRequestFilters' {..} =
    Prelude.rnf applicationIDs
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf waveIDs

instance Data.ToJSON ListApplicationsRequestFilters where
  toJSON ListApplicationsRequestFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationIDs" Data..=)
              Prelude.<$> applicationIDs,
            ("isArchived" Data..=) Prelude.<$> isArchived,
            ("waveIDs" Data..=) Prelude.<$> waveIDs
          ]
      )
