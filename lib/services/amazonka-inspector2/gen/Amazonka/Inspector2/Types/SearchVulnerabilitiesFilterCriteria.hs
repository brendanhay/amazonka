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
-- Module      : Amazonka.Inspector2.Types.SearchVulnerabilitiesFilterCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.SearchVulnerabilitiesFilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on the criteria used to define the filter for a vulnerability
-- search.
--
-- /See:/ 'newSearchVulnerabilitiesFilterCriteria' smart constructor.
data SearchVulnerabilitiesFilterCriteria = SearchVulnerabilitiesFilterCriteria'
  { -- | The IDs for specific vulnerabilities.
    vulnerabilityIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchVulnerabilitiesFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vulnerabilityIds', 'searchVulnerabilitiesFilterCriteria_vulnerabilityIds' - The IDs for specific vulnerabilities.
newSearchVulnerabilitiesFilterCriteria ::
  -- | 'vulnerabilityIds'
  Prelude.NonEmpty Prelude.Text ->
  SearchVulnerabilitiesFilterCriteria
newSearchVulnerabilitiesFilterCriteria
  pVulnerabilityIds_ =
    SearchVulnerabilitiesFilterCriteria'
      { vulnerabilityIds =
          Lens.coerced
            Lens.# pVulnerabilityIds_
      }

-- | The IDs for specific vulnerabilities.
searchVulnerabilitiesFilterCriteria_vulnerabilityIds :: Lens.Lens' SearchVulnerabilitiesFilterCriteria (Prelude.NonEmpty Prelude.Text)
searchVulnerabilitiesFilterCriteria_vulnerabilityIds = Lens.lens (\SearchVulnerabilitiesFilterCriteria' {vulnerabilityIds} -> vulnerabilityIds) (\s@SearchVulnerabilitiesFilterCriteria' {} a -> s {vulnerabilityIds = a} :: SearchVulnerabilitiesFilterCriteria) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    SearchVulnerabilitiesFilterCriteria
  where
  hashWithSalt
    _salt
    SearchVulnerabilitiesFilterCriteria' {..} =
      _salt `Prelude.hashWithSalt` vulnerabilityIds

instance
  Prelude.NFData
    SearchVulnerabilitiesFilterCriteria
  where
  rnf SearchVulnerabilitiesFilterCriteria' {..} =
    Prelude.rnf vulnerabilityIds

instance
  Data.ToJSON
    SearchVulnerabilitiesFilterCriteria
  where
  toJSON SearchVulnerabilitiesFilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("vulnerabilityIds" Data..= vulnerabilityIds)
          ]
      )
