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
-- Module      : Amazonka.GuardDuty.Types.CoverageFilterCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageFilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.CoverageFilterCriterion
import qualified Amazonka.Prelude as Prelude

-- | Represents the criteria used in the filter.
--
-- /See:/ 'newCoverageFilterCriteria' smart constructor.
data CoverageFilterCriteria = CoverageFilterCriteria'
  { -- | Represents a condition that when matched will be added to the response
    -- of the operation.
    filterCriterion :: Prelude.Maybe [CoverageFilterCriterion]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterCriterion', 'coverageFilterCriteria_filterCriterion' - Represents a condition that when matched will be added to the response
-- of the operation.
newCoverageFilterCriteria ::
  CoverageFilterCriteria
newCoverageFilterCriteria =
  CoverageFilterCriteria'
    { filterCriterion =
        Prelude.Nothing
    }

-- | Represents a condition that when matched will be added to the response
-- of the operation.
coverageFilterCriteria_filterCriterion :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe [CoverageFilterCriterion])
coverageFilterCriteria_filterCriterion = Lens.lens (\CoverageFilterCriteria' {filterCriterion} -> filterCriterion) (\s@CoverageFilterCriteria' {} a -> s {filterCriterion = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable CoverageFilterCriteria where
  hashWithSalt _salt CoverageFilterCriteria' {..} =
    _salt `Prelude.hashWithSalt` filterCriterion

instance Prelude.NFData CoverageFilterCriteria where
  rnf CoverageFilterCriteria' {..} =
    Prelude.rnf filterCriterion

instance Data.ToJSON CoverageFilterCriteria where
  toJSON CoverageFilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterCriterion" Data..=)
              Prelude.<$> filterCriterion
          ]
      )
