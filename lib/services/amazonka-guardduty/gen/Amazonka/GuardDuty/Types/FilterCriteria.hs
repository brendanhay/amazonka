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
-- Module      : Amazonka.GuardDuty.Types.FilterCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.FilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.FilterCriterion
import qualified Amazonka.Prelude as Prelude

-- | Represents the criteria to be used in the filter for describing scan
-- entries.
--
-- /See:/ 'newFilterCriteria' smart constructor.
data FilterCriteria = FilterCriteria'
  { -- | Represents a condition that when matched will be added to the response
    -- of the operation.
    filterCriterion :: Prelude.Maybe [FilterCriterion]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterCriterion', 'filterCriteria_filterCriterion' - Represents a condition that when matched will be added to the response
-- of the operation.
newFilterCriteria ::
  FilterCriteria
newFilterCriteria =
  FilterCriteria' {filterCriterion = Prelude.Nothing}

-- | Represents a condition that when matched will be added to the response
-- of the operation.
filterCriteria_filterCriterion :: Lens.Lens' FilterCriteria (Prelude.Maybe [FilterCriterion])
filterCriteria_filterCriterion = Lens.lens (\FilterCriteria' {filterCriterion} -> filterCriterion) (\s@FilterCriteria' {} a -> s {filterCriterion = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable FilterCriteria where
  hashWithSalt _salt FilterCriteria' {..} =
    _salt `Prelude.hashWithSalt` filterCriterion

instance Prelude.NFData FilterCriteria where
  rnf FilterCriteria' {..} = Prelude.rnf filterCriterion

instance Data.ToJSON FilterCriteria where
  toJSON FilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterCriterion" Data..=)
              Prelude.<$> filterCriterion
          ]
      )
