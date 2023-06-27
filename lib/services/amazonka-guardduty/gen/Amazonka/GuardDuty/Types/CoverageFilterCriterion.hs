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
-- Module      : Amazonka.GuardDuty.Types.CoverageFilterCriterion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageFilterCriterion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.CoverageFilterCondition
import Amazonka.GuardDuty.Types.CoverageFilterCriterionKey
import qualified Amazonka.Prelude as Prelude

-- | Represents a condition that when matched will be added to the response
-- of the operation.
--
-- /See:/ 'newCoverageFilterCriterion' smart constructor.
data CoverageFilterCriterion = CoverageFilterCriterion'
  { -- | An enum value representing possible filter fields.
    criterionKey :: Prelude.Maybe CoverageFilterCriterionKey,
    -- | Contains information about the condition.
    filterCondition :: Prelude.Maybe CoverageFilterCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageFilterCriterion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'criterionKey', 'coverageFilterCriterion_criterionKey' - An enum value representing possible filter fields.
--
-- 'filterCondition', 'coverageFilterCriterion_filterCondition' - Contains information about the condition.
newCoverageFilterCriterion ::
  CoverageFilterCriterion
newCoverageFilterCriterion =
  CoverageFilterCriterion'
    { criterionKey =
        Prelude.Nothing,
      filterCondition = Prelude.Nothing
    }

-- | An enum value representing possible filter fields.
coverageFilterCriterion_criterionKey :: Lens.Lens' CoverageFilterCriterion (Prelude.Maybe CoverageFilterCriterionKey)
coverageFilterCriterion_criterionKey = Lens.lens (\CoverageFilterCriterion' {criterionKey} -> criterionKey) (\s@CoverageFilterCriterion' {} a -> s {criterionKey = a} :: CoverageFilterCriterion)

-- | Contains information about the condition.
coverageFilterCriterion_filterCondition :: Lens.Lens' CoverageFilterCriterion (Prelude.Maybe CoverageFilterCondition)
coverageFilterCriterion_filterCondition = Lens.lens (\CoverageFilterCriterion' {filterCondition} -> filterCondition) (\s@CoverageFilterCriterion' {} a -> s {filterCondition = a} :: CoverageFilterCriterion)

instance Prelude.Hashable CoverageFilterCriterion where
  hashWithSalt _salt CoverageFilterCriterion' {..} =
    _salt
      `Prelude.hashWithSalt` criterionKey
      `Prelude.hashWithSalt` filterCondition

instance Prelude.NFData CoverageFilterCriterion where
  rnf CoverageFilterCriterion' {..} =
    Prelude.rnf criterionKey
      `Prelude.seq` Prelude.rnf filterCondition

instance Data.ToJSON CoverageFilterCriterion where
  toJSON CoverageFilterCriterion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("criterionKey" Data..=) Prelude.<$> criterionKey,
            ("filterCondition" Data..=)
              Prelude.<$> filterCondition
          ]
      )
