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
-- Module      : Amazonka.GuardDuty.Types.FilterCriterion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.FilterCriterion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.CriterionKey
import Amazonka.GuardDuty.Types.FilterCondition
import qualified Amazonka.Prelude as Prelude

-- | Represents a condition that when matched will be added to the response
-- of the operation. Irrespective of using any filter criteria, an
-- administrator account can view the scan entries for all of its member
-- accounts. However, each member account can view the scan entries only
-- for their own account.
--
-- /See:/ 'newFilterCriterion' smart constructor.
data FilterCriterion = FilterCriterion'
  { -- | An enum value representing possible scan properties to match with given
    -- scan entries.
    criterionKey :: Prelude.Maybe CriterionKey,
    -- | Contains information about the condition.
    filterCondition :: Prelude.Maybe FilterCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterCriterion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'criterionKey', 'filterCriterion_criterionKey' - An enum value representing possible scan properties to match with given
-- scan entries.
--
-- 'filterCondition', 'filterCriterion_filterCondition' - Contains information about the condition.
newFilterCriterion ::
  FilterCriterion
newFilterCriterion =
  FilterCriterion'
    { criterionKey = Prelude.Nothing,
      filterCondition = Prelude.Nothing
    }

-- | An enum value representing possible scan properties to match with given
-- scan entries.
filterCriterion_criterionKey :: Lens.Lens' FilterCriterion (Prelude.Maybe CriterionKey)
filterCriterion_criterionKey = Lens.lens (\FilterCriterion' {criterionKey} -> criterionKey) (\s@FilterCriterion' {} a -> s {criterionKey = a} :: FilterCriterion)

-- | Contains information about the condition.
filterCriterion_filterCondition :: Lens.Lens' FilterCriterion (Prelude.Maybe FilterCondition)
filterCriterion_filterCondition = Lens.lens (\FilterCriterion' {filterCondition} -> filterCondition) (\s@FilterCriterion' {} a -> s {filterCondition = a} :: FilterCriterion)

instance Prelude.Hashable FilterCriterion where
  hashWithSalt _salt FilterCriterion' {..} =
    _salt
      `Prelude.hashWithSalt` criterionKey
      `Prelude.hashWithSalt` filterCondition

instance Prelude.NFData FilterCriterion where
  rnf FilterCriterion' {..} =
    Prelude.rnf criterionKey
      `Prelude.seq` Prelude.rnf filterCondition

instance Data.ToJSON FilterCriterion where
  toJSON FilterCriterion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("criterionKey" Data..=) Prelude.<$> criterionKey,
            ("filterCondition" Data..=)
              Prelude.<$> filterCondition
          ]
      )
