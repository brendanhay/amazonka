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
-- Module      : Amazonka.MacieV2.Types.FindingCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FindingCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.CriterionAdditionalProperties
import qualified Amazonka.Prelude as Prelude

-- | Specifies, as a map, one or more property-based conditions that filter
-- the results of a query for findings.
--
-- /See:/ 'newFindingCriteria' smart constructor.
data FindingCriteria = FindingCriteria'
  { -- | A condition that specifies the property, operator, and one or more
    -- values to use to filter the results.
    criterion :: Prelude.Maybe (Prelude.HashMap Prelude.Text CriterionAdditionalProperties)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'criterion', 'findingCriteria_criterion' - A condition that specifies the property, operator, and one or more
-- values to use to filter the results.
newFindingCriteria ::
  FindingCriteria
newFindingCriteria =
  FindingCriteria' {criterion = Prelude.Nothing}

-- | A condition that specifies the property, operator, and one or more
-- values to use to filter the results.
findingCriteria_criterion :: Lens.Lens' FindingCriteria (Prelude.Maybe (Prelude.HashMap Prelude.Text CriterionAdditionalProperties))
findingCriteria_criterion = Lens.lens (\FindingCriteria' {criterion} -> criterion) (\s@FindingCriteria' {} a -> s {criterion = a} :: FindingCriteria) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FindingCriteria where
  parseJSON =
    Data.withObject
      "FindingCriteria"
      ( \x ->
          FindingCriteria'
            Prelude.<$> (x Data..:? "criterion" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FindingCriteria where
  hashWithSalt _salt FindingCriteria' {..} =
    _salt `Prelude.hashWithSalt` criterion

instance Prelude.NFData FindingCriteria where
  rnf FindingCriteria' {..} = Prelude.rnf criterion

instance Data.ToJSON FindingCriteria where
  toJSON FindingCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [("criterion" Data..=) Prelude.<$> criterion]
      )
