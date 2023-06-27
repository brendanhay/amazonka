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
-- Module      : Amazonka.GuardDuty.Types.CoverageFilterCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageFilterCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a condition that when matched will be added to the response
-- of the operation.
--
-- /See:/ 'newCoverageFilterCondition' smart constructor.
data CoverageFilterCondition = CoverageFilterCondition'
  { -- | Represents an equal condition that is applied to a single field while
    -- retrieving the coverage details.
    equals :: Prelude.Maybe [Prelude.Text],
    -- | Represents a not equal condition that is applied to a single field while
    -- retrieving the coverage details.
    notEquals :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageFilterCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'equals', 'coverageFilterCondition_equals' - Represents an equal condition that is applied to a single field while
-- retrieving the coverage details.
--
-- 'notEquals', 'coverageFilterCondition_notEquals' - Represents a not equal condition that is applied to a single field while
-- retrieving the coverage details.
newCoverageFilterCondition ::
  CoverageFilterCondition
newCoverageFilterCondition =
  CoverageFilterCondition'
    { equals = Prelude.Nothing,
      notEquals = Prelude.Nothing
    }

-- | Represents an equal condition that is applied to a single field while
-- retrieving the coverage details.
coverageFilterCondition_equals :: Lens.Lens' CoverageFilterCondition (Prelude.Maybe [Prelude.Text])
coverageFilterCondition_equals = Lens.lens (\CoverageFilterCondition' {equals} -> equals) (\s@CoverageFilterCondition' {} a -> s {equals = a} :: CoverageFilterCondition) Prelude.. Lens.mapping Lens.coerced

-- | Represents a not equal condition that is applied to a single field while
-- retrieving the coverage details.
coverageFilterCondition_notEquals :: Lens.Lens' CoverageFilterCondition (Prelude.Maybe [Prelude.Text])
coverageFilterCondition_notEquals = Lens.lens (\CoverageFilterCondition' {notEquals} -> notEquals) (\s@CoverageFilterCondition' {} a -> s {notEquals = a} :: CoverageFilterCondition) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable CoverageFilterCondition where
  hashWithSalt _salt CoverageFilterCondition' {..} =
    _salt
      `Prelude.hashWithSalt` equals
      `Prelude.hashWithSalt` notEquals

instance Prelude.NFData CoverageFilterCondition where
  rnf CoverageFilterCondition' {..} =
    Prelude.rnf equals
      `Prelude.seq` Prelude.rnf notEquals

instance Data.ToJSON CoverageFilterCondition where
  toJSON CoverageFilterCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("equals" Data..=) Prelude.<$> equals,
            ("notEquals" Data..=) Prelude.<$> notEquals
          ]
      )
