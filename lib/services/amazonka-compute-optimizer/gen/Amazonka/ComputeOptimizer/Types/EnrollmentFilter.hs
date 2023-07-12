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
-- Module      : Amazonka.ComputeOptimizer.Types.EnrollmentFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.EnrollmentFilter where

import Amazonka.ComputeOptimizer.Types.EnrollmentFilterName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a filter that returns a more specific list of account
-- enrollment statuses. Use this filter with the
-- GetEnrollmentStatusesForOrganization action.
--
-- /See:/ 'newEnrollmentFilter' smart constructor.
data EnrollmentFilter = EnrollmentFilter'
  { -- | The name of the filter.
    --
    -- Specify @Status@ to return accounts with a specific enrollment status
    -- (for example, @Active@).
    name :: Prelude.Maybe EnrollmentFilterName,
    -- | The value of the filter.
    --
    -- The valid values are @Active@, @Inactive@, @Pending@, and @Failed@.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnrollmentFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'enrollmentFilter_name' - The name of the filter.
--
-- Specify @Status@ to return accounts with a specific enrollment status
-- (for example, @Active@).
--
-- 'values', 'enrollmentFilter_values' - The value of the filter.
--
-- The valid values are @Active@, @Inactive@, @Pending@, and @Failed@.
newEnrollmentFilter ::
  EnrollmentFilter
newEnrollmentFilter =
  EnrollmentFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter.
--
-- Specify @Status@ to return accounts with a specific enrollment status
-- (for example, @Active@).
enrollmentFilter_name :: Lens.Lens' EnrollmentFilter (Prelude.Maybe EnrollmentFilterName)
enrollmentFilter_name = Lens.lens (\EnrollmentFilter' {name} -> name) (\s@EnrollmentFilter' {} a -> s {name = a} :: EnrollmentFilter)

-- | The value of the filter.
--
-- The valid values are @Active@, @Inactive@, @Pending@, and @Failed@.
enrollmentFilter_values :: Lens.Lens' EnrollmentFilter (Prelude.Maybe [Prelude.Text])
enrollmentFilter_values = Lens.lens (\EnrollmentFilter' {values} -> values) (\s@EnrollmentFilter' {} a -> s {values = a} :: EnrollmentFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable EnrollmentFilter where
  hashWithSalt _salt EnrollmentFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData EnrollmentFilter where
  rnf EnrollmentFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON EnrollmentFilter where
  toJSON EnrollmentFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
