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
-- Module      : Amazonka.SSM.Types.DescribeActivationsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DescribeActivationsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.DescribeActivationsFilterKeys

-- | Filter for the DescribeActivation API.
--
-- /See:/ 'newDescribeActivationsFilter' smart constructor.
data DescribeActivationsFilter = DescribeActivationsFilter'
  { -- | The name of the filter.
    filterKey :: Prelude.Maybe DescribeActivationsFilterKeys,
    -- | The filter values.
    filterValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeActivationsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterKey', 'describeActivationsFilter_filterKey' - The name of the filter.
--
-- 'filterValues', 'describeActivationsFilter_filterValues' - The filter values.
newDescribeActivationsFilter ::
  DescribeActivationsFilter
newDescribeActivationsFilter =
  DescribeActivationsFilter'
    { filterKey =
        Prelude.Nothing,
      filterValues = Prelude.Nothing
    }

-- | The name of the filter.
describeActivationsFilter_filterKey :: Lens.Lens' DescribeActivationsFilter (Prelude.Maybe DescribeActivationsFilterKeys)
describeActivationsFilter_filterKey = Lens.lens (\DescribeActivationsFilter' {filterKey} -> filterKey) (\s@DescribeActivationsFilter' {} a -> s {filterKey = a} :: DescribeActivationsFilter)

-- | The filter values.
describeActivationsFilter_filterValues :: Lens.Lens' DescribeActivationsFilter (Prelude.Maybe [Prelude.Text])
describeActivationsFilter_filterValues = Lens.lens (\DescribeActivationsFilter' {filterValues} -> filterValues) (\s@DescribeActivationsFilter' {} a -> s {filterValues = a} :: DescribeActivationsFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable DescribeActivationsFilter where
  hashWithSalt _salt DescribeActivationsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` filterKey
      `Prelude.hashWithSalt` filterValues

instance Prelude.NFData DescribeActivationsFilter where
  rnf DescribeActivationsFilter' {..} =
    Prelude.rnf filterKey
      `Prelude.seq` Prelude.rnf filterValues

instance Data.ToJSON DescribeActivationsFilter where
  toJSON DescribeActivationsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterKey" Data..=) Prelude.<$> filterKey,
            ("FilterValues" Data..=) Prelude.<$> filterValues
          ]
      )
