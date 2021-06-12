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
-- Module      : Network.AWS.SSM.Types.DescribeActivationsFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DescribeActivationsFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.DescribeActivationsFilterKeys

-- | Filter for the DescribeActivation API.
--
-- /See:/ 'newDescribeActivationsFilter' smart constructor.
data DescribeActivationsFilter = DescribeActivationsFilter'
  { -- | The name of the filter.
    filterKey :: Core.Maybe DescribeActivationsFilterKeys,
    -- | The filter values.
    filterValues :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      filterValues = Core.Nothing
    }

-- | The name of the filter.
describeActivationsFilter_filterKey :: Lens.Lens' DescribeActivationsFilter (Core.Maybe DescribeActivationsFilterKeys)
describeActivationsFilter_filterKey = Lens.lens (\DescribeActivationsFilter' {filterKey} -> filterKey) (\s@DescribeActivationsFilter' {} a -> s {filterKey = a} :: DescribeActivationsFilter)

-- | The filter values.
describeActivationsFilter_filterValues :: Lens.Lens' DescribeActivationsFilter (Core.Maybe [Core.Text])
describeActivationsFilter_filterValues = Lens.lens (\DescribeActivationsFilter' {filterValues} -> filterValues) (\s@DescribeActivationsFilter' {} a -> s {filterValues = a} :: DescribeActivationsFilter) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable DescribeActivationsFilter

instance Core.NFData DescribeActivationsFilter

instance Core.ToJSON DescribeActivationsFilter where
  toJSON DescribeActivationsFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FilterKey" Core..=) Core.<$> filterKey,
            ("FilterValues" Core..=) Core.<$> filterValues
          ]
      )
