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
-- Module      : Network.AWS.ElasticSearch.Types.DescribePackagesFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DescribePackagesFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
import qualified Network.AWS.Lens as Lens

-- | Filter to apply in @DescribePackage@ response.
--
-- /See:/ 'newDescribePackagesFilter' smart constructor.
data DescribePackagesFilter = DescribePackagesFilter'
  { -- | Any field from @PackageDetails@.
    name :: Core.Maybe DescribePackagesFilterName,
    -- | A list of values for the specified field.
    value :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePackagesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describePackagesFilter_name' - Any field from @PackageDetails@.
--
-- 'value', 'describePackagesFilter_value' - A list of values for the specified field.
newDescribePackagesFilter ::
  DescribePackagesFilter
newDescribePackagesFilter =
  DescribePackagesFilter'
    { name = Core.Nothing,
      value = Core.Nothing
    }

-- | Any field from @PackageDetails@.
describePackagesFilter_name :: Lens.Lens' DescribePackagesFilter (Core.Maybe DescribePackagesFilterName)
describePackagesFilter_name = Lens.lens (\DescribePackagesFilter' {name} -> name) (\s@DescribePackagesFilter' {} a -> s {name = a} :: DescribePackagesFilter)

-- | A list of values for the specified field.
describePackagesFilter_value :: Lens.Lens' DescribePackagesFilter (Core.Maybe [Core.Text])
describePackagesFilter_value = Lens.lens (\DescribePackagesFilter' {value} -> value) (\s@DescribePackagesFilter' {} a -> s {value = a} :: DescribePackagesFilter) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable DescribePackagesFilter

instance Core.NFData DescribePackagesFilter

instance Core.ToJSON DescribePackagesFilter where
  toJSON DescribePackagesFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Value" Core..=) Core.<$> value
          ]
      )
