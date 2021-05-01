{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Filter to apply in @DescribePackage@ response.
--
-- /See:/ 'newDescribePackagesFilter' smart constructor.
data DescribePackagesFilter = DescribePackagesFilter'
  { -- | Any field from @PackageDetails@.
    name :: Prelude.Maybe DescribePackagesFilterName,
    -- | A list of values for the specified field.
    value :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Any field from @PackageDetails@.
describePackagesFilter_name :: Lens.Lens' DescribePackagesFilter (Prelude.Maybe DescribePackagesFilterName)
describePackagesFilter_name = Lens.lens (\DescribePackagesFilter' {name} -> name) (\s@DescribePackagesFilter' {} a -> s {name = a} :: DescribePackagesFilter)

-- | A list of values for the specified field.
describePackagesFilter_value :: Lens.Lens' DescribePackagesFilter (Prelude.Maybe [Prelude.Text])
describePackagesFilter_value = Lens.lens (\DescribePackagesFilter' {value} -> value) (\s@DescribePackagesFilter' {} a -> s {value = a} :: DescribePackagesFilter) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable DescribePackagesFilter

instance Prelude.NFData DescribePackagesFilter

instance Prelude.ToJSON DescribePackagesFilter where
  toJSON DescribePackagesFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
