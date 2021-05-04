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
-- Module      : Network.AWS.SSM.Types.DescribeActivationsFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DescribeActivationsFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.DescribeActivationsFilterKeys

-- | Filter for the DescribeActivation API.
--
-- /See:/ 'newDescribeActivationsFilter' smart constructor.
data DescribeActivationsFilter = DescribeActivationsFilter'
  { -- | The name of the filter.
    filterKey :: Prelude.Maybe DescribeActivationsFilterKeys,
    -- | The filter values.
    filterValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeActivationsFilter_filterValues = Lens.lens (\DescribeActivationsFilter' {filterValues} -> filterValues) (\s@DescribeActivationsFilter' {} a -> s {filterValues = a} :: DescribeActivationsFilter) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable DescribeActivationsFilter

instance Prelude.NFData DescribeActivationsFilter

instance Prelude.ToJSON DescribeActivationsFilter where
  toJSON DescribeActivationsFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FilterKey" Prelude..=) Prelude.<$> filterKey,
            ("FilterValues" Prelude..=)
              Prelude.<$> filterValues
          ]
      )
