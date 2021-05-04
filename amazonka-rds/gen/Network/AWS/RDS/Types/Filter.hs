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
-- Module      : Network.AWS.RDS.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Filter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A filter name and value pair that is used to return a more specific list
-- of results from a describe operation. Filters can be used to match a set
-- of resources by specific criteria, such as IDs. The filters supported by
-- a describe operation are documented with the describe operation.
--
-- Currently, wildcards are not supported in filters.
--
-- The following actions can be filtered:
--
-- -   @DescribeDBClusterBacktracks@
--
-- -   @DescribeDBClusterEndpoints@
--
-- -   @DescribeDBClusters@
--
-- -   @DescribeDBInstances@
--
-- -   @DescribePendingMaintenanceActions@
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The name of the filter. Filter names are case-sensitive.
    name :: Prelude.Text,
    -- | One or more filter values. Filter values are case-sensitive.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filter_name' - The name of the filter. Filter names are case-sensitive.
--
-- 'values', 'filter_values' - One or more filter values. Filter values are case-sensitive.
newFilter ::
  -- | 'name'
  Prelude.Text ->
  Filter
newFilter pName_ =
  Filter' {name = pName_, values = Prelude.mempty}

-- | The name of the filter. Filter names are case-sensitive.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | One or more filter values. Filter values are case-sensitive.
filter_values :: Lens.Lens' Filter [Prelude.Text]
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Prelude._Coerce

instance Prelude.Hashable Filter

instance Prelude.NFData Filter

instance Prelude.ToQuery Filter where
  toQuery Filter' {..} =
    Prelude.mconcat
      [ "Name" Prelude.=: name,
        "Values"
          Prelude.=: Prelude.toQueryList "Value" values
      ]
