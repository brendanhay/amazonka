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
-- Module      : Network.AWS.EC2.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Filter where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | A filter name and value pair that is used to return a more specific list
-- of results from a describe operation. Filters can be used to match a set
-- of resources by specific criteria, such as tags, attributes, or IDs.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The filter values. Filter values are case-sensitive.
    values :: Core.Maybe [Core.Text],
    -- | The name of the filter. Filter names are case-sensitive.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'filter_values' - The filter values. Filter values are case-sensitive.
--
-- 'name', 'filter_name' - The name of the filter. Filter names are case-sensitive.
newFilter ::
  -- | 'name'
  Core.Text ->
  Filter
newFilter pName_ =
  Filter' {values = Core.Nothing, name = pName_}

-- | The filter values. Filter values are case-sensitive.
filter_values :: Lens.Lens' Filter (Core.Maybe [Core.Text])
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Core.. Lens.mapping Lens._Coerce

-- | The name of the filter. Filter names are case-sensitive.
filter_name :: Lens.Lens' Filter Core.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

instance Core.Hashable Filter

instance Core.NFData Filter

instance Core.ToQuery Filter where
  toQuery Filter' {..} =
    Core.mconcat
      [ Core.toQuery
          (Core.toQueryList "Value" Core.<$> values),
        "Name" Core.=: name
      ]
