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
-- Module      : Network.AWS.Discovery.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.Filter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A filter that can use conditional operators.
--
-- For more information about filters, see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html Querying Discovered Configuration Items>
-- in the /AWS Application Discovery Service User Guide/.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The name of the filter.
    name :: Core.Text,
    -- | A string value on which to filter. For example, if you choose the
    -- @destinationServer.osVersion@ filter name, you could specify @Ubuntu@
    -- for the value.
    values :: [Core.Text],
    -- | A conditional operator. The following operators are valid: EQUALS,
    -- NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the
    -- system utilizes all filters as though concatenated by /AND/. If you
    -- specify multiple values for a particular filter, the system
    -- differentiates the values using /OR/. Calling either
    -- /DescribeConfigurations/ or /ListConfigurations/ returns attributes of
    -- matching configuration items.
    condition :: Core.Text
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
-- 'name', 'filter_name' - The name of the filter.
--
-- 'values', 'filter_values' - A string value on which to filter. For example, if you choose the
-- @destinationServer.osVersion@ filter name, you could specify @Ubuntu@
-- for the value.
--
-- 'condition', 'filter_condition' - A conditional operator. The following operators are valid: EQUALS,
-- NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the
-- system utilizes all filters as though concatenated by /AND/. If you
-- specify multiple values for a particular filter, the system
-- differentiates the values using /OR/. Calling either
-- /DescribeConfigurations/ or /ListConfigurations/ returns attributes of
-- matching configuration items.
newFilter ::
  -- | 'name'
  Core.Text ->
  -- | 'condition'
  Core.Text ->
  Filter
newFilter pName_ pCondition_ =
  Filter'
    { name = pName_,
      values = Core.mempty,
      condition = pCondition_
    }

-- | The name of the filter.
filter_name :: Lens.Lens' Filter Core.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | A string value on which to filter. For example, if you choose the
-- @destinationServer.osVersion@ filter name, you could specify @Ubuntu@
-- for the value.
filter_values :: Lens.Lens' Filter [Core.Text]
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Core.. Lens._Coerce

-- | A conditional operator. The following operators are valid: EQUALS,
-- NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the
-- system utilizes all filters as though concatenated by /AND/. If you
-- specify multiple values for a particular filter, the system
-- differentiates the values using /OR/. Calling either
-- /DescribeConfigurations/ or /ListConfigurations/ returns attributes of
-- matching configuration items.
filter_condition :: Lens.Lens' Filter Core.Text
filter_condition = Lens.lens (\Filter' {condition} -> condition) (\s@Filter' {} a -> s {condition = a} :: Filter)

instance Core.Hashable Filter

instance Core.NFData Filter

instance Core.ToJSON Filter where
  toJSON Filter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("values" Core..= values),
            Core.Just ("condition" Core..= condition)
          ]
      )
