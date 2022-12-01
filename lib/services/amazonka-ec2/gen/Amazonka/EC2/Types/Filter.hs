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
-- Module      : Amazonka.EC2.Types.Filter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | A filter name and value pair that is used to return a more specific list
-- of results from a describe operation. Filters can be used to match a set
-- of resources by specific criteria, such as tags, attributes, or IDs.
--
-- If you specify multiple filters, the filters are joined with an @AND@,
-- and the request returns only results that match all of the specified
-- filters.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The filter values. Filter values are case-sensitive. If you specify
    -- multiple values for a filter, the values are joined with an @OR@, and
    -- the request returns all results that match any of the specified values.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the filter. Filter names are case-sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'filter_values' - The filter values. Filter values are case-sensitive. If you specify
-- multiple values for a filter, the values are joined with an @OR@, and
-- the request returns all results that match any of the specified values.
--
-- 'name', 'filter_name' - The name of the filter. Filter names are case-sensitive.
newFilter ::
  -- | 'name'
  Prelude.Text ->
  Filter
newFilter pName_ =
  Filter' {values = Prelude.Nothing, name = pName_}

-- | The filter values. Filter values are case-sensitive. If you specify
-- multiple values for a filter, the values are joined with an @OR@, and
-- the request returns all results that match any of the specified values.
filter_values :: Lens.Lens' Filter (Prelude.Maybe [Prelude.Text])
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the filter. Filter names are case-sensitive.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` name

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf name

instance Core.ToQuery Filter where
  toQuery Filter' {..} =
    Prelude.mconcat
      [ Core.toQuery
          (Core.toQueryList "Value" Prelude.<$> values),
        "Name" Core.=: name
      ]
