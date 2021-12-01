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
-- Module      : Amazonka.ElasticSearch.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A filter used to limit results when describing inbound or outbound
-- cross-cluster search connections. Multiple values can be specified per
-- filter. A cross-cluster search connection must match at least one of the
-- specified values for it to be returned from an operation.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | Contains one or more values for the filter.
    values :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies the name of the filter.
    name :: Prelude.Maybe Prelude.Text
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
-- 'values', 'filter_values' - Contains one or more values for the filter.
--
-- 'name', 'filter_name' - Specifies the name of the filter.
newFilter ::
  Filter
newFilter =
  Filter'
    { values = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Contains one or more values for the filter.
filter_values :: Lens.Lens' Filter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the filter.
filter_name :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

instance Prelude.Hashable Filter where
  hashWithSalt salt' Filter' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf name

instance Core.ToJSON Filter where
  toJSON Filter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("Name" Core..=) Prelude.<$> name
          ]
      )
