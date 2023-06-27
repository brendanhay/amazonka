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
-- Module      : Amazonka.AlexaBusiness.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter name and value pair that is used to return a more specific list
-- of results. Filters can be used to match a set of resources by various
-- criteria.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The key of a filter.
    key :: Prelude.Text,
    -- | The values of a filter.
    values :: [Prelude.Text]
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
-- 'key', 'filter_key' - The key of a filter.
--
-- 'values', 'filter_values' - The values of a filter.
newFilter ::
  -- | 'key'
  Prelude.Text ->
  Filter
newFilter pKey_ =
  Filter' {key = pKey_, values = Prelude.mempty}

-- | The key of a filter.
filter_key :: Lens.Lens' Filter Prelude.Text
filter_key = Lens.lens (\Filter' {key} -> key) (\s@Filter' {} a -> s {key = a} :: Filter)

-- | The values of a filter.
filter_values :: Lens.Lens' Filter [Prelude.Text]
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Lens.coerced

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values)
          ]
      )
