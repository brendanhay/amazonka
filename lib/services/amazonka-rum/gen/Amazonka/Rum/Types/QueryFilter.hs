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
-- Module      : Amazonka.Rum.Types.QueryFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.QueryFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines a key and values that you can use to filter the
-- results. The only performance events that are returned are those that
-- have values matching the ones that you specify in one of your
-- @QueryFilter@ structures.
--
-- For example, you could specify @Browser@ as the @Name@ and specify
-- @Chrome,Firefox@ as the @Values@ to return events generated only from
-- those browsers.
--
-- Specifying @Invert@ as the @Name@ works as a \"not equal to\" filter.
-- For example, specify @Invert@ as the @Name@ and specify @Chrome@ as the
-- value to return all events except events from user sessions with the
-- Chrome browser.
--
-- /See:/ 'newQueryFilter' smart constructor.
data QueryFilter = QueryFilter'
  { -- | The name of a key to search for. The filter returns only the events that
    -- match the @Name@ and @Values@ that you specify.
    --
    -- Valid values for @Name@ are @Browser@ | @Device@ | @Country@ | @Page@ |
    -- @OS@ | @EventType@ | @Invert@
    name :: Prelude.Maybe Prelude.Text,
    -- | The values of the @Name@ that are to be be included in the returned
    -- results.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'queryFilter_name' - The name of a key to search for. The filter returns only the events that
-- match the @Name@ and @Values@ that you specify.
--
-- Valid values for @Name@ are @Browser@ | @Device@ | @Country@ | @Page@ |
-- @OS@ | @EventType@ | @Invert@
--
-- 'values', 'queryFilter_values' - The values of the @Name@ that are to be be included in the returned
-- results.
newQueryFilter ::
  QueryFilter
newQueryFilter =
  QueryFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of a key to search for. The filter returns only the events that
-- match the @Name@ and @Values@ that you specify.
--
-- Valid values for @Name@ are @Browser@ | @Device@ | @Country@ | @Page@ |
-- @OS@ | @EventType@ | @Invert@
queryFilter_name :: Lens.Lens' QueryFilter (Prelude.Maybe Prelude.Text)
queryFilter_name = Lens.lens (\QueryFilter' {name} -> name) (\s@QueryFilter' {} a -> s {name = a} :: QueryFilter)

-- | The values of the @Name@ that are to be be included in the returned
-- results.
queryFilter_values :: Lens.Lens' QueryFilter (Prelude.Maybe [Prelude.Text])
queryFilter_values = Lens.lens (\QueryFilter' {values} -> values) (\s@QueryFilter' {} a -> s {values = a} :: QueryFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable QueryFilter where
  hashWithSalt _salt QueryFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData QueryFilter where
  rnf QueryFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON QueryFilter where
  toJSON QueryFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
