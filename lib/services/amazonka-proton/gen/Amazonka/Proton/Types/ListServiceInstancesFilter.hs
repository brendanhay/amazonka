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
-- Module      : Amazonka.Proton.Types.ListServiceInstancesFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ListServiceInstancesFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.ListServiceInstancesFilterBy

-- | A filtering criterion to scope down the result list of the
-- ListServiceInstances action.
--
-- /See:/ 'newListServiceInstancesFilter' smart constructor.
data ListServiceInstancesFilter = ListServiceInstancesFilter'
  { -- | The name of a filtering criterion.
    key :: Prelude.Maybe ListServiceInstancesFilterBy,
    -- | A value to filter by.
    --
    -- With the date\/time keys (@*At{Before,After}@), the value is a valid
    -- <https://datatracker.ietf.org/doc/html/rfc3339.html RFC 3339> string
    -- with no UTC offset and with an optional fractional precision (for
    -- example, @1985-04-12T23:20:50.52Z@).
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceInstancesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'listServiceInstancesFilter_key' - The name of a filtering criterion.
--
-- 'value', 'listServiceInstancesFilter_value' - A value to filter by.
--
-- With the date\/time keys (@*At{Before,After}@), the value is a valid
-- <https://datatracker.ietf.org/doc/html/rfc3339.html RFC 3339> string
-- with no UTC offset and with an optional fractional precision (for
-- example, @1985-04-12T23:20:50.52Z@).
newListServiceInstancesFilter ::
  ListServiceInstancesFilter
newListServiceInstancesFilter =
  ListServiceInstancesFilter'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of a filtering criterion.
listServiceInstancesFilter_key :: Lens.Lens' ListServiceInstancesFilter (Prelude.Maybe ListServiceInstancesFilterBy)
listServiceInstancesFilter_key = Lens.lens (\ListServiceInstancesFilter' {key} -> key) (\s@ListServiceInstancesFilter' {} a -> s {key = a} :: ListServiceInstancesFilter)

-- | A value to filter by.
--
-- With the date\/time keys (@*At{Before,After}@), the value is a valid
-- <https://datatracker.ietf.org/doc/html/rfc3339.html RFC 3339> string
-- with no UTC offset and with an optional fractional precision (for
-- example, @1985-04-12T23:20:50.52Z@).
listServiceInstancesFilter_value :: Lens.Lens' ListServiceInstancesFilter (Prelude.Maybe Prelude.Text)
listServiceInstancesFilter_value = Lens.lens (\ListServiceInstancesFilter' {value} -> value) (\s@ListServiceInstancesFilter' {} a -> s {value = a} :: ListServiceInstancesFilter)

instance Prelude.Hashable ListServiceInstancesFilter where
  hashWithSalt _salt ListServiceInstancesFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ListServiceInstancesFilter where
  rnf ListServiceInstancesFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ListServiceInstancesFilter where
  toJSON ListServiceInstancesFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("value" Data..=) Prelude.<$> value
          ]
      )
