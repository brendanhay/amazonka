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
-- Module      : Amazonka.DataSync.Types.LocationFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.LocationFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.LocationFilterName
import Amazonka.DataSync.Types.Operator
import qualified Amazonka.Prelude as Prelude

-- | Narrow down the list of resources returned by @ListLocations@. For
-- example, to see all your Amazon S3 locations, create a filter using
-- @\"Name\": \"LocationType\"@, @\"Operator\": \"Equals\"@, and
-- @\"Values\": \"S3\"@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/query-resources.html filtering resources>.
--
-- /See:/ 'newLocationFilter' smart constructor.
data LocationFilter = LocationFilter'
  { -- | The name of the filter being used. Each API call supports a list of
    -- filters that are available for it (for example, @LocationType@ for
    -- @ListLocations@).
    name :: LocationFilterName,
    -- | The values that you want to filter for. For example, you might want to
    -- display only Amazon S3 locations.
    values :: [Prelude.Text],
    -- | The operator that is used to compare filter values (for example,
    -- @Equals@ or @Contains@).
    operator :: Operator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'locationFilter_name' - The name of the filter being used. Each API call supports a list of
-- filters that are available for it (for example, @LocationType@ for
-- @ListLocations@).
--
-- 'values', 'locationFilter_values' - The values that you want to filter for. For example, you might want to
-- display only Amazon S3 locations.
--
-- 'operator', 'locationFilter_operator' - The operator that is used to compare filter values (for example,
-- @Equals@ or @Contains@).
newLocationFilter ::
  -- | 'name'
  LocationFilterName ->
  -- | 'operator'
  Operator ->
  LocationFilter
newLocationFilter pName_ pOperator_ =
  LocationFilter'
    { name = pName_,
      values = Prelude.mempty,
      operator = pOperator_
    }

-- | The name of the filter being used. Each API call supports a list of
-- filters that are available for it (for example, @LocationType@ for
-- @ListLocations@).
locationFilter_name :: Lens.Lens' LocationFilter LocationFilterName
locationFilter_name = Lens.lens (\LocationFilter' {name} -> name) (\s@LocationFilter' {} a -> s {name = a} :: LocationFilter)

-- | The values that you want to filter for. For example, you might want to
-- display only Amazon S3 locations.
locationFilter_values :: Lens.Lens' LocationFilter [Prelude.Text]
locationFilter_values = Lens.lens (\LocationFilter' {values} -> values) (\s@LocationFilter' {} a -> s {values = a} :: LocationFilter) Prelude.. Lens.coerced

-- | The operator that is used to compare filter values (for example,
-- @Equals@ or @Contains@).
locationFilter_operator :: Lens.Lens' LocationFilter Operator
locationFilter_operator = Lens.lens (\LocationFilter' {operator} -> operator) (\s@LocationFilter' {} a -> s {operator = a} :: LocationFilter)

instance Prelude.Hashable LocationFilter where
  hashWithSalt _salt LocationFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData LocationFilter where
  rnf LocationFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON LocationFilter where
  toJSON LocationFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values),
            Prelude.Just ("Operator" Data..= operator)
          ]
      )
