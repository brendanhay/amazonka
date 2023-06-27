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
-- Module      : Amazonka.IotTwinMaker.Types.TabularConditions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.TabularConditions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.OrderBy
import Amazonka.IotTwinMaker.Types.PropertyFilter
import qualified Amazonka.Prelude as Prelude

-- | The tabular conditions.
--
-- /See:/ 'newTabularConditions' smart constructor.
data TabularConditions = TabularConditions'
  { -- | Filter criteria that orders the output. It can be sorted in ascending or
    -- descending order.
    orderBy :: Prelude.Maybe (Prelude.NonEmpty OrderBy),
    -- | You can filter the request using various logical operators and a
    -- key-value format. For example:
    --
    -- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
    propertyFilters :: Prelude.Maybe (Prelude.NonEmpty PropertyFilter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TabularConditions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderBy', 'tabularConditions_orderBy' - Filter criteria that orders the output. It can be sorted in ascending or
-- descending order.
--
-- 'propertyFilters', 'tabularConditions_propertyFilters' - You can filter the request using various logical operators and a
-- key-value format. For example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
newTabularConditions ::
  TabularConditions
newTabularConditions =
  TabularConditions'
    { orderBy = Prelude.Nothing,
      propertyFilters = Prelude.Nothing
    }

-- | Filter criteria that orders the output. It can be sorted in ascending or
-- descending order.
tabularConditions_orderBy :: Lens.Lens' TabularConditions (Prelude.Maybe (Prelude.NonEmpty OrderBy))
tabularConditions_orderBy = Lens.lens (\TabularConditions' {orderBy} -> orderBy) (\s@TabularConditions' {} a -> s {orderBy = a} :: TabularConditions) Prelude.. Lens.mapping Lens.coerced

-- | You can filter the request using various logical operators and a
-- key-value format. For example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
tabularConditions_propertyFilters :: Lens.Lens' TabularConditions (Prelude.Maybe (Prelude.NonEmpty PropertyFilter))
tabularConditions_propertyFilters = Lens.lens (\TabularConditions' {propertyFilters} -> propertyFilters) (\s@TabularConditions' {} a -> s {propertyFilters = a} :: TabularConditions) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable TabularConditions where
  hashWithSalt _salt TabularConditions' {..} =
    _salt
      `Prelude.hashWithSalt` orderBy
      `Prelude.hashWithSalt` propertyFilters

instance Prelude.NFData TabularConditions where
  rnf TabularConditions' {..} =
    Prelude.rnf orderBy
      `Prelude.seq` Prelude.rnf propertyFilters

instance Data.ToJSON TabularConditions where
  toJSON TabularConditions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("orderBy" Data..=) Prelude.<$> orderBy,
            ("propertyFilters" Data..=)
              Prelude.<$> propertyFilters
          ]
      )
