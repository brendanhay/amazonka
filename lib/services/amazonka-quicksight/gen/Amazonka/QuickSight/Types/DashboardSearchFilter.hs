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
-- Module      : Amazonka.QuickSight.Types.DashboardSearchFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardSearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardFilterAttribute
import Amazonka.QuickSight.Types.FilterOperator

-- | A filter that you apply when searching for dashboards.
--
-- /See:/ 'newDashboardSearchFilter' smart constructor.
data DashboardSearchFilter = DashboardSearchFilter'
  { -- | The value of the named item, in this case @QUICKSIGHT_USER@, that you
    -- want to use as a filter, for example,
    -- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of the value that you want to use as a filter, for example,
    -- @\"Name\": \"QUICKSIGHT_USER\"@.
    name :: Prelude.Maybe DashboardFilterAttribute,
    -- | The comparison operator that you want to use as a filter, for example,
    -- @\"Operator\": \"StringEquals\"@.
    operator :: FilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'dashboardSearchFilter_value' - The value of the named item, in this case @QUICKSIGHT_USER@, that you
-- want to use as a filter, for example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
--
-- 'name', 'dashboardSearchFilter_name' - The name of the value that you want to use as a filter, for example,
-- @\"Name\": \"QUICKSIGHT_USER\"@.
--
-- 'operator', 'dashboardSearchFilter_operator' - The comparison operator that you want to use as a filter, for example,
-- @\"Operator\": \"StringEquals\"@.
newDashboardSearchFilter ::
  -- | 'operator'
  FilterOperator ->
  DashboardSearchFilter
newDashboardSearchFilter pOperator_ =
  DashboardSearchFilter'
    { value = Prelude.Nothing,
      name = Prelude.Nothing,
      operator = pOperator_
    }

-- | The value of the named item, in this case @QUICKSIGHT_USER@, that you
-- want to use as a filter, for example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
dashboardSearchFilter_value :: Lens.Lens' DashboardSearchFilter (Prelude.Maybe Prelude.Text)
dashboardSearchFilter_value = Lens.lens (\DashboardSearchFilter' {value} -> value) (\s@DashboardSearchFilter' {} a -> s {value = a} :: DashboardSearchFilter)

-- | The name of the value that you want to use as a filter, for example,
-- @\"Name\": \"QUICKSIGHT_USER\"@.
dashboardSearchFilter_name :: Lens.Lens' DashboardSearchFilter (Prelude.Maybe DashboardFilterAttribute)
dashboardSearchFilter_name = Lens.lens (\DashboardSearchFilter' {name} -> name) (\s@DashboardSearchFilter' {} a -> s {name = a} :: DashboardSearchFilter)

-- | The comparison operator that you want to use as a filter, for example,
-- @\"Operator\": \"StringEquals\"@.
dashboardSearchFilter_operator :: Lens.Lens' DashboardSearchFilter FilterOperator
dashboardSearchFilter_operator = Lens.lens (\DashboardSearchFilter' {operator} -> operator) (\s@DashboardSearchFilter' {} a -> s {operator = a} :: DashboardSearchFilter)

instance Prelude.Hashable DashboardSearchFilter where
  hashWithSalt salt' DashboardSearchFilter' {..} =
    salt' `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData DashboardSearchFilter where
  rnf DashboardSearchFilter' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf name

instance Core.ToJSON DashboardSearchFilter where
  toJSON DashboardSearchFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Name" Core..=) Prelude.<$> name,
            Prelude.Just ("Operator" Core..= operator)
          ]
      )
