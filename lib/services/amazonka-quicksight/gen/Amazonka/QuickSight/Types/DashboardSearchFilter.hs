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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardSearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardFilterAttribute
import Amazonka.QuickSight.Types.FilterOperator

-- | A filter that you apply when searching for dashboards.
--
-- /See:/ 'newDashboardSearchFilter' smart constructor.
data DashboardSearchFilter = DashboardSearchFilter'
  { -- | The name of the value that you want to use as a filter, for example,
    -- @\"Name\": \"QUICKSIGHT_OWNER\"@.
    --
    -- Valid values are defined as follows:
    --
    -- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
    --     any dashboards with that ARN listed as one of the dashboards\'s
    --     owners or viewers are returned. Implicit permissions from folders or
    --     groups are considered.
    --
    -- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
    --     dashboards with that ARN listed as one of the owners of the
    --     dashboards are returned. Implicit permissions from folders or groups
    --     are considered.
    --
    -- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
    --     and any dashboards with that ARN listed as the only owner of the
    --     dashboard are returned. Implicit permissions from folders or groups
    --     are not considered.
    --
    -- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
    --     any dashboards with that ARN listed as one of the owners of the
    --     dashboards are returned. Implicit permissions from folders or groups
    --     are not considered.
    --
    -- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
    --     group, and any dashboards with that ARN listed as one of the owners
    --     or viewers of the dashboards are returned. Implicit permissions from
    --     folders or groups are not considered.
    --
    -- -   @DASHBOARD_NAME@: Any dashboards whose names have a substring match
    --     to this value will be returned.
    name :: Prelude.Maybe DashboardFilterAttribute,
    -- | The value of the named item, in this case @QUICKSIGHT_USER@, that you
    -- want to use as a filter, for example,
    -- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
    value :: Prelude.Maybe Prelude.Text,
    -- | The comparison operator that you want to use as a filter, for example
    -- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
    -- and @\"StringLike\"@.
    --
    -- If you set the operator value to @\"StringEquals\"@, you need to provide
    -- an ownership related filter in the @\"NAME\"@ field and the arn of the
    -- user or group whose folders you want to search in the @\"Value\"@ field.
    -- For example,
    -- @\"Name\":\"DIRECT_QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
    --
    -- If you set the value to @\"StringLike\"@, you need to provide the name
    -- of the folders you are searching for. For example,
    -- @\"Name\":\"DASHBOARD_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
    -- The @\"StringLike\"@ operator only supports the @NAME@ value
    -- @DASHBOARD_NAME@.
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
-- 'name', 'dashboardSearchFilter_name' - The name of the value that you want to use as a filter, for example,
-- @\"Name\": \"QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
--     any dashboards with that ARN listed as one of the dashboards\'s
--     owners or viewers are returned. Implicit permissions from folders or
--     groups are considered.
--
-- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
--     dashboards with that ARN listed as one of the owners of the
--     dashboards are returned. Implicit permissions from folders or groups
--     are considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any dashboards with that ARN listed as the only owner of the
--     dashboard are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any dashboards with that ARN listed as one of the owners of the
--     dashboards are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any dashboards with that ARN listed as one of the owners
--     or viewers of the dashboards are returned. Implicit permissions from
--     folders or groups are not considered.
--
-- -   @DASHBOARD_NAME@: Any dashboards whose names have a substring match
--     to this value will be returned.
--
-- 'value', 'dashboardSearchFilter_value' - The value of the named item, in this case @QUICKSIGHT_USER@, that you
-- want to use as a filter, for example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
--
-- 'operator', 'dashboardSearchFilter_operator' - The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
-- and @\"StringLike\"@.
--
-- If you set the operator value to @\"StringEquals\"@, you need to provide
-- an ownership related filter in the @\"NAME\"@ field and the arn of the
-- user or group whose folders you want to search in the @\"Value\"@ field.
-- For example,
-- @\"Name\":\"DIRECT_QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
--
-- If you set the value to @\"StringLike\"@, you need to provide the name
-- of the folders you are searching for. For example,
-- @\"Name\":\"DASHBOARD_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @DASHBOARD_NAME@.
newDashboardSearchFilter ::
  -- | 'operator'
  FilterOperator ->
  DashboardSearchFilter
newDashboardSearchFilter pOperator_ =
  DashboardSearchFilter'
    { name = Prelude.Nothing,
      value = Prelude.Nothing,
      operator = pOperator_
    }

-- | The name of the value that you want to use as a filter, for example,
-- @\"Name\": \"QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
--     any dashboards with that ARN listed as one of the dashboards\'s
--     owners or viewers are returned. Implicit permissions from folders or
--     groups are considered.
--
-- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
--     dashboards with that ARN listed as one of the owners of the
--     dashboards are returned. Implicit permissions from folders or groups
--     are considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any dashboards with that ARN listed as the only owner of the
--     dashboard are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any dashboards with that ARN listed as one of the owners of the
--     dashboards are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any dashboards with that ARN listed as one of the owners
--     or viewers of the dashboards are returned. Implicit permissions from
--     folders or groups are not considered.
--
-- -   @DASHBOARD_NAME@: Any dashboards whose names have a substring match
--     to this value will be returned.
dashboardSearchFilter_name :: Lens.Lens' DashboardSearchFilter (Prelude.Maybe DashboardFilterAttribute)
dashboardSearchFilter_name = Lens.lens (\DashboardSearchFilter' {name} -> name) (\s@DashboardSearchFilter' {} a -> s {name = a} :: DashboardSearchFilter)

-- | The value of the named item, in this case @QUICKSIGHT_USER@, that you
-- want to use as a filter, for example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
dashboardSearchFilter_value :: Lens.Lens' DashboardSearchFilter (Prelude.Maybe Prelude.Text)
dashboardSearchFilter_value = Lens.lens (\DashboardSearchFilter' {value} -> value) (\s@DashboardSearchFilter' {} a -> s {value = a} :: DashboardSearchFilter)

-- | The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
-- and @\"StringLike\"@.
--
-- If you set the operator value to @\"StringEquals\"@, you need to provide
-- an ownership related filter in the @\"NAME\"@ field and the arn of the
-- user or group whose folders you want to search in the @\"Value\"@ field.
-- For example,
-- @\"Name\":\"DIRECT_QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
--
-- If you set the value to @\"StringLike\"@, you need to provide the name
-- of the folders you are searching for. For example,
-- @\"Name\":\"DASHBOARD_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @DASHBOARD_NAME@.
dashboardSearchFilter_operator :: Lens.Lens' DashboardSearchFilter FilterOperator
dashboardSearchFilter_operator = Lens.lens (\DashboardSearchFilter' {operator} -> operator) (\s@DashboardSearchFilter' {} a -> s {operator = a} :: DashboardSearchFilter)

instance Prelude.Hashable DashboardSearchFilter where
  hashWithSalt _salt DashboardSearchFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` operator

instance Prelude.NFData DashboardSearchFilter where
  rnf DashboardSearchFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON DashboardSearchFilter where
  toJSON DashboardSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("Operator" Data..= operator)
          ]
      )
