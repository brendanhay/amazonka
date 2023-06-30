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
-- Module      : Amazonka.QuickSight.Types.DataSourceSearchFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSourceSearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSourceFilterAttribute
import Amazonka.QuickSight.Types.FilterOperator

-- | A filter that you apply when searching for data sources.
--
-- /See:/ 'newDataSourceSearchFilter' smart constructor.
data DataSourceSearchFilter = DataSourceSearchFilter'
  { -- | The comparison operator that you want to use as a filter, for example
    -- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
    -- and @\"StringLike\"@.
    --
    -- If you set the operator value to @\"StringEquals\"@, you need to provide
    -- an ownership related filter in the @\"NAME\"@ field and the arn of the
    -- user or group whose data sources you want to search in the @\"Value\"@
    -- field. For example,
    -- @\"Name\":\"DIRECT_QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
    --
    -- If you set the value to @\"StringLike\"@, you need to provide the name
    -- of the data sources you are searching for. For example,
    -- @\"Name\":\"DATASOURCE_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
    -- The @\"StringLike\"@ operator only supports the @NAME@ value
    -- @DATASOURCE_NAME@.
    operator :: FilterOperator,
    -- | The name of the value that you want to use as a filter, for example,
    -- @\"Name\": \"DIRECT_QUICKSIGHT_OWNER\"@.
    --
    -- Valid values are defined as follows:
    --
    -- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
    --     group, and any data sources with that ARN listed as one of the
    --     owners or viewers of the data sources are returned. Implicit
    --     permissions from folders or groups are not considered.
    --
    -- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
    --     any data sources with that ARN listed as one of the owners if the
    --     data source are returned. Implicit permissions from folders or
    --     groups are not considered.
    --
    -- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
    --     and any data sources with that ARN listed as the only owner of the
    --     data source are returned. Implicit permissions from folders or
    --     groups are not considered.
    --
    -- -   @DATASOURCE_NAME@: Any data sources whose names have a substring
    --     match to the provided value are returned.
    name :: DataSourceFilterAttribute,
    -- | The value of the named item, for example @DIRECT_QUICKSIGHT_OWNER@, that
    -- you want to use as a filter, for example,
    -- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operator', 'dataSourceSearchFilter_operator' - The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
-- and @\"StringLike\"@.
--
-- If you set the operator value to @\"StringEquals\"@, you need to provide
-- an ownership related filter in the @\"NAME\"@ field and the arn of the
-- user or group whose data sources you want to search in the @\"Value\"@
-- field. For example,
-- @\"Name\":\"DIRECT_QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
--
-- If you set the value to @\"StringLike\"@, you need to provide the name
-- of the data sources you are searching for. For example,
-- @\"Name\":\"DATASOURCE_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @DATASOURCE_NAME@.
--
-- 'name', 'dataSourceSearchFilter_name' - The name of the value that you want to use as a filter, for example,
-- @\"Name\": \"DIRECT_QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any data sources with that ARN listed as one of the
--     owners or viewers of the data sources are returned. Implicit
--     permissions from folders or groups are not considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any data sources with that ARN listed as one of the owners if the
--     data source are returned. Implicit permissions from folders or
--     groups are not considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any data sources with that ARN listed as the only owner of the
--     data source are returned. Implicit permissions from folders or
--     groups are not considered.
--
-- -   @DATASOURCE_NAME@: Any data sources whose names have a substring
--     match to the provided value are returned.
--
-- 'value', 'dataSourceSearchFilter_value' - The value of the named item, for example @DIRECT_QUICKSIGHT_OWNER@, that
-- you want to use as a filter, for example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
newDataSourceSearchFilter ::
  -- | 'operator'
  FilterOperator ->
  -- | 'name'
  DataSourceFilterAttribute ->
  -- | 'value'
  Prelude.Text ->
  DataSourceSearchFilter
newDataSourceSearchFilter pOperator_ pName_ pValue_ =
  DataSourceSearchFilter'
    { operator = pOperator_,
      name = pName_,
      value = pValue_
    }

-- | The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
-- and @\"StringLike\"@.
--
-- If you set the operator value to @\"StringEquals\"@, you need to provide
-- an ownership related filter in the @\"NAME\"@ field and the arn of the
-- user or group whose data sources you want to search in the @\"Value\"@
-- field. For example,
-- @\"Name\":\"DIRECT_QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
--
-- If you set the value to @\"StringLike\"@, you need to provide the name
-- of the data sources you are searching for. For example,
-- @\"Name\":\"DATASOURCE_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @DATASOURCE_NAME@.
dataSourceSearchFilter_operator :: Lens.Lens' DataSourceSearchFilter FilterOperator
dataSourceSearchFilter_operator = Lens.lens (\DataSourceSearchFilter' {operator} -> operator) (\s@DataSourceSearchFilter' {} a -> s {operator = a} :: DataSourceSearchFilter)

-- | The name of the value that you want to use as a filter, for example,
-- @\"Name\": \"DIRECT_QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any data sources with that ARN listed as one of the
--     owners or viewers of the data sources are returned. Implicit
--     permissions from folders or groups are not considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any data sources with that ARN listed as one of the owners if the
--     data source are returned. Implicit permissions from folders or
--     groups are not considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any data sources with that ARN listed as the only owner of the
--     data source are returned. Implicit permissions from folders or
--     groups are not considered.
--
-- -   @DATASOURCE_NAME@: Any data sources whose names have a substring
--     match to the provided value are returned.
dataSourceSearchFilter_name :: Lens.Lens' DataSourceSearchFilter DataSourceFilterAttribute
dataSourceSearchFilter_name = Lens.lens (\DataSourceSearchFilter' {name} -> name) (\s@DataSourceSearchFilter' {} a -> s {name = a} :: DataSourceSearchFilter)

-- | The value of the named item, for example @DIRECT_QUICKSIGHT_OWNER@, that
-- you want to use as a filter, for example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
dataSourceSearchFilter_value :: Lens.Lens' DataSourceSearchFilter Prelude.Text
dataSourceSearchFilter_value = Lens.lens (\DataSourceSearchFilter' {value} -> value) (\s@DataSourceSearchFilter' {} a -> s {value = a} :: DataSourceSearchFilter)

instance Prelude.Hashable DataSourceSearchFilter where
  hashWithSalt _salt DataSourceSearchFilter' {..} =
    _salt
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData DataSourceSearchFilter where
  rnf DataSourceSearchFilter' {..} =
    Prelude.rnf operator
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON DataSourceSearchFilter where
  toJSON DataSourceSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Operator" Data..= operator),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
