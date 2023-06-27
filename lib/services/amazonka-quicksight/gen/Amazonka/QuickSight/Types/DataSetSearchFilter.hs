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
-- Module      : Amazonka.QuickSight.Types.DataSetSearchFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSetSearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSetFilterAttribute
import Amazonka.QuickSight.Types.FilterOperator

-- | A filter that you apply when searching for datasets.
--
-- /See:/ 'newDataSetSearchFilter' smart constructor.
data DataSetSearchFilter = DataSetSearchFilter'
  { -- | The comparison operator that you want to use as a filter, for example
    -- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
    -- and @\"StringLike\"@.
    --
    -- If you set the operator value to @\"StringEquals\"@, you need to provide
    -- an ownership related filter in the @\"NAME\"@ field and the arn of the
    -- user or group whose datasets you want to search in the @\"Value\"@
    -- field. For example,
    -- @\"Name\":\"QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east- 1:1:user\/default\/UserName1\"@.
    --
    -- If you set the value to @\"StringLike\"@, you need to provide the name
    -- of the datasets you are searching for. For example,
    -- @\"Name\":\"DATASET_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
    -- The @\"StringLike\"@ operator only supports the @NAME@ value
    -- @DATASET_NAME@.
    operator :: FilterOperator,
    -- | The name of the value that you want to use as a filter, for example,
    -- @\"Name\": \"QUICKSIGHT_OWNER\"@.
    --
    -- Valid values are defined as follows:
    --
    -- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
    --     any datasets with that ARN listed as one of the dataset owners or
    --     viewers are returned. Implicit permissions from folders or groups
    --     are considered.
    --
    -- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
    --     datasets with that ARN listed as one of the owners of the dataset
    --     are returned. Implicit permissions from folders or groups are
    --     considered.
    --
    -- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
    --     and any datasets with that ARN listed as the only owner of the
    --     dataset are returned. Implicit permissions from folders or groups
    --     are not considered.
    --
    -- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
    --     any datasets with that ARN listed as one of the owners if the
    --     dataset are returned. Implicit permissions from folders or groups
    --     are not considered.
    --
    -- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
    --     group, and any datasets with that ARN listed as one of the owners or
    --     viewers of the dataset are returned. Implicit permissions from
    --     folders or groups are not considered.
    --
    -- -   @DATASET_NAME@: Any datasets whose names have a substring match to
    --     this value will be returned.
    name :: DataSetFilterAttribute,
    -- | The value of the named item, in this case @QUICKSIGHT_OWNER@, that you
    -- want to use as a filter, for example,
    -- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operator', 'dataSetSearchFilter_operator' - The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
-- and @\"StringLike\"@.
--
-- If you set the operator value to @\"StringEquals\"@, you need to provide
-- an ownership related filter in the @\"NAME\"@ field and the arn of the
-- user or group whose datasets you want to search in the @\"Value\"@
-- field. For example,
-- @\"Name\":\"QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east- 1:1:user\/default\/UserName1\"@.
--
-- If you set the value to @\"StringLike\"@, you need to provide the name
-- of the datasets you are searching for. For example,
-- @\"Name\":\"DATASET_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @DATASET_NAME@.
--
-- 'name', 'dataSetSearchFilter_name' - The name of the value that you want to use as a filter, for example,
-- @\"Name\": \"QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
--     any datasets with that ARN listed as one of the dataset owners or
--     viewers are returned. Implicit permissions from folders or groups
--     are considered.
--
-- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
--     datasets with that ARN listed as one of the owners of the dataset
--     are returned. Implicit permissions from folders or groups are
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any datasets with that ARN listed as the only owner of the
--     dataset are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any datasets with that ARN listed as one of the owners if the
--     dataset are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any datasets with that ARN listed as one of the owners or
--     viewers of the dataset are returned. Implicit permissions from
--     folders or groups are not considered.
--
-- -   @DATASET_NAME@: Any datasets whose names have a substring match to
--     this value will be returned.
--
-- 'value', 'dataSetSearchFilter_value' - The value of the named item, in this case @QUICKSIGHT_OWNER@, that you
-- want to use as a filter, for example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
newDataSetSearchFilter ::
  -- | 'operator'
  FilterOperator ->
  -- | 'name'
  DataSetFilterAttribute ->
  -- | 'value'
  Prelude.Text ->
  DataSetSearchFilter
newDataSetSearchFilter pOperator_ pName_ pValue_ =
  DataSetSearchFilter'
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
-- user or group whose datasets you want to search in the @\"Value\"@
-- field. For example,
-- @\"Name\":\"QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east- 1:1:user\/default\/UserName1\"@.
--
-- If you set the value to @\"StringLike\"@, you need to provide the name
-- of the datasets you are searching for. For example,
-- @\"Name\":\"DATASET_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @DATASET_NAME@.
dataSetSearchFilter_operator :: Lens.Lens' DataSetSearchFilter FilterOperator
dataSetSearchFilter_operator = Lens.lens (\DataSetSearchFilter' {operator} -> operator) (\s@DataSetSearchFilter' {} a -> s {operator = a} :: DataSetSearchFilter)

-- | The name of the value that you want to use as a filter, for example,
-- @\"Name\": \"QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
--     any datasets with that ARN listed as one of the dataset owners or
--     viewers are returned. Implicit permissions from folders or groups
--     are considered.
--
-- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
--     datasets with that ARN listed as one of the owners of the dataset
--     are returned. Implicit permissions from folders or groups are
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any datasets with that ARN listed as the only owner of the
--     dataset are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any datasets with that ARN listed as one of the owners if the
--     dataset are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any datasets with that ARN listed as one of the owners or
--     viewers of the dataset are returned. Implicit permissions from
--     folders or groups are not considered.
--
-- -   @DATASET_NAME@: Any datasets whose names have a substring match to
--     this value will be returned.
dataSetSearchFilter_name :: Lens.Lens' DataSetSearchFilter DataSetFilterAttribute
dataSetSearchFilter_name = Lens.lens (\DataSetSearchFilter' {name} -> name) (\s@DataSetSearchFilter' {} a -> s {name = a} :: DataSetSearchFilter)

-- | The value of the named item, in this case @QUICKSIGHT_OWNER@, that you
-- want to use as a filter, for example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
dataSetSearchFilter_value :: Lens.Lens' DataSetSearchFilter Prelude.Text
dataSetSearchFilter_value = Lens.lens (\DataSetSearchFilter' {value} -> value) (\s@DataSetSearchFilter' {} a -> s {value = a} :: DataSetSearchFilter)

instance Prelude.Hashable DataSetSearchFilter where
  hashWithSalt _salt DataSetSearchFilter' {..} =
    _salt
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData DataSetSearchFilter where
  rnf DataSetSearchFilter' {..} =
    Prelude.rnf operator
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON DataSetSearchFilter where
  toJSON DataSetSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Operator" Data..= operator),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
