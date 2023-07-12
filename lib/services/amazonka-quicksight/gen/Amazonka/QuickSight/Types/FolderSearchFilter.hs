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
-- Module      : Amazonka.QuickSight.Types.FolderSearchFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FolderSearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilterOperator
import Amazonka.QuickSight.Types.FolderFilterAttribute

-- | A filter to use to search an Amazon QuickSight folder.
--
-- /See:/ 'newFolderSearchFilter' smart constructor.
data FolderSearchFilter = FolderSearchFilter'
  { -- | The name of a value that you want to use in the filter. For example,
    -- @\"Name\": \"QUICKSIGHT_OWNER\"@.
    --
    -- Valid values are defined as follows:
    --
    -- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
    --     any folders with that ARN listed as one of the folder\'s owners or
    --     viewers are returned. Implicit permissions from folders or groups
    --     are considered.
    --
    -- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
    --     folders with that ARN listed as one of the owners of the folders are
    --     returned. Implicit permissions from folders or groups are
    --     considered.
    --
    -- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
    --     and any folders with that ARN listed as the only owner of the folder
    --     are returned. Implicit permissions from folders or groups are not
    --     considered.
    --
    -- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
    --     any folders with that ARN listed as one of the owners of the folders
    --     are returned. Implicit permissions from folders or groups are not
    --     considered.
    --
    -- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
    --     group, and any folders with that ARN listed as one of the owners or
    --     viewers of the folders are returned. Implicit permissions from
    --     folders or groups are not considered.
    --
    -- -   @FOLDER_NAME@: Any folders whose names have a substring match to
    --     this value will be returned.
    --
    -- -   @PARENT_FOLDER_ARN@: Provide an ARN of a folder, and any folders
    --     that are directly under that parent folder are returned. If you
    --     choose to use this option and leave the value blank, all root-level
    --     folders in the account are returned.
    name :: Prelude.Maybe FolderFilterAttribute,
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
    -- @\"Name\":\"FOLDER_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
    -- The @\"StringLike\"@ operator only supports the @NAME@ value
    -- @FOLDER_NAME@.
    operator :: Prelude.Maybe FilterOperator,
    -- | The value of the named item (in this example, @PARENT_FOLDER_ARN@), that
    -- you want to use as a filter. For example,
    -- @\"Value\": \"arn:aws:quicksight:us-east-1:1:folder\/folderId\"@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FolderSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'folderSearchFilter_name' - The name of a value that you want to use in the filter. For example,
-- @\"Name\": \"QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
--     any folders with that ARN listed as one of the folder\'s owners or
--     viewers are returned. Implicit permissions from folders or groups
--     are considered.
--
-- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
--     folders with that ARN listed as one of the owners of the folders are
--     returned. Implicit permissions from folders or groups are
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any folders with that ARN listed as the only owner of the folder
--     are returned. Implicit permissions from folders or groups are not
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any folders with that ARN listed as one of the owners of the folders
--     are returned. Implicit permissions from folders or groups are not
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any folders with that ARN listed as one of the owners or
--     viewers of the folders are returned. Implicit permissions from
--     folders or groups are not considered.
--
-- -   @FOLDER_NAME@: Any folders whose names have a substring match to
--     this value will be returned.
--
-- -   @PARENT_FOLDER_ARN@: Provide an ARN of a folder, and any folders
--     that are directly under that parent folder are returned. If you
--     choose to use this option and leave the value blank, all root-level
--     folders in the account are returned.
--
-- 'operator', 'folderSearchFilter_operator' - The comparison operator that you want to use as a filter, for example
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
-- @\"Name\":\"FOLDER_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @FOLDER_NAME@.
--
-- 'value', 'folderSearchFilter_value' - The value of the named item (in this example, @PARENT_FOLDER_ARN@), that
-- you want to use as a filter. For example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:folder\/folderId\"@.
newFolderSearchFilter ::
  FolderSearchFilter
newFolderSearchFilter =
  FolderSearchFilter'
    { name = Prelude.Nothing,
      operator = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of a value that you want to use in the filter. For example,
-- @\"Name\": \"QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
--     any folders with that ARN listed as one of the folder\'s owners or
--     viewers are returned. Implicit permissions from folders or groups
--     are considered.
--
-- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
--     folders with that ARN listed as one of the owners of the folders are
--     returned. Implicit permissions from folders or groups are
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any folders with that ARN listed as the only owner of the folder
--     are returned. Implicit permissions from folders or groups are not
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any folders with that ARN listed as one of the owners of the folders
--     are returned. Implicit permissions from folders or groups are not
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any folders with that ARN listed as one of the owners or
--     viewers of the folders are returned. Implicit permissions from
--     folders or groups are not considered.
--
-- -   @FOLDER_NAME@: Any folders whose names have a substring match to
--     this value will be returned.
--
-- -   @PARENT_FOLDER_ARN@: Provide an ARN of a folder, and any folders
--     that are directly under that parent folder are returned. If you
--     choose to use this option and leave the value blank, all root-level
--     folders in the account are returned.
folderSearchFilter_name :: Lens.Lens' FolderSearchFilter (Prelude.Maybe FolderFilterAttribute)
folderSearchFilter_name = Lens.lens (\FolderSearchFilter' {name} -> name) (\s@FolderSearchFilter' {} a -> s {name = a} :: FolderSearchFilter)

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
-- @\"Name\":\"FOLDER_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @FOLDER_NAME@.
folderSearchFilter_operator :: Lens.Lens' FolderSearchFilter (Prelude.Maybe FilterOperator)
folderSearchFilter_operator = Lens.lens (\FolderSearchFilter' {operator} -> operator) (\s@FolderSearchFilter' {} a -> s {operator = a} :: FolderSearchFilter)

-- | The value of the named item (in this example, @PARENT_FOLDER_ARN@), that
-- you want to use as a filter. For example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:folder\/folderId\"@.
folderSearchFilter_value :: Lens.Lens' FolderSearchFilter (Prelude.Maybe Prelude.Text)
folderSearchFilter_value = Lens.lens (\FolderSearchFilter' {value} -> value) (\s@FolderSearchFilter' {} a -> s {value = a} :: FolderSearchFilter)

instance Prelude.Hashable FolderSearchFilter where
  hashWithSalt _salt FolderSearchFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` value

instance Prelude.NFData FolderSearchFilter where
  rnf FolderSearchFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON FolderSearchFilter where
  toJSON FolderSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Operator" Data..=) Prelude.<$> operator,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
