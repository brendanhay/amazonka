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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FolderSearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilterOperator
import Amazonka.QuickSight.Types.FolderFilterAttribute

-- | Searches a folder by a filter.
--
-- /See:/ 'newFolderSearchFilter' smart constructor.
data FolderSearchFilter = FolderSearchFilter'
  { -- | The name of the value that you want to use as a filter. For example,
    -- @\"Name\": \"PARENT_FOLDER_ARN\"@.
    name :: Prelude.Maybe FolderFilterAttribute,
    -- | The comparison operator that you want to use as a filter. For example,
    -- @\"Operator\": \"StringEquals\"@.
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
-- 'name', 'folderSearchFilter_name' - The name of the value that you want to use as a filter. For example,
-- @\"Name\": \"PARENT_FOLDER_ARN\"@.
--
-- 'operator', 'folderSearchFilter_operator' - The comparison operator that you want to use as a filter. For example,
-- @\"Operator\": \"StringEquals\"@.
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

-- | The name of the value that you want to use as a filter. For example,
-- @\"Name\": \"PARENT_FOLDER_ARN\"@.
folderSearchFilter_name :: Lens.Lens' FolderSearchFilter (Prelude.Maybe FolderFilterAttribute)
folderSearchFilter_name = Lens.lens (\FolderSearchFilter' {name} -> name) (\s@FolderSearchFilter' {} a -> s {name = a} :: FolderSearchFilter)

-- | The comparison operator that you want to use as a filter. For example,
-- @\"Operator\": \"StringEquals\"@.
folderSearchFilter_operator :: Lens.Lens' FolderSearchFilter (Prelude.Maybe FilterOperator)
folderSearchFilter_operator = Lens.lens (\FolderSearchFilter' {operator} -> operator) (\s@FolderSearchFilter' {} a -> s {operator = a} :: FolderSearchFilter)

-- | The value of the named item (in this example, @PARENT_FOLDER_ARN@), that
-- you want to use as a filter. For example,
-- @\"Value\": \"arn:aws:quicksight:us-east-1:1:folder\/folderId\"@.
folderSearchFilter_value :: Lens.Lens' FolderSearchFilter (Prelude.Maybe Prelude.Text)
folderSearchFilter_value = Lens.lens (\FolderSearchFilter' {value} -> value) (\s@FolderSearchFilter' {} a -> s {value = a} :: FolderSearchFilter)

instance Prelude.Hashable FolderSearchFilter where
  hashWithSalt _salt FolderSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` value

instance Prelude.NFData FolderSearchFilter where
  rnf FolderSearchFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON FolderSearchFilter where
  toJSON FolderSearchFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Operator" Core..=) Prelude.<$> operator,
            ("Value" Core..=) Prelude.<$> value
          ]
      )
