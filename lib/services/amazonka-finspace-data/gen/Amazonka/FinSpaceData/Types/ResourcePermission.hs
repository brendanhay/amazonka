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
-- Module      : Amazonka.FinSpaceData.Types.ResourcePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ResourcePermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Resource permission for a dataset. When you create a dataset, all the
-- other members of the same user group inherit access to the dataset. You
-- can only create a dataset if your user group has application permission
-- for Create Datasets.
--
-- The following is a list of valid dataset permissions that you can apply:
--
-- -   @ViewDatasetDetails@
--
-- -   @ReadDatasetDetails@
--
-- -   @AddDatasetData@
--
-- -   @CreateDataView@
--
-- -   @EditDatasetMetadata@
--
-- -   @DeleteDataset@
--
-- For more information on the dataset permissions, see
-- <https://docs.aws.amazon.com/finspace/latest/userguide/managing-user-permissions.html#supported-dataset-permissions Supported Dataset Permissions>
-- in the FinSpace User Guide.
--
-- /See:/ 'newResourcePermission' smart constructor.
data ResourcePermission = ResourcePermission'
  { -- | Permission for a resource.
    permission :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourcePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permission', 'resourcePermission_permission' - Permission for a resource.
newResourcePermission ::
  ResourcePermission
newResourcePermission =
  ResourcePermission' {permission = Prelude.Nothing}

-- | Permission for a resource.
resourcePermission_permission :: Lens.Lens' ResourcePermission (Prelude.Maybe Prelude.Text)
resourcePermission_permission = Lens.lens (\ResourcePermission' {permission} -> permission) (\s@ResourcePermission' {} a -> s {permission = a} :: ResourcePermission)

instance Prelude.Hashable ResourcePermission where
  hashWithSalt _salt ResourcePermission' {..} =
    _salt `Prelude.hashWithSalt` permission

instance Prelude.NFData ResourcePermission where
  rnf ResourcePermission' {..} = Prelude.rnf permission

instance Data.ToJSON ResourcePermission where
  toJSON ResourcePermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [("permission" Data..=) Prelude.<$> permission]
      )
