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
-- Module      : Amazonka.FinSpaceData.Types.PermissionGroupParams
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.PermissionGroupParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ResourcePermission
import qualified Amazonka.Prelude as Prelude

-- | Permission group parameters for Dataset permissions.
--
-- Here is an example of how you could specify the @PermissionGroupParams@:
--
-- @ { \"permissionGroupId\": \"0r6fCRtSTUk4XPfXQe3M0g\", \"datasetPermissions\": [ {\"permission\": \"ViewDatasetDetails\"}, {\"permission\": \"AddDatasetData\"}, {\"permission\": \"EditDatasetMetadata\"}, {\"permission\": \"DeleteDataset\"} ] } @
--
-- /See:/ 'newPermissionGroupParams' smart constructor.
data PermissionGroupParams = PermissionGroupParams'
  { -- | List of resource permissions.
    datasetPermissions :: Prelude.Maybe [ResourcePermission],
    -- | The unique identifier for the @PermissionGroup@.
    permissionGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PermissionGroupParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetPermissions', 'permissionGroupParams_datasetPermissions' - List of resource permissions.
--
-- 'permissionGroupId', 'permissionGroupParams_permissionGroupId' - The unique identifier for the @PermissionGroup@.
newPermissionGroupParams ::
  PermissionGroupParams
newPermissionGroupParams =
  PermissionGroupParams'
    { datasetPermissions =
        Prelude.Nothing,
      permissionGroupId = Prelude.Nothing
    }

-- | List of resource permissions.
permissionGroupParams_datasetPermissions :: Lens.Lens' PermissionGroupParams (Prelude.Maybe [ResourcePermission])
permissionGroupParams_datasetPermissions = Lens.lens (\PermissionGroupParams' {datasetPermissions} -> datasetPermissions) (\s@PermissionGroupParams' {} a -> s {datasetPermissions = a} :: PermissionGroupParams) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the @PermissionGroup@.
permissionGroupParams_permissionGroupId :: Lens.Lens' PermissionGroupParams (Prelude.Maybe Prelude.Text)
permissionGroupParams_permissionGroupId = Lens.lens (\PermissionGroupParams' {permissionGroupId} -> permissionGroupId) (\s@PermissionGroupParams' {} a -> s {permissionGroupId = a} :: PermissionGroupParams)

instance Prelude.Hashable PermissionGroupParams where
  hashWithSalt _salt PermissionGroupParams' {..} =
    _salt `Prelude.hashWithSalt` datasetPermissions
      `Prelude.hashWithSalt` permissionGroupId

instance Prelude.NFData PermissionGroupParams where
  rnf PermissionGroupParams' {..} =
    Prelude.rnf datasetPermissions
      `Prelude.seq` Prelude.rnf permissionGroupId

instance Data.ToJSON PermissionGroupParams where
  toJSON PermissionGroupParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("datasetPermissions" Data..=)
              Prelude.<$> datasetPermissions,
            ("permissionGroupId" Data..=)
              Prelude.<$> permissionGroupId
          ]
      )
