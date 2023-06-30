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
-- Module      : Amazonka.DataExchange.Types.LakeFormationDataPermissionAsset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.LakeFormationDataPermissionAsset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.LFPermission
import Amazonka.DataExchange.Types.LakeFormationDataPermissionDetails
import Amazonka.DataExchange.Types.LakeFormationDataPermissionType
import qualified Amazonka.Prelude as Prelude

-- | The AWS Lake Formation data permission asset.
--
-- /See:/ 'newLakeFormationDataPermissionAsset' smart constructor.
data LakeFormationDataPermissionAsset = LakeFormationDataPermissionAsset'
  { -- | The IAM role\'s ARN that allows AWS Data Exchange to assume the role and
    -- grant and revoke permissions to AWS Lake Formation data permissions.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Details about the AWS Lake Formation data permission.
    lakeFormationDataPermissionDetails :: LakeFormationDataPermissionDetails,
    -- | The data permission type.
    lakeFormationDataPermissionType :: LakeFormationDataPermissionType,
    -- | The permissions granted to the subscribers on the resource.
    permissions :: [LFPermission]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LakeFormationDataPermissionAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'lakeFormationDataPermissionAsset_roleArn' - The IAM role\'s ARN that allows AWS Data Exchange to assume the role and
-- grant and revoke permissions to AWS Lake Formation data permissions.
--
-- 'lakeFormationDataPermissionDetails', 'lakeFormationDataPermissionAsset_lakeFormationDataPermissionDetails' - Details about the AWS Lake Formation data permission.
--
-- 'lakeFormationDataPermissionType', 'lakeFormationDataPermissionAsset_lakeFormationDataPermissionType' - The data permission type.
--
-- 'permissions', 'lakeFormationDataPermissionAsset_permissions' - The permissions granted to the subscribers on the resource.
newLakeFormationDataPermissionAsset ::
  -- | 'lakeFormationDataPermissionDetails'
  LakeFormationDataPermissionDetails ->
  -- | 'lakeFormationDataPermissionType'
  LakeFormationDataPermissionType ->
  LakeFormationDataPermissionAsset
newLakeFormationDataPermissionAsset
  pLakeFormationDataPermissionDetails_
  pLakeFormationDataPermissionType_ =
    LakeFormationDataPermissionAsset'
      { roleArn =
          Prelude.Nothing,
        lakeFormationDataPermissionDetails =
          pLakeFormationDataPermissionDetails_,
        lakeFormationDataPermissionType =
          pLakeFormationDataPermissionType_,
        permissions = Prelude.mempty
      }

-- | The IAM role\'s ARN that allows AWS Data Exchange to assume the role and
-- grant and revoke permissions to AWS Lake Formation data permissions.
lakeFormationDataPermissionAsset_roleArn :: Lens.Lens' LakeFormationDataPermissionAsset (Prelude.Maybe Prelude.Text)
lakeFormationDataPermissionAsset_roleArn = Lens.lens (\LakeFormationDataPermissionAsset' {roleArn} -> roleArn) (\s@LakeFormationDataPermissionAsset' {} a -> s {roleArn = a} :: LakeFormationDataPermissionAsset)

-- | Details about the AWS Lake Formation data permission.
lakeFormationDataPermissionAsset_lakeFormationDataPermissionDetails :: Lens.Lens' LakeFormationDataPermissionAsset LakeFormationDataPermissionDetails
lakeFormationDataPermissionAsset_lakeFormationDataPermissionDetails = Lens.lens (\LakeFormationDataPermissionAsset' {lakeFormationDataPermissionDetails} -> lakeFormationDataPermissionDetails) (\s@LakeFormationDataPermissionAsset' {} a -> s {lakeFormationDataPermissionDetails = a} :: LakeFormationDataPermissionAsset)

-- | The data permission type.
lakeFormationDataPermissionAsset_lakeFormationDataPermissionType :: Lens.Lens' LakeFormationDataPermissionAsset LakeFormationDataPermissionType
lakeFormationDataPermissionAsset_lakeFormationDataPermissionType = Lens.lens (\LakeFormationDataPermissionAsset' {lakeFormationDataPermissionType} -> lakeFormationDataPermissionType) (\s@LakeFormationDataPermissionAsset' {} a -> s {lakeFormationDataPermissionType = a} :: LakeFormationDataPermissionAsset)

-- | The permissions granted to the subscribers on the resource.
lakeFormationDataPermissionAsset_permissions :: Lens.Lens' LakeFormationDataPermissionAsset [LFPermission]
lakeFormationDataPermissionAsset_permissions = Lens.lens (\LakeFormationDataPermissionAsset' {permissions} -> permissions) (\s@LakeFormationDataPermissionAsset' {} a -> s {permissions = a} :: LakeFormationDataPermissionAsset) Prelude.. Lens.coerced

instance
  Data.FromJSON
    LakeFormationDataPermissionAsset
  where
  parseJSON =
    Data.withObject
      "LakeFormationDataPermissionAsset"
      ( \x ->
          LakeFormationDataPermissionAsset'
            Prelude.<$> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..: "LakeFormationDataPermissionDetails")
            Prelude.<*> (x Data..: "LakeFormationDataPermissionType")
            Prelude.<*> (x Data..:? "Permissions" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    LakeFormationDataPermissionAsset
  where
  hashWithSalt
    _salt
    LakeFormationDataPermissionAsset' {..} =
      _salt
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` lakeFormationDataPermissionDetails
        `Prelude.hashWithSalt` lakeFormationDataPermissionType
        `Prelude.hashWithSalt` permissions

instance
  Prelude.NFData
    LakeFormationDataPermissionAsset
  where
  rnf LakeFormationDataPermissionAsset' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf lakeFormationDataPermissionDetails
      `Prelude.seq` Prelude.rnf lakeFormationDataPermissionType
      `Prelude.seq` Prelude.rnf permissions
