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
-- Module      : Amazonka.RAM.Types.ResourceSharePermissionDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceSharePermissionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an RAM permission.
--
-- /See:/ 'newResourceSharePermissionDetail' smart constructor.
data ResourceSharePermissionDetail = ResourceSharePermissionDetail'
  { -- | The date and time when the permission was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The resource type to which the permission applies.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the permission was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the permission.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the version of the permission is set to the default
    -- version for this permission.
    defaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The name of the permission.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the version of the permission.
    version :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the version of the permission is set to the default
    -- version for this resource type.
    isResourceTypeDefault :: Prelude.Maybe Prelude.Bool,
    -- | The permission\'s effect and actions in JSON format. The @effect@
    -- indicates whether the actions are allowed or denied. The @actions@ list
    -- the API actions to which the principal is granted or denied access.
    permission :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSharePermissionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'resourceSharePermissionDetail_creationTime' - The date and time when the permission was created.
--
-- 'resourceType', 'resourceSharePermissionDetail_resourceType' - The resource type to which the permission applies.
--
-- 'lastUpdatedTime', 'resourceSharePermissionDetail_lastUpdatedTime' - The date and time when the permission was last updated.
--
-- 'arn', 'resourceSharePermissionDetail_arn' - The Amazon Resource Name (ARN) of the permission.
--
-- 'defaultVersion', 'resourceSharePermissionDetail_defaultVersion' - Specifies whether the version of the permission is set to the default
-- version for this permission.
--
-- 'name', 'resourceSharePermissionDetail_name' - The name of the permission.
--
-- 'version', 'resourceSharePermissionDetail_version' - The identifier for the version of the permission.
--
-- 'isResourceTypeDefault', 'resourceSharePermissionDetail_isResourceTypeDefault' - Specifies whether the version of the permission is set to the default
-- version for this resource type.
--
-- 'permission', 'resourceSharePermissionDetail_permission' - The permission\'s effect and actions in JSON format. The @effect@
-- indicates whether the actions are allowed or denied. The @actions@ list
-- the API actions to which the principal is granted or denied access.
newResourceSharePermissionDetail ::
  ResourceSharePermissionDetail
newResourceSharePermissionDetail =
  ResourceSharePermissionDetail'
    { creationTime =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      isResourceTypeDefault = Prelude.Nothing,
      permission = Prelude.Nothing
    }

-- | The date and time when the permission was created.
resourceSharePermissionDetail_creationTime :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionDetail_creationTime = Lens.lens (\ResourceSharePermissionDetail' {creationTime} -> creationTime) (\s@ResourceSharePermissionDetail' {} a -> s {creationTime = a} :: ResourceSharePermissionDetail) Prelude.. Lens.mapping Core._Time

-- | The resource type to which the permission applies.
resourceSharePermissionDetail_resourceType :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_resourceType = Lens.lens (\ResourceSharePermissionDetail' {resourceType} -> resourceType) (\s@ResourceSharePermissionDetail' {} a -> s {resourceType = a} :: ResourceSharePermissionDetail)

-- | The date and time when the permission was last updated.
resourceSharePermissionDetail_lastUpdatedTime :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionDetail_lastUpdatedTime = Lens.lens (\ResourceSharePermissionDetail' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourceSharePermissionDetail' {} a -> s {lastUpdatedTime = a} :: ResourceSharePermissionDetail) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the permission.
resourceSharePermissionDetail_arn :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_arn = Lens.lens (\ResourceSharePermissionDetail' {arn} -> arn) (\s@ResourceSharePermissionDetail' {} a -> s {arn = a} :: ResourceSharePermissionDetail)

-- | Specifies whether the version of the permission is set to the default
-- version for this permission.
resourceSharePermissionDetail_defaultVersion :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Bool)
resourceSharePermissionDetail_defaultVersion = Lens.lens (\ResourceSharePermissionDetail' {defaultVersion} -> defaultVersion) (\s@ResourceSharePermissionDetail' {} a -> s {defaultVersion = a} :: ResourceSharePermissionDetail)

-- | The name of the permission.
resourceSharePermissionDetail_name :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_name = Lens.lens (\ResourceSharePermissionDetail' {name} -> name) (\s@ResourceSharePermissionDetail' {} a -> s {name = a} :: ResourceSharePermissionDetail)

-- | The identifier for the version of the permission.
resourceSharePermissionDetail_version :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_version = Lens.lens (\ResourceSharePermissionDetail' {version} -> version) (\s@ResourceSharePermissionDetail' {} a -> s {version = a} :: ResourceSharePermissionDetail)

-- | Specifies whether the version of the permission is set to the default
-- version for this resource type.
resourceSharePermissionDetail_isResourceTypeDefault :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Bool)
resourceSharePermissionDetail_isResourceTypeDefault = Lens.lens (\ResourceSharePermissionDetail' {isResourceTypeDefault} -> isResourceTypeDefault) (\s@ResourceSharePermissionDetail' {} a -> s {isResourceTypeDefault = a} :: ResourceSharePermissionDetail)

-- | The permission\'s effect and actions in JSON format. The @effect@
-- indicates whether the actions are allowed or denied. The @actions@ list
-- the API actions to which the principal is granted or denied access.
resourceSharePermissionDetail_permission :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_permission = Lens.lens (\ResourceSharePermissionDetail' {permission} -> permission) (\s@ResourceSharePermissionDetail' {} a -> s {permission = a} :: ResourceSharePermissionDetail)

instance Core.FromJSON ResourceSharePermissionDetail where
  parseJSON =
    Core.withObject
      "ResourceSharePermissionDetail"
      ( \x ->
          ResourceSharePermissionDetail'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "defaultVersion")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "isResourceTypeDefault")
            Prelude.<*> (x Core..:? "permission")
      )

instance
  Prelude.Hashable
    ResourceSharePermissionDetail
  where
  hashWithSalt _salt ResourceSharePermissionDetail' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` defaultVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` isResourceTypeDefault
      `Prelude.hashWithSalt` permission

instance Prelude.NFData ResourceSharePermissionDetail where
  rnf ResourceSharePermissionDetail' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf isResourceTypeDefault
      `Prelude.seq` Prelude.rnf permission
