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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceSharePermissionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an RAM permission.
--
-- /See:/ 'newResourceSharePermissionDetail' smart constructor.
data ResourceSharePermissionDetail = ResourceSharePermissionDetail'
  { -- | The resource type to which this permission applies.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of this permission.
    name :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of this RAM permission.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The permission\'s effect and actions in JSON format. The @effect@
    -- indicates whether the specified actions are allowed or denied. The
    -- @actions@ list the operations to which the principal is granted or
    -- denied access.
    permission :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the permission was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the version of the permission represented in this
    -- structure is the default version for this permission.
    defaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the permission was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the version of the permission represented in this
    -- structure is the default version for all resources of this resource
    -- type.
    isResourceTypeDefault :: Prelude.Maybe Prelude.Bool,
    -- | The version of the permission represented in this structure.
    version :: Prelude.Maybe Prelude.Text
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
-- 'resourceType', 'resourceSharePermissionDetail_resourceType' - The resource type to which this permission applies.
--
-- 'name', 'resourceSharePermissionDetail_name' - The name of this permission.
--
-- 'arn', 'resourceSharePermissionDetail_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of this RAM permission.
--
-- 'permission', 'resourceSharePermissionDetail_permission' - The permission\'s effect and actions in JSON format. The @effect@
-- indicates whether the specified actions are allowed or denied. The
-- @actions@ list the operations to which the principal is granted or
-- denied access.
--
-- 'lastUpdatedTime', 'resourceSharePermissionDetail_lastUpdatedTime' - The date and time when the permission was last updated.
--
-- 'defaultVersion', 'resourceSharePermissionDetail_defaultVersion' - Specifies whether the version of the permission represented in this
-- structure is the default version for this permission.
--
-- 'creationTime', 'resourceSharePermissionDetail_creationTime' - The date and time when the permission was created.
--
-- 'isResourceTypeDefault', 'resourceSharePermissionDetail_isResourceTypeDefault' - Specifies whether the version of the permission represented in this
-- structure is the default version for all resources of this resource
-- type.
--
-- 'version', 'resourceSharePermissionDetail_version' - The version of the permission represented in this structure.
newResourceSharePermissionDetail ::
  ResourceSharePermissionDetail
newResourceSharePermissionDetail =
  ResourceSharePermissionDetail'
    { resourceType =
        Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      permission = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      isResourceTypeDefault = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The resource type to which this permission applies.
resourceSharePermissionDetail_resourceType :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_resourceType = Lens.lens (\ResourceSharePermissionDetail' {resourceType} -> resourceType) (\s@ResourceSharePermissionDetail' {} a -> s {resourceType = a} :: ResourceSharePermissionDetail)

-- | The name of this permission.
resourceSharePermissionDetail_name :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_name = Lens.lens (\ResourceSharePermissionDetail' {name} -> name) (\s@ResourceSharePermissionDetail' {} a -> s {name = a} :: ResourceSharePermissionDetail)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of this RAM permission.
resourceSharePermissionDetail_arn :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_arn = Lens.lens (\ResourceSharePermissionDetail' {arn} -> arn) (\s@ResourceSharePermissionDetail' {} a -> s {arn = a} :: ResourceSharePermissionDetail)

-- | The permission\'s effect and actions in JSON format. The @effect@
-- indicates whether the specified actions are allowed or denied. The
-- @actions@ list the operations to which the principal is granted or
-- denied access.
resourceSharePermissionDetail_permission :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_permission = Lens.lens (\ResourceSharePermissionDetail' {permission} -> permission) (\s@ResourceSharePermissionDetail' {} a -> s {permission = a} :: ResourceSharePermissionDetail)

-- | The date and time when the permission was last updated.
resourceSharePermissionDetail_lastUpdatedTime :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionDetail_lastUpdatedTime = Lens.lens (\ResourceSharePermissionDetail' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourceSharePermissionDetail' {} a -> s {lastUpdatedTime = a} :: ResourceSharePermissionDetail) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the version of the permission represented in this
-- structure is the default version for this permission.
resourceSharePermissionDetail_defaultVersion :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Bool)
resourceSharePermissionDetail_defaultVersion = Lens.lens (\ResourceSharePermissionDetail' {defaultVersion} -> defaultVersion) (\s@ResourceSharePermissionDetail' {} a -> s {defaultVersion = a} :: ResourceSharePermissionDetail)

-- | The date and time when the permission was created.
resourceSharePermissionDetail_creationTime :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionDetail_creationTime = Lens.lens (\ResourceSharePermissionDetail' {creationTime} -> creationTime) (\s@ResourceSharePermissionDetail' {} a -> s {creationTime = a} :: ResourceSharePermissionDetail) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the version of the permission represented in this
-- structure is the default version for all resources of this resource
-- type.
resourceSharePermissionDetail_isResourceTypeDefault :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Bool)
resourceSharePermissionDetail_isResourceTypeDefault = Lens.lens (\ResourceSharePermissionDetail' {isResourceTypeDefault} -> isResourceTypeDefault) (\s@ResourceSharePermissionDetail' {} a -> s {isResourceTypeDefault = a} :: ResourceSharePermissionDetail)

-- | The version of the permission represented in this structure.
resourceSharePermissionDetail_version :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_version = Lens.lens (\ResourceSharePermissionDetail' {version} -> version) (\s@ResourceSharePermissionDetail' {} a -> s {version = a} :: ResourceSharePermissionDetail)

instance Data.FromJSON ResourceSharePermissionDetail where
  parseJSON =
    Data.withObject
      "ResourceSharePermissionDetail"
      ( \x ->
          ResourceSharePermissionDetail'
            Prelude.<$> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "permission")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "defaultVersion")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "isResourceTypeDefault")
            Prelude.<*> (x Data..:? "version")
      )

instance
  Prelude.Hashable
    ResourceSharePermissionDetail
  where
  hashWithSalt _salt ResourceSharePermissionDetail' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` permission
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` defaultVersion
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` isResourceTypeDefault
      `Prelude.hashWithSalt` version

instance Prelude.NFData ResourceSharePermissionDetail where
  rnf ResourceSharePermissionDetail' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf permission
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf isResourceTypeDefault
      `Prelude.seq` Prelude.rnf version
