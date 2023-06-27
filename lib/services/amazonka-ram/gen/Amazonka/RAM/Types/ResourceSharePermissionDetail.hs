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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceSharePermissionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.PermissionFeatureSet
import Amazonka.RAM.Types.PermissionStatus
import Amazonka.RAM.Types.PermissionType
import Amazonka.RAM.Types.Tag

-- | Information about a RAM managed permission.
--
-- /See:/ 'newResourceSharePermissionDetail' smart constructor.
data ResourceSharePermissionDetail = ResourceSharePermissionDetail'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of this RAM managed permission.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the permission was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the version of the permission represented in this
    -- response is the default version for this permission.
    defaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | Indicates what features are available for this resource share. This
    -- parameter can have one of the following values:
    --
    -- -   __STANDARD__ – A resource share that supports all functionality.
    --     These resource shares are visible to all principals you share the
    --     resource share with. You can modify these resource shares in RAM
    --     using the console or APIs. This resource share might have been
    --     created by RAM, or it might have been __CREATED_FROM_POLICY__ and
    --     then promoted.
    --
    -- -   __CREATED_FROM_POLICY__ – The customer manually shared a resource by
    --     attaching a resource-based policy. That policy did not match any
    --     existing managed permissions, so RAM created this customer managed
    --     permission automatically on the customer\'s behalf based on the
    --     attached policy document. This type of resource share is visible
    --     only to the Amazon Web Services account that created it. You can\'t
    --     modify it in RAM unless you promote it. For more information, see
    --     PromoteResourceShareCreatedFromPolicy.
    --
    -- -   __PROMOTING_TO_STANDARD__ – This resource share was originally
    --     @CREATED_FROM_POLICY@, but the customer ran the
    --     PromoteResourceShareCreatedFromPolicy and that operation is still in
    --     progress. This value changes to @STANDARD@ when complete.
    featureSet :: Prelude.Maybe PermissionFeatureSet,
    -- | Specifies whether the version of the permission represented in this
    -- response is the default version for all resources of this resource type.
    isResourceTypeDefault :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the permission was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of this permission.
    name :: Prelude.Maybe Prelude.Text,
    -- | The permission\'s effect and actions in JSON format. The @effect@
    -- indicates whether the specified actions are allowed or denied. The
    -- @actions@ list the operations to which the principal is granted or
    -- denied access.
    permission :: Prelude.Maybe Prelude.Text,
    -- | The type of managed permission. This can be one of the following values:
    --
    -- -   @AWS_MANAGED@ – Amazon Web Services created and manages this managed
    --     permission. You can associate it with your resource shares, but you
    --     can\'t modify it.
    --
    -- -   @CUSTOMER_MANAGED@ – You, or another principal in your account
    --     created this managed permission. You can associate it with your
    --     resource shares and create new versions that have different
    --     permissions.
    permissionType :: Prelude.Maybe PermissionType,
    -- | The resource type to which this permission applies.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The current status of the association between the permission and the
    -- resource share. The following are the possible values:
    --
    -- -   @ATTACHABLE@ – This permission or version can be associated with
    --     resource shares.
    --
    -- -   @UNATTACHABLE@ – This permission or version can\'t currently be
    --     associated with resource shares.
    --
    -- -   @DELETING@ – This permission or version is in the process of being
    --     deleted.
    --
    -- -   @DELETED@ – This permission or version is deleted.
    status :: Prelude.Maybe PermissionStatus,
    -- | The tag key and value pairs attached to the resource share.
    tags :: Prelude.Maybe [Tag],
    -- | The version of the permission described in this response.
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
-- 'arn', 'resourceSharePermissionDetail_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of this RAM managed permission.
--
-- 'creationTime', 'resourceSharePermissionDetail_creationTime' - The date and time when the permission was created.
--
-- 'defaultVersion', 'resourceSharePermissionDetail_defaultVersion' - Specifies whether the version of the permission represented in this
-- response is the default version for this permission.
--
-- 'featureSet', 'resourceSharePermissionDetail_featureSet' - Indicates what features are available for this resource share. This
-- parameter can have one of the following values:
--
-- -   __STANDARD__ – A resource share that supports all functionality.
--     These resource shares are visible to all principals you share the
--     resource share with. You can modify these resource shares in RAM
--     using the console or APIs. This resource share might have been
--     created by RAM, or it might have been __CREATED_FROM_POLICY__ and
--     then promoted.
--
-- -   __CREATED_FROM_POLICY__ – The customer manually shared a resource by
--     attaching a resource-based policy. That policy did not match any
--     existing managed permissions, so RAM created this customer managed
--     permission automatically on the customer\'s behalf based on the
--     attached policy document. This type of resource share is visible
--     only to the Amazon Web Services account that created it. You can\'t
--     modify it in RAM unless you promote it. For more information, see
--     PromoteResourceShareCreatedFromPolicy.
--
-- -   __PROMOTING_TO_STANDARD__ – This resource share was originally
--     @CREATED_FROM_POLICY@, but the customer ran the
--     PromoteResourceShareCreatedFromPolicy and that operation is still in
--     progress. This value changes to @STANDARD@ when complete.
--
-- 'isResourceTypeDefault', 'resourceSharePermissionDetail_isResourceTypeDefault' - Specifies whether the version of the permission represented in this
-- response is the default version for all resources of this resource type.
--
-- 'lastUpdatedTime', 'resourceSharePermissionDetail_lastUpdatedTime' - The date and time when the permission was last updated.
--
-- 'name', 'resourceSharePermissionDetail_name' - The name of this permission.
--
-- 'permission', 'resourceSharePermissionDetail_permission' - The permission\'s effect and actions in JSON format. The @effect@
-- indicates whether the specified actions are allowed or denied. The
-- @actions@ list the operations to which the principal is granted or
-- denied access.
--
-- 'permissionType', 'resourceSharePermissionDetail_permissionType' - The type of managed permission. This can be one of the following values:
--
-- -   @AWS_MANAGED@ – Amazon Web Services created and manages this managed
--     permission. You can associate it with your resource shares, but you
--     can\'t modify it.
--
-- -   @CUSTOMER_MANAGED@ – You, or another principal in your account
--     created this managed permission. You can associate it with your
--     resource shares and create new versions that have different
--     permissions.
--
-- 'resourceType', 'resourceSharePermissionDetail_resourceType' - The resource type to which this permission applies.
--
-- 'status', 'resourceSharePermissionDetail_status' - The current status of the association between the permission and the
-- resource share. The following are the possible values:
--
-- -   @ATTACHABLE@ – This permission or version can be associated with
--     resource shares.
--
-- -   @UNATTACHABLE@ – This permission or version can\'t currently be
--     associated with resource shares.
--
-- -   @DELETING@ – This permission or version is in the process of being
--     deleted.
--
-- -   @DELETED@ – This permission or version is deleted.
--
-- 'tags', 'resourceSharePermissionDetail_tags' - The tag key and value pairs attached to the resource share.
--
-- 'version', 'resourceSharePermissionDetail_version' - The version of the permission described in this response.
newResourceSharePermissionDetail ::
  ResourceSharePermissionDetail
newResourceSharePermissionDetail =
  ResourceSharePermissionDetail'
    { arn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      featureSet = Prelude.Nothing,
      isResourceTypeDefault = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      permission = Prelude.Nothing,
      permissionType = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of this RAM managed permission.
resourceSharePermissionDetail_arn :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_arn = Lens.lens (\ResourceSharePermissionDetail' {arn} -> arn) (\s@ResourceSharePermissionDetail' {} a -> s {arn = a} :: ResourceSharePermissionDetail)

-- | The date and time when the permission was created.
resourceSharePermissionDetail_creationTime :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionDetail_creationTime = Lens.lens (\ResourceSharePermissionDetail' {creationTime} -> creationTime) (\s@ResourceSharePermissionDetail' {} a -> s {creationTime = a} :: ResourceSharePermissionDetail) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the version of the permission represented in this
-- response is the default version for this permission.
resourceSharePermissionDetail_defaultVersion :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Bool)
resourceSharePermissionDetail_defaultVersion = Lens.lens (\ResourceSharePermissionDetail' {defaultVersion} -> defaultVersion) (\s@ResourceSharePermissionDetail' {} a -> s {defaultVersion = a} :: ResourceSharePermissionDetail)

-- | Indicates what features are available for this resource share. This
-- parameter can have one of the following values:
--
-- -   __STANDARD__ – A resource share that supports all functionality.
--     These resource shares are visible to all principals you share the
--     resource share with. You can modify these resource shares in RAM
--     using the console or APIs. This resource share might have been
--     created by RAM, or it might have been __CREATED_FROM_POLICY__ and
--     then promoted.
--
-- -   __CREATED_FROM_POLICY__ – The customer manually shared a resource by
--     attaching a resource-based policy. That policy did not match any
--     existing managed permissions, so RAM created this customer managed
--     permission automatically on the customer\'s behalf based on the
--     attached policy document. This type of resource share is visible
--     only to the Amazon Web Services account that created it. You can\'t
--     modify it in RAM unless you promote it. For more information, see
--     PromoteResourceShareCreatedFromPolicy.
--
-- -   __PROMOTING_TO_STANDARD__ – This resource share was originally
--     @CREATED_FROM_POLICY@, but the customer ran the
--     PromoteResourceShareCreatedFromPolicy and that operation is still in
--     progress. This value changes to @STANDARD@ when complete.
resourceSharePermissionDetail_featureSet :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe PermissionFeatureSet)
resourceSharePermissionDetail_featureSet = Lens.lens (\ResourceSharePermissionDetail' {featureSet} -> featureSet) (\s@ResourceSharePermissionDetail' {} a -> s {featureSet = a} :: ResourceSharePermissionDetail)

-- | Specifies whether the version of the permission represented in this
-- response is the default version for all resources of this resource type.
resourceSharePermissionDetail_isResourceTypeDefault :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Bool)
resourceSharePermissionDetail_isResourceTypeDefault = Lens.lens (\ResourceSharePermissionDetail' {isResourceTypeDefault} -> isResourceTypeDefault) (\s@ResourceSharePermissionDetail' {} a -> s {isResourceTypeDefault = a} :: ResourceSharePermissionDetail)

-- | The date and time when the permission was last updated.
resourceSharePermissionDetail_lastUpdatedTime :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionDetail_lastUpdatedTime = Lens.lens (\ResourceSharePermissionDetail' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourceSharePermissionDetail' {} a -> s {lastUpdatedTime = a} :: ResourceSharePermissionDetail) Prelude.. Lens.mapping Data._Time

-- | The name of this permission.
resourceSharePermissionDetail_name :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_name = Lens.lens (\ResourceSharePermissionDetail' {name} -> name) (\s@ResourceSharePermissionDetail' {} a -> s {name = a} :: ResourceSharePermissionDetail)

-- | The permission\'s effect and actions in JSON format. The @effect@
-- indicates whether the specified actions are allowed or denied. The
-- @actions@ list the operations to which the principal is granted or
-- denied access.
resourceSharePermissionDetail_permission :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_permission = Lens.lens (\ResourceSharePermissionDetail' {permission} -> permission) (\s@ResourceSharePermissionDetail' {} a -> s {permission = a} :: ResourceSharePermissionDetail)

-- | The type of managed permission. This can be one of the following values:
--
-- -   @AWS_MANAGED@ – Amazon Web Services created and manages this managed
--     permission. You can associate it with your resource shares, but you
--     can\'t modify it.
--
-- -   @CUSTOMER_MANAGED@ – You, or another principal in your account
--     created this managed permission. You can associate it with your
--     resource shares and create new versions that have different
--     permissions.
resourceSharePermissionDetail_permissionType :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe PermissionType)
resourceSharePermissionDetail_permissionType = Lens.lens (\ResourceSharePermissionDetail' {permissionType} -> permissionType) (\s@ResourceSharePermissionDetail' {} a -> s {permissionType = a} :: ResourceSharePermissionDetail)

-- | The resource type to which this permission applies.
resourceSharePermissionDetail_resourceType :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_resourceType = Lens.lens (\ResourceSharePermissionDetail' {resourceType} -> resourceType) (\s@ResourceSharePermissionDetail' {} a -> s {resourceType = a} :: ResourceSharePermissionDetail)

-- | The current status of the association between the permission and the
-- resource share. The following are the possible values:
--
-- -   @ATTACHABLE@ – This permission or version can be associated with
--     resource shares.
--
-- -   @UNATTACHABLE@ – This permission or version can\'t currently be
--     associated with resource shares.
--
-- -   @DELETING@ – This permission or version is in the process of being
--     deleted.
--
-- -   @DELETED@ – This permission or version is deleted.
resourceSharePermissionDetail_status :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe PermissionStatus)
resourceSharePermissionDetail_status = Lens.lens (\ResourceSharePermissionDetail' {status} -> status) (\s@ResourceSharePermissionDetail' {} a -> s {status = a} :: ResourceSharePermissionDetail)

-- | The tag key and value pairs attached to the resource share.
resourceSharePermissionDetail_tags :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe [Tag])
resourceSharePermissionDetail_tags = Lens.lens (\ResourceSharePermissionDetail' {tags} -> tags) (\s@ResourceSharePermissionDetail' {} a -> s {tags = a} :: ResourceSharePermissionDetail) Prelude.. Lens.mapping Lens.coerced

-- | The version of the permission described in this response.
resourceSharePermissionDetail_version :: Lens.Lens' ResourceSharePermissionDetail (Prelude.Maybe Prelude.Text)
resourceSharePermissionDetail_version = Lens.lens (\ResourceSharePermissionDetail' {version} -> version) (\s@ResourceSharePermissionDetail' {} a -> s {version = a} :: ResourceSharePermissionDetail)

instance Data.FromJSON ResourceSharePermissionDetail where
  parseJSON =
    Data.withObject
      "ResourceSharePermissionDetail"
      ( \x ->
          ResourceSharePermissionDetail'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "defaultVersion")
            Prelude.<*> (x Data..:? "featureSet")
            Prelude.<*> (x Data..:? "isResourceTypeDefault")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "permission")
            Prelude.<*> (x Data..:? "permissionType")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "version")
      )

instance
  Prelude.Hashable
    ResourceSharePermissionDetail
  where
  hashWithSalt _salt ResourceSharePermissionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` defaultVersion
      `Prelude.hashWithSalt` featureSet
      `Prelude.hashWithSalt` isResourceTypeDefault
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` permission
      `Prelude.hashWithSalt` permissionType
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` version

instance Prelude.NFData ResourceSharePermissionDetail where
  rnf ResourceSharePermissionDetail' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf featureSet
      `Prelude.seq` Prelude.rnf isResourceTypeDefault
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf permission
      `Prelude.seq` Prelude.rnf permissionType
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf version
