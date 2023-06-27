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
-- Module      : Amazonka.RAM.Types.ResourceSharePermissionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceSharePermissionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.PermissionFeatureSet
import Amazonka.RAM.Types.PermissionType
import Amazonka.RAM.Types.Tag

-- | Information about an RAM permission.
--
-- /See:/ 'newResourceSharePermissionSummary' smart constructor.
data ResourceSharePermissionSummary = ResourceSharePermissionSummary'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the permission you want information about.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the permission was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the version of the managed permission used by this
    -- resource share is the default version for this managed permission.
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
    -- | Specifies whether the managed permission associated with this resource
    -- share is the default managed permission for all resources of this
    -- resource type.
    isResourceTypeDefault :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the permission was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of this managed permission.
    name :: Prelude.Maybe Prelude.Text,
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
    -- | The type of resource to which this permission applies. This takes the
    -- form of: @service-code@:@resource-code@, and is case-insensitive. For
    -- example, an Amazon EC2 Subnet would be represented by the string
    -- @ec2:subnet@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The current status of the permission.
    status :: Prelude.Maybe Prelude.Text,
    -- | A list of the tag key value pairs currently attached to the permission.
    tags :: Prelude.Maybe [Tag],
    -- | The version of the permission associated with this resource share.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSharePermissionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceSharePermissionSummary_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the permission you want information about.
--
-- 'creationTime', 'resourceSharePermissionSummary_creationTime' - The date and time when the permission was created.
--
-- 'defaultVersion', 'resourceSharePermissionSummary_defaultVersion' - Specifies whether the version of the managed permission used by this
-- resource share is the default version for this managed permission.
--
-- 'featureSet', 'resourceSharePermissionSummary_featureSet' - Indicates what features are available for this resource share. This
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
-- 'isResourceTypeDefault', 'resourceSharePermissionSummary_isResourceTypeDefault' - Specifies whether the managed permission associated with this resource
-- share is the default managed permission for all resources of this
-- resource type.
--
-- 'lastUpdatedTime', 'resourceSharePermissionSummary_lastUpdatedTime' - The date and time when the permission was last updated.
--
-- 'name', 'resourceSharePermissionSummary_name' - The name of this managed permission.
--
-- 'permissionType', 'resourceSharePermissionSummary_permissionType' - The type of managed permission. This can be one of the following values:
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
-- 'resourceType', 'resourceSharePermissionSummary_resourceType' - The type of resource to which this permission applies. This takes the
-- form of: @service-code@:@resource-code@, and is case-insensitive. For
-- example, an Amazon EC2 Subnet would be represented by the string
-- @ec2:subnet@.
--
-- 'status', 'resourceSharePermissionSummary_status' - The current status of the permission.
--
-- 'tags', 'resourceSharePermissionSummary_tags' - A list of the tag key value pairs currently attached to the permission.
--
-- 'version', 'resourceSharePermissionSummary_version' - The version of the permission associated with this resource share.
newResourceSharePermissionSummary ::
  ResourceSharePermissionSummary
newResourceSharePermissionSummary =
  ResourceSharePermissionSummary'
    { arn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      featureSet = Prelude.Nothing,
      isResourceTypeDefault = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      permissionType = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the permission you want information about.
resourceSharePermissionSummary_arn :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_arn = Lens.lens (\ResourceSharePermissionSummary' {arn} -> arn) (\s@ResourceSharePermissionSummary' {} a -> s {arn = a} :: ResourceSharePermissionSummary)

-- | The date and time when the permission was created.
resourceSharePermissionSummary_creationTime :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionSummary_creationTime = Lens.lens (\ResourceSharePermissionSummary' {creationTime} -> creationTime) (\s@ResourceSharePermissionSummary' {} a -> s {creationTime = a} :: ResourceSharePermissionSummary) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the version of the managed permission used by this
-- resource share is the default version for this managed permission.
resourceSharePermissionSummary_defaultVersion :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Bool)
resourceSharePermissionSummary_defaultVersion = Lens.lens (\ResourceSharePermissionSummary' {defaultVersion} -> defaultVersion) (\s@ResourceSharePermissionSummary' {} a -> s {defaultVersion = a} :: ResourceSharePermissionSummary)

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
resourceSharePermissionSummary_featureSet :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe PermissionFeatureSet)
resourceSharePermissionSummary_featureSet = Lens.lens (\ResourceSharePermissionSummary' {featureSet} -> featureSet) (\s@ResourceSharePermissionSummary' {} a -> s {featureSet = a} :: ResourceSharePermissionSummary)

-- | Specifies whether the managed permission associated with this resource
-- share is the default managed permission for all resources of this
-- resource type.
resourceSharePermissionSummary_isResourceTypeDefault :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Bool)
resourceSharePermissionSummary_isResourceTypeDefault = Lens.lens (\ResourceSharePermissionSummary' {isResourceTypeDefault} -> isResourceTypeDefault) (\s@ResourceSharePermissionSummary' {} a -> s {isResourceTypeDefault = a} :: ResourceSharePermissionSummary)

-- | The date and time when the permission was last updated.
resourceSharePermissionSummary_lastUpdatedTime :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.UTCTime)
resourceSharePermissionSummary_lastUpdatedTime = Lens.lens (\ResourceSharePermissionSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourceSharePermissionSummary' {} a -> s {lastUpdatedTime = a} :: ResourceSharePermissionSummary) Prelude.. Lens.mapping Data._Time

-- | The name of this managed permission.
resourceSharePermissionSummary_name :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_name = Lens.lens (\ResourceSharePermissionSummary' {name} -> name) (\s@ResourceSharePermissionSummary' {} a -> s {name = a} :: ResourceSharePermissionSummary)

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
resourceSharePermissionSummary_permissionType :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe PermissionType)
resourceSharePermissionSummary_permissionType = Lens.lens (\ResourceSharePermissionSummary' {permissionType} -> permissionType) (\s@ResourceSharePermissionSummary' {} a -> s {permissionType = a} :: ResourceSharePermissionSummary)

-- | The type of resource to which this permission applies. This takes the
-- form of: @service-code@:@resource-code@, and is case-insensitive. For
-- example, an Amazon EC2 Subnet would be represented by the string
-- @ec2:subnet@.
resourceSharePermissionSummary_resourceType :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_resourceType = Lens.lens (\ResourceSharePermissionSummary' {resourceType} -> resourceType) (\s@ResourceSharePermissionSummary' {} a -> s {resourceType = a} :: ResourceSharePermissionSummary)

-- | The current status of the permission.
resourceSharePermissionSummary_status :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_status = Lens.lens (\ResourceSharePermissionSummary' {status} -> status) (\s@ResourceSharePermissionSummary' {} a -> s {status = a} :: ResourceSharePermissionSummary)

-- | A list of the tag key value pairs currently attached to the permission.
resourceSharePermissionSummary_tags :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe [Tag])
resourceSharePermissionSummary_tags = Lens.lens (\ResourceSharePermissionSummary' {tags} -> tags) (\s@ResourceSharePermissionSummary' {} a -> s {tags = a} :: ResourceSharePermissionSummary) Prelude.. Lens.mapping Lens.coerced

-- | The version of the permission associated with this resource share.
resourceSharePermissionSummary_version :: Lens.Lens' ResourceSharePermissionSummary (Prelude.Maybe Prelude.Text)
resourceSharePermissionSummary_version = Lens.lens (\ResourceSharePermissionSummary' {version} -> version) (\s@ResourceSharePermissionSummary' {} a -> s {version = a} :: ResourceSharePermissionSummary)

instance Data.FromJSON ResourceSharePermissionSummary where
  parseJSON =
    Data.withObject
      "ResourceSharePermissionSummary"
      ( \x ->
          ResourceSharePermissionSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "defaultVersion")
            Prelude.<*> (x Data..:? "featureSet")
            Prelude.<*> (x Data..:? "isResourceTypeDefault")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "permissionType")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "version")
      )

instance
  Prelude.Hashable
    ResourceSharePermissionSummary
  where
  hashWithSalt
    _salt
    ResourceSharePermissionSummary' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` defaultVersion
        `Prelude.hashWithSalt` featureSet
        `Prelude.hashWithSalt` isResourceTypeDefault
        `Prelude.hashWithSalt` lastUpdatedTime
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` permissionType
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` version

instance
  Prelude.NFData
    ResourceSharePermissionSummary
  where
  rnf ResourceSharePermissionSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf featureSet
      `Prelude.seq` Prelude.rnf isResourceTypeDefault
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf permissionType
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf version
