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
-- Module      : Amazonka.RAM.Types.AssociatedPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.AssociatedPermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.PermissionFeatureSet

-- | An object that describes a managed permission associated with a resource
-- share.
--
-- /See:/ 'newAssociatedPermission' smart constructor.
data AssociatedPermission = AssociatedPermission'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the associated managed permission.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the associated resource share is using the default
    -- version of the permission.
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
    -- | The date and time when the association between the permission and the
    -- resource share was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The version of the permission currently associated with the resource
    -- share.
    permissionVersion :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of a resource share associated with this permission.
    resourceShareArn :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'associatedPermission_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the associated managed permission.
--
-- 'defaultVersion', 'associatedPermission_defaultVersion' - Indicates whether the associated resource share is using the default
-- version of the permission.
--
-- 'featureSet', 'associatedPermission_featureSet' - Indicates what features are available for this resource share. This
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
-- 'lastUpdatedTime', 'associatedPermission_lastUpdatedTime' - The date and time when the association between the permission and the
-- resource share was last updated.
--
-- 'permissionVersion', 'associatedPermission_permissionVersion' - The version of the permission currently associated with the resource
-- share.
--
-- 'resourceShareArn', 'associatedPermission_resourceShareArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of a resource share associated with this permission.
--
-- 'resourceType', 'associatedPermission_resourceType' - The resource type to which this permission applies.
--
-- 'status', 'associatedPermission_status' - The current status of the association between the permission and the
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
newAssociatedPermission ::
  AssociatedPermission
newAssociatedPermission =
  AssociatedPermission'
    { arn = Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      featureSet = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      permissionVersion = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the associated managed permission.
associatedPermission_arn :: Lens.Lens' AssociatedPermission (Prelude.Maybe Prelude.Text)
associatedPermission_arn = Lens.lens (\AssociatedPermission' {arn} -> arn) (\s@AssociatedPermission' {} a -> s {arn = a} :: AssociatedPermission)

-- | Indicates whether the associated resource share is using the default
-- version of the permission.
associatedPermission_defaultVersion :: Lens.Lens' AssociatedPermission (Prelude.Maybe Prelude.Bool)
associatedPermission_defaultVersion = Lens.lens (\AssociatedPermission' {defaultVersion} -> defaultVersion) (\s@AssociatedPermission' {} a -> s {defaultVersion = a} :: AssociatedPermission)

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
associatedPermission_featureSet :: Lens.Lens' AssociatedPermission (Prelude.Maybe PermissionFeatureSet)
associatedPermission_featureSet = Lens.lens (\AssociatedPermission' {featureSet} -> featureSet) (\s@AssociatedPermission' {} a -> s {featureSet = a} :: AssociatedPermission)

-- | The date and time when the association between the permission and the
-- resource share was last updated.
associatedPermission_lastUpdatedTime :: Lens.Lens' AssociatedPermission (Prelude.Maybe Prelude.UTCTime)
associatedPermission_lastUpdatedTime = Lens.lens (\AssociatedPermission' {lastUpdatedTime} -> lastUpdatedTime) (\s@AssociatedPermission' {} a -> s {lastUpdatedTime = a} :: AssociatedPermission) Prelude.. Lens.mapping Data._Time

-- | The version of the permission currently associated with the resource
-- share.
associatedPermission_permissionVersion :: Lens.Lens' AssociatedPermission (Prelude.Maybe Prelude.Text)
associatedPermission_permissionVersion = Lens.lens (\AssociatedPermission' {permissionVersion} -> permissionVersion) (\s@AssociatedPermission' {} a -> s {permissionVersion = a} :: AssociatedPermission)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of a resource share associated with this permission.
associatedPermission_resourceShareArn :: Lens.Lens' AssociatedPermission (Prelude.Maybe Prelude.Text)
associatedPermission_resourceShareArn = Lens.lens (\AssociatedPermission' {resourceShareArn} -> resourceShareArn) (\s@AssociatedPermission' {} a -> s {resourceShareArn = a} :: AssociatedPermission)

-- | The resource type to which this permission applies.
associatedPermission_resourceType :: Lens.Lens' AssociatedPermission (Prelude.Maybe Prelude.Text)
associatedPermission_resourceType = Lens.lens (\AssociatedPermission' {resourceType} -> resourceType) (\s@AssociatedPermission' {} a -> s {resourceType = a} :: AssociatedPermission)

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
associatedPermission_status :: Lens.Lens' AssociatedPermission (Prelude.Maybe Prelude.Text)
associatedPermission_status = Lens.lens (\AssociatedPermission' {status} -> status) (\s@AssociatedPermission' {} a -> s {status = a} :: AssociatedPermission)

instance Data.FromJSON AssociatedPermission where
  parseJSON =
    Data.withObject
      "AssociatedPermission"
      ( \x ->
          AssociatedPermission'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "defaultVersion")
            Prelude.<*> (x Data..:? "featureSet")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "permissionVersion")
            Prelude.<*> (x Data..:? "resourceShareArn")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable AssociatedPermission where
  hashWithSalt _salt AssociatedPermission' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` defaultVersion
      `Prelude.hashWithSalt` featureSet
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` permissionVersion
      `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` status

instance Prelude.NFData AssociatedPermission where
  rnf AssociatedPermission' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf featureSet
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf permissionVersion
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf status
