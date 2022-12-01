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
-- Module      : Amazonka.RAM.Types.ResourceShareAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceShareAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.ResourceShareAssociationStatus
import Amazonka.RAM.Types.ResourceShareAssociationType

-- | Describes an association with a resource share and either a principal or
-- a resource.
--
-- /See:/ 'newResourceShareAssociation' smart constructor.
data ResourceShareAssociation = ResourceShareAssociation'
  { -- | Indicates whether the principal belongs to the same organization in
    -- Organizations as the Amazon Web Services account that owns the resource
    -- share.
    external :: Prelude.Maybe Prelude.Bool,
    -- | The type of entity included in this association.
    associationType :: Prelude.Maybe ResourceShareAssociationType,
    -- | The associated entity. This can be either of the following:
    --
    -- -   For a resource association, this is the
    --     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    --     of the resource.
    --
    -- -   For principal associations, this is one of the following:
    --
    --     -   The ID of an Amazon Web Services account
    --
    --     -   The
    --         <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    --         of an organization in Organizations
    --
    --     -   The ARN of an organizational unit (OU) in Organizations
    --
    --     -   The ARN of an IAM role
    --
    --     -   The ARN of an IAM user
    associatedEntity :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share.
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the association.
    status :: Prelude.Maybe ResourceShareAssociationStatus,
    -- | The date and time when the association was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time when the association was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | A message about the status of the association.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource share.
    resourceShareName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceShareAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'external', 'resourceShareAssociation_external' - Indicates whether the principal belongs to the same organization in
-- Organizations as the Amazon Web Services account that owns the resource
-- share.
--
-- 'associationType', 'resourceShareAssociation_associationType' - The type of entity included in this association.
--
-- 'associatedEntity', 'resourceShareAssociation_associatedEntity' - The associated entity. This can be either of the following:
--
-- -   For a resource association, this is the
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
--     of the resource.
--
-- -   For principal associations, this is one of the following:
--
--     -   The ID of an Amazon Web Services account
--
--     -   The
--         <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
--         of an organization in Organizations
--
--     -   The ARN of an organizational unit (OU) in Organizations
--
--     -   The ARN of an IAM role
--
--     -   The ARN of an IAM user
--
-- 'resourceShareArn', 'resourceShareAssociation_resourceShareArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share.
--
-- 'status', 'resourceShareAssociation_status' - The current status of the association.
--
-- 'lastUpdatedTime', 'resourceShareAssociation_lastUpdatedTime' - The date and time when the association was last updated.
--
-- 'creationTime', 'resourceShareAssociation_creationTime' - The date and time when the association was created.
--
-- 'statusMessage', 'resourceShareAssociation_statusMessage' - A message about the status of the association.
--
-- 'resourceShareName', 'resourceShareAssociation_resourceShareName' - The name of the resource share.
newResourceShareAssociation ::
  ResourceShareAssociation
newResourceShareAssociation =
  ResourceShareAssociation'
    { external =
        Prelude.Nothing,
      associationType = Prelude.Nothing,
      associatedEntity = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      resourceShareName = Prelude.Nothing
    }

-- | Indicates whether the principal belongs to the same organization in
-- Organizations as the Amazon Web Services account that owns the resource
-- share.
resourceShareAssociation_external :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Bool)
resourceShareAssociation_external = Lens.lens (\ResourceShareAssociation' {external} -> external) (\s@ResourceShareAssociation' {} a -> s {external = a} :: ResourceShareAssociation)

-- | The type of entity included in this association.
resourceShareAssociation_associationType :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe ResourceShareAssociationType)
resourceShareAssociation_associationType = Lens.lens (\ResourceShareAssociation' {associationType} -> associationType) (\s@ResourceShareAssociation' {} a -> s {associationType = a} :: ResourceShareAssociation)

-- | The associated entity. This can be either of the following:
--
-- -   For a resource association, this is the
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
--     of the resource.
--
-- -   For principal associations, this is one of the following:
--
--     -   The ID of an Amazon Web Services account
--
--     -   The
--         <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
--         of an organization in Organizations
--
--     -   The ARN of an organizational unit (OU) in Organizations
--
--     -   The ARN of an IAM role
--
--     -   The ARN of an IAM user
resourceShareAssociation_associatedEntity :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Text)
resourceShareAssociation_associatedEntity = Lens.lens (\ResourceShareAssociation' {associatedEntity} -> associatedEntity) (\s@ResourceShareAssociation' {} a -> s {associatedEntity = a} :: ResourceShareAssociation)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share.
resourceShareAssociation_resourceShareArn :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Text)
resourceShareAssociation_resourceShareArn = Lens.lens (\ResourceShareAssociation' {resourceShareArn} -> resourceShareArn) (\s@ResourceShareAssociation' {} a -> s {resourceShareArn = a} :: ResourceShareAssociation)

-- | The current status of the association.
resourceShareAssociation_status :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe ResourceShareAssociationStatus)
resourceShareAssociation_status = Lens.lens (\ResourceShareAssociation' {status} -> status) (\s@ResourceShareAssociation' {} a -> s {status = a} :: ResourceShareAssociation)

-- | The date and time when the association was last updated.
resourceShareAssociation_lastUpdatedTime :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.UTCTime)
resourceShareAssociation_lastUpdatedTime = Lens.lens (\ResourceShareAssociation' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourceShareAssociation' {} a -> s {lastUpdatedTime = a} :: ResourceShareAssociation) Prelude.. Lens.mapping Core._Time

-- | The date and time when the association was created.
resourceShareAssociation_creationTime :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.UTCTime)
resourceShareAssociation_creationTime = Lens.lens (\ResourceShareAssociation' {creationTime} -> creationTime) (\s@ResourceShareAssociation' {} a -> s {creationTime = a} :: ResourceShareAssociation) Prelude.. Lens.mapping Core._Time

-- | A message about the status of the association.
resourceShareAssociation_statusMessage :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Text)
resourceShareAssociation_statusMessage = Lens.lens (\ResourceShareAssociation' {statusMessage} -> statusMessage) (\s@ResourceShareAssociation' {} a -> s {statusMessage = a} :: ResourceShareAssociation)

-- | The name of the resource share.
resourceShareAssociation_resourceShareName :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Text)
resourceShareAssociation_resourceShareName = Lens.lens (\ResourceShareAssociation' {resourceShareName} -> resourceShareName) (\s@ResourceShareAssociation' {} a -> s {resourceShareName = a} :: ResourceShareAssociation)

instance Core.FromJSON ResourceShareAssociation where
  parseJSON =
    Core.withObject
      "ResourceShareAssociation"
      ( \x ->
          ResourceShareAssociation'
            Prelude.<$> (x Core..:? "external")
            Prelude.<*> (x Core..:? "associationType")
            Prelude.<*> (x Core..:? "associatedEntity")
            Prelude.<*> (x Core..:? "resourceShareArn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "resourceShareName")
      )

instance Prelude.Hashable ResourceShareAssociation where
  hashWithSalt _salt ResourceShareAssociation' {..} =
    _salt `Prelude.hashWithSalt` external
      `Prelude.hashWithSalt` associationType
      `Prelude.hashWithSalt` associatedEntity
      `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` resourceShareName

instance Prelude.NFData ResourceShareAssociation where
  rnf ResourceShareAssociation' {..} =
    Prelude.rnf external
      `Prelude.seq` Prelude.rnf associationType
      `Prelude.seq` Prelude.rnf associatedEntity
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf resourceShareName
