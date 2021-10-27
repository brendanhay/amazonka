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
-- Module      : Network.AWS.RAM.Types.ResourceShareAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RAM.Types.ResourceShareAssociation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RAM.Types.ResourceShareAssociationStatus
import Network.AWS.RAM.Types.ResourceShareAssociationType

-- | Describes an association with a resource share.
--
-- /See:/ 'newResourceShareAssociation' smart constructor.
data ResourceShareAssociation = ResourceShareAssociation'
  { -- | The time when the association was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the association.
    status :: Prelude.Maybe ResourceShareAssociationStatus,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the association was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether the principal belongs to the same organization in
    -- Organizations as the Amazon Web Services account that owns the resource
    -- share.
    external :: Prelude.Maybe Prelude.Bool,
    -- | The name of the resource share.
    resourceShareName :: Prelude.Maybe Prelude.Text,
    -- | The associated entity. For resource associations, this is the Amazon
    -- Resource Name (ARN) of the resource. For principal associations, this is
    -- one of the following:
    --
    -- -   An Amazon Web Services account ID
    --
    -- -   An ARN of an organization in Organizations
    --
    -- -   An ARN of an organizational unit (OU) in Organizations
    --
    -- -   An ARN of an IAM role
    --
    -- -   An ARN of an IAM user
    associatedEntity :: Prelude.Maybe Prelude.Text,
    -- | The association type.
    associationType :: Prelude.Maybe ResourceShareAssociationType,
    -- | A message about the status of the association.
    statusMessage :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'resourceShareAssociation_creationTime' - The time when the association was created.
--
-- 'status', 'resourceShareAssociation_status' - The status of the association.
--
-- 'resourceShareArn', 'resourceShareAssociation_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
--
-- 'lastUpdatedTime', 'resourceShareAssociation_lastUpdatedTime' - The time when the association was last updated.
--
-- 'external', 'resourceShareAssociation_external' - Indicates whether the principal belongs to the same organization in
-- Organizations as the Amazon Web Services account that owns the resource
-- share.
--
-- 'resourceShareName', 'resourceShareAssociation_resourceShareName' - The name of the resource share.
--
-- 'associatedEntity', 'resourceShareAssociation_associatedEntity' - The associated entity. For resource associations, this is the Amazon
-- Resource Name (ARN) of the resource. For principal associations, this is
-- one of the following:
--
-- -   An Amazon Web Services account ID
--
-- -   An ARN of an organization in Organizations
--
-- -   An ARN of an organizational unit (OU) in Organizations
--
-- -   An ARN of an IAM role
--
-- -   An ARN of an IAM user
--
-- 'associationType', 'resourceShareAssociation_associationType' - The association type.
--
-- 'statusMessage', 'resourceShareAssociation_statusMessage' - A message about the status of the association.
newResourceShareAssociation ::
  ResourceShareAssociation
newResourceShareAssociation =
  ResourceShareAssociation'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      external = Prelude.Nothing,
      resourceShareName = Prelude.Nothing,
      associatedEntity = Prelude.Nothing,
      associationType = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The time when the association was created.
resourceShareAssociation_creationTime :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.UTCTime)
resourceShareAssociation_creationTime = Lens.lens (\ResourceShareAssociation' {creationTime} -> creationTime) (\s@ResourceShareAssociation' {} a -> s {creationTime = a} :: ResourceShareAssociation) Prelude.. Lens.mapping Core._Time

-- | The status of the association.
resourceShareAssociation_status :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe ResourceShareAssociationStatus)
resourceShareAssociation_status = Lens.lens (\ResourceShareAssociation' {status} -> status) (\s@ResourceShareAssociation' {} a -> s {status = a} :: ResourceShareAssociation)

-- | The Amazon Resource Name (ARN) of the resource share.
resourceShareAssociation_resourceShareArn :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Text)
resourceShareAssociation_resourceShareArn = Lens.lens (\ResourceShareAssociation' {resourceShareArn} -> resourceShareArn) (\s@ResourceShareAssociation' {} a -> s {resourceShareArn = a} :: ResourceShareAssociation)

-- | The time when the association was last updated.
resourceShareAssociation_lastUpdatedTime :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.UTCTime)
resourceShareAssociation_lastUpdatedTime = Lens.lens (\ResourceShareAssociation' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourceShareAssociation' {} a -> s {lastUpdatedTime = a} :: ResourceShareAssociation) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the principal belongs to the same organization in
-- Organizations as the Amazon Web Services account that owns the resource
-- share.
resourceShareAssociation_external :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Bool)
resourceShareAssociation_external = Lens.lens (\ResourceShareAssociation' {external} -> external) (\s@ResourceShareAssociation' {} a -> s {external = a} :: ResourceShareAssociation)

-- | The name of the resource share.
resourceShareAssociation_resourceShareName :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Text)
resourceShareAssociation_resourceShareName = Lens.lens (\ResourceShareAssociation' {resourceShareName} -> resourceShareName) (\s@ResourceShareAssociation' {} a -> s {resourceShareName = a} :: ResourceShareAssociation)

-- | The associated entity. For resource associations, this is the Amazon
-- Resource Name (ARN) of the resource. For principal associations, this is
-- one of the following:
--
-- -   An Amazon Web Services account ID
--
-- -   An ARN of an organization in Organizations
--
-- -   An ARN of an organizational unit (OU) in Organizations
--
-- -   An ARN of an IAM role
--
-- -   An ARN of an IAM user
resourceShareAssociation_associatedEntity :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Text)
resourceShareAssociation_associatedEntity = Lens.lens (\ResourceShareAssociation' {associatedEntity} -> associatedEntity) (\s@ResourceShareAssociation' {} a -> s {associatedEntity = a} :: ResourceShareAssociation)

-- | The association type.
resourceShareAssociation_associationType :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe ResourceShareAssociationType)
resourceShareAssociation_associationType = Lens.lens (\ResourceShareAssociation' {associationType} -> associationType) (\s@ResourceShareAssociation' {} a -> s {associationType = a} :: ResourceShareAssociation)

-- | A message about the status of the association.
resourceShareAssociation_statusMessage :: Lens.Lens' ResourceShareAssociation (Prelude.Maybe Prelude.Text)
resourceShareAssociation_statusMessage = Lens.lens (\ResourceShareAssociation' {statusMessage} -> statusMessage) (\s@ResourceShareAssociation' {} a -> s {statusMessage = a} :: ResourceShareAssociation)

instance Core.FromJSON ResourceShareAssociation where
  parseJSON =
    Core.withObject
      "ResourceShareAssociation"
      ( \x ->
          ResourceShareAssociation'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "resourceShareArn")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "external")
            Prelude.<*> (x Core..:? "resourceShareName")
            Prelude.<*> (x Core..:? "associatedEntity")
            Prelude.<*> (x Core..:? "associationType")
            Prelude.<*> (x Core..:? "statusMessage")
      )

instance Prelude.Hashable ResourceShareAssociation

instance Prelude.NFData ResourceShareAssociation
