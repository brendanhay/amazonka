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
-- Module      : Network.AWS.RAM.Types.ResourceShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RAM.Types.ResourceShare where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RAM.Types.ResourceShareFeatureSet
import Network.AWS.RAM.Types.ResourceShareStatus
import Network.AWS.RAM.Types.Tag

-- | Describes a resource share.
--
-- /See:/ 'newResourceShare' smart constructor.
data ResourceShare = ResourceShare'
  { -- | The time when the resource share was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the resource share.
    status :: Prelude.Maybe ResourceShareStatus,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the resource share.
    owningAccountId :: Prelude.Maybe Prelude.Text,
    -- | The time when the resource share was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether principals outside your organization in Organizations
    -- can be associated with a resource share.
    allowExternalPrincipals :: Prelude.Maybe Prelude.Bool,
    -- | The name of the resource share.
    name :: Prelude.Maybe Prelude.Text,
    -- | A message about the status of the resource share.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Indicates how the resource share was created. Possible values include:
    --
    -- -   @CREATED_FROM_POLICY@ - Indicates that the resource share was
    --     created from an Amazon Web Services Identity and Access Management
    --     (Amazon Web Services IAM) policy attached to a resource. These
    --     resource shares are visible only to the Amazon Web Services account
    --     that created it. They cannot be modified in RAM.
    --
    -- -   @PROMOTING_TO_STANDARD@ - The resource share is in the process of
    --     being promoted. For more information, see
    --     PromoteResourceShareCreatedFromPolicy.
    --
    -- -   @STANDARD@ - Indicates that the resource share was created in RAM
    --     using the console or APIs. These resource shares are visible to all
    --     principals. They can be modified in RAM.
    featureSet :: Prelude.Maybe ResourceShareFeatureSet,
    -- | The tags for the resource share.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'resourceShare_creationTime' - The time when the resource share was created.
--
-- 'status', 'resourceShare_status' - The status of the resource share.
--
-- 'resourceShareArn', 'resourceShare_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
--
-- 'owningAccountId', 'resourceShare_owningAccountId' - The ID of the Amazon Web Services account that owns the resource share.
--
-- 'lastUpdatedTime', 'resourceShare_lastUpdatedTime' - The time when the resource share was last updated.
--
-- 'allowExternalPrincipals', 'resourceShare_allowExternalPrincipals' - Indicates whether principals outside your organization in Organizations
-- can be associated with a resource share.
--
-- 'name', 'resourceShare_name' - The name of the resource share.
--
-- 'statusMessage', 'resourceShare_statusMessage' - A message about the status of the resource share.
--
-- 'featureSet', 'resourceShare_featureSet' - Indicates how the resource share was created. Possible values include:
--
-- -   @CREATED_FROM_POLICY@ - Indicates that the resource share was
--     created from an Amazon Web Services Identity and Access Management
--     (Amazon Web Services IAM) policy attached to a resource. These
--     resource shares are visible only to the Amazon Web Services account
--     that created it. They cannot be modified in RAM.
--
-- -   @PROMOTING_TO_STANDARD@ - The resource share is in the process of
--     being promoted. For more information, see
--     PromoteResourceShareCreatedFromPolicy.
--
-- -   @STANDARD@ - Indicates that the resource share was created in RAM
--     using the console or APIs. These resource shares are visible to all
--     principals. They can be modified in RAM.
--
-- 'tags', 'resourceShare_tags' - The tags for the resource share.
newResourceShare ::
  ResourceShare
newResourceShare =
  ResourceShare'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing,
      owningAccountId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      allowExternalPrincipals = Prelude.Nothing,
      name = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      featureSet = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The time when the resource share was created.
resourceShare_creationTime :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.UTCTime)
resourceShare_creationTime = Lens.lens (\ResourceShare' {creationTime} -> creationTime) (\s@ResourceShare' {} a -> s {creationTime = a} :: ResourceShare) Prelude.. Lens.mapping Core._Time

-- | The status of the resource share.
resourceShare_status :: Lens.Lens' ResourceShare (Prelude.Maybe ResourceShareStatus)
resourceShare_status = Lens.lens (\ResourceShare' {status} -> status) (\s@ResourceShare' {} a -> s {status = a} :: ResourceShare)

-- | The Amazon Resource Name (ARN) of the resource share.
resourceShare_resourceShareArn :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Text)
resourceShare_resourceShareArn = Lens.lens (\ResourceShare' {resourceShareArn} -> resourceShareArn) (\s@ResourceShare' {} a -> s {resourceShareArn = a} :: ResourceShare)

-- | The ID of the Amazon Web Services account that owns the resource share.
resourceShare_owningAccountId :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Text)
resourceShare_owningAccountId = Lens.lens (\ResourceShare' {owningAccountId} -> owningAccountId) (\s@ResourceShare' {} a -> s {owningAccountId = a} :: ResourceShare)

-- | The time when the resource share was last updated.
resourceShare_lastUpdatedTime :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.UTCTime)
resourceShare_lastUpdatedTime = Lens.lens (\ResourceShare' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourceShare' {} a -> s {lastUpdatedTime = a} :: ResourceShare) Prelude.. Lens.mapping Core._Time

-- | Indicates whether principals outside your organization in Organizations
-- can be associated with a resource share.
resourceShare_allowExternalPrincipals :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Bool)
resourceShare_allowExternalPrincipals = Lens.lens (\ResourceShare' {allowExternalPrincipals} -> allowExternalPrincipals) (\s@ResourceShare' {} a -> s {allowExternalPrincipals = a} :: ResourceShare)

-- | The name of the resource share.
resourceShare_name :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Text)
resourceShare_name = Lens.lens (\ResourceShare' {name} -> name) (\s@ResourceShare' {} a -> s {name = a} :: ResourceShare)

-- | A message about the status of the resource share.
resourceShare_statusMessage :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Text)
resourceShare_statusMessage = Lens.lens (\ResourceShare' {statusMessage} -> statusMessage) (\s@ResourceShare' {} a -> s {statusMessage = a} :: ResourceShare)

-- | Indicates how the resource share was created. Possible values include:
--
-- -   @CREATED_FROM_POLICY@ - Indicates that the resource share was
--     created from an Amazon Web Services Identity and Access Management
--     (Amazon Web Services IAM) policy attached to a resource. These
--     resource shares are visible only to the Amazon Web Services account
--     that created it. They cannot be modified in RAM.
--
-- -   @PROMOTING_TO_STANDARD@ - The resource share is in the process of
--     being promoted. For more information, see
--     PromoteResourceShareCreatedFromPolicy.
--
-- -   @STANDARD@ - Indicates that the resource share was created in RAM
--     using the console or APIs. These resource shares are visible to all
--     principals. They can be modified in RAM.
resourceShare_featureSet :: Lens.Lens' ResourceShare (Prelude.Maybe ResourceShareFeatureSet)
resourceShare_featureSet = Lens.lens (\ResourceShare' {featureSet} -> featureSet) (\s@ResourceShare' {} a -> s {featureSet = a} :: ResourceShare)

-- | The tags for the resource share.
resourceShare_tags :: Lens.Lens' ResourceShare (Prelude.Maybe [Tag])
resourceShare_tags = Lens.lens (\ResourceShare' {tags} -> tags) (\s@ResourceShare' {} a -> s {tags = a} :: ResourceShare) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ResourceShare where
  parseJSON =
    Core.withObject
      "ResourceShare"
      ( \x ->
          ResourceShare'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "resourceShareArn")
            Prelude.<*> (x Core..:? "owningAccountId")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "allowExternalPrincipals")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "featureSet")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceShare

instance Prelude.NFData ResourceShare
