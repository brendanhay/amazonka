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
-- Module      : Network.AWS.SSM.Types.OpsItemRelatedItemSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemRelatedItemSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.OpsItemIdentity

-- | Summary information about related-item resources for an OpsItem.
--
-- /See:/ 'newOpsItemRelatedItemSummary' smart constructor.
data OpsItemRelatedItemSummary = OpsItemRelatedItemSummary'
  { -- | The time the related-item association was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The OpsItem ID.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the related-item resource.
    resourceUri :: Prelude.Maybe Prelude.Text,
    -- | The association type.
    associationType :: Prelude.Maybe Prelude.Text,
    -- | The time the related-item association was last updated.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    createdBy :: Prelude.Maybe OpsItemIdentity,
    lastModifiedBy :: Prelude.Maybe OpsItemIdentity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsItemRelatedItemSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'opsItemRelatedItemSummary_createdTime' - The time the related-item association was created.
--
-- 'resourceType', 'opsItemRelatedItemSummary_resourceType' - The resource type.
--
-- 'opsItemId', 'opsItemRelatedItemSummary_opsItemId' - The OpsItem ID.
--
-- 'associationId', 'opsItemRelatedItemSummary_associationId' - The association ID.
--
-- 'resourceUri', 'opsItemRelatedItemSummary_resourceUri' - The Amazon Resource Name (ARN) of the related-item resource.
--
-- 'associationType', 'opsItemRelatedItemSummary_associationType' - The association type.
--
-- 'lastModifiedTime', 'opsItemRelatedItemSummary_lastModifiedTime' - The time the related-item association was last updated.
--
-- 'createdBy', 'opsItemRelatedItemSummary_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'opsItemRelatedItemSummary_lastModifiedBy' - Undocumented member.
newOpsItemRelatedItemSummary ::
  OpsItemRelatedItemSummary
newOpsItemRelatedItemSummary =
  OpsItemRelatedItemSummary'
    { createdTime =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      associationId = Prelude.Nothing,
      resourceUri = Prelude.Nothing,
      associationType = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing
    }

-- | The time the related-item association was created.
opsItemRelatedItemSummary_createdTime :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemRelatedItemSummary_createdTime = Lens.lens (\OpsItemRelatedItemSummary' {createdTime} -> createdTime) (\s@OpsItemRelatedItemSummary' {} a -> s {createdTime = a} :: OpsItemRelatedItemSummary) Prelude.. Lens.mapping Core._Time

-- | The resource type.
opsItemRelatedItemSummary_resourceType :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_resourceType = Lens.lens (\OpsItemRelatedItemSummary' {resourceType} -> resourceType) (\s@OpsItemRelatedItemSummary' {} a -> s {resourceType = a} :: OpsItemRelatedItemSummary)

-- | The OpsItem ID.
opsItemRelatedItemSummary_opsItemId :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_opsItemId = Lens.lens (\OpsItemRelatedItemSummary' {opsItemId} -> opsItemId) (\s@OpsItemRelatedItemSummary' {} a -> s {opsItemId = a} :: OpsItemRelatedItemSummary)

-- | The association ID.
opsItemRelatedItemSummary_associationId :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_associationId = Lens.lens (\OpsItemRelatedItemSummary' {associationId} -> associationId) (\s@OpsItemRelatedItemSummary' {} a -> s {associationId = a} :: OpsItemRelatedItemSummary)

-- | The Amazon Resource Name (ARN) of the related-item resource.
opsItemRelatedItemSummary_resourceUri :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_resourceUri = Lens.lens (\OpsItemRelatedItemSummary' {resourceUri} -> resourceUri) (\s@OpsItemRelatedItemSummary' {} a -> s {resourceUri = a} :: OpsItemRelatedItemSummary)

-- | The association type.
opsItemRelatedItemSummary_associationType :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_associationType = Lens.lens (\OpsItemRelatedItemSummary' {associationType} -> associationType) (\s@OpsItemRelatedItemSummary' {} a -> s {associationType = a} :: OpsItemRelatedItemSummary)

-- | The time the related-item association was last updated.
opsItemRelatedItemSummary_lastModifiedTime :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemRelatedItemSummary_lastModifiedTime = Lens.lens (\OpsItemRelatedItemSummary' {lastModifiedTime} -> lastModifiedTime) (\s@OpsItemRelatedItemSummary' {} a -> s {lastModifiedTime = a} :: OpsItemRelatedItemSummary) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
opsItemRelatedItemSummary_createdBy :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe OpsItemIdentity)
opsItemRelatedItemSummary_createdBy = Lens.lens (\OpsItemRelatedItemSummary' {createdBy} -> createdBy) (\s@OpsItemRelatedItemSummary' {} a -> s {createdBy = a} :: OpsItemRelatedItemSummary)

-- | Undocumented member.
opsItemRelatedItemSummary_lastModifiedBy :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe OpsItemIdentity)
opsItemRelatedItemSummary_lastModifiedBy = Lens.lens (\OpsItemRelatedItemSummary' {lastModifiedBy} -> lastModifiedBy) (\s@OpsItemRelatedItemSummary' {} a -> s {lastModifiedBy = a} :: OpsItemRelatedItemSummary)

instance Core.FromJSON OpsItemRelatedItemSummary where
  parseJSON =
    Core.withObject
      "OpsItemRelatedItemSummary"
      ( \x ->
          OpsItemRelatedItemSummary'
            Prelude.<$> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "OpsItemId")
            Prelude.<*> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "ResourceUri")
            Prelude.<*> (x Core..:? "AssociationType")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "LastModifiedBy")
      )

instance Prelude.Hashable OpsItemRelatedItemSummary

instance Prelude.NFData OpsItemRelatedItemSummary
