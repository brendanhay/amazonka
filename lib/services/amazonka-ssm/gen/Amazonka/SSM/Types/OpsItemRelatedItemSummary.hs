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
-- Module      : Amazonka.SSM.Types.OpsItemRelatedItemSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemRelatedItemSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OpsItemIdentity

-- | Summary information about related-item resources for an OpsItem.
--
-- /See:/ 'newOpsItemRelatedItemSummary' smart constructor.
data OpsItemRelatedItemSummary = OpsItemRelatedItemSummary'
  { -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The association type.
    associationType :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe OpsItemIdentity,
    -- | The time the related-item association was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    lastModifiedBy :: Prelude.Maybe OpsItemIdentity,
    -- | The time the related-item association was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The OpsItem ID.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the related-item resource.
    resourceUri :: Prelude.Maybe Prelude.Text
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
-- 'associationId', 'opsItemRelatedItemSummary_associationId' - The association ID.
--
-- 'associationType', 'opsItemRelatedItemSummary_associationType' - The association type.
--
-- 'createdBy', 'opsItemRelatedItemSummary_createdBy' - Undocumented member.
--
-- 'createdTime', 'opsItemRelatedItemSummary_createdTime' - The time the related-item association was created.
--
-- 'lastModifiedBy', 'opsItemRelatedItemSummary_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'opsItemRelatedItemSummary_lastModifiedTime' - The time the related-item association was last updated.
--
-- 'opsItemId', 'opsItemRelatedItemSummary_opsItemId' - The OpsItem ID.
--
-- 'resourceType', 'opsItemRelatedItemSummary_resourceType' - The resource type.
--
-- 'resourceUri', 'opsItemRelatedItemSummary_resourceUri' - The Amazon Resource Name (ARN) of the related-item resource.
newOpsItemRelatedItemSummary ::
  OpsItemRelatedItemSummary
newOpsItemRelatedItemSummary =
  OpsItemRelatedItemSummary'
    { associationId =
        Prelude.Nothing,
      associationType = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      opsItemId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceUri = Prelude.Nothing
    }

-- | The association ID.
opsItemRelatedItemSummary_associationId :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_associationId = Lens.lens (\OpsItemRelatedItemSummary' {associationId} -> associationId) (\s@OpsItemRelatedItemSummary' {} a -> s {associationId = a} :: OpsItemRelatedItemSummary)

-- | The association type.
opsItemRelatedItemSummary_associationType :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_associationType = Lens.lens (\OpsItemRelatedItemSummary' {associationType} -> associationType) (\s@OpsItemRelatedItemSummary' {} a -> s {associationType = a} :: OpsItemRelatedItemSummary)

-- | Undocumented member.
opsItemRelatedItemSummary_createdBy :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe OpsItemIdentity)
opsItemRelatedItemSummary_createdBy = Lens.lens (\OpsItemRelatedItemSummary' {createdBy} -> createdBy) (\s@OpsItemRelatedItemSummary' {} a -> s {createdBy = a} :: OpsItemRelatedItemSummary)

-- | The time the related-item association was created.
opsItemRelatedItemSummary_createdTime :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemRelatedItemSummary_createdTime = Lens.lens (\OpsItemRelatedItemSummary' {createdTime} -> createdTime) (\s@OpsItemRelatedItemSummary' {} a -> s {createdTime = a} :: OpsItemRelatedItemSummary) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
opsItemRelatedItemSummary_lastModifiedBy :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe OpsItemIdentity)
opsItemRelatedItemSummary_lastModifiedBy = Lens.lens (\OpsItemRelatedItemSummary' {lastModifiedBy} -> lastModifiedBy) (\s@OpsItemRelatedItemSummary' {} a -> s {lastModifiedBy = a} :: OpsItemRelatedItemSummary)

-- | The time the related-item association was last updated.
opsItemRelatedItemSummary_lastModifiedTime :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.UTCTime)
opsItemRelatedItemSummary_lastModifiedTime = Lens.lens (\OpsItemRelatedItemSummary' {lastModifiedTime} -> lastModifiedTime) (\s@OpsItemRelatedItemSummary' {} a -> s {lastModifiedTime = a} :: OpsItemRelatedItemSummary) Prelude.. Lens.mapping Data._Time

-- | The OpsItem ID.
opsItemRelatedItemSummary_opsItemId :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_opsItemId = Lens.lens (\OpsItemRelatedItemSummary' {opsItemId} -> opsItemId) (\s@OpsItemRelatedItemSummary' {} a -> s {opsItemId = a} :: OpsItemRelatedItemSummary)

-- | The resource type.
opsItemRelatedItemSummary_resourceType :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_resourceType = Lens.lens (\OpsItemRelatedItemSummary' {resourceType} -> resourceType) (\s@OpsItemRelatedItemSummary' {} a -> s {resourceType = a} :: OpsItemRelatedItemSummary)

-- | The Amazon Resource Name (ARN) of the related-item resource.
opsItemRelatedItemSummary_resourceUri :: Lens.Lens' OpsItemRelatedItemSummary (Prelude.Maybe Prelude.Text)
opsItemRelatedItemSummary_resourceUri = Lens.lens (\OpsItemRelatedItemSummary' {resourceUri} -> resourceUri) (\s@OpsItemRelatedItemSummary' {} a -> s {resourceUri = a} :: OpsItemRelatedItemSummary)

instance Data.FromJSON OpsItemRelatedItemSummary where
  parseJSON =
    Data.withObject
      "OpsItemRelatedItemSummary"
      ( \x ->
          OpsItemRelatedItemSummary'
            Prelude.<$> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "AssociationType")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "OpsItemId")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "ResourceUri")
      )

instance Prelude.Hashable OpsItemRelatedItemSummary where
  hashWithSalt _salt OpsItemRelatedItemSummary' {..} =
    _salt `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` associationType
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` opsItemId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceUri

instance Prelude.NFData OpsItemRelatedItemSummary where
  rnf OpsItemRelatedItemSummary' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf associationType
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf opsItemId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceUri
