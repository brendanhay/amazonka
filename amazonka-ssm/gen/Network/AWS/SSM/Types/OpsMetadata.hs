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
-- Module      : Network.AWS.SSM.Types.OpsMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Operational metadata for an application in Application Manager.
--
-- /See:/ 'newOpsMetadata' smart constructor.
data OpsMetadata = OpsMetadata'
  { -- | The ID of the Application Manager application.
    resourceId :: Core.Maybe Core.Text,
    -- | The date the OpsMetadata object was last updated.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
    opsMetadataArn :: Core.Maybe Core.Text,
    -- | The date the OpsMetadata objects was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The user name who last updated the OpsMetadata object.
    lastModifiedUser :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'opsMetadata_resourceId' - The ID of the Application Manager application.
--
-- 'lastModifiedDate', 'opsMetadata_lastModifiedDate' - The date the OpsMetadata object was last updated.
--
-- 'opsMetadataArn', 'opsMetadata_opsMetadataArn' - The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
--
-- 'creationDate', 'opsMetadata_creationDate' - The date the OpsMetadata objects was created.
--
-- 'lastModifiedUser', 'opsMetadata_lastModifiedUser' - The user name who last updated the OpsMetadata object.
newOpsMetadata ::
  OpsMetadata
newOpsMetadata =
  OpsMetadata'
    { resourceId = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      opsMetadataArn = Core.Nothing,
      creationDate = Core.Nothing,
      lastModifiedUser = Core.Nothing
    }

-- | The ID of the Application Manager application.
opsMetadata_resourceId :: Lens.Lens' OpsMetadata (Core.Maybe Core.Text)
opsMetadata_resourceId = Lens.lens (\OpsMetadata' {resourceId} -> resourceId) (\s@OpsMetadata' {} a -> s {resourceId = a} :: OpsMetadata)

-- | The date the OpsMetadata object was last updated.
opsMetadata_lastModifiedDate :: Lens.Lens' OpsMetadata (Core.Maybe Core.UTCTime)
opsMetadata_lastModifiedDate = Lens.lens (\OpsMetadata' {lastModifiedDate} -> lastModifiedDate) (\s@OpsMetadata' {} a -> s {lastModifiedDate = a} :: OpsMetadata) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
opsMetadata_opsMetadataArn :: Lens.Lens' OpsMetadata (Core.Maybe Core.Text)
opsMetadata_opsMetadataArn = Lens.lens (\OpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@OpsMetadata' {} a -> s {opsMetadataArn = a} :: OpsMetadata)

-- | The date the OpsMetadata objects was created.
opsMetadata_creationDate :: Lens.Lens' OpsMetadata (Core.Maybe Core.UTCTime)
opsMetadata_creationDate = Lens.lens (\OpsMetadata' {creationDate} -> creationDate) (\s@OpsMetadata' {} a -> s {creationDate = a} :: OpsMetadata) Core.. Lens.mapping Core._Time

-- | The user name who last updated the OpsMetadata object.
opsMetadata_lastModifiedUser :: Lens.Lens' OpsMetadata (Core.Maybe Core.Text)
opsMetadata_lastModifiedUser = Lens.lens (\OpsMetadata' {lastModifiedUser} -> lastModifiedUser) (\s@OpsMetadata' {} a -> s {lastModifiedUser = a} :: OpsMetadata)

instance Core.FromJSON OpsMetadata where
  parseJSON =
    Core.withObject
      "OpsMetadata"
      ( \x ->
          OpsMetadata'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "OpsMetadataArn")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "LastModifiedUser")
      )

instance Core.Hashable OpsMetadata

instance Core.NFData OpsMetadata
