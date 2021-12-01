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
-- Module      : Amazonka.SSM.Types.OpsMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Operational metadata for an application in Application Manager.
--
-- /See:/ 'newOpsMetadata' smart constructor.
data OpsMetadata = OpsMetadata'
  { -- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
    opsMetadataArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Application Manager application.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The date the OpsMetadata object was last updated.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The user name who last updated the OpsMetadata object.
    lastModifiedUser :: Prelude.Maybe Prelude.Text,
    -- | The date the OpsMetadata objects was created.
    creationDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsMetadataArn', 'opsMetadata_opsMetadataArn' - The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
--
-- 'resourceId', 'opsMetadata_resourceId' - The ID of the Application Manager application.
--
-- 'lastModifiedDate', 'opsMetadata_lastModifiedDate' - The date the OpsMetadata object was last updated.
--
-- 'lastModifiedUser', 'opsMetadata_lastModifiedUser' - The user name who last updated the OpsMetadata object.
--
-- 'creationDate', 'opsMetadata_creationDate' - The date the OpsMetadata objects was created.
newOpsMetadata ::
  OpsMetadata
newOpsMetadata =
  OpsMetadata'
    { opsMetadataArn = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      lastModifiedUser = Prelude.Nothing,
      creationDate = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
opsMetadata_opsMetadataArn :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.Text)
opsMetadata_opsMetadataArn = Lens.lens (\OpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@OpsMetadata' {} a -> s {opsMetadataArn = a} :: OpsMetadata)

-- | The ID of the Application Manager application.
opsMetadata_resourceId :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.Text)
opsMetadata_resourceId = Lens.lens (\OpsMetadata' {resourceId} -> resourceId) (\s@OpsMetadata' {} a -> s {resourceId = a} :: OpsMetadata)

-- | The date the OpsMetadata object was last updated.
opsMetadata_lastModifiedDate :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.UTCTime)
opsMetadata_lastModifiedDate = Lens.lens (\OpsMetadata' {lastModifiedDate} -> lastModifiedDate) (\s@OpsMetadata' {} a -> s {lastModifiedDate = a} :: OpsMetadata) Prelude.. Lens.mapping Core._Time

-- | The user name who last updated the OpsMetadata object.
opsMetadata_lastModifiedUser :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.Text)
opsMetadata_lastModifiedUser = Lens.lens (\OpsMetadata' {lastModifiedUser} -> lastModifiedUser) (\s@OpsMetadata' {} a -> s {lastModifiedUser = a} :: OpsMetadata)

-- | The date the OpsMetadata objects was created.
opsMetadata_creationDate :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.UTCTime)
opsMetadata_creationDate = Lens.lens (\OpsMetadata' {creationDate} -> creationDate) (\s@OpsMetadata' {} a -> s {creationDate = a} :: OpsMetadata) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON OpsMetadata where
  parseJSON =
    Core.withObject
      "OpsMetadata"
      ( \x ->
          OpsMetadata'
            Prelude.<$> (x Core..:? "OpsMetadataArn")
            Prelude.<*> (x Core..:? "ResourceId")
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "LastModifiedUser")
            Prelude.<*> (x Core..:? "CreationDate")
      )

instance Prelude.Hashable OpsMetadata where
  hashWithSalt salt' OpsMetadata' {..} =
    salt' `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastModifiedUser
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` opsMetadataArn

instance Prelude.NFData OpsMetadata where
  rnf OpsMetadata' {..} =
    Prelude.rnf opsMetadataArn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastModifiedUser
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf resourceId
