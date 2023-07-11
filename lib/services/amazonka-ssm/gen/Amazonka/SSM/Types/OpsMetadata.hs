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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Operational metadata for an application in Application Manager.
--
-- /See:/ 'newOpsMetadata' smart constructor.
data OpsMetadata = OpsMetadata'
  { -- | The date the OpsMetadata objects was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The date the OpsMetadata object was last updated.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The user name who last updated the OpsMetadata object.
    lastModifiedUser :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
    opsMetadataArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Application Manager application.
    resourceId :: Prelude.Maybe Prelude.Text
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
-- 'creationDate', 'opsMetadata_creationDate' - The date the OpsMetadata objects was created.
--
-- 'lastModifiedDate', 'opsMetadata_lastModifiedDate' - The date the OpsMetadata object was last updated.
--
-- 'lastModifiedUser', 'opsMetadata_lastModifiedUser' - The user name who last updated the OpsMetadata object.
--
-- 'opsMetadataArn', 'opsMetadata_opsMetadataArn' - The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
--
-- 'resourceId', 'opsMetadata_resourceId' - The ID of the Application Manager application.
newOpsMetadata ::
  OpsMetadata
newOpsMetadata =
  OpsMetadata'
    { creationDate = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      lastModifiedUser = Prelude.Nothing,
      opsMetadataArn = Prelude.Nothing,
      resourceId = Prelude.Nothing
    }

-- | The date the OpsMetadata objects was created.
opsMetadata_creationDate :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.UTCTime)
opsMetadata_creationDate = Lens.lens (\OpsMetadata' {creationDate} -> creationDate) (\s@OpsMetadata' {} a -> s {creationDate = a} :: OpsMetadata) Prelude.. Lens.mapping Data._Time

-- | The date the OpsMetadata object was last updated.
opsMetadata_lastModifiedDate :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.UTCTime)
opsMetadata_lastModifiedDate = Lens.lens (\OpsMetadata' {lastModifiedDate} -> lastModifiedDate) (\s@OpsMetadata' {} a -> s {lastModifiedDate = a} :: OpsMetadata) Prelude.. Lens.mapping Data._Time

-- | The user name who last updated the OpsMetadata object.
opsMetadata_lastModifiedUser :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.Text)
opsMetadata_lastModifiedUser = Lens.lens (\OpsMetadata' {lastModifiedUser} -> lastModifiedUser) (\s@OpsMetadata' {} a -> s {lastModifiedUser = a} :: OpsMetadata)

-- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
opsMetadata_opsMetadataArn :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.Text)
opsMetadata_opsMetadataArn = Lens.lens (\OpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@OpsMetadata' {} a -> s {opsMetadataArn = a} :: OpsMetadata)

-- | The ID of the Application Manager application.
opsMetadata_resourceId :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.Text)
opsMetadata_resourceId = Lens.lens (\OpsMetadata' {resourceId} -> resourceId) (\s@OpsMetadata' {} a -> s {resourceId = a} :: OpsMetadata)

instance Data.FromJSON OpsMetadata where
  parseJSON =
    Data.withObject
      "OpsMetadata"
      ( \x ->
          OpsMetadata'
            Prelude.<$> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "LastModifiedUser")
            Prelude.<*> (x Data..:? "OpsMetadataArn")
            Prelude.<*> (x Data..:? "ResourceId")
      )

instance Prelude.Hashable OpsMetadata where
  hashWithSalt _salt OpsMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` lastModifiedUser
      `Prelude.hashWithSalt` opsMetadataArn
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData OpsMetadata where
  rnf OpsMetadata' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf lastModifiedUser
      `Prelude.seq` Prelude.rnf opsMetadataArn
      `Prelude.seq` Prelude.rnf resourceId
