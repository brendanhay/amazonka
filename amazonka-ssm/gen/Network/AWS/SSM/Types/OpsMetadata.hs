{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Operational metadata for an application in Application Manager.
--
-- /See:/ 'newOpsMetadata' smart constructor.
data OpsMetadata = OpsMetadata'
  { -- | The ID of the Application Manager application.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The date the OpsMetadata object was last updated.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
    opsMetadataArn :: Prelude.Maybe Prelude.Text,
    -- | The date the OpsMetadata objects was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The user name who last updated the OpsMetadata object.
    lastModifiedUser :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resourceId = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      opsMetadataArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lastModifiedUser = Prelude.Nothing
    }

-- | The ID of the Application Manager application.
opsMetadata_resourceId :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.Text)
opsMetadata_resourceId = Lens.lens (\OpsMetadata' {resourceId} -> resourceId) (\s@OpsMetadata' {} a -> s {resourceId = a} :: OpsMetadata)

-- | The date the OpsMetadata object was last updated.
opsMetadata_lastModifiedDate :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.UTCTime)
opsMetadata_lastModifiedDate = Lens.lens (\OpsMetadata' {lastModifiedDate} -> lastModifiedDate) (\s@OpsMetadata' {} a -> s {lastModifiedDate = a} :: OpsMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob.
opsMetadata_opsMetadataArn :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.Text)
opsMetadata_opsMetadataArn = Lens.lens (\OpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@OpsMetadata' {} a -> s {opsMetadataArn = a} :: OpsMetadata)

-- | The date the OpsMetadata objects was created.
opsMetadata_creationDate :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.UTCTime)
opsMetadata_creationDate = Lens.lens (\OpsMetadata' {creationDate} -> creationDate) (\s@OpsMetadata' {} a -> s {creationDate = a} :: OpsMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The user name who last updated the OpsMetadata object.
opsMetadata_lastModifiedUser :: Lens.Lens' OpsMetadata (Prelude.Maybe Prelude.Text)
opsMetadata_lastModifiedUser = Lens.lens (\OpsMetadata' {lastModifiedUser} -> lastModifiedUser) (\s@OpsMetadata' {} a -> s {lastModifiedUser = a} :: OpsMetadata)

instance Prelude.FromJSON OpsMetadata where
  parseJSON =
    Prelude.withObject
      "OpsMetadata"
      ( \x ->
          OpsMetadata'
            Prelude.<$> (x Prelude..:? "ResourceId")
            Prelude.<*> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "OpsMetadataArn")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "LastModifiedUser")
      )

instance Prelude.Hashable OpsMetadata

instance Prelude.NFData OpsMetadata
