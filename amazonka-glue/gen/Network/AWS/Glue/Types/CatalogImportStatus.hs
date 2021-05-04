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
-- Module      : Network.AWS.Glue.Types.CatalogImportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogImportStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure containing migration status information.
--
-- /See:/ 'newCatalogImportStatus' smart constructor.
data CatalogImportStatus = CatalogImportStatus'
  { -- | The name of the person who initiated the migration.
    importedBy :: Prelude.Maybe Prelude.Text,
    -- | @True@ if the migration has completed, or @False@ otherwise.
    importCompleted :: Prelude.Maybe Prelude.Bool,
    -- | The time that the migration was started.
    importTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CatalogImportStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importedBy', 'catalogImportStatus_importedBy' - The name of the person who initiated the migration.
--
-- 'importCompleted', 'catalogImportStatus_importCompleted' - @True@ if the migration has completed, or @False@ otherwise.
--
-- 'importTime', 'catalogImportStatus_importTime' - The time that the migration was started.
newCatalogImportStatus ::
  CatalogImportStatus
newCatalogImportStatus =
  CatalogImportStatus'
    { importedBy = Prelude.Nothing,
      importCompleted = Prelude.Nothing,
      importTime = Prelude.Nothing
    }

-- | The name of the person who initiated the migration.
catalogImportStatus_importedBy :: Lens.Lens' CatalogImportStatus (Prelude.Maybe Prelude.Text)
catalogImportStatus_importedBy = Lens.lens (\CatalogImportStatus' {importedBy} -> importedBy) (\s@CatalogImportStatus' {} a -> s {importedBy = a} :: CatalogImportStatus)

-- | @True@ if the migration has completed, or @False@ otherwise.
catalogImportStatus_importCompleted :: Lens.Lens' CatalogImportStatus (Prelude.Maybe Prelude.Bool)
catalogImportStatus_importCompleted = Lens.lens (\CatalogImportStatus' {importCompleted} -> importCompleted) (\s@CatalogImportStatus' {} a -> s {importCompleted = a} :: CatalogImportStatus)

-- | The time that the migration was started.
catalogImportStatus_importTime :: Lens.Lens' CatalogImportStatus (Prelude.Maybe Prelude.UTCTime)
catalogImportStatus_importTime = Lens.lens (\CatalogImportStatus' {importTime} -> importTime) (\s@CatalogImportStatus' {} a -> s {importTime = a} :: CatalogImportStatus) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON CatalogImportStatus where
  parseJSON =
    Prelude.withObject
      "CatalogImportStatus"
      ( \x ->
          CatalogImportStatus'
            Prelude.<$> (x Prelude..:? "ImportedBy")
            Prelude.<*> (x Prelude..:? "ImportCompleted")
            Prelude.<*> (x Prelude..:? "ImportTime")
      )

instance Prelude.Hashable CatalogImportStatus

instance Prelude.NFData CatalogImportStatus
