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
-- Module      : Amazonka.Glue.Types.CatalogImportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogImportStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing migration status information.
--
-- /See:/ 'newCatalogImportStatus' smart constructor.
data CatalogImportStatus = CatalogImportStatus'
  { -- | @True@ if the migration has completed, or @False@ otherwise.
    importCompleted :: Prelude.Maybe Prelude.Bool,
    -- | The time that the migration was started.
    importTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the person who initiated the migration.
    importedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogImportStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importCompleted', 'catalogImportStatus_importCompleted' - @True@ if the migration has completed, or @False@ otherwise.
--
-- 'importTime', 'catalogImportStatus_importTime' - The time that the migration was started.
--
-- 'importedBy', 'catalogImportStatus_importedBy' - The name of the person who initiated the migration.
newCatalogImportStatus ::
  CatalogImportStatus
newCatalogImportStatus =
  CatalogImportStatus'
    { importCompleted =
        Prelude.Nothing,
      importTime = Prelude.Nothing,
      importedBy = Prelude.Nothing
    }

-- | @True@ if the migration has completed, or @False@ otherwise.
catalogImportStatus_importCompleted :: Lens.Lens' CatalogImportStatus (Prelude.Maybe Prelude.Bool)
catalogImportStatus_importCompleted = Lens.lens (\CatalogImportStatus' {importCompleted} -> importCompleted) (\s@CatalogImportStatus' {} a -> s {importCompleted = a} :: CatalogImportStatus)

-- | The time that the migration was started.
catalogImportStatus_importTime :: Lens.Lens' CatalogImportStatus (Prelude.Maybe Prelude.UTCTime)
catalogImportStatus_importTime = Lens.lens (\CatalogImportStatus' {importTime} -> importTime) (\s@CatalogImportStatus' {} a -> s {importTime = a} :: CatalogImportStatus) Prelude.. Lens.mapping Data._Time

-- | The name of the person who initiated the migration.
catalogImportStatus_importedBy :: Lens.Lens' CatalogImportStatus (Prelude.Maybe Prelude.Text)
catalogImportStatus_importedBy = Lens.lens (\CatalogImportStatus' {importedBy} -> importedBy) (\s@CatalogImportStatus' {} a -> s {importedBy = a} :: CatalogImportStatus)

instance Data.FromJSON CatalogImportStatus where
  parseJSON =
    Data.withObject
      "CatalogImportStatus"
      ( \x ->
          CatalogImportStatus'
            Prelude.<$> (x Data..:? "ImportCompleted")
            Prelude.<*> (x Data..:? "ImportTime")
            Prelude.<*> (x Data..:? "ImportedBy")
      )

instance Prelude.Hashable CatalogImportStatus where
  hashWithSalt _salt CatalogImportStatus' {..} =
    _salt
      `Prelude.hashWithSalt` importCompleted
      `Prelude.hashWithSalt` importTime
      `Prelude.hashWithSalt` importedBy

instance Prelude.NFData CatalogImportStatus where
  rnf CatalogImportStatus' {..} =
    Prelude.rnf importCompleted
      `Prelude.seq` Prelude.rnf importTime
      `Prelude.seq` Prelude.rnf importedBy
