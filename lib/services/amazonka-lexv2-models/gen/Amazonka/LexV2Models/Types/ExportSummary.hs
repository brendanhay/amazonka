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
-- Module      : Amazonka.LexV2Models.Types.ExportSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ExportSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ExportResourceSpecification
import Amazonka.LexV2Models.Types.ExportStatus
import Amazonka.LexV2Models.Types.ImportExportFileFormat
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about an export in an export list.
--
-- /See:/ 'newExportSummary' smart constructor.
data ExportSummary = ExportSummary'
  { -- | The date and time that the export was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier that Amazon Lex assigned to the export.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The status of the export. When the status is @Completed@ the export is
    -- ready to download.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | The file format used in the export files.
    fileFormat :: Prelude.Maybe ImportExportFileFormat,
    -- | The date and time that the export was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Information about the bot or bot locale that was exported.
    resourceSpecification :: Prelude.Maybe ExportResourceSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'exportSummary_creationDateTime' - The date and time that the export was created.
--
-- 'exportId', 'exportSummary_exportId' - The unique identifier that Amazon Lex assigned to the export.
--
-- 'exportStatus', 'exportSummary_exportStatus' - The status of the export. When the status is @Completed@ the export is
-- ready to download.
--
-- 'fileFormat', 'exportSummary_fileFormat' - The file format used in the export files.
--
-- 'lastUpdatedDateTime', 'exportSummary_lastUpdatedDateTime' - The date and time that the export was last updated.
--
-- 'resourceSpecification', 'exportSummary_resourceSpecification' - Information about the bot or bot locale that was exported.
newExportSummary ::
  ExportSummary
newExportSummary =
  ExportSummary'
    { creationDateTime = Prelude.Nothing,
      exportId = Prelude.Nothing,
      exportStatus = Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      resourceSpecification = Prelude.Nothing
    }

-- | The date and time that the export was created.
exportSummary_creationDateTime :: Lens.Lens' ExportSummary (Prelude.Maybe Prelude.UTCTime)
exportSummary_creationDateTime = Lens.lens (\ExportSummary' {creationDateTime} -> creationDateTime) (\s@ExportSummary' {} a -> s {creationDateTime = a} :: ExportSummary) Prelude.. Lens.mapping Data._Time

-- | The unique identifier that Amazon Lex assigned to the export.
exportSummary_exportId :: Lens.Lens' ExportSummary (Prelude.Maybe Prelude.Text)
exportSummary_exportId = Lens.lens (\ExportSummary' {exportId} -> exportId) (\s@ExportSummary' {} a -> s {exportId = a} :: ExportSummary)

-- | The status of the export. When the status is @Completed@ the export is
-- ready to download.
exportSummary_exportStatus :: Lens.Lens' ExportSummary (Prelude.Maybe ExportStatus)
exportSummary_exportStatus = Lens.lens (\ExportSummary' {exportStatus} -> exportStatus) (\s@ExportSummary' {} a -> s {exportStatus = a} :: ExportSummary)

-- | The file format used in the export files.
exportSummary_fileFormat :: Lens.Lens' ExportSummary (Prelude.Maybe ImportExportFileFormat)
exportSummary_fileFormat = Lens.lens (\ExportSummary' {fileFormat} -> fileFormat) (\s@ExportSummary' {} a -> s {fileFormat = a} :: ExportSummary)

-- | The date and time that the export was last updated.
exportSummary_lastUpdatedDateTime :: Lens.Lens' ExportSummary (Prelude.Maybe Prelude.UTCTime)
exportSummary_lastUpdatedDateTime = Lens.lens (\ExportSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@ExportSummary' {} a -> s {lastUpdatedDateTime = a} :: ExportSummary) Prelude.. Lens.mapping Data._Time

-- | Information about the bot or bot locale that was exported.
exportSummary_resourceSpecification :: Lens.Lens' ExportSummary (Prelude.Maybe ExportResourceSpecification)
exportSummary_resourceSpecification = Lens.lens (\ExportSummary' {resourceSpecification} -> resourceSpecification) (\s@ExportSummary' {} a -> s {resourceSpecification = a} :: ExportSummary)

instance Data.FromJSON ExportSummary where
  parseJSON =
    Data.withObject
      "ExportSummary"
      ( \x ->
          ExportSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "exportId")
            Prelude.<*> (x Data..:? "exportStatus")
            Prelude.<*> (x Data..:? "fileFormat")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "resourceSpecification")
      )

instance Prelude.Hashable ExportSummary where
  hashWithSalt _salt ExportSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` exportId
      `Prelude.hashWithSalt` exportStatus
      `Prelude.hashWithSalt` fileFormat
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` resourceSpecification

instance Prelude.NFData ExportSummary where
  rnf ExportSummary' {..} =
    Prelude.rnf creationDateTime `Prelude.seq`
      Prelude.rnf exportId `Prelude.seq`
        Prelude.rnf exportStatus `Prelude.seq`
          Prelude.rnf fileFormat `Prelude.seq`
            Prelude.rnf lastUpdatedDateTime `Prelude.seq`
              Prelude.rnf resourceSpecification
