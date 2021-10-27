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
-- Module      : Network.AWS.LexV2Models.Types.ExportSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.ExportSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.ExportResourceSpecification
import Network.AWS.LexV2Models.Types.ExportStatus
import Network.AWS.LexV2Models.Types.ImportExportFileFormat
import qualified Network.AWS.Prelude as Prelude

-- | Provides summary information about an export in an export list.
--
-- /See:/ 'newExportSummary' smart constructor.
data ExportSummary = ExportSummary'
  { -- | Information about the bot or bot locale that was exported.
    resourceSpecification :: Prelude.Maybe ExportResourceSpecification,
    -- | The file format used in the export files.
    fileFormat :: Prelude.Maybe ImportExportFileFormat,
    -- | The status of the export. When the status is @Completed@ the export is
    -- ready to download.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | The date and time that the export was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the export was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier that Amazon Lex assigned to the export.
    exportId :: Prelude.Maybe Prelude.Text
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
-- 'resourceSpecification', 'exportSummary_resourceSpecification' - Information about the bot or bot locale that was exported.
--
-- 'fileFormat', 'exportSummary_fileFormat' - The file format used in the export files.
--
-- 'exportStatus', 'exportSummary_exportStatus' - The status of the export. When the status is @Completed@ the export is
-- ready to download.
--
-- 'lastUpdatedDateTime', 'exportSummary_lastUpdatedDateTime' - The date and time that the export was last updated.
--
-- 'creationDateTime', 'exportSummary_creationDateTime' - The date and time that the export was created.
--
-- 'exportId', 'exportSummary_exportId' - The unique identifier that Amazon Lex assigned to the export.
newExportSummary ::
  ExportSummary
newExportSummary =
  ExportSummary'
    { resourceSpecification =
        Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      exportStatus = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      exportId = Prelude.Nothing
    }

-- | Information about the bot or bot locale that was exported.
exportSummary_resourceSpecification :: Lens.Lens' ExportSummary (Prelude.Maybe ExportResourceSpecification)
exportSummary_resourceSpecification = Lens.lens (\ExportSummary' {resourceSpecification} -> resourceSpecification) (\s@ExportSummary' {} a -> s {resourceSpecification = a} :: ExportSummary)

-- | The file format used in the export files.
exportSummary_fileFormat :: Lens.Lens' ExportSummary (Prelude.Maybe ImportExportFileFormat)
exportSummary_fileFormat = Lens.lens (\ExportSummary' {fileFormat} -> fileFormat) (\s@ExportSummary' {} a -> s {fileFormat = a} :: ExportSummary)

-- | The status of the export. When the status is @Completed@ the export is
-- ready to download.
exportSummary_exportStatus :: Lens.Lens' ExportSummary (Prelude.Maybe ExportStatus)
exportSummary_exportStatus = Lens.lens (\ExportSummary' {exportStatus} -> exportStatus) (\s@ExportSummary' {} a -> s {exportStatus = a} :: ExportSummary)

-- | The date and time that the export was last updated.
exportSummary_lastUpdatedDateTime :: Lens.Lens' ExportSummary (Prelude.Maybe Prelude.UTCTime)
exportSummary_lastUpdatedDateTime = Lens.lens (\ExportSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@ExportSummary' {} a -> s {lastUpdatedDateTime = a} :: ExportSummary) Prelude.. Lens.mapping Core._Time

-- | The date and time that the export was created.
exportSummary_creationDateTime :: Lens.Lens' ExportSummary (Prelude.Maybe Prelude.UTCTime)
exportSummary_creationDateTime = Lens.lens (\ExportSummary' {creationDateTime} -> creationDateTime) (\s@ExportSummary' {} a -> s {creationDateTime = a} :: ExportSummary) Prelude.. Lens.mapping Core._Time

-- | The unique identifier that Amazon Lex assigned to the export.
exportSummary_exportId :: Lens.Lens' ExportSummary (Prelude.Maybe Prelude.Text)
exportSummary_exportId = Lens.lens (\ExportSummary' {exportId} -> exportId) (\s@ExportSummary' {} a -> s {exportId = a} :: ExportSummary)

instance Core.FromJSON ExportSummary where
  parseJSON =
    Core.withObject
      "ExportSummary"
      ( \x ->
          ExportSummary'
            Prelude.<$> (x Core..:? "resourceSpecification")
            Prelude.<*> (x Core..:? "fileFormat")
            Prelude.<*> (x Core..:? "exportStatus")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "exportId")
      )

instance Prelude.Hashable ExportSummary

instance Prelude.NFData ExportSummary
