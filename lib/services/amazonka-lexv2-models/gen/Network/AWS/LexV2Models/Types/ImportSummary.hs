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
-- Module      : Network.AWS.LexV2Models.Types.ImportSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.ImportSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.ImportStatus
import Network.AWS.LexV2Models.Types.MergeStrategy
import qualified Network.AWS.Prelude as Prelude

-- | Provides summary information about an import in an import list.
--
-- /See:/ 'newImportSummary' smart constructor.
data ImportSummary = ImportSummary'
  { -- | The unique identifier that Amazon Lex assigned to the import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier that Amazon Lex assigned to the imported resource.
    importedResourceId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the import was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name that you gave the imported resource.
    importedResourceName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the import was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The strategy used to merge existing bot or bot locale definitions with
    -- the imported definition.
    mergeStrategy :: Prelude.Maybe MergeStrategy,
    -- | The status of the resource. When the status is @Completed@ the resource
    -- is ready to build.
    importStatus :: Prelude.Maybe ImportStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importId', 'importSummary_importId' - The unique identifier that Amazon Lex assigned to the import.
--
-- 'importedResourceId', 'importSummary_importedResourceId' - The unique identifier that Amazon Lex assigned to the imported resource.
--
-- 'lastUpdatedDateTime', 'importSummary_lastUpdatedDateTime' - The date and time that the import was last updated.
--
-- 'importedResourceName', 'importSummary_importedResourceName' - The name that you gave the imported resource.
--
-- 'creationDateTime', 'importSummary_creationDateTime' - The date and time that the import was created.
--
-- 'mergeStrategy', 'importSummary_mergeStrategy' - The strategy used to merge existing bot or bot locale definitions with
-- the imported definition.
--
-- 'importStatus', 'importSummary_importStatus' - The status of the resource. When the status is @Completed@ the resource
-- is ready to build.
newImportSummary ::
  ImportSummary
newImportSummary =
  ImportSummary'
    { importId = Prelude.Nothing,
      importedResourceId = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      importedResourceName = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      mergeStrategy = Prelude.Nothing,
      importStatus = Prelude.Nothing
    }

-- | The unique identifier that Amazon Lex assigned to the import.
importSummary_importId :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.Text)
importSummary_importId = Lens.lens (\ImportSummary' {importId} -> importId) (\s@ImportSummary' {} a -> s {importId = a} :: ImportSummary)

-- | The unique identifier that Amazon Lex assigned to the imported resource.
importSummary_importedResourceId :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.Text)
importSummary_importedResourceId = Lens.lens (\ImportSummary' {importedResourceId} -> importedResourceId) (\s@ImportSummary' {} a -> s {importedResourceId = a} :: ImportSummary)

-- | The date and time that the import was last updated.
importSummary_lastUpdatedDateTime :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.UTCTime)
importSummary_lastUpdatedDateTime = Lens.lens (\ImportSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@ImportSummary' {} a -> s {lastUpdatedDateTime = a} :: ImportSummary) Prelude.. Lens.mapping Core._Time

-- | The name that you gave the imported resource.
importSummary_importedResourceName :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.Text)
importSummary_importedResourceName = Lens.lens (\ImportSummary' {importedResourceName} -> importedResourceName) (\s@ImportSummary' {} a -> s {importedResourceName = a} :: ImportSummary)

-- | The date and time that the import was created.
importSummary_creationDateTime :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.UTCTime)
importSummary_creationDateTime = Lens.lens (\ImportSummary' {creationDateTime} -> creationDateTime) (\s@ImportSummary' {} a -> s {creationDateTime = a} :: ImportSummary) Prelude.. Lens.mapping Core._Time

-- | The strategy used to merge existing bot or bot locale definitions with
-- the imported definition.
importSummary_mergeStrategy :: Lens.Lens' ImportSummary (Prelude.Maybe MergeStrategy)
importSummary_mergeStrategy = Lens.lens (\ImportSummary' {mergeStrategy} -> mergeStrategy) (\s@ImportSummary' {} a -> s {mergeStrategy = a} :: ImportSummary)

-- | The status of the resource. When the status is @Completed@ the resource
-- is ready to build.
importSummary_importStatus :: Lens.Lens' ImportSummary (Prelude.Maybe ImportStatus)
importSummary_importStatus = Lens.lens (\ImportSummary' {importStatus} -> importStatus) (\s@ImportSummary' {} a -> s {importStatus = a} :: ImportSummary)

instance Core.FromJSON ImportSummary where
  parseJSON =
    Core.withObject
      "ImportSummary"
      ( \x ->
          ImportSummary'
            Prelude.<$> (x Core..:? "importId")
            Prelude.<*> (x Core..:? "importedResourceId")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "importedResourceName")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "mergeStrategy")
            Prelude.<*> (x Core..:? "importStatus")
      )

instance Prelude.Hashable ImportSummary

instance Prelude.NFData ImportSummary
