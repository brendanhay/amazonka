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
-- Module      : Amazonka.LexV2Models.Types.ImportSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ImportSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ImportResourceType
import Amazonka.LexV2Models.Types.ImportStatus
import Amazonka.LexV2Models.Types.MergeStrategy
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about an import in an import list.
--
-- /See:/ 'newImportSummary' smart constructor.
data ImportSummary = ImportSummary'
  { -- | The date and time that the import was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier that Amazon Lex assigned to the import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The status of the resource. When the status is @Completed@ the resource
    -- is ready to build.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | The unique identifier that Amazon Lex assigned to the imported resource.
    importedResourceId :: Prelude.Maybe Prelude.Text,
    -- | The name that you gave the imported resource.
    importedResourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of resource that was imported.
    importedResourceType :: Prelude.Maybe ImportResourceType,
    -- | The date and time that the import was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The strategy used to merge existing bot or bot locale definitions with
    -- the imported definition.
    mergeStrategy :: Prelude.Maybe MergeStrategy
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
-- 'creationDateTime', 'importSummary_creationDateTime' - The date and time that the import was created.
--
-- 'importId', 'importSummary_importId' - The unique identifier that Amazon Lex assigned to the import.
--
-- 'importStatus', 'importSummary_importStatus' - The status of the resource. When the status is @Completed@ the resource
-- is ready to build.
--
-- 'importedResourceId', 'importSummary_importedResourceId' - The unique identifier that Amazon Lex assigned to the imported resource.
--
-- 'importedResourceName', 'importSummary_importedResourceName' - The name that you gave the imported resource.
--
-- 'importedResourceType', 'importSummary_importedResourceType' - The type of resource that was imported.
--
-- 'lastUpdatedDateTime', 'importSummary_lastUpdatedDateTime' - The date and time that the import was last updated.
--
-- 'mergeStrategy', 'importSummary_mergeStrategy' - The strategy used to merge existing bot or bot locale definitions with
-- the imported definition.
newImportSummary ::
  ImportSummary
newImportSummary =
  ImportSummary'
    { creationDateTime = Prelude.Nothing,
      importId = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      importedResourceId = Prelude.Nothing,
      importedResourceName = Prelude.Nothing,
      importedResourceType = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      mergeStrategy = Prelude.Nothing
    }

-- | The date and time that the import was created.
importSummary_creationDateTime :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.UTCTime)
importSummary_creationDateTime = Lens.lens (\ImportSummary' {creationDateTime} -> creationDateTime) (\s@ImportSummary' {} a -> s {creationDateTime = a} :: ImportSummary) Prelude.. Lens.mapping Data._Time

-- | The unique identifier that Amazon Lex assigned to the import.
importSummary_importId :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.Text)
importSummary_importId = Lens.lens (\ImportSummary' {importId} -> importId) (\s@ImportSummary' {} a -> s {importId = a} :: ImportSummary)

-- | The status of the resource. When the status is @Completed@ the resource
-- is ready to build.
importSummary_importStatus :: Lens.Lens' ImportSummary (Prelude.Maybe ImportStatus)
importSummary_importStatus = Lens.lens (\ImportSummary' {importStatus} -> importStatus) (\s@ImportSummary' {} a -> s {importStatus = a} :: ImportSummary)

-- | The unique identifier that Amazon Lex assigned to the imported resource.
importSummary_importedResourceId :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.Text)
importSummary_importedResourceId = Lens.lens (\ImportSummary' {importedResourceId} -> importedResourceId) (\s@ImportSummary' {} a -> s {importedResourceId = a} :: ImportSummary)

-- | The name that you gave the imported resource.
importSummary_importedResourceName :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.Text)
importSummary_importedResourceName = Lens.lens (\ImportSummary' {importedResourceName} -> importedResourceName) (\s@ImportSummary' {} a -> s {importedResourceName = a} :: ImportSummary)

-- | The type of resource that was imported.
importSummary_importedResourceType :: Lens.Lens' ImportSummary (Prelude.Maybe ImportResourceType)
importSummary_importedResourceType = Lens.lens (\ImportSummary' {importedResourceType} -> importedResourceType) (\s@ImportSummary' {} a -> s {importedResourceType = a} :: ImportSummary)

-- | The date and time that the import was last updated.
importSummary_lastUpdatedDateTime :: Lens.Lens' ImportSummary (Prelude.Maybe Prelude.UTCTime)
importSummary_lastUpdatedDateTime = Lens.lens (\ImportSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@ImportSummary' {} a -> s {lastUpdatedDateTime = a} :: ImportSummary) Prelude.. Lens.mapping Data._Time

-- | The strategy used to merge existing bot or bot locale definitions with
-- the imported definition.
importSummary_mergeStrategy :: Lens.Lens' ImportSummary (Prelude.Maybe MergeStrategy)
importSummary_mergeStrategy = Lens.lens (\ImportSummary' {mergeStrategy} -> mergeStrategy) (\s@ImportSummary' {} a -> s {mergeStrategy = a} :: ImportSummary)

instance Data.FromJSON ImportSummary where
  parseJSON =
    Data.withObject
      "ImportSummary"
      ( \x ->
          ImportSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "importId")
            Prelude.<*> (x Data..:? "importStatus")
            Prelude.<*> (x Data..:? "importedResourceId")
            Prelude.<*> (x Data..:? "importedResourceName")
            Prelude.<*> (x Data..:? "importedResourceType")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "mergeStrategy")
      )

instance Prelude.Hashable ImportSummary where
  hashWithSalt _salt ImportSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` importId
      `Prelude.hashWithSalt` importStatus
      `Prelude.hashWithSalt` importedResourceId
      `Prelude.hashWithSalt` importedResourceName
      `Prelude.hashWithSalt` importedResourceType
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` mergeStrategy

instance Prelude.NFData ImportSummary where
  rnf ImportSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf importId
      `Prelude.seq` Prelude.rnf importStatus
      `Prelude.seq` Prelude.rnf importedResourceId
      `Prelude.seq` Prelude.rnf importedResourceName
      `Prelude.seq` Prelude.rnf importedResourceType
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf mergeStrategy
