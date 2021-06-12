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
-- Module      : Network.AWS.Translate.Types.ParallelDataProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Translate.Types.EncryptionKey
import Network.AWS.Translate.Types.ParallelDataConfig
import Network.AWS.Translate.Types.ParallelDataStatus

-- | The properties of a parallel data resource.
--
-- /See:/ 'newParallelDataProperties' smart constructor.
data ParallelDataProperties = ParallelDataProperties'
  { -- | The status of the parallel data resource. When the parallel data is
    -- ready for you to use, the status is @ACTIVE@.
    status :: Core.Maybe ParallelDataStatus,
    -- | The number of UTF-8 characters that Amazon Translate imported from the
    -- parallel data input file. This number includes only the characters in
    -- your translation examples. It does not include characters that are used
    -- to format your file. For example, if you provided a Translation Memory
    -- Exchange (.tmx) file, this number does not include the tags.
    importedDataSize :: Core.Maybe Core.Integer,
    -- | The number of items in the input file that Amazon Translate skipped when
    -- you created or updated the parallel data resource. For example, Amazon
    -- Translate skips empty records, empty target texts, and empty lines.
    skippedRecordCount :: Core.Maybe Core.Integer,
    -- | The status of the most recent update attempt for the parallel data
    -- resource.
    latestUpdateAttemptStatus :: Core.Maybe ParallelDataStatus,
    -- | Additional information from Amazon Translate about the parallel data
    -- resource.
    message :: Core.Maybe Core.Text,
    encryptionKey :: Core.Maybe EncryptionKey,
    -- | The Amazon Resource Name (ARN) of the parallel data resource.
    arn :: Core.Maybe Core.Text,
    -- | The language codes for the target languages available in the parallel
    -- data file. All possible target languages are returned as an array.
    targetLanguageCodes :: Core.Maybe [Core.Text],
    -- | The time at which the parallel data resource was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The number of records unsuccessfully imported from the parallel data
    -- input file.
    failedRecordCount :: Core.Maybe Core.Integer,
    -- | The time that the most recent update was attempted.
    latestUpdateAttemptAt :: Core.Maybe Core.POSIX,
    -- | The custom name assigned to the parallel data resource.
    name :: Core.Maybe Core.Text,
    -- | Specifies the format and S3 location of the parallel data input file.
    parallelDataConfig :: Core.Maybe ParallelDataConfig,
    -- | The description assigned to the parallel data resource.
    description :: Core.Maybe Core.Text,
    -- | The source language of the translations in the parallel data file.
    sourceLanguageCode :: Core.Maybe Core.Text,
    -- | The number of records successfully imported from the parallel data input
    -- file.
    importedRecordCount :: Core.Maybe Core.Integer,
    -- | The time at which the parallel data resource was last updated.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParallelDataProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'parallelDataProperties_status' - The status of the parallel data resource. When the parallel data is
-- ready for you to use, the status is @ACTIVE@.
--
-- 'importedDataSize', 'parallelDataProperties_importedDataSize' - The number of UTF-8 characters that Amazon Translate imported from the
-- parallel data input file. This number includes only the characters in
-- your translation examples. It does not include characters that are used
-- to format your file. For example, if you provided a Translation Memory
-- Exchange (.tmx) file, this number does not include the tags.
--
-- 'skippedRecordCount', 'parallelDataProperties_skippedRecordCount' - The number of items in the input file that Amazon Translate skipped when
-- you created or updated the parallel data resource. For example, Amazon
-- Translate skips empty records, empty target texts, and empty lines.
--
-- 'latestUpdateAttemptStatus', 'parallelDataProperties_latestUpdateAttemptStatus' - The status of the most recent update attempt for the parallel data
-- resource.
--
-- 'message', 'parallelDataProperties_message' - Additional information from Amazon Translate about the parallel data
-- resource.
--
-- 'encryptionKey', 'parallelDataProperties_encryptionKey' - Undocumented member.
--
-- 'arn', 'parallelDataProperties_arn' - The Amazon Resource Name (ARN) of the parallel data resource.
--
-- 'targetLanguageCodes', 'parallelDataProperties_targetLanguageCodes' - The language codes for the target languages available in the parallel
-- data file. All possible target languages are returned as an array.
--
-- 'createdAt', 'parallelDataProperties_createdAt' - The time at which the parallel data resource was created.
--
-- 'failedRecordCount', 'parallelDataProperties_failedRecordCount' - The number of records unsuccessfully imported from the parallel data
-- input file.
--
-- 'latestUpdateAttemptAt', 'parallelDataProperties_latestUpdateAttemptAt' - The time that the most recent update was attempted.
--
-- 'name', 'parallelDataProperties_name' - The custom name assigned to the parallel data resource.
--
-- 'parallelDataConfig', 'parallelDataProperties_parallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
--
-- 'description', 'parallelDataProperties_description' - The description assigned to the parallel data resource.
--
-- 'sourceLanguageCode', 'parallelDataProperties_sourceLanguageCode' - The source language of the translations in the parallel data file.
--
-- 'importedRecordCount', 'parallelDataProperties_importedRecordCount' - The number of records successfully imported from the parallel data input
-- file.
--
-- 'lastUpdatedAt', 'parallelDataProperties_lastUpdatedAt' - The time at which the parallel data resource was last updated.
newParallelDataProperties ::
  ParallelDataProperties
newParallelDataProperties =
  ParallelDataProperties'
    { status = Core.Nothing,
      importedDataSize = Core.Nothing,
      skippedRecordCount = Core.Nothing,
      latestUpdateAttemptStatus = Core.Nothing,
      message = Core.Nothing,
      encryptionKey = Core.Nothing,
      arn = Core.Nothing,
      targetLanguageCodes = Core.Nothing,
      createdAt = Core.Nothing,
      failedRecordCount = Core.Nothing,
      latestUpdateAttemptAt = Core.Nothing,
      name = Core.Nothing,
      parallelDataConfig = Core.Nothing,
      description = Core.Nothing,
      sourceLanguageCode = Core.Nothing,
      importedRecordCount = Core.Nothing,
      lastUpdatedAt = Core.Nothing
    }

-- | The status of the parallel data resource. When the parallel data is
-- ready for you to use, the status is @ACTIVE@.
parallelDataProperties_status :: Lens.Lens' ParallelDataProperties (Core.Maybe ParallelDataStatus)
parallelDataProperties_status = Lens.lens (\ParallelDataProperties' {status} -> status) (\s@ParallelDataProperties' {} a -> s {status = a} :: ParallelDataProperties)

-- | The number of UTF-8 characters that Amazon Translate imported from the
-- parallel data input file. This number includes only the characters in
-- your translation examples. It does not include characters that are used
-- to format your file. For example, if you provided a Translation Memory
-- Exchange (.tmx) file, this number does not include the tags.
parallelDataProperties_importedDataSize :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Integer)
parallelDataProperties_importedDataSize = Lens.lens (\ParallelDataProperties' {importedDataSize} -> importedDataSize) (\s@ParallelDataProperties' {} a -> s {importedDataSize = a} :: ParallelDataProperties)

-- | The number of items in the input file that Amazon Translate skipped when
-- you created or updated the parallel data resource. For example, Amazon
-- Translate skips empty records, empty target texts, and empty lines.
parallelDataProperties_skippedRecordCount :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Integer)
parallelDataProperties_skippedRecordCount = Lens.lens (\ParallelDataProperties' {skippedRecordCount} -> skippedRecordCount) (\s@ParallelDataProperties' {} a -> s {skippedRecordCount = a} :: ParallelDataProperties)

-- | The status of the most recent update attempt for the parallel data
-- resource.
parallelDataProperties_latestUpdateAttemptStatus :: Lens.Lens' ParallelDataProperties (Core.Maybe ParallelDataStatus)
parallelDataProperties_latestUpdateAttemptStatus = Lens.lens (\ParallelDataProperties' {latestUpdateAttemptStatus} -> latestUpdateAttemptStatus) (\s@ParallelDataProperties' {} a -> s {latestUpdateAttemptStatus = a} :: ParallelDataProperties)

-- | Additional information from Amazon Translate about the parallel data
-- resource.
parallelDataProperties_message :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Text)
parallelDataProperties_message = Lens.lens (\ParallelDataProperties' {message} -> message) (\s@ParallelDataProperties' {} a -> s {message = a} :: ParallelDataProperties)

-- | Undocumented member.
parallelDataProperties_encryptionKey :: Lens.Lens' ParallelDataProperties (Core.Maybe EncryptionKey)
parallelDataProperties_encryptionKey = Lens.lens (\ParallelDataProperties' {encryptionKey} -> encryptionKey) (\s@ParallelDataProperties' {} a -> s {encryptionKey = a} :: ParallelDataProperties)

-- | The Amazon Resource Name (ARN) of the parallel data resource.
parallelDataProperties_arn :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Text)
parallelDataProperties_arn = Lens.lens (\ParallelDataProperties' {arn} -> arn) (\s@ParallelDataProperties' {} a -> s {arn = a} :: ParallelDataProperties)

-- | The language codes for the target languages available in the parallel
-- data file. All possible target languages are returned as an array.
parallelDataProperties_targetLanguageCodes :: Lens.Lens' ParallelDataProperties (Core.Maybe [Core.Text])
parallelDataProperties_targetLanguageCodes = Lens.lens (\ParallelDataProperties' {targetLanguageCodes} -> targetLanguageCodes) (\s@ParallelDataProperties' {} a -> s {targetLanguageCodes = a} :: ParallelDataProperties) Core.. Lens.mapping Lens._Coerce

-- | The time at which the parallel data resource was created.
parallelDataProperties_createdAt :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.UTCTime)
parallelDataProperties_createdAt = Lens.lens (\ParallelDataProperties' {createdAt} -> createdAt) (\s@ParallelDataProperties' {} a -> s {createdAt = a} :: ParallelDataProperties) Core.. Lens.mapping Core._Time

-- | The number of records unsuccessfully imported from the parallel data
-- input file.
parallelDataProperties_failedRecordCount :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Integer)
parallelDataProperties_failedRecordCount = Lens.lens (\ParallelDataProperties' {failedRecordCount} -> failedRecordCount) (\s@ParallelDataProperties' {} a -> s {failedRecordCount = a} :: ParallelDataProperties)

-- | The time that the most recent update was attempted.
parallelDataProperties_latestUpdateAttemptAt :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.UTCTime)
parallelDataProperties_latestUpdateAttemptAt = Lens.lens (\ParallelDataProperties' {latestUpdateAttemptAt} -> latestUpdateAttemptAt) (\s@ParallelDataProperties' {} a -> s {latestUpdateAttemptAt = a} :: ParallelDataProperties) Core.. Lens.mapping Core._Time

-- | The custom name assigned to the parallel data resource.
parallelDataProperties_name :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Text)
parallelDataProperties_name = Lens.lens (\ParallelDataProperties' {name} -> name) (\s@ParallelDataProperties' {} a -> s {name = a} :: ParallelDataProperties)

-- | Specifies the format and S3 location of the parallel data input file.
parallelDataProperties_parallelDataConfig :: Lens.Lens' ParallelDataProperties (Core.Maybe ParallelDataConfig)
parallelDataProperties_parallelDataConfig = Lens.lens (\ParallelDataProperties' {parallelDataConfig} -> parallelDataConfig) (\s@ParallelDataProperties' {} a -> s {parallelDataConfig = a} :: ParallelDataProperties)

-- | The description assigned to the parallel data resource.
parallelDataProperties_description :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Text)
parallelDataProperties_description = Lens.lens (\ParallelDataProperties' {description} -> description) (\s@ParallelDataProperties' {} a -> s {description = a} :: ParallelDataProperties)

-- | The source language of the translations in the parallel data file.
parallelDataProperties_sourceLanguageCode :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Text)
parallelDataProperties_sourceLanguageCode = Lens.lens (\ParallelDataProperties' {sourceLanguageCode} -> sourceLanguageCode) (\s@ParallelDataProperties' {} a -> s {sourceLanguageCode = a} :: ParallelDataProperties)

-- | The number of records successfully imported from the parallel data input
-- file.
parallelDataProperties_importedRecordCount :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Integer)
parallelDataProperties_importedRecordCount = Lens.lens (\ParallelDataProperties' {importedRecordCount} -> importedRecordCount) (\s@ParallelDataProperties' {} a -> s {importedRecordCount = a} :: ParallelDataProperties)

-- | The time at which the parallel data resource was last updated.
parallelDataProperties_lastUpdatedAt :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.UTCTime)
parallelDataProperties_lastUpdatedAt = Lens.lens (\ParallelDataProperties' {lastUpdatedAt} -> lastUpdatedAt) (\s@ParallelDataProperties' {} a -> s {lastUpdatedAt = a} :: ParallelDataProperties) Core.. Lens.mapping Core._Time

instance Core.FromJSON ParallelDataProperties where
  parseJSON =
    Core.withObject
      "ParallelDataProperties"
      ( \x ->
          ParallelDataProperties'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "ImportedDataSize")
            Core.<*> (x Core..:? "SkippedRecordCount")
            Core.<*> (x Core..:? "LatestUpdateAttemptStatus")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "EncryptionKey")
            Core.<*> (x Core..:? "Arn")
            Core.<*> ( x Core..:? "TargetLanguageCodes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "FailedRecordCount")
            Core.<*> (x Core..:? "LatestUpdateAttemptAt")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "ParallelDataConfig")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "SourceLanguageCode")
            Core.<*> (x Core..:? "ImportedRecordCount")
            Core.<*> (x Core..:? "LastUpdatedAt")
      )

instance Core.Hashable ParallelDataProperties

instance Core.NFData ParallelDataProperties
