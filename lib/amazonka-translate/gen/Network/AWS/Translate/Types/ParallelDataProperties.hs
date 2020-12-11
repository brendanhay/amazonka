-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataProperties
  ( ParallelDataProperties (..),

    -- * Smart constructor
    mkParallelDataProperties,

    -- * Lenses
    pdpStatus,
    pdpLastUpdatedAt,
    pdpImportedRecordCount,
    pdpARN,
    pdpTargetLanguageCodes,
    pdpCreatedAt,
    pdpFailedRecordCount,
    pdpImportedDataSize,
    pdpName,
    pdpSourceLanguageCode,
    pdpLatestUpdateAttemptAt,
    pdpEncryptionKey,
    pdpLatestUpdateAttemptStatus,
    pdpMessage,
    pdpDescription,
    pdpSkippedRecordCount,
    pdpParallelDataConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Translate.Types.EncryptionKey
import Network.AWS.Translate.Types.ParallelDataConfig
import Network.AWS.Translate.Types.ParallelDataStatus

-- | The properties of a parallel data resource.
--
-- /See:/ 'mkParallelDataProperties' smart constructor.
data ParallelDataProperties = ParallelDataProperties'
  { status ::
      Lude.Maybe ParallelDataStatus,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    importedRecordCount ::
      Lude.Maybe Lude.Integer,
    arn :: Lude.Maybe Lude.Text,
    targetLanguageCodes :: Lude.Maybe [Lude.Text],
    createdAt :: Lude.Maybe Lude.Timestamp,
    failedRecordCount :: Lude.Maybe Lude.Integer,
    importedDataSize :: Lude.Maybe Lude.Integer,
    name :: Lude.Maybe Lude.Text,
    sourceLanguageCode :: Lude.Maybe Lude.Text,
    latestUpdateAttemptAt ::
      Lude.Maybe Lude.Timestamp,
    encryptionKey :: Lude.Maybe EncryptionKey,
    latestUpdateAttemptStatus ::
      Lude.Maybe ParallelDataStatus,
    message :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    skippedRecordCount :: Lude.Maybe Lude.Integer,
    parallelDataConfig ::
      Lude.Maybe ParallelDataConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParallelDataProperties' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the parallel data resource.
-- * 'createdAt' - The time at which the parallel data resource was created.
-- * 'description' - The description assigned to the parallel data resource.
-- * 'encryptionKey' - Undocumented field.
-- * 'failedRecordCount' - The number of records unsuccessfully imported from the parallel data input file.
-- * 'importedDataSize' - The number of UTF-8 characters that Amazon Translate imported from the parallel data input file. This number includes only the characters in your translation examples. It does not include characters that are used to format your file. For example, if you provided a Translation Memory Exchange (.tmx) file, this number does not include the tags.
-- * 'importedRecordCount' - The number of records successfully imported from the parallel data input file.
-- * 'lastUpdatedAt' - The time at which the parallel data resource was last updated.
-- * 'latestUpdateAttemptAt' - The time that the most recent update was attempted.
-- * 'latestUpdateAttemptStatus' - The status of the most recent update attempt for the parallel data resource.
-- * 'message' - Additional information from Amazon Translate about the parallel data resource.
-- * 'name' - The custom name assigned to the parallel data resource.
-- * 'parallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
-- * 'skippedRecordCount' - The number of items in the input file that Amazon Translate skipped when you created or updated the parallel data resource. For example, Amazon Translate skips empty records, empty target texts, and empty lines.
-- * 'sourceLanguageCode' - The source language of the translations in the parallel data file.
-- * 'status' - The status of the parallel data resource. When the parallel data is ready for you to use, the status is @ACTIVE@ .
-- * 'targetLanguageCodes' - The language codes for the target languages available in the parallel data file. All possible target languages are returned as an array.
mkParallelDataProperties ::
  ParallelDataProperties
mkParallelDataProperties =
  ParallelDataProperties'
    { status = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      importedRecordCount = Lude.Nothing,
      arn = Lude.Nothing,
      targetLanguageCodes = Lude.Nothing,
      createdAt = Lude.Nothing,
      failedRecordCount = Lude.Nothing,
      importedDataSize = Lude.Nothing,
      name = Lude.Nothing,
      sourceLanguageCode = Lude.Nothing,
      latestUpdateAttemptAt = Lude.Nothing,
      encryptionKey = Lude.Nothing,
      latestUpdateAttemptStatus = Lude.Nothing,
      message = Lude.Nothing,
      description = Lude.Nothing,
      skippedRecordCount = Lude.Nothing,
      parallelDataConfig = Lude.Nothing
    }

-- | The status of the parallel data resource. When the parallel data is ready for you to use, the status is @ACTIVE@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpStatus :: Lens.Lens' ParallelDataProperties (Lude.Maybe ParallelDataStatus)
pdpStatus = Lens.lens (status :: ParallelDataProperties -> Lude.Maybe ParallelDataStatus) (\s a -> s {status = a} :: ParallelDataProperties)
{-# DEPRECATED pdpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time at which the parallel data resource was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpLastUpdatedAt :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Timestamp)
pdpLastUpdatedAt = Lens.lens (lastUpdatedAt :: ParallelDataProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: ParallelDataProperties)
{-# DEPRECATED pdpLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The number of records successfully imported from the parallel data input file.
--
-- /Note:/ Consider using 'importedRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpImportedRecordCount :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Integer)
pdpImportedRecordCount = Lens.lens (importedRecordCount :: ParallelDataProperties -> Lude.Maybe Lude.Integer) (\s a -> s {importedRecordCount = a} :: ParallelDataProperties)
{-# DEPRECATED pdpImportedRecordCount "Use generic-lens or generic-optics with 'importedRecordCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the parallel data resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpARN :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Text)
pdpARN = Lens.lens (arn :: ParallelDataProperties -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ParallelDataProperties)
{-# DEPRECATED pdpARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The language codes for the target languages available in the parallel data file. All possible target languages are returned as an array.
--
-- /Note:/ Consider using 'targetLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpTargetLanguageCodes :: Lens.Lens' ParallelDataProperties (Lude.Maybe [Lude.Text])
pdpTargetLanguageCodes = Lens.lens (targetLanguageCodes :: ParallelDataProperties -> Lude.Maybe [Lude.Text]) (\s a -> s {targetLanguageCodes = a} :: ParallelDataProperties)
{-# DEPRECATED pdpTargetLanguageCodes "Use generic-lens or generic-optics with 'targetLanguageCodes' instead." #-}

-- | The time at which the parallel data resource was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpCreatedAt :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Timestamp)
pdpCreatedAt = Lens.lens (createdAt :: ParallelDataProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ParallelDataProperties)
{-# DEPRECATED pdpCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The number of records unsuccessfully imported from the parallel data input file.
--
-- /Note:/ Consider using 'failedRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpFailedRecordCount :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Integer)
pdpFailedRecordCount = Lens.lens (failedRecordCount :: ParallelDataProperties -> Lude.Maybe Lude.Integer) (\s a -> s {failedRecordCount = a} :: ParallelDataProperties)
{-# DEPRECATED pdpFailedRecordCount "Use generic-lens or generic-optics with 'failedRecordCount' instead." #-}

-- | The number of UTF-8 characters that Amazon Translate imported from the parallel data input file. This number includes only the characters in your translation examples. It does not include characters that are used to format your file. For example, if you provided a Translation Memory Exchange (.tmx) file, this number does not include the tags.
--
-- /Note:/ Consider using 'importedDataSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpImportedDataSize :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Integer)
pdpImportedDataSize = Lens.lens (importedDataSize :: ParallelDataProperties -> Lude.Maybe Lude.Integer) (\s a -> s {importedDataSize = a} :: ParallelDataProperties)
{-# DEPRECATED pdpImportedDataSize "Use generic-lens or generic-optics with 'importedDataSize' instead." #-}

-- | The custom name assigned to the parallel data resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpName :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Text)
pdpName = Lens.lens (name :: ParallelDataProperties -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ParallelDataProperties)
{-# DEPRECATED pdpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The source language of the translations in the parallel data file.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpSourceLanguageCode :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Text)
pdpSourceLanguageCode = Lens.lens (sourceLanguageCode :: ParallelDataProperties -> Lude.Maybe Lude.Text) (\s a -> s {sourceLanguageCode = a} :: ParallelDataProperties)
{-# DEPRECATED pdpSourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead." #-}

-- | The time that the most recent update was attempted.
--
-- /Note:/ Consider using 'latestUpdateAttemptAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpLatestUpdateAttemptAt :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Timestamp)
pdpLatestUpdateAttemptAt = Lens.lens (latestUpdateAttemptAt :: ParallelDataProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestUpdateAttemptAt = a} :: ParallelDataProperties)
{-# DEPRECATED pdpLatestUpdateAttemptAt "Use generic-lens or generic-optics with 'latestUpdateAttemptAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpEncryptionKey :: Lens.Lens' ParallelDataProperties (Lude.Maybe EncryptionKey)
pdpEncryptionKey = Lens.lens (encryptionKey :: ParallelDataProperties -> Lude.Maybe EncryptionKey) (\s a -> s {encryptionKey = a} :: ParallelDataProperties)
{-# DEPRECATED pdpEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | The status of the most recent update attempt for the parallel data resource.
--
-- /Note:/ Consider using 'latestUpdateAttemptStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpLatestUpdateAttemptStatus :: Lens.Lens' ParallelDataProperties (Lude.Maybe ParallelDataStatus)
pdpLatestUpdateAttemptStatus = Lens.lens (latestUpdateAttemptStatus :: ParallelDataProperties -> Lude.Maybe ParallelDataStatus) (\s a -> s {latestUpdateAttemptStatus = a} :: ParallelDataProperties)
{-# DEPRECATED pdpLatestUpdateAttemptStatus "Use generic-lens or generic-optics with 'latestUpdateAttemptStatus' instead." #-}

-- | Additional information from Amazon Translate about the parallel data resource.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpMessage :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Text)
pdpMessage = Lens.lens (message :: ParallelDataProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ParallelDataProperties)
{-# DEPRECATED pdpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The description assigned to the parallel data resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpDescription :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Text)
pdpDescription = Lens.lens (description :: ParallelDataProperties -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ParallelDataProperties)
{-# DEPRECATED pdpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The number of items in the input file that Amazon Translate skipped when you created or updated the parallel data resource. For example, Amazon Translate skips empty records, empty target texts, and empty lines.
--
-- /Note:/ Consider using 'skippedRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpSkippedRecordCount :: Lens.Lens' ParallelDataProperties (Lude.Maybe Lude.Integer)
pdpSkippedRecordCount = Lens.lens (skippedRecordCount :: ParallelDataProperties -> Lude.Maybe Lude.Integer) (\s a -> s {skippedRecordCount = a} :: ParallelDataProperties)
{-# DEPRECATED pdpSkippedRecordCount "Use generic-lens or generic-optics with 'skippedRecordCount' instead." #-}

-- | Specifies the format and S3 location of the parallel data input file.
--
-- /Note:/ Consider using 'parallelDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpParallelDataConfig :: Lens.Lens' ParallelDataProperties (Lude.Maybe ParallelDataConfig)
pdpParallelDataConfig = Lens.lens (parallelDataConfig :: ParallelDataProperties -> Lude.Maybe ParallelDataConfig) (\s a -> s {parallelDataConfig = a} :: ParallelDataProperties)
{-# DEPRECATED pdpParallelDataConfig "Use generic-lens or generic-optics with 'parallelDataConfig' instead." #-}

instance Lude.FromJSON ParallelDataProperties where
  parseJSON =
    Lude.withObject
      "ParallelDataProperties"
      ( \x ->
          ParallelDataProperties'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LastUpdatedAt")
            Lude.<*> (x Lude..:? "ImportedRecordCount")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "TargetLanguageCodes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "FailedRecordCount")
            Lude.<*> (x Lude..:? "ImportedDataSize")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "SourceLanguageCode")
            Lude.<*> (x Lude..:? "LatestUpdateAttemptAt")
            Lude.<*> (x Lude..:? "EncryptionKey")
            Lude.<*> (x Lude..:? "LatestUpdateAttemptStatus")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "SkippedRecordCount")
            Lude.<*> (x Lude..:? "ParallelDataConfig")
      )
