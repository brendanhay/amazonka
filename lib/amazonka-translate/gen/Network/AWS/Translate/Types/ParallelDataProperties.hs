{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.ParallelDataProperties
  ( ParallelDataProperties (..)
  -- * Smart constructor
  , mkParallelDataProperties
  -- * Lenses
  , pdpArn
  , pdpCreatedAt
  , pdpDescription
  , pdpEncryptionKey
  , pdpFailedRecordCount
  , pdpImportedDataSize
  , pdpImportedRecordCount
  , pdpLastUpdatedAt
  , pdpLatestUpdateAttemptAt
  , pdpLatestUpdateAttemptStatus
  , pdpMessage
  , pdpName
  , pdpParallelDataConfig
  , pdpSkippedRecordCount
  , pdpSourceLanguageCode
  , pdpStatus
  , pdpTargetLanguageCodes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Translate.Types.Arn as Types
import qualified Network.AWS.Translate.Types.Description as Types
import qualified Network.AWS.Translate.Types.EncryptionKey as Types
import qualified Network.AWS.Translate.Types.LanguageCodeString as Types
import qualified Network.AWS.Translate.Types.Message as Types
import qualified Network.AWS.Translate.Types.Name as Types
import qualified Network.AWS.Translate.Types.ParallelDataConfig as Types
import qualified Network.AWS.Translate.Types.ParallelDataStatus as Types

-- | The properties of a parallel data resource.
--
-- /See:/ 'mkParallelDataProperties' smart constructor.
data ParallelDataProperties = ParallelDataProperties'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the parallel data resource.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the parallel data resource was created.
  , description :: Core.Maybe Types.Description
    -- ^ The description assigned to the parallel data resource.
  , encryptionKey :: Core.Maybe Types.EncryptionKey
  , failedRecordCount :: Core.Maybe Core.Integer
    -- ^ The number of records unsuccessfully imported from the parallel data input file.
  , importedDataSize :: Core.Maybe Core.Integer
    -- ^ The number of UTF-8 characters that Amazon Translate imported from the parallel data input file. This number includes only the characters in your translation examples. It does not include characters that are used to format your file. For example, if you provided a Translation Memory Exchange (.tmx) file, this number does not include the tags.
  , importedRecordCount :: Core.Maybe Core.Integer
    -- ^ The number of records successfully imported from the parallel data input file.
  , lastUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the parallel data resource was last updated.
  , latestUpdateAttemptAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the most recent update was attempted.
  , latestUpdateAttemptStatus :: Core.Maybe Types.ParallelDataStatus
    -- ^ The status of the most recent update attempt for the parallel data resource.
  , message :: Core.Maybe Types.Message
    -- ^ Additional information from Amazon Translate about the parallel data resource. 
  , name :: Core.Maybe Types.Name
    -- ^ The custom name assigned to the parallel data resource.
  , parallelDataConfig :: Core.Maybe Types.ParallelDataConfig
    -- ^ Specifies the format and S3 location of the parallel data input file.
  , skippedRecordCount :: Core.Maybe Core.Integer
    -- ^ The number of items in the input file that Amazon Translate skipped when you created or updated the parallel data resource. For example, Amazon Translate skips empty records, empty target texts, and empty lines.
  , sourceLanguageCode :: Core.Maybe Types.LanguageCodeString
    -- ^ The source language of the translations in the parallel data file.
  , status :: Core.Maybe Types.ParallelDataStatus
    -- ^ The status of the parallel data resource. When the parallel data is ready for you to use, the status is @ACTIVE@ .
  , targetLanguageCodes :: Core.Maybe [Types.LanguageCodeString]
    -- ^ The language codes for the target languages available in the parallel data file. All possible target languages are returned as an array.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ParallelDataProperties' value with any optional fields omitted.
mkParallelDataProperties
    :: ParallelDataProperties
mkParallelDataProperties
  = ParallelDataProperties'{arn = Core.Nothing,
                            createdAt = Core.Nothing, description = Core.Nothing,
                            encryptionKey = Core.Nothing, failedRecordCount = Core.Nothing,
                            importedDataSize = Core.Nothing,
                            importedRecordCount = Core.Nothing, lastUpdatedAt = Core.Nothing,
                            latestUpdateAttemptAt = Core.Nothing,
                            latestUpdateAttemptStatus = Core.Nothing, message = Core.Nothing,
                            name = Core.Nothing, parallelDataConfig = Core.Nothing,
                            skippedRecordCount = Core.Nothing,
                            sourceLanguageCode = Core.Nothing, status = Core.Nothing,
                            targetLanguageCodes = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the parallel data resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpArn :: Lens.Lens' ParallelDataProperties (Core.Maybe Types.Arn)
pdpArn = Lens.field @"arn"
{-# INLINEABLE pdpArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time at which the parallel data resource was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpCreatedAt :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.NominalDiffTime)
pdpCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE pdpCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The description assigned to the parallel data resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpDescription :: Lens.Lens' ParallelDataProperties (Core.Maybe Types.Description)
pdpDescription = Lens.field @"description"
{-# INLINEABLE pdpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpEncryptionKey :: Lens.Lens' ParallelDataProperties (Core.Maybe Types.EncryptionKey)
pdpEncryptionKey = Lens.field @"encryptionKey"
{-# INLINEABLE pdpEncryptionKey #-}
{-# DEPRECATED encryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead"  #-}

-- | The number of records unsuccessfully imported from the parallel data input file.
--
-- /Note:/ Consider using 'failedRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpFailedRecordCount :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Integer)
pdpFailedRecordCount = Lens.field @"failedRecordCount"
{-# INLINEABLE pdpFailedRecordCount #-}
{-# DEPRECATED failedRecordCount "Use generic-lens or generic-optics with 'failedRecordCount' instead"  #-}

-- | The number of UTF-8 characters that Amazon Translate imported from the parallel data input file. This number includes only the characters in your translation examples. It does not include characters that are used to format your file. For example, if you provided a Translation Memory Exchange (.tmx) file, this number does not include the tags.
--
-- /Note:/ Consider using 'importedDataSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpImportedDataSize :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Integer)
pdpImportedDataSize = Lens.field @"importedDataSize"
{-# INLINEABLE pdpImportedDataSize #-}
{-# DEPRECATED importedDataSize "Use generic-lens or generic-optics with 'importedDataSize' instead"  #-}

-- | The number of records successfully imported from the parallel data input file.
--
-- /Note:/ Consider using 'importedRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpImportedRecordCount :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Integer)
pdpImportedRecordCount = Lens.field @"importedRecordCount"
{-# INLINEABLE pdpImportedRecordCount #-}
{-# DEPRECATED importedRecordCount "Use generic-lens or generic-optics with 'importedRecordCount' instead"  #-}

-- | The time at which the parallel data resource was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpLastUpdatedAt :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.NominalDiffTime)
pdpLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE pdpLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | The time that the most recent update was attempted.
--
-- /Note:/ Consider using 'latestUpdateAttemptAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpLatestUpdateAttemptAt :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.NominalDiffTime)
pdpLatestUpdateAttemptAt = Lens.field @"latestUpdateAttemptAt"
{-# INLINEABLE pdpLatestUpdateAttemptAt #-}
{-# DEPRECATED latestUpdateAttemptAt "Use generic-lens or generic-optics with 'latestUpdateAttemptAt' instead"  #-}

-- | The status of the most recent update attempt for the parallel data resource.
--
-- /Note:/ Consider using 'latestUpdateAttemptStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpLatestUpdateAttemptStatus :: Lens.Lens' ParallelDataProperties (Core.Maybe Types.ParallelDataStatus)
pdpLatestUpdateAttemptStatus = Lens.field @"latestUpdateAttemptStatus"
{-# INLINEABLE pdpLatestUpdateAttemptStatus #-}
{-# DEPRECATED latestUpdateAttemptStatus "Use generic-lens or generic-optics with 'latestUpdateAttemptStatus' instead"  #-}

-- | Additional information from Amazon Translate about the parallel data resource. 
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpMessage :: Lens.Lens' ParallelDataProperties (Core.Maybe Types.Message)
pdpMessage = Lens.field @"message"
{-# INLINEABLE pdpMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The custom name assigned to the parallel data resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpName :: Lens.Lens' ParallelDataProperties (Core.Maybe Types.Name)
pdpName = Lens.field @"name"
{-# INLINEABLE pdpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies the format and S3 location of the parallel data input file.
--
-- /Note:/ Consider using 'parallelDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpParallelDataConfig :: Lens.Lens' ParallelDataProperties (Core.Maybe Types.ParallelDataConfig)
pdpParallelDataConfig = Lens.field @"parallelDataConfig"
{-# INLINEABLE pdpParallelDataConfig #-}
{-# DEPRECATED parallelDataConfig "Use generic-lens or generic-optics with 'parallelDataConfig' instead"  #-}

-- | The number of items in the input file that Amazon Translate skipped when you created or updated the parallel data resource. For example, Amazon Translate skips empty records, empty target texts, and empty lines.
--
-- /Note:/ Consider using 'skippedRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpSkippedRecordCount :: Lens.Lens' ParallelDataProperties (Core.Maybe Core.Integer)
pdpSkippedRecordCount = Lens.field @"skippedRecordCount"
{-# INLINEABLE pdpSkippedRecordCount #-}
{-# DEPRECATED skippedRecordCount "Use generic-lens or generic-optics with 'skippedRecordCount' instead"  #-}

-- | The source language of the translations in the parallel data file.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpSourceLanguageCode :: Lens.Lens' ParallelDataProperties (Core.Maybe Types.LanguageCodeString)
pdpSourceLanguageCode = Lens.field @"sourceLanguageCode"
{-# INLINEABLE pdpSourceLanguageCode #-}
{-# DEPRECATED sourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead"  #-}

-- | The status of the parallel data resource. When the parallel data is ready for you to use, the status is @ACTIVE@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpStatus :: Lens.Lens' ParallelDataProperties (Core.Maybe Types.ParallelDataStatus)
pdpStatus = Lens.field @"status"
{-# INLINEABLE pdpStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The language codes for the target languages available in the parallel data file. All possible target languages are returned as an array.
--
-- /Note:/ Consider using 'targetLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdpTargetLanguageCodes :: Lens.Lens' ParallelDataProperties (Core.Maybe [Types.LanguageCodeString])
pdpTargetLanguageCodes = Lens.field @"targetLanguageCodes"
{-# INLINEABLE pdpTargetLanguageCodes #-}
{-# DEPRECATED targetLanguageCodes "Use generic-lens or generic-optics with 'targetLanguageCodes' instead"  #-}

instance Core.FromJSON ParallelDataProperties where
        parseJSON
          = Core.withObject "ParallelDataProperties" Core.$
              \ x ->
                ParallelDataProperties' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "CreatedAt" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "EncryptionKey"
                    Core.<*> x Core..:? "FailedRecordCount"
                    Core.<*> x Core..:? "ImportedDataSize"
                    Core.<*> x Core..:? "ImportedRecordCount"
                    Core.<*> x Core..:? "LastUpdatedAt"
                    Core.<*> x Core..:? "LatestUpdateAttemptAt"
                    Core.<*> x Core..:? "LatestUpdateAttemptStatus"
                    Core.<*> x Core..:? "Message"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "ParallelDataConfig"
                    Core.<*> x Core..:? "SkippedRecordCount"
                    Core.<*> x Core..:? "SourceLanguageCode"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "TargetLanguageCodes"
