{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.GlacierJobDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.GlacierJobDescription
  ( GlacierJobDescription (..)
  -- * Smart constructor
  , mkGlacierJobDescription
  -- * Lenses
  , gjdAction
  , gjdArchiveId
  , gjdArchiveSHA256TreeHash
  , gjdArchiveSizeInBytes
  , gjdCompleted
  , gjdCompletionDate
  , gjdCreationDate
  , gjdInventoryRetrievalParameters
  , gjdInventorySizeInBytes
  , gjdJobDescription
  , gjdJobId
  , gjdJobOutputPath
  , gjdOutputLocation
  , gjdRetrievalByteRange
  , gjdSHA256TreeHash
  , gjdSNSTopic
  , gjdSelectParameters
  , gjdStatusCode
  , gjdStatusMessage
  , gjdTier
  , gjdVaultARN
  ) where

import qualified Network.AWS.Glacier.Types.ActionCode as Types
import qualified Network.AWS.Glacier.Types.InventoryRetrievalJobDescription as Types
import qualified Network.AWS.Glacier.Types.OutputLocation as Types
import qualified Network.AWS.Glacier.Types.SelectParameters as Types
import qualified Network.AWS.Glacier.Types.StatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the description of an Amazon S3 Glacier job.
--
-- /See:/ 'mkGlacierJobDescription' smart constructor.
data GlacierJobDescription = GlacierJobDescription'
  { action :: Core.Maybe Types.ActionCode
    -- ^ The job type. This value is either @ArchiveRetrieval@ , @InventoryRetrieval@ , or @Select@ . 
  , archiveId :: Core.Maybe Core.Text
    -- ^ The archive ID requested for a select job or archive retrieval. Otherwise, this field is null.
  , archiveSHA256TreeHash :: Core.Maybe Core.Text
    -- ^ The SHA256 tree hash of the entire archive for an archive retrieval. For inventory retrieval or select jobs, this field is null.
  , archiveSizeInBytes :: Core.Maybe Core.Integer
    -- ^ For an archive retrieval job, this value is the size in bytes of the archive being requested for download. For an inventory retrieval or select job, this value is null.
  , completed :: Core.Maybe Core.Bool
    -- ^ The job status. When a job is completed, you get the job's output using Get Job Output (GET output).
  , completionDate :: Core.Maybe Core.Text
    -- ^ The UTC time that the job request completed. While the job is in progress, the value is null.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The UTC date when the job was created. This value is a string representation of ISO 8601 date format, for example @"2012-03-20T17:03:43.221Z"@ .
  , inventoryRetrievalParameters :: Core.Maybe Types.InventoryRetrievalJobDescription
    -- ^ Parameters used for range inventory retrieval.
  , inventorySizeInBytes :: Core.Maybe Core.Integer
    -- ^ For an inventory retrieval job, this value is the size in bytes of the inventory requested for download. For an archive retrieval or select job, this value is null.
  , jobDescription :: Core.Maybe Core.Text
    -- ^ The job description provided when initiating the job.
  , jobId :: Core.Maybe Core.Text
    -- ^ An opaque string that identifies an Amazon S3 Glacier job.
  , jobOutputPath :: Core.Maybe Core.Text
    -- ^ Contains the job output location.
  , outputLocation :: Core.Maybe Types.OutputLocation
    -- ^ Contains the location where the data from the select job is stored.
  , retrievalByteRange :: Core.Maybe Core.Text
    -- ^ The retrieved byte range for archive retrieval jobs in the form /StartByteValue/ -/EndByteValue/ . If no range was specified in the archive retrieval, then the whole archive is retrieved. In this case, /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the archive minus 1. For inventory retrieval or select jobs, this field is null. 
  , sHA256TreeHash :: Core.Maybe Core.Text
    -- ^ For an archive retrieval job, this value is the checksum of the archive. Otherwise, this value is null.
--
-- The SHA256 tree hash value for the requested range of an archive. If the __InitiateJob__ request for an archive specified a tree-hash aligned range, then this field returns a value.
-- If the whole archive is retrieved, this value is the same as the ArchiveSHA256TreeHash value.
-- This field is null for the following:
--
--     * Archive retrieval jobs that specify a range that is not tree-hash aligned
--
--
--
--     * Archival jobs that specify a range that is equal to the whole archive, when the job status is @InProgress@ 
--
--
--
--     * Inventory jobs
--
--
--     * Select jobs
--
--
  , sNSTopic :: Core.Maybe Core.Text
    -- ^ An Amazon SNS topic that receives notification.
  , selectParameters :: Core.Maybe Types.SelectParameters
    -- ^ Contains the parameters used for a select.
  , statusCode :: Core.Maybe Types.StatusCode
    -- ^ The status code can be @InProgress@ , @Succeeded@ , or @Failed@ , and indicates the status of the job.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ A friendly message that describes the job status.
  , tier :: Core.Maybe Core.Text
    -- ^ The tier to use for a select or an archive retrieval. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
  , vaultARN :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the vault from which an archive retrieval was requested.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlacierJobDescription' value with any optional fields omitted.
mkGlacierJobDescription
    :: GlacierJobDescription
mkGlacierJobDescription
  = GlacierJobDescription'{action = Core.Nothing,
                           archiveId = Core.Nothing, archiveSHA256TreeHash = Core.Nothing,
                           archiveSizeInBytes = Core.Nothing, completed = Core.Nothing,
                           completionDate = Core.Nothing, creationDate = Core.Nothing,
                           inventoryRetrievalParameters = Core.Nothing,
                           inventorySizeInBytes = Core.Nothing, jobDescription = Core.Nothing,
                           jobId = Core.Nothing, jobOutputPath = Core.Nothing,
                           outputLocation = Core.Nothing, retrievalByteRange = Core.Nothing,
                           sHA256TreeHash = Core.Nothing, sNSTopic = Core.Nothing,
                           selectParameters = Core.Nothing, statusCode = Core.Nothing,
                           statusMessage = Core.Nothing, tier = Core.Nothing,
                           vaultARN = Core.Nothing}

-- | The job type. This value is either @ArchiveRetrieval@ , @InventoryRetrieval@ , or @Select@ . 
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdAction :: Lens.Lens' GlacierJobDescription (Core.Maybe Types.ActionCode)
gjdAction = Lens.field @"action"
{-# INLINEABLE gjdAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The archive ID requested for a select job or archive retrieval. Otherwise, this field is null.
--
-- /Note:/ Consider using 'archiveId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdArchiveId :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdArchiveId = Lens.field @"archiveId"
{-# INLINEABLE gjdArchiveId #-}
{-# DEPRECATED archiveId "Use generic-lens or generic-optics with 'archiveId' instead"  #-}

-- | The SHA256 tree hash of the entire archive for an archive retrieval. For inventory retrieval or select jobs, this field is null.
--
-- /Note:/ Consider using 'archiveSHA256TreeHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdArchiveSHA256TreeHash :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdArchiveSHA256TreeHash = Lens.field @"archiveSHA256TreeHash"
{-# INLINEABLE gjdArchiveSHA256TreeHash #-}
{-# DEPRECATED archiveSHA256TreeHash "Use generic-lens or generic-optics with 'archiveSHA256TreeHash' instead"  #-}

-- | For an archive retrieval job, this value is the size in bytes of the archive being requested for download. For an inventory retrieval or select job, this value is null.
--
-- /Note:/ Consider using 'archiveSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdArchiveSizeInBytes :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Integer)
gjdArchiveSizeInBytes = Lens.field @"archiveSizeInBytes"
{-# INLINEABLE gjdArchiveSizeInBytes #-}
{-# DEPRECATED archiveSizeInBytes "Use generic-lens or generic-optics with 'archiveSizeInBytes' instead"  #-}

-- | The job status. When a job is completed, you get the job's output using Get Job Output (GET output).
--
-- /Note:/ Consider using 'completed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdCompleted :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Bool)
gjdCompleted = Lens.field @"completed"
{-# INLINEABLE gjdCompleted #-}
{-# DEPRECATED completed "Use generic-lens or generic-optics with 'completed' instead"  #-}

-- | The UTC time that the job request completed. While the job is in progress, the value is null.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdCompletionDate :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdCompletionDate = Lens.field @"completionDate"
{-# INLINEABLE gjdCompletionDate #-}
{-# DEPRECATED completionDate "Use generic-lens or generic-optics with 'completionDate' instead"  #-}

-- | The UTC date when the job was created. This value is a string representation of ISO 8601 date format, for example @"2012-03-20T17:03:43.221Z"@ .
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdCreationDate :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdCreationDate = Lens.field @"creationDate"
{-# INLINEABLE gjdCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | Parameters used for range inventory retrieval.
--
-- /Note:/ Consider using 'inventoryRetrievalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdInventoryRetrievalParameters :: Lens.Lens' GlacierJobDescription (Core.Maybe Types.InventoryRetrievalJobDescription)
gjdInventoryRetrievalParameters = Lens.field @"inventoryRetrievalParameters"
{-# INLINEABLE gjdInventoryRetrievalParameters #-}
{-# DEPRECATED inventoryRetrievalParameters "Use generic-lens or generic-optics with 'inventoryRetrievalParameters' instead"  #-}

-- | For an inventory retrieval job, this value is the size in bytes of the inventory requested for download. For an archive retrieval or select job, this value is null.
--
-- /Note:/ Consider using 'inventorySizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdInventorySizeInBytes :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Integer)
gjdInventorySizeInBytes = Lens.field @"inventorySizeInBytes"
{-# INLINEABLE gjdInventorySizeInBytes #-}
{-# DEPRECATED inventorySizeInBytes "Use generic-lens or generic-optics with 'inventorySizeInBytes' instead"  #-}

-- | The job description provided when initiating the job.
--
-- /Note:/ Consider using 'jobDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobDescription :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdJobDescription = Lens.field @"jobDescription"
{-# INLINEABLE gjdJobDescription #-}
{-# DEPRECATED jobDescription "Use generic-lens or generic-optics with 'jobDescription' instead"  #-}

-- | An opaque string that identifies an Amazon S3 Glacier job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobId :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdJobId = Lens.field @"jobId"
{-# INLINEABLE gjdJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Contains the job output location.
--
-- /Note:/ Consider using 'jobOutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobOutputPath :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdJobOutputPath = Lens.field @"jobOutputPath"
{-# INLINEABLE gjdJobOutputPath #-}
{-# DEPRECATED jobOutputPath "Use generic-lens or generic-optics with 'jobOutputPath' instead"  #-}

-- | Contains the location where the data from the select job is stored.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdOutputLocation :: Lens.Lens' GlacierJobDescription (Core.Maybe Types.OutputLocation)
gjdOutputLocation = Lens.field @"outputLocation"
{-# INLINEABLE gjdOutputLocation #-}
{-# DEPRECATED outputLocation "Use generic-lens or generic-optics with 'outputLocation' instead"  #-}

-- | The retrieved byte range for archive retrieval jobs in the form /StartByteValue/ -/EndByteValue/ . If no range was specified in the archive retrieval, then the whole archive is retrieved. In this case, /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the archive minus 1. For inventory retrieval or select jobs, this field is null. 
--
-- /Note:/ Consider using 'retrievalByteRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdRetrievalByteRange :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdRetrievalByteRange = Lens.field @"retrievalByteRange"
{-# INLINEABLE gjdRetrievalByteRange #-}
{-# DEPRECATED retrievalByteRange "Use generic-lens or generic-optics with 'retrievalByteRange' instead"  #-}

-- | For an archive retrieval job, this value is the checksum of the archive. Otherwise, this value is null.
--
-- The SHA256 tree hash value for the requested range of an archive. If the __InitiateJob__ request for an archive specified a tree-hash aligned range, then this field returns a value.
-- If the whole archive is retrieved, this value is the same as the ArchiveSHA256TreeHash value.
-- This field is null for the following:
--
--     * Archive retrieval jobs that specify a range that is not tree-hash aligned
--
--
--
--     * Archival jobs that specify a range that is equal to the whole archive, when the job status is @InProgress@ 
--
--
--
--     * Inventory jobs
--
--
--     * Select jobs
--
--
--
-- /Note:/ Consider using 'sHA256TreeHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdSHA256TreeHash :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdSHA256TreeHash = Lens.field @"sHA256TreeHash"
{-# INLINEABLE gjdSHA256TreeHash #-}
{-# DEPRECATED sHA256TreeHash "Use generic-lens or generic-optics with 'sHA256TreeHash' instead"  #-}

-- | An Amazon SNS topic that receives notification.
--
-- /Note:/ Consider using 'sNSTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdSNSTopic :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdSNSTopic = Lens.field @"sNSTopic"
{-# INLINEABLE gjdSNSTopic #-}
{-# DEPRECATED sNSTopic "Use generic-lens or generic-optics with 'sNSTopic' instead"  #-}

-- | Contains the parameters used for a select.
--
-- /Note:/ Consider using 'selectParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdSelectParameters :: Lens.Lens' GlacierJobDescription (Core.Maybe Types.SelectParameters)
gjdSelectParameters = Lens.field @"selectParameters"
{-# INLINEABLE gjdSelectParameters #-}
{-# DEPRECATED selectParameters "Use generic-lens or generic-optics with 'selectParameters' instead"  #-}

-- | The status code can be @InProgress@ , @Succeeded@ , or @Failed@ , and indicates the status of the job.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdStatusCode :: Lens.Lens' GlacierJobDescription (Core.Maybe Types.StatusCode)
gjdStatusCode = Lens.field @"statusCode"
{-# INLINEABLE gjdStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

-- | A friendly message that describes the job status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdStatusMessage :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE gjdStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The tier to use for a select or an archive retrieval. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdTier :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdTier = Lens.field @"tier"
{-# INLINEABLE gjdTier #-}
{-# DEPRECATED tier "Use generic-lens or generic-optics with 'tier' instead"  #-}

-- | The Amazon Resource Name (ARN) of the vault from which an archive retrieval was requested.
--
-- /Note:/ Consider using 'vaultARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdVaultARN :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
gjdVaultARN = Lens.field @"vaultARN"
{-# INLINEABLE gjdVaultARN #-}
{-# DEPRECATED vaultARN "Use generic-lens or generic-optics with 'vaultARN' instead"  #-}

instance Core.FromJSON GlacierJobDescription where
        parseJSON
          = Core.withObject "GlacierJobDescription" Core.$
              \ x ->
                GlacierJobDescription' Core.<$>
                  (x Core..:? "Action") Core.<*> x Core..:? "ArchiveId" Core.<*>
                    x Core..:? "ArchiveSHA256TreeHash"
                    Core.<*> x Core..:? "ArchiveSizeInBytes"
                    Core.<*> x Core..:? "Completed"
                    Core.<*> x Core..:? "CompletionDate"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "InventoryRetrievalParameters"
                    Core.<*> x Core..:? "InventorySizeInBytes"
                    Core.<*> x Core..:? "JobDescription"
                    Core.<*> x Core..:? "JobId"
                    Core.<*> x Core..:? "JobOutputPath"
                    Core.<*> x Core..:? "OutputLocation"
                    Core.<*> x Core..:? "RetrievalByteRange"
                    Core.<*> x Core..:? "SHA256TreeHash"
                    Core.<*> x Core..:? "SNSTopic"
                    Core.<*> x Core..:? "SelectParameters"
                    Core.<*> x Core..:? "StatusCode"
                    Core.<*> x Core..:? "StatusMessage"
                    Core.<*> x Core..:? "Tier"
                    Core.<*> x Core..:? "VaultARN"
