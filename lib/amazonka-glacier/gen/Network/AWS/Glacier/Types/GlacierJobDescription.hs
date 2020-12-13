{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.GlacierJobDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.GlacierJobDescription
  ( GlacierJobDescription (..),

    -- * Smart constructor
    mkGlacierJobDescription,

    -- * Lenses
    gjdSHA256TreeHash,
    gjdArchiveId,
    gjdSelectParameters,
    gjdJobId,
    gjdJobOutputPath,
    gjdRetrievalByteRange,
    gjdInventoryRetrievalParameters,
    gjdAction,
    gjdJobDescription,
    gjdSNSTopic,
    gjdStatusMessage,
    gjdVaultARN,
    gjdOutputLocation,
    gjdTier,
    gjdArchiveSHA256TreeHash,
    gjdCreationDate,
    gjdCompleted,
    gjdCompletionDate,
    gjdInventorySizeInBytes,
    gjdArchiveSizeInBytes,
    gjdStatusCode,
  )
where

import Network.AWS.Glacier.Types.ActionCode
import Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
import Network.AWS.Glacier.Types.OutputLocation
import Network.AWS.Glacier.Types.SelectParameters
import Network.AWS.Glacier.Types.StatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the description of an Amazon S3 Glacier job.
--
-- /See:/ 'mkGlacierJobDescription' smart constructor.
data GlacierJobDescription = GlacierJobDescription'
  { -- | For an archive retrieval job, this value is the checksum of the archive. Otherwise, this value is null.
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
    sHA256TreeHash :: Lude.Maybe Lude.Text,
    -- | The archive ID requested for a select job or archive retrieval. Otherwise, this field is null.
    archiveId :: Lude.Maybe Lude.Text,
    -- | Contains the parameters used for a select.
    selectParameters :: Lude.Maybe SelectParameters,
    -- | An opaque string that identifies an Amazon S3 Glacier job.
    jobId :: Lude.Maybe Lude.Text,
    -- | Contains the job output location.
    jobOutputPath :: Lude.Maybe Lude.Text,
    -- | The retrieved byte range for archive retrieval jobs in the form /StartByteValue/ -/EndByteValue/ . If no range was specified in the archive retrieval, then the whole archive is retrieved. In this case, /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the archive minus 1. For inventory retrieval or select jobs, this field is null.
    retrievalByteRange :: Lude.Maybe Lude.Text,
    -- | Parameters used for range inventory retrieval.
    inventoryRetrievalParameters :: Lude.Maybe InventoryRetrievalJobDescription,
    -- | The job type. This value is either @ArchiveRetrieval@ , @InventoryRetrieval@ , or @Select@ .
    action :: Lude.Maybe ActionCode,
    -- | The job description provided when initiating the job.
    jobDescription :: Lude.Maybe Lude.Text,
    -- | An Amazon SNS topic that receives notification.
    snsTopic :: Lude.Maybe Lude.Text,
    -- | A friendly message that describes the job status.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the vault from which an archive retrieval was requested.
    vaultARN :: Lude.Maybe Lude.Text,
    -- | Contains the location where the data from the select job is stored.
    outputLocation :: Lude.Maybe OutputLocation,
    -- | The tier to use for a select or an archive retrieval. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
    tier :: Lude.Maybe Lude.Text,
    -- | The SHA256 tree hash of the entire archive for an archive retrieval. For inventory retrieval or select jobs, this field is null.
    archiveSHA256TreeHash :: Lude.Maybe Lude.Text,
    -- | The UTC date when the job was created. This value is a string representation of ISO 8601 date format, for example @"2012-03-20T17:03:43.221Z"@ .
    creationDate :: Lude.Maybe Lude.Text,
    -- | The job status. When a job is completed, you get the job's output using Get Job Output (GET output).
    completed :: Lude.Maybe Lude.Bool,
    -- | The UTC time that the job request completed. While the job is in progress, the value is null.
    completionDate :: Lude.Maybe Lude.Text,
    -- | For an inventory retrieval job, this value is the size in bytes of the inventory requested for download. For an archive retrieval or select job, this value is null.
    inventorySizeInBytes :: Lude.Maybe Lude.Integer,
    -- | For an archive retrieval job, this value is the size in bytes of the archive being requested for download. For an inventory retrieval or select job, this value is null.
    archiveSizeInBytes :: Lude.Maybe Lude.Integer,
    -- | The status code can be @InProgress@ , @Succeeded@ , or @Failed@ , and indicates the status of the job.
    statusCode :: Lude.Maybe StatusCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlacierJobDescription' with the minimum fields required to make a request.
--
-- * 'sHA256TreeHash' - For an archive retrieval job, this value is the checksum of the archive. Otherwise, this value is null.
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
-- * 'archiveId' - The archive ID requested for a select job or archive retrieval. Otherwise, this field is null.
-- * 'selectParameters' - Contains the parameters used for a select.
-- * 'jobId' - An opaque string that identifies an Amazon S3 Glacier job.
-- * 'jobOutputPath' - Contains the job output location.
-- * 'retrievalByteRange' - The retrieved byte range for archive retrieval jobs in the form /StartByteValue/ -/EndByteValue/ . If no range was specified in the archive retrieval, then the whole archive is retrieved. In this case, /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the archive minus 1. For inventory retrieval or select jobs, this field is null.
-- * 'inventoryRetrievalParameters' - Parameters used for range inventory retrieval.
-- * 'action' - The job type. This value is either @ArchiveRetrieval@ , @InventoryRetrieval@ , or @Select@ .
-- * 'jobDescription' - The job description provided when initiating the job.
-- * 'snsTopic' - An Amazon SNS topic that receives notification.
-- * 'statusMessage' - A friendly message that describes the job status.
-- * 'vaultARN' - The Amazon Resource Name (ARN) of the vault from which an archive retrieval was requested.
-- * 'outputLocation' - Contains the location where the data from the select job is stored.
-- * 'tier' - The tier to use for a select or an archive retrieval. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
-- * 'archiveSHA256TreeHash' - The SHA256 tree hash of the entire archive for an archive retrieval. For inventory retrieval or select jobs, this field is null.
-- * 'creationDate' - The UTC date when the job was created. This value is a string representation of ISO 8601 date format, for example @"2012-03-20T17:03:43.221Z"@ .
-- * 'completed' - The job status. When a job is completed, you get the job's output using Get Job Output (GET output).
-- * 'completionDate' - The UTC time that the job request completed. While the job is in progress, the value is null.
-- * 'inventorySizeInBytes' - For an inventory retrieval job, this value is the size in bytes of the inventory requested for download. For an archive retrieval or select job, this value is null.
-- * 'archiveSizeInBytes' - For an archive retrieval job, this value is the size in bytes of the archive being requested for download. For an inventory retrieval or select job, this value is null.
-- * 'statusCode' - The status code can be @InProgress@ , @Succeeded@ , or @Failed@ , and indicates the status of the job.
mkGlacierJobDescription ::
  GlacierJobDescription
mkGlacierJobDescription =
  GlacierJobDescription'
    { sHA256TreeHash = Lude.Nothing,
      archiveId = Lude.Nothing,
      selectParameters = Lude.Nothing,
      jobId = Lude.Nothing,
      jobOutputPath = Lude.Nothing,
      retrievalByteRange = Lude.Nothing,
      inventoryRetrievalParameters = Lude.Nothing,
      action = Lude.Nothing,
      jobDescription = Lude.Nothing,
      snsTopic = Lude.Nothing,
      statusMessage = Lude.Nothing,
      vaultARN = Lude.Nothing,
      outputLocation = Lude.Nothing,
      tier = Lude.Nothing,
      archiveSHA256TreeHash = Lude.Nothing,
      creationDate = Lude.Nothing,
      completed = Lude.Nothing,
      completionDate = Lude.Nothing,
      inventorySizeInBytes = Lude.Nothing,
      archiveSizeInBytes = Lude.Nothing,
      statusCode = Lude.Nothing
    }

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
gjdSHA256TreeHash :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdSHA256TreeHash = Lens.lens (sHA256TreeHash :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {sHA256TreeHash = a} :: GlacierJobDescription)
{-# DEPRECATED gjdSHA256TreeHash "Use generic-lens or generic-optics with 'sHA256TreeHash' instead." #-}

-- | The archive ID requested for a select job or archive retrieval. Otherwise, this field is null.
--
-- /Note:/ Consider using 'archiveId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdArchiveId :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdArchiveId = Lens.lens (archiveId :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {archiveId = a} :: GlacierJobDescription)
{-# DEPRECATED gjdArchiveId "Use generic-lens or generic-optics with 'archiveId' instead." #-}

-- | Contains the parameters used for a select.
--
-- /Note:/ Consider using 'selectParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdSelectParameters :: Lens.Lens' GlacierJobDescription (Lude.Maybe SelectParameters)
gjdSelectParameters = Lens.lens (selectParameters :: GlacierJobDescription -> Lude.Maybe SelectParameters) (\s a -> s {selectParameters = a} :: GlacierJobDescription)
{-# DEPRECATED gjdSelectParameters "Use generic-lens or generic-optics with 'selectParameters' instead." #-}

-- | An opaque string that identifies an Amazon S3 Glacier job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobId :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdJobId = Lens.lens (jobId :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: GlacierJobDescription)
{-# DEPRECATED gjdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Contains the job output location.
--
-- /Note:/ Consider using 'jobOutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobOutputPath :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdJobOutputPath = Lens.lens (jobOutputPath :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {jobOutputPath = a} :: GlacierJobDescription)
{-# DEPRECATED gjdJobOutputPath "Use generic-lens or generic-optics with 'jobOutputPath' instead." #-}

-- | The retrieved byte range for archive retrieval jobs in the form /StartByteValue/ -/EndByteValue/ . If no range was specified in the archive retrieval, then the whole archive is retrieved. In this case, /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the archive minus 1. For inventory retrieval or select jobs, this field is null.
--
-- /Note:/ Consider using 'retrievalByteRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdRetrievalByteRange :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdRetrievalByteRange = Lens.lens (retrievalByteRange :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {retrievalByteRange = a} :: GlacierJobDescription)
{-# DEPRECATED gjdRetrievalByteRange "Use generic-lens or generic-optics with 'retrievalByteRange' instead." #-}

-- | Parameters used for range inventory retrieval.
--
-- /Note:/ Consider using 'inventoryRetrievalParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdInventoryRetrievalParameters :: Lens.Lens' GlacierJobDescription (Lude.Maybe InventoryRetrievalJobDescription)
gjdInventoryRetrievalParameters = Lens.lens (inventoryRetrievalParameters :: GlacierJobDescription -> Lude.Maybe InventoryRetrievalJobDescription) (\s a -> s {inventoryRetrievalParameters = a} :: GlacierJobDescription)
{-# DEPRECATED gjdInventoryRetrievalParameters "Use generic-lens or generic-optics with 'inventoryRetrievalParameters' instead." #-}

-- | The job type. This value is either @ArchiveRetrieval@ , @InventoryRetrieval@ , or @Select@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdAction :: Lens.Lens' GlacierJobDescription (Lude.Maybe ActionCode)
gjdAction = Lens.lens (action :: GlacierJobDescription -> Lude.Maybe ActionCode) (\s a -> s {action = a} :: GlacierJobDescription)
{-# DEPRECATED gjdAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The job description provided when initiating the job.
--
-- /Note:/ Consider using 'jobDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobDescription :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdJobDescription = Lens.lens (jobDescription :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {jobDescription = a} :: GlacierJobDescription)
{-# DEPRECATED gjdJobDescription "Use generic-lens or generic-optics with 'jobDescription' instead." #-}

-- | An Amazon SNS topic that receives notification.
--
-- /Note:/ Consider using 'snsTopic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdSNSTopic :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdSNSTopic = Lens.lens (snsTopic :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {snsTopic = a} :: GlacierJobDescription)
{-# DEPRECATED gjdSNSTopic "Use generic-lens or generic-optics with 'snsTopic' instead." #-}

-- | A friendly message that describes the job status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdStatusMessage :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdStatusMessage = Lens.lens (statusMessage :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: GlacierJobDescription)
{-# DEPRECATED gjdStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The Amazon Resource Name (ARN) of the vault from which an archive retrieval was requested.
--
-- /Note:/ Consider using 'vaultARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdVaultARN :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdVaultARN = Lens.lens (vaultARN :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {vaultARN = a} :: GlacierJobDescription)
{-# DEPRECATED gjdVaultARN "Use generic-lens or generic-optics with 'vaultARN' instead." #-}

-- | Contains the location where the data from the select job is stored.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdOutputLocation :: Lens.Lens' GlacierJobDescription (Lude.Maybe OutputLocation)
gjdOutputLocation = Lens.lens (outputLocation :: GlacierJobDescription -> Lude.Maybe OutputLocation) (\s a -> s {outputLocation = a} :: GlacierJobDescription)
{-# DEPRECATED gjdOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | The tier to use for a select or an archive retrieval. Valid values are @Expedited@ , @Standard@ , or @Bulk@ . @Standard@ is the default.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdTier :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdTier = Lens.lens (tier :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {tier = a} :: GlacierJobDescription)
{-# DEPRECATED gjdTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | The SHA256 tree hash of the entire archive for an archive retrieval. For inventory retrieval or select jobs, this field is null.
--
-- /Note:/ Consider using 'archiveSHA256TreeHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdArchiveSHA256TreeHash :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdArchiveSHA256TreeHash = Lens.lens (archiveSHA256TreeHash :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {archiveSHA256TreeHash = a} :: GlacierJobDescription)
{-# DEPRECATED gjdArchiveSHA256TreeHash "Use generic-lens or generic-optics with 'archiveSHA256TreeHash' instead." #-}

-- | The UTC date when the job was created. This value is a string representation of ISO 8601 date format, for example @"2012-03-20T17:03:43.221Z"@ .
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdCreationDate :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdCreationDate = Lens.lens (creationDate :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: GlacierJobDescription)
{-# DEPRECATED gjdCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The job status. When a job is completed, you get the job's output using Get Job Output (GET output).
--
-- /Note:/ Consider using 'completed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdCompleted :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Bool)
gjdCompleted = Lens.lens (completed :: GlacierJobDescription -> Lude.Maybe Lude.Bool) (\s a -> s {completed = a} :: GlacierJobDescription)
{-# DEPRECATED gjdCompleted "Use generic-lens or generic-optics with 'completed' instead." #-}

-- | The UTC time that the job request completed. While the job is in progress, the value is null.
--
-- /Note:/ Consider using 'completionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdCompletionDate :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Text)
gjdCompletionDate = Lens.lens (completionDate :: GlacierJobDescription -> Lude.Maybe Lude.Text) (\s a -> s {completionDate = a} :: GlacierJobDescription)
{-# DEPRECATED gjdCompletionDate "Use generic-lens or generic-optics with 'completionDate' instead." #-}

-- | For an inventory retrieval job, this value is the size in bytes of the inventory requested for download. For an archive retrieval or select job, this value is null.
--
-- /Note:/ Consider using 'inventorySizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdInventorySizeInBytes :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Integer)
gjdInventorySizeInBytes = Lens.lens (inventorySizeInBytes :: GlacierJobDescription -> Lude.Maybe Lude.Integer) (\s a -> s {inventorySizeInBytes = a} :: GlacierJobDescription)
{-# DEPRECATED gjdInventorySizeInBytes "Use generic-lens or generic-optics with 'inventorySizeInBytes' instead." #-}

-- | For an archive retrieval job, this value is the size in bytes of the archive being requested for download. For an inventory retrieval or select job, this value is null.
--
-- /Note:/ Consider using 'archiveSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdArchiveSizeInBytes :: Lens.Lens' GlacierJobDescription (Lude.Maybe Lude.Integer)
gjdArchiveSizeInBytes = Lens.lens (archiveSizeInBytes :: GlacierJobDescription -> Lude.Maybe Lude.Integer) (\s a -> s {archiveSizeInBytes = a} :: GlacierJobDescription)
{-# DEPRECATED gjdArchiveSizeInBytes "Use generic-lens or generic-optics with 'archiveSizeInBytes' instead." #-}

-- | The status code can be @InProgress@ , @Succeeded@ , or @Failed@ , and indicates the status of the job.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdStatusCode :: Lens.Lens' GlacierJobDescription (Lude.Maybe StatusCode)
gjdStatusCode = Lens.lens (statusCode :: GlacierJobDescription -> Lude.Maybe StatusCode) (\s a -> s {statusCode = a} :: GlacierJobDescription)
{-# DEPRECATED gjdStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON GlacierJobDescription where
  parseJSON =
    Lude.withObject
      "GlacierJobDescription"
      ( \x ->
          GlacierJobDescription'
            Lude.<$> (x Lude..:? "SHA256TreeHash")
            Lude.<*> (x Lude..:? "ArchiveId")
            Lude.<*> (x Lude..:? "SelectParameters")
            Lude.<*> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "JobOutputPath")
            Lude.<*> (x Lude..:? "RetrievalByteRange")
            Lude.<*> (x Lude..:? "InventoryRetrievalParameters")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "JobDescription")
            Lude.<*> (x Lude..:? "SNSTopic")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "VaultARN")
            Lude.<*> (x Lude..:? "OutputLocation")
            Lude.<*> (x Lude..:? "Tier")
            Lude.<*> (x Lude..:? "ArchiveSHA256TreeHash")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "Completed")
            Lude.<*> (x Lude..:? "CompletionDate")
            Lude.<*> (x Lude..:? "InventorySizeInBytes")
            Lude.<*> (x Lude..:? "ArchiveSizeInBytes")
            Lude.<*> (x Lude..:? "StatusCode")
      )
