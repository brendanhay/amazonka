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
-- Module      : Network.AWS.Glacier.Types.GlacierJobDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.GlacierJobDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types.ActionCode
import Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
import Network.AWS.Glacier.Types.OutputLocation
import Network.AWS.Glacier.Types.SelectParameters
import Network.AWS.Glacier.Types.StatusCode
import qualified Network.AWS.Lens as Lens

-- | Contains the description of an Amazon S3 Glacier job.
--
-- /See:/ 'newGlacierJobDescription' smart constructor.
data GlacierJobDescription = GlacierJobDescription'
  { -- | For an archive retrieval job, this value is the checksum of the archive.
    -- Otherwise, this value is null.
    --
    -- The SHA256 tree hash value for the requested range of an archive. If the
    -- __InitiateJob__ request for an archive specified a tree-hash aligned
    -- range, then this field returns a value.
    --
    -- If the whole archive is retrieved, this value is the same as the
    -- ArchiveSHA256TreeHash value.
    --
    -- This field is null for the following:
    --
    -- -   Archive retrieval jobs that specify a range that is not tree-hash
    --     aligned
    --
    -- -   Archival jobs that specify a range that is equal to the whole
    --     archive, when the job status is @InProgress@
    --
    -- -   Inventory jobs
    --
    -- -   Select jobs
    sHA256TreeHash :: Core.Maybe Core.Text,
    -- | A friendly message that describes the job status.
    statusMessage :: Core.Maybe Core.Text,
    -- | The job description provided when initiating the job.
    jobDescription :: Core.Maybe Core.Text,
    -- | The retrieved byte range for archive retrieval jobs in the form
    -- /StartByteValue/-/EndByteValue/. If no range was specified in the
    -- archive retrieval, then the whole archive is retrieved. In this case,
    -- /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the
    -- archive minus 1. For inventory retrieval or select jobs, this field is
    -- null.
    retrievalByteRange :: Core.Maybe Core.Text,
    -- | The UTC date when the job was created. This value is a string
    -- representation of ISO 8601 date format, for example
    -- @\"2012-03-20T17:03:43.221Z\"@.
    creationDate :: Core.Maybe Core.Text,
    -- | Contains the job output location.
    jobOutputPath :: Core.Maybe Core.Text,
    -- | Contains the parameters used for a select.
    selectParameters :: Core.Maybe SelectParameters,
    -- | The Amazon Resource Name (ARN) of the vault from which an archive
    -- retrieval was requested.
    vaultARN :: Core.Maybe Core.Text,
    -- | The archive ID requested for a select job or archive retrieval.
    -- Otherwise, this field is null.
    archiveId :: Core.Maybe Core.Text,
    -- | An Amazon SNS topic that receives notification.
    sNSTopic :: Core.Maybe Core.Text,
    -- | For an inventory retrieval job, this value is the size in bytes of the
    -- inventory requested for download. For an archive retrieval or select
    -- job, this value is null.
    inventorySizeInBytes :: Core.Maybe Core.Integer,
    -- | The status code can be @InProgress@, @Succeeded@, or @Failed@, and
    -- indicates the status of the job.
    statusCode :: Core.Maybe StatusCode,
    -- | For an archive retrieval job, this value is the size in bytes of the
    -- archive being requested for download. For an inventory retrieval or
    -- select job, this value is null.
    archiveSizeInBytes :: Core.Maybe Core.Integer,
    -- | The job type. This value is either @ArchiveRetrieval@,
    -- @InventoryRetrieval@, or @Select@.
    action :: Core.Maybe ActionCode,
    -- | Parameters used for range inventory retrieval.
    inventoryRetrievalParameters :: Core.Maybe InventoryRetrievalJobDescription,
    -- | The UTC time that the job request completed. While the job is in
    -- progress, the value is null.
    completionDate :: Core.Maybe Core.Text,
    -- | The SHA256 tree hash of the entire archive for an archive retrieval. For
    -- inventory retrieval or select jobs, this field is null.
    archiveSHA256TreeHash :: Core.Maybe Core.Text,
    -- | The job status. When a job is completed, you get the job\'s output using
    -- Get Job Output (GET output).
    completed :: Core.Maybe Core.Bool,
    -- | An opaque string that identifies an Amazon S3 Glacier job.
    jobId :: Core.Maybe Core.Text,
    -- | Contains the location where the data from the select job is stored.
    outputLocation :: Core.Maybe OutputLocation,
    -- | The tier to use for a select or an archive retrieval. Valid values are
    -- @Expedited@, @Standard@, or @Bulk@. @Standard@ is the default.
    tier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GlacierJobDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sHA256TreeHash', 'glacierJobDescription_sHA256TreeHash' - For an archive retrieval job, this value is the checksum of the archive.
-- Otherwise, this value is null.
--
-- The SHA256 tree hash value for the requested range of an archive. If the
-- __InitiateJob__ request for an archive specified a tree-hash aligned
-- range, then this field returns a value.
--
-- If the whole archive is retrieved, this value is the same as the
-- ArchiveSHA256TreeHash value.
--
-- This field is null for the following:
--
-- -   Archive retrieval jobs that specify a range that is not tree-hash
--     aligned
--
-- -   Archival jobs that specify a range that is equal to the whole
--     archive, when the job status is @InProgress@
--
-- -   Inventory jobs
--
-- -   Select jobs
--
-- 'statusMessage', 'glacierJobDescription_statusMessage' - A friendly message that describes the job status.
--
-- 'jobDescription', 'glacierJobDescription_jobDescription' - The job description provided when initiating the job.
--
-- 'retrievalByteRange', 'glacierJobDescription_retrievalByteRange' - The retrieved byte range for archive retrieval jobs in the form
-- /StartByteValue/-/EndByteValue/. If no range was specified in the
-- archive retrieval, then the whole archive is retrieved. In this case,
-- /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the
-- archive minus 1. For inventory retrieval or select jobs, this field is
-- null.
--
-- 'creationDate', 'glacierJobDescription_creationDate' - The UTC date when the job was created. This value is a string
-- representation of ISO 8601 date format, for example
-- @\"2012-03-20T17:03:43.221Z\"@.
--
-- 'jobOutputPath', 'glacierJobDescription_jobOutputPath' - Contains the job output location.
--
-- 'selectParameters', 'glacierJobDescription_selectParameters' - Contains the parameters used for a select.
--
-- 'vaultARN', 'glacierJobDescription_vaultARN' - The Amazon Resource Name (ARN) of the vault from which an archive
-- retrieval was requested.
--
-- 'archiveId', 'glacierJobDescription_archiveId' - The archive ID requested for a select job or archive retrieval.
-- Otherwise, this field is null.
--
-- 'sNSTopic', 'glacierJobDescription_sNSTopic' - An Amazon SNS topic that receives notification.
--
-- 'inventorySizeInBytes', 'glacierJobDescription_inventorySizeInBytes' - For an inventory retrieval job, this value is the size in bytes of the
-- inventory requested for download. For an archive retrieval or select
-- job, this value is null.
--
-- 'statusCode', 'glacierJobDescription_statusCode' - The status code can be @InProgress@, @Succeeded@, or @Failed@, and
-- indicates the status of the job.
--
-- 'archiveSizeInBytes', 'glacierJobDescription_archiveSizeInBytes' - For an archive retrieval job, this value is the size in bytes of the
-- archive being requested for download. For an inventory retrieval or
-- select job, this value is null.
--
-- 'action', 'glacierJobDescription_action' - The job type. This value is either @ArchiveRetrieval@,
-- @InventoryRetrieval@, or @Select@.
--
-- 'inventoryRetrievalParameters', 'glacierJobDescription_inventoryRetrievalParameters' - Parameters used for range inventory retrieval.
--
-- 'completionDate', 'glacierJobDescription_completionDate' - The UTC time that the job request completed. While the job is in
-- progress, the value is null.
--
-- 'archiveSHA256TreeHash', 'glacierJobDescription_archiveSHA256TreeHash' - The SHA256 tree hash of the entire archive for an archive retrieval. For
-- inventory retrieval or select jobs, this field is null.
--
-- 'completed', 'glacierJobDescription_completed' - The job status. When a job is completed, you get the job\'s output using
-- Get Job Output (GET output).
--
-- 'jobId', 'glacierJobDescription_jobId' - An opaque string that identifies an Amazon S3 Glacier job.
--
-- 'outputLocation', 'glacierJobDescription_outputLocation' - Contains the location where the data from the select job is stored.
--
-- 'tier', 'glacierJobDescription_tier' - The tier to use for a select or an archive retrieval. Valid values are
-- @Expedited@, @Standard@, or @Bulk@. @Standard@ is the default.
newGlacierJobDescription ::
  GlacierJobDescription
newGlacierJobDescription =
  GlacierJobDescription'
    { sHA256TreeHash =
        Core.Nothing,
      statusMessage = Core.Nothing,
      jobDescription = Core.Nothing,
      retrievalByteRange = Core.Nothing,
      creationDate = Core.Nothing,
      jobOutputPath = Core.Nothing,
      selectParameters = Core.Nothing,
      vaultARN = Core.Nothing,
      archiveId = Core.Nothing,
      sNSTopic = Core.Nothing,
      inventorySizeInBytes = Core.Nothing,
      statusCode = Core.Nothing,
      archiveSizeInBytes = Core.Nothing,
      action = Core.Nothing,
      inventoryRetrievalParameters = Core.Nothing,
      completionDate = Core.Nothing,
      archiveSHA256TreeHash = Core.Nothing,
      completed = Core.Nothing,
      jobId = Core.Nothing,
      outputLocation = Core.Nothing,
      tier = Core.Nothing
    }

-- | For an archive retrieval job, this value is the checksum of the archive.
-- Otherwise, this value is null.
--
-- The SHA256 tree hash value for the requested range of an archive. If the
-- __InitiateJob__ request for an archive specified a tree-hash aligned
-- range, then this field returns a value.
--
-- If the whole archive is retrieved, this value is the same as the
-- ArchiveSHA256TreeHash value.
--
-- This field is null for the following:
--
-- -   Archive retrieval jobs that specify a range that is not tree-hash
--     aligned
--
-- -   Archival jobs that specify a range that is equal to the whole
--     archive, when the job status is @InProgress@
--
-- -   Inventory jobs
--
-- -   Select jobs
glacierJobDescription_sHA256TreeHash :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_sHA256TreeHash = Lens.lens (\GlacierJobDescription' {sHA256TreeHash} -> sHA256TreeHash) (\s@GlacierJobDescription' {} a -> s {sHA256TreeHash = a} :: GlacierJobDescription)

-- | A friendly message that describes the job status.
glacierJobDescription_statusMessage :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_statusMessage = Lens.lens (\GlacierJobDescription' {statusMessage} -> statusMessage) (\s@GlacierJobDescription' {} a -> s {statusMessage = a} :: GlacierJobDescription)

-- | The job description provided when initiating the job.
glacierJobDescription_jobDescription :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_jobDescription = Lens.lens (\GlacierJobDescription' {jobDescription} -> jobDescription) (\s@GlacierJobDescription' {} a -> s {jobDescription = a} :: GlacierJobDescription)

-- | The retrieved byte range for archive retrieval jobs in the form
-- /StartByteValue/-/EndByteValue/. If no range was specified in the
-- archive retrieval, then the whole archive is retrieved. In this case,
-- /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the
-- archive minus 1. For inventory retrieval or select jobs, this field is
-- null.
glacierJobDescription_retrievalByteRange :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_retrievalByteRange = Lens.lens (\GlacierJobDescription' {retrievalByteRange} -> retrievalByteRange) (\s@GlacierJobDescription' {} a -> s {retrievalByteRange = a} :: GlacierJobDescription)

-- | The UTC date when the job was created. This value is a string
-- representation of ISO 8601 date format, for example
-- @\"2012-03-20T17:03:43.221Z\"@.
glacierJobDescription_creationDate :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_creationDate = Lens.lens (\GlacierJobDescription' {creationDate} -> creationDate) (\s@GlacierJobDescription' {} a -> s {creationDate = a} :: GlacierJobDescription)

-- | Contains the job output location.
glacierJobDescription_jobOutputPath :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_jobOutputPath = Lens.lens (\GlacierJobDescription' {jobOutputPath} -> jobOutputPath) (\s@GlacierJobDescription' {} a -> s {jobOutputPath = a} :: GlacierJobDescription)

-- | Contains the parameters used for a select.
glacierJobDescription_selectParameters :: Lens.Lens' GlacierJobDescription (Core.Maybe SelectParameters)
glacierJobDescription_selectParameters = Lens.lens (\GlacierJobDescription' {selectParameters} -> selectParameters) (\s@GlacierJobDescription' {} a -> s {selectParameters = a} :: GlacierJobDescription)

-- | The Amazon Resource Name (ARN) of the vault from which an archive
-- retrieval was requested.
glacierJobDescription_vaultARN :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_vaultARN = Lens.lens (\GlacierJobDescription' {vaultARN} -> vaultARN) (\s@GlacierJobDescription' {} a -> s {vaultARN = a} :: GlacierJobDescription)

-- | The archive ID requested for a select job or archive retrieval.
-- Otherwise, this field is null.
glacierJobDescription_archiveId :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_archiveId = Lens.lens (\GlacierJobDescription' {archiveId} -> archiveId) (\s@GlacierJobDescription' {} a -> s {archiveId = a} :: GlacierJobDescription)

-- | An Amazon SNS topic that receives notification.
glacierJobDescription_sNSTopic :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_sNSTopic = Lens.lens (\GlacierJobDescription' {sNSTopic} -> sNSTopic) (\s@GlacierJobDescription' {} a -> s {sNSTopic = a} :: GlacierJobDescription)

-- | For an inventory retrieval job, this value is the size in bytes of the
-- inventory requested for download. For an archive retrieval or select
-- job, this value is null.
glacierJobDescription_inventorySizeInBytes :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Integer)
glacierJobDescription_inventorySizeInBytes = Lens.lens (\GlacierJobDescription' {inventorySizeInBytes} -> inventorySizeInBytes) (\s@GlacierJobDescription' {} a -> s {inventorySizeInBytes = a} :: GlacierJobDescription)

-- | The status code can be @InProgress@, @Succeeded@, or @Failed@, and
-- indicates the status of the job.
glacierJobDescription_statusCode :: Lens.Lens' GlacierJobDescription (Core.Maybe StatusCode)
glacierJobDescription_statusCode = Lens.lens (\GlacierJobDescription' {statusCode} -> statusCode) (\s@GlacierJobDescription' {} a -> s {statusCode = a} :: GlacierJobDescription)

-- | For an archive retrieval job, this value is the size in bytes of the
-- archive being requested for download. For an inventory retrieval or
-- select job, this value is null.
glacierJobDescription_archiveSizeInBytes :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Integer)
glacierJobDescription_archiveSizeInBytes = Lens.lens (\GlacierJobDescription' {archiveSizeInBytes} -> archiveSizeInBytes) (\s@GlacierJobDescription' {} a -> s {archiveSizeInBytes = a} :: GlacierJobDescription)

-- | The job type. This value is either @ArchiveRetrieval@,
-- @InventoryRetrieval@, or @Select@.
glacierJobDescription_action :: Lens.Lens' GlacierJobDescription (Core.Maybe ActionCode)
glacierJobDescription_action = Lens.lens (\GlacierJobDescription' {action} -> action) (\s@GlacierJobDescription' {} a -> s {action = a} :: GlacierJobDescription)

-- | Parameters used for range inventory retrieval.
glacierJobDescription_inventoryRetrievalParameters :: Lens.Lens' GlacierJobDescription (Core.Maybe InventoryRetrievalJobDescription)
glacierJobDescription_inventoryRetrievalParameters = Lens.lens (\GlacierJobDescription' {inventoryRetrievalParameters} -> inventoryRetrievalParameters) (\s@GlacierJobDescription' {} a -> s {inventoryRetrievalParameters = a} :: GlacierJobDescription)

-- | The UTC time that the job request completed. While the job is in
-- progress, the value is null.
glacierJobDescription_completionDate :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_completionDate = Lens.lens (\GlacierJobDescription' {completionDate} -> completionDate) (\s@GlacierJobDescription' {} a -> s {completionDate = a} :: GlacierJobDescription)

-- | The SHA256 tree hash of the entire archive for an archive retrieval. For
-- inventory retrieval or select jobs, this field is null.
glacierJobDescription_archiveSHA256TreeHash :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_archiveSHA256TreeHash = Lens.lens (\GlacierJobDescription' {archiveSHA256TreeHash} -> archiveSHA256TreeHash) (\s@GlacierJobDescription' {} a -> s {archiveSHA256TreeHash = a} :: GlacierJobDescription)

-- | The job status. When a job is completed, you get the job\'s output using
-- Get Job Output (GET output).
glacierJobDescription_completed :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Bool)
glacierJobDescription_completed = Lens.lens (\GlacierJobDescription' {completed} -> completed) (\s@GlacierJobDescription' {} a -> s {completed = a} :: GlacierJobDescription)

-- | An opaque string that identifies an Amazon S3 Glacier job.
glacierJobDescription_jobId :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_jobId = Lens.lens (\GlacierJobDescription' {jobId} -> jobId) (\s@GlacierJobDescription' {} a -> s {jobId = a} :: GlacierJobDescription)

-- | Contains the location where the data from the select job is stored.
glacierJobDescription_outputLocation :: Lens.Lens' GlacierJobDescription (Core.Maybe OutputLocation)
glacierJobDescription_outputLocation = Lens.lens (\GlacierJobDescription' {outputLocation} -> outputLocation) (\s@GlacierJobDescription' {} a -> s {outputLocation = a} :: GlacierJobDescription)

-- | The tier to use for a select or an archive retrieval. Valid values are
-- @Expedited@, @Standard@, or @Bulk@. @Standard@ is the default.
glacierJobDescription_tier :: Lens.Lens' GlacierJobDescription (Core.Maybe Core.Text)
glacierJobDescription_tier = Lens.lens (\GlacierJobDescription' {tier} -> tier) (\s@GlacierJobDescription' {} a -> s {tier = a} :: GlacierJobDescription)

instance Core.FromJSON GlacierJobDescription where
  parseJSON =
    Core.withObject
      "GlacierJobDescription"
      ( \x ->
          GlacierJobDescription'
            Core.<$> (x Core..:? "SHA256TreeHash")
            Core.<*> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "JobDescription")
            Core.<*> (x Core..:? "RetrievalByteRange")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "JobOutputPath")
            Core.<*> (x Core..:? "SelectParameters")
            Core.<*> (x Core..:? "VaultARN")
            Core.<*> (x Core..:? "ArchiveId")
            Core.<*> (x Core..:? "SNSTopic")
            Core.<*> (x Core..:? "InventorySizeInBytes")
            Core.<*> (x Core..:? "StatusCode")
            Core.<*> (x Core..:? "ArchiveSizeInBytes")
            Core.<*> (x Core..:? "Action")
            Core.<*> (x Core..:? "InventoryRetrievalParameters")
            Core.<*> (x Core..:? "CompletionDate")
            Core.<*> (x Core..:? "ArchiveSHA256TreeHash")
            Core.<*> (x Core..:? "Completed")
            Core.<*> (x Core..:? "JobId")
            Core.<*> (x Core..:? "OutputLocation")
            Core.<*> (x Core..:? "Tier")
      )

instance Core.Hashable GlacierJobDescription

instance Core.NFData GlacierJobDescription
