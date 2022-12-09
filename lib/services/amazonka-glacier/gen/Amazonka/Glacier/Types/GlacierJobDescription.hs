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
-- Module      : Amazonka.Glacier.Types.GlacierJobDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.GlacierJobDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.ActionCode
import Amazonka.Glacier.Types.InventoryRetrievalJobDescription
import Amazonka.Glacier.Types.OutputLocation
import Amazonka.Glacier.Types.SelectParameters
import Amazonka.Glacier.Types.StatusCode
import qualified Amazonka.Prelude as Prelude

-- | Contains the description of an Amazon S3 Glacier job.
--
-- /See:/ 'newGlacierJobDescription' smart constructor.
data GlacierJobDescription = GlacierJobDescription'
  { -- | The job type. This value is either @ArchiveRetrieval@,
    -- @InventoryRetrieval@, or @Select@.
    action :: Prelude.Maybe ActionCode,
    -- | The archive ID requested for a select job or archive retrieval.
    -- Otherwise, this field is null.
    archiveId :: Prelude.Maybe Prelude.Text,
    -- | The SHA256 tree hash of the entire archive for an archive retrieval. For
    -- inventory retrieval or select jobs, this field is null.
    archiveSHA256TreeHash :: Prelude.Maybe Prelude.Text,
    -- | For an archive retrieval job, this value is the size in bytes of the
    -- archive being requested for download. For an inventory retrieval or
    -- select job, this value is null.
    archiveSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The job status. When a job is completed, you get the job\'s output using
    -- Get Job Output (GET output).
    completed :: Prelude.Maybe Prelude.Bool,
    -- | The UTC time that the job request completed. While the job is in
    -- progress, the value is null.
    completionDate :: Prelude.Maybe Prelude.Text,
    -- | The UTC date when the job was created. This value is a string
    -- representation of ISO 8601 date format, for example
    -- @\"2012-03-20T17:03:43.221Z\"@.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | Parameters used for range inventory retrieval.
    inventoryRetrievalParameters :: Prelude.Maybe InventoryRetrievalJobDescription,
    -- | For an inventory retrieval job, this value is the size in bytes of the
    -- inventory requested for download. For an archive retrieval or select
    -- job, this value is null.
    inventorySizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The job description provided when initiating the job.
    jobDescription :: Prelude.Maybe Prelude.Text,
    -- | An opaque string that identifies an Amazon S3 Glacier job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | Contains the job output location.
    jobOutputPath :: Prelude.Maybe Prelude.Text,
    -- | Contains the location where the data from the select job is stored.
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | The retrieved byte range for archive retrieval jobs in the form
    -- /StartByteValue/-/EndByteValue/. If no range was specified in the
    -- archive retrieval, then the whole archive is retrieved. In this case,
    -- /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the
    -- archive minus 1. For inventory retrieval or select jobs, this field is
    -- null.
    retrievalByteRange :: Prelude.Maybe Prelude.Text,
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
    sHA256TreeHash :: Prelude.Maybe Prelude.Text,
    -- | An Amazon SNS topic that receives notification.
    sNSTopic :: Prelude.Maybe Prelude.Text,
    -- | Contains the parameters used for a select.
    selectParameters :: Prelude.Maybe SelectParameters,
    -- | The status code can be @InProgress@, @Succeeded@, or @Failed@, and
    -- indicates the status of the job.
    statusCode :: Prelude.Maybe StatusCode,
    -- | A friendly message that describes the job status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The tier to use for a select or an archive retrieval. Valid values are
    -- @Expedited@, @Standard@, or @Bulk@. @Standard@ is the default.
    tier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the vault from which an archive
    -- retrieval was requested.
    vaultARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlacierJobDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'glacierJobDescription_action' - The job type. This value is either @ArchiveRetrieval@,
-- @InventoryRetrieval@, or @Select@.
--
-- 'archiveId', 'glacierJobDescription_archiveId' - The archive ID requested for a select job or archive retrieval.
-- Otherwise, this field is null.
--
-- 'archiveSHA256TreeHash', 'glacierJobDescription_archiveSHA256TreeHash' - The SHA256 tree hash of the entire archive for an archive retrieval. For
-- inventory retrieval or select jobs, this field is null.
--
-- 'archiveSizeInBytes', 'glacierJobDescription_archiveSizeInBytes' - For an archive retrieval job, this value is the size in bytes of the
-- archive being requested for download. For an inventory retrieval or
-- select job, this value is null.
--
-- 'completed', 'glacierJobDescription_completed' - The job status. When a job is completed, you get the job\'s output using
-- Get Job Output (GET output).
--
-- 'completionDate', 'glacierJobDescription_completionDate' - The UTC time that the job request completed. While the job is in
-- progress, the value is null.
--
-- 'creationDate', 'glacierJobDescription_creationDate' - The UTC date when the job was created. This value is a string
-- representation of ISO 8601 date format, for example
-- @\"2012-03-20T17:03:43.221Z\"@.
--
-- 'inventoryRetrievalParameters', 'glacierJobDescription_inventoryRetrievalParameters' - Parameters used for range inventory retrieval.
--
-- 'inventorySizeInBytes', 'glacierJobDescription_inventorySizeInBytes' - For an inventory retrieval job, this value is the size in bytes of the
-- inventory requested for download. For an archive retrieval or select
-- job, this value is null.
--
-- 'jobDescription', 'glacierJobDescription_jobDescription' - The job description provided when initiating the job.
--
-- 'jobId', 'glacierJobDescription_jobId' - An opaque string that identifies an Amazon S3 Glacier job.
--
-- 'jobOutputPath', 'glacierJobDescription_jobOutputPath' - Contains the job output location.
--
-- 'outputLocation', 'glacierJobDescription_outputLocation' - Contains the location where the data from the select job is stored.
--
-- 'retrievalByteRange', 'glacierJobDescription_retrievalByteRange' - The retrieved byte range for archive retrieval jobs in the form
-- /StartByteValue/-/EndByteValue/. If no range was specified in the
-- archive retrieval, then the whole archive is retrieved. In this case,
-- /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the
-- archive minus 1. For inventory retrieval or select jobs, this field is
-- null.
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
-- 'sNSTopic', 'glacierJobDescription_sNSTopic' - An Amazon SNS topic that receives notification.
--
-- 'selectParameters', 'glacierJobDescription_selectParameters' - Contains the parameters used for a select.
--
-- 'statusCode', 'glacierJobDescription_statusCode' - The status code can be @InProgress@, @Succeeded@, or @Failed@, and
-- indicates the status of the job.
--
-- 'statusMessage', 'glacierJobDescription_statusMessage' - A friendly message that describes the job status.
--
-- 'tier', 'glacierJobDescription_tier' - The tier to use for a select or an archive retrieval. Valid values are
-- @Expedited@, @Standard@, or @Bulk@. @Standard@ is the default.
--
-- 'vaultARN', 'glacierJobDescription_vaultARN' - The Amazon Resource Name (ARN) of the vault from which an archive
-- retrieval was requested.
newGlacierJobDescription ::
  GlacierJobDescription
newGlacierJobDescription =
  GlacierJobDescription'
    { action = Prelude.Nothing,
      archiveId = Prelude.Nothing,
      archiveSHA256TreeHash = Prelude.Nothing,
      archiveSizeInBytes = Prelude.Nothing,
      completed = Prelude.Nothing,
      completionDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      inventoryRetrievalParameters = Prelude.Nothing,
      inventorySizeInBytes = Prelude.Nothing,
      jobDescription = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobOutputPath = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      retrievalByteRange = Prelude.Nothing,
      sHA256TreeHash = Prelude.Nothing,
      sNSTopic = Prelude.Nothing,
      selectParameters = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tier = Prelude.Nothing,
      vaultARN = Prelude.Nothing
    }

-- | The job type. This value is either @ArchiveRetrieval@,
-- @InventoryRetrieval@, or @Select@.
glacierJobDescription_action :: Lens.Lens' GlacierJobDescription (Prelude.Maybe ActionCode)
glacierJobDescription_action = Lens.lens (\GlacierJobDescription' {action} -> action) (\s@GlacierJobDescription' {} a -> s {action = a} :: GlacierJobDescription)

-- | The archive ID requested for a select job or archive retrieval.
-- Otherwise, this field is null.
glacierJobDescription_archiveId :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_archiveId = Lens.lens (\GlacierJobDescription' {archiveId} -> archiveId) (\s@GlacierJobDescription' {} a -> s {archiveId = a} :: GlacierJobDescription)

-- | The SHA256 tree hash of the entire archive for an archive retrieval. For
-- inventory retrieval or select jobs, this field is null.
glacierJobDescription_archiveSHA256TreeHash :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_archiveSHA256TreeHash = Lens.lens (\GlacierJobDescription' {archiveSHA256TreeHash} -> archiveSHA256TreeHash) (\s@GlacierJobDescription' {} a -> s {archiveSHA256TreeHash = a} :: GlacierJobDescription)

-- | For an archive retrieval job, this value is the size in bytes of the
-- archive being requested for download. For an inventory retrieval or
-- select job, this value is null.
glacierJobDescription_archiveSizeInBytes :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Integer)
glacierJobDescription_archiveSizeInBytes = Lens.lens (\GlacierJobDescription' {archiveSizeInBytes} -> archiveSizeInBytes) (\s@GlacierJobDescription' {} a -> s {archiveSizeInBytes = a} :: GlacierJobDescription)

-- | The job status. When a job is completed, you get the job\'s output using
-- Get Job Output (GET output).
glacierJobDescription_completed :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Bool)
glacierJobDescription_completed = Lens.lens (\GlacierJobDescription' {completed} -> completed) (\s@GlacierJobDescription' {} a -> s {completed = a} :: GlacierJobDescription)

-- | The UTC time that the job request completed. While the job is in
-- progress, the value is null.
glacierJobDescription_completionDate :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_completionDate = Lens.lens (\GlacierJobDescription' {completionDate} -> completionDate) (\s@GlacierJobDescription' {} a -> s {completionDate = a} :: GlacierJobDescription)

-- | The UTC date when the job was created. This value is a string
-- representation of ISO 8601 date format, for example
-- @\"2012-03-20T17:03:43.221Z\"@.
glacierJobDescription_creationDate :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_creationDate = Lens.lens (\GlacierJobDescription' {creationDate} -> creationDate) (\s@GlacierJobDescription' {} a -> s {creationDate = a} :: GlacierJobDescription)

-- | Parameters used for range inventory retrieval.
glacierJobDescription_inventoryRetrievalParameters :: Lens.Lens' GlacierJobDescription (Prelude.Maybe InventoryRetrievalJobDescription)
glacierJobDescription_inventoryRetrievalParameters = Lens.lens (\GlacierJobDescription' {inventoryRetrievalParameters} -> inventoryRetrievalParameters) (\s@GlacierJobDescription' {} a -> s {inventoryRetrievalParameters = a} :: GlacierJobDescription)

-- | For an inventory retrieval job, this value is the size in bytes of the
-- inventory requested for download. For an archive retrieval or select
-- job, this value is null.
glacierJobDescription_inventorySizeInBytes :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Integer)
glacierJobDescription_inventorySizeInBytes = Lens.lens (\GlacierJobDescription' {inventorySizeInBytes} -> inventorySizeInBytes) (\s@GlacierJobDescription' {} a -> s {inventorySizeInBytes = a} :: GlacierJobDescription)

-- | The job description provided when initiating the job.
glacierJobDescription_jobDescription :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_jobDescription = Lens.lens (\GlacierJobDescription' {jobDescription} -> jobDescription) (\s@GlacierJobDescription' {} a -> s {jobDescription = a} :: GlacierJobDescription)

-- | An opaque string that identifies an Amazon S3 Glacier job.
glacierJobDescription_jobId :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_jobId = Lens.lens (\GlacierJobDescription' {jobId} -> jobId) (\s@GlacierJobDescription' {} a -> s {jobId = a} :: GlacierJobDescription)

-- | Contains the job output location.
glacierJobDescription_jobOutputPath :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_jobOutputPath = Lens.lens (\GlacierJobDescription' {jobOutputPath} -> jobOutputPath) (\s@GlacierJobDescription' {} a -> s {jobOutputPath = a} :: GlacierJobDescription)

-- | Contains the location where the data from the select job is stored.
glacierJobDescription_outputLocation :: Lens.Lens' GlacierJobDescription (Prelude.Maybe OutputLocation)
glacierJobDescription_outputLocation = Lens.lens (\GlacierJobDescription' {outputLocation} -> outputLocation) (\s@GlacierJobDescription' {} a -> s {outputLocation = a} :: GlacierJobDescription)

-- | The retrieved byte range for archive retrieval jobs in the form
-- /StartByteValue/-/EndByteValue/. If no range was specified in the
-- archive retrieval, then the whole archive is retrieved. In this case,
-- /StartByteValue/ equals 0 and /EndByteValue/ equals the size of the
-- archive minus 1. For inventory retrieval or select jobs, this field is
-- null.
glacierJobDescription_retrievalByteRange :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_retrievalByteRange = Lens.lens (\GlacierJobDescription' {retrievalByteRange} -> retrievalByteRange) (\s@GlacierJobDescription' {} a -> s {retrievalByteRange = a} :: GlacierJobDescription)

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
glacierJobDescription_sHA256TreeHash :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_sHA256TreeHash = Lens.lens (\GlacierJobDescription' {sHA256TreeHash} -> sHA256TreeHash) (\s@GlacierJobDescription' {} a -> s {sHA256TreeHash = a} :: GlacierJobDescription)

-- | An Amazon SNS topic that receives notification.
glacierJobDescription_sNSTopic :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_sNSTopic = Lens.lens (\GlacierJobDescription' {sNSTopic} -> sNSTopic) (\s@GlacierJobDescription' {} a -> s {sNSTopic = a} :: GlacierJobDescription)

-- | Contains the parameters used for a select.
glacierJobDescription_selectParameters :: Lens.Lens' GlacierJobDescription (Prelude.Maybe SelectParameters)
glacierJobDescription_selectParameters = Lens.lens (\GlacierJobDescription' {selectParameters} -> selectParameters) (\s@GlacierJobDescription' {} a -> s {selectParameters = a} :: GlacierJobDescription)

-- | The status code can be @InProgress@, @Succeeded@, or @Failed@, and
-- indicates the status of the job.
glacierJobDescription_statusCode :: Lens.Lens' GlacierJobDescription (Prelude.Maybe StatusCode)
glacierJobDescription_statusCode = Lens.lens (\GlacierJobDescription' {statusCode} -> statusCode) (\s@GlacierJobDescription' {} a -> s {statusCode = a} :: GlacierJobDescription)

-- | A friendly message that describes the job status.
glacierJobDescription_statusMessage :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_statusMessage = Lens.lens (\GlacierJobDescription' {statusMessage} -> statusMessage) (\s@GlacierJobDescription' {} a -> s {statusMessage = a} :: GlacierJobDescription)

-- | The tier to use for a select or an archive retrieval. Valid values are
-- @Expedited@, @Standard@, or @Bulk@. @Standard@ is the default.
glacierJobDescription_tier :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_tier = Lens.lens (\GlacierJobDescription' {tier} -> tier) (\s@GlacierJobDescription' {} a -> s {tier = a} :: GlacierJobDescription)

-- | The Amazon Resource Name (ARN) of the vault from which an archive
-- retrieval was requested.
glacierJobDescription_vaultARN :: Lens.Lens' GlacierJobDescription (Prelude.Maybe Prelude.Text)
glacierJobDescription_vaultARN = Lens.lens (\GlacierJobDescription' {vaultARN} -> vaultARN) (\s@GlacierJobDescription' {} a -> s {vaultARN = a} :: GlacierJobDescription)

instance Data.FromJSON GlacierJobDescription where
  parseJSON =
    Data.withObject
      "GlacierJobDescription"
      ( \x ->
          GlacierJobDescription'
            Prelude.<$> (x Data..:? "Action")
            Prelude.<*> (x Data..:? "ArchiveId")
            Prelude.<*> (x Data..:? "ArchiveSHA256TreeHash")
            Prelude.<*> (x Data..:? "ArchiveSizeInBytes")
            Prelude.<*> (x Data..:? "Completed")
            Prelude.<*> (x Data..:? "CompletionDate")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "InventoryRetrievalParameters")
            Prelude.<*> (x Data..:? "InventorySizeInBytes")
            Prelude.<*> (x Data..:? "JobDescription")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobOutputPath")
            Prelude.<*> (x Data..:? "OutputLocation")
            Prelude.<*> (x Data..:? "RetrievalByteRange")
            Prelude.<*> (x Data..:? "SHA256TreeHash")
            Prelude.<*> (x Data..:? "SNSTopic")
            Prelude.<*> (x Data..:? "SelectParameters")
            Prelude.<*> (x Data..:? "StatusCode")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "Tier")
            Prelude.<*> (x Data..:? "VaultARN")
      )

instance Prelude.Hashable GlacierJobDescription where
  hashWithSalt _salt GlacierJobDescription' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` archiveId
      `Prelude.hashWithSalt` archiveSHA256TreeHash
      `Prelude.hashWithSalt` archiveSizeInBytes
      `Prelude.hashWithSalt` completed
      `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` inventoryRetrievalParameters
      `Prelude.hashWithSalt` inventorySizeInBytes
      `Prelude.hashWithSalt` jobDescription
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobOutputPath
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` retrievalByteRange
      `Prelude.hashWithSalt` sHA256TreeHash
      `Prelude.hashWithSalt` sNSTopic
      `Prelude.hashWithSalt` selectParameters
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` tier
      `Prelude.hashWithSalt` vaultARN

instance Prelude.NFData GlacierJobDescription where
  rnf GlacierJobDescription' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf archiveId
      `Prelude.seq` Prelude.rnf archiveSHA256TreeHash
      `Prelude.seq` Prelude.rnf archiveSizeInBytes
      `Prelude.seq` Prelude.rnf completed
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf inventoryRetrievalParameters
      `Prelude.seq` Prelude.rnf inventorySizeInBytes
      `Prelude.seq` Prelude.rnf jobDescription
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobOutputPath
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf retrievalByteRange
      `Prelude.seq` Prelude.rnf sHA256TreeHash
      `Prelude.seq` Prelude.rnf sNSTopic
      `Prelude.seq` Prelude.rnf selectParameters
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf tier
      `Prelude.seq` Prelude.rnf vaultARN
