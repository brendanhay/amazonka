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
-- Module      : Amazonka.QLDB.Types.JournalS3ExportDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.JournalS3ExportDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types.ExportStatus
import Amazonka.QLDB.Types.OutputFormat
import Amazonka.QLDB.Types.S3ExportConfiguration

-- | Information about a journal export job, including the ledger name,
-- export ID, creation time, current status, and the parameters of the
-- original export creation request.
--
-- /See:/ 'newJournalS3ExportDescription' smart constructor.
data JournalS3ExportDescription = JournalS3ExportDescription'
  { -- | The output format of the exported journal data.
    outputFormat :: Prelude.Maybe OutputFormat,
    -- | The name of the ledger.
    ledgerName :: Prelude.Text,
    -- | The UUID (represented in Base62-encoded text) of the journal export job.
    exportId :: Prelude.Text,
    -- | The date and time, in epoch time format, when the export job was
    -- created. (Epoch time format is the number of seconds elapsed since
    -- 12:00:00 AM January 1, 1970 UTC.)
    exportCreationTime :: Data.POSIX,
    -- | The current state of the journal export job.
    status :: ExportStatus,
    -- | The inclusive start date and time for the range of journal contents that
    -- was specified in the original export request.
    inclusiveStartTime :: Data.POSIX,
    -- | The exclusive end date and time for the range of journal contents that
    -- was specified in the original export request.
    exclusiveEndTime :: Data.POSIX,
    s3ExportConfiguration :: S3ExportConfiguration,
    -- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
    -- permissions for a journal export job to do the following:
    --
    -- -   Write objects into your Amazon Simple Storage Service (Amazon S3)
    --     bucket.
    --
    -- -   (Optional) Use your customer managed key in Key Management Service
    --     (KMS) for server-side encryption of your exported data.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JournalS3ExportDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputFormat', 'journalS3ExportDescription_outputFormat' - The output format of the exported journal data.
--
-- 'ledgerName', 'journalS3ExportDescription_ledgerName' - The name of the ledger.
--
-- 'exportId', 'journalS3ExportDescription_exportId' - The UUID (represented in Base62-encoded text) of the journal export job.
--
-- 'exportCreationTime', 'journalS3ExportDescription_exportCreationTime' - The date and time, in epoch time format, when the export job was
-- created. (Epoch time format is the number of seconds elapsed since
-- 12:00:00 AM January 1, 1970 UTC.)
--
-- 'status', 'journalS3ExportDescription_status' - The current state of the journal export job.
--
-- 'inclusiveStartTime', 'journalS3ExportDescription_inclusiveStartTime' - The inclusive start date and time for the range of journal contents that
-- was specified in the original export request.
--
-- 'exclusiveEndTime', 'journalS3ExportDescription_exclusiveEndTime' - The exclusive end date and time for the range of journal contents that
-- was specified in the original export request.
--
-- 's3ExportConfiguration', 'journalS3ExportDescription_s3ExportConfiguration' - Undocumented member.
--
-- 'roleArn', 'journalS3ExportDescription_roleArn' - The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal export job to do the following:
--
-- -   Write objects into your Amazon Simple Storage Service (Amazon S3)
--     bucket.
--
-- -   (Optional) Use your customer managed key in Key Management Service
--     (KMS) for server-side encryption of your exported data.
newJournalS3ExportDescription ::
  -- | 'ledgerName'
  Prelude.Text ->
  -- | 'exportId'
  Prelude.Text ->
  -- | 'exportCreationTime'
  Prelude.UTCTime ->
  -- | 'status'
  ExportStatus ->
  -- | 'inclusiveStartTime'
  Prelude.UTCTime ->
  -- | 'exclusiveEndTime'
  Prelude.UTCTime ->
  -- | 's3ExportConfiguration'
  S3ExportConfiguration ->
  -- | 'roleArn'
  Prelude.Text ->
  JournalS3ExportDescription
newJournalS3ExportDescription
  pLedgerName_
  pExportId_
  pExportCreationTime_
  pStatus_
  pInclusiveStartTime_
  pExclusiveEndTime_
  pS3ExportConfiguration_
  pRoleArn_ =
    JournalS3ExportDescription'
      { outputFormat =
          Prelude.Nothing,
        ledgerName = pLedgerName_,
        exportId = pExportId_,
        exportCreationTime =
          Data._Time Lens.# pExportCreationTime_,
        status = pStatus_,
        inclusiveStartTime =
          Data._Time Lens.# pInclusiveStartTime_,
        exclusiveEndTime =
          Data._Time Lens.# pExclusiveEndTime_,
        s3ExportConfiguration = pS3ExportConfiguration_,
        roleArn = pRoleArn_
      }

-- | The output format of the exported journal data.
journalS3ExportDescription_outputFormat :: Lens.Lens' JournalS3ExportDescription (Prelude.Maybe OutputFormat)
journalS3ExportDescription_outputFormat = Lens.lens (\JournalS3ExportDescription' {outputFormat} -> outputFormat) (\s@JournalS3ExportDescription' {} a -> s {outputFormat = a} :: JournalS3ExportDescription)

-- | The name of the ledger.
journalS3ExportDescription_ledgerName :: Lens.Lens' JournalS3ExportDescription Prelude.Text
journalS3ExportDescription_ledgerName = Lens.lens (\JournalS3ExportDescription' {ledgerName} -> ledgerName) (\s@JournalS3ExportDescription' {} a -> s {ledgerName = a} :: JournalS3ExportDescription)

-- | The UUID (represented in Base62-encoded text) of the journal export job.
journalS3ExportDescription_exportId :: Lens.Lens' JournalS3ExportDescription Prelude.Text
journalS3ExportDescription_exportId = Lens.lens (\JournalS3ExportDescription' {exportId} -> exportId) (\s@JournalS3ExportDescription' {} a -> s {exportId = a} :: JournalS3ExportDescription)

-- | The date and time, in epoch time format, when the export job was
-- created. (Epoch time format is the number of seconds elapsed since
-- 12:00:00 AM January 1, 1970 UTC.)
journalS3ExportDescription_exportCreationTime :: Lens.Lens' JournalS3ExportDescription Prelude.UTCTime
journalS3ExportDescription_exportCreationTime = Lens.lens (\JournalS3ExportDescription' {exportCreationTime} -> exportCreationTime) (\s@JournalS3ExportDescription' {} a -> s {exportCreationTime = a} :: JournalS3ExportDescription) Prelude.. Data._Time

-- | The current state of the journal export job.
journalS3ExportDescription_status :: Lens.Lens' JournalS3ExportDescription ExportStatus
journalS3ExportDescription_status = Lens.lens (\JournalS3ExportDescription' {status} -> status) (\s@JournalS3ExportDescription' {} a -> s {status = a} :: JournalS3ExportDescription)

-- | The inclusive start date and time for the range of journal contents that
-- was specified in the original export request.
journalS3ExportDescription_inclusiveStartTime :: Lens.Lens' JournalS3ExportDescription Prelude.UTCTime
journalS3ExportDescription_inclusiveStartTime = Lens.lens (\JournalS3ExportDescription' {inclusiveStartTime} -> inclusiveStartTime) (\s@JournalS3ExportDescription' {} a -> s {inclusiveStartTime = a} :: JournalS3ExportDescription) Prelude.. Data._Time

-- | The exclusive end date and time for the range of journal contents that
-- was specified in the original export request.
journalS3ExportDescription_exclusiveEndTime :: Lens.Lens' JournalS3ExportDescription Prelude.UTCTime
journalS3ExportDescription_exclusiveEndTime = Lens.lens (\JournalS3ExportDescription' {exclusiveEndTime} -> exclusiveEndTime) (\s@JournalS3ExportDescription' {} a -> s {exclusiveEndTime = a} :: JournalS3ExportDescription) Prelude.. Data._Time

-- | Undocumented member.
journalS3ExportDescription_s3ExportConfiguration :: Lens.Lens' JournalS3ExportDescription S3ExportConfiguration
journalS3ExportDescription_s3ExportConfiguration = Lens.lens (\JournalS3ExportDescription' {s3ExportConfiguration} -> s3ExportConfiguration) (\s@JournalS3ExportDescription' {} a -> s {s3ExportConfiguration = a} :: JournalS3ExportDescription)

-- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal export job to do the following:
--
-- -   Write objects into your Amazon Simple Storage Service (Amazon S3)
--     bucket.
--
-- -   (Optional) Use your customer managed key in Key Management Service
--     (KMS) for server-side encryption of your exported data.
journalS3ExportDescription_roleArn :: Lens.Lens' JournalS3ExportDescription Prelude.Text
journalS3ExportDescription_roleArn = Lens.lens (\JournalS3ExportDescription' {roleArn} -> roleArn) (\s@JournalS3ExportDescription' {} a -> s {roleArn = a} :: JournalS3ExportDescription)

instance Data.FromJSON JournalS3ExportDescription where
  parseJSON =
    Data.withObject
      "JournalS3ExportDescription"
      ( \x ->
          JournalS3ExportDescription'
            Prelude.<$> (x Data..:? "OutputFormat")
            Prelude.<*> (x Data..: "LedgerName")
            Prelude.<*> (x Data..: "ExportId")
            Prelude.<*> (x Data..: "ExportCreationTime")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "InclusiveStartTime")
            Prelude.<*> (x Data..: "ExclusiveEndTime")
            Prelude.<*> (x Data..: "S3ExportConfiguration")
            Prelude.<*> (x Data..: "RoleArn")
      )

instance Prelude.Hashable JournalS3ExportDescription where
  hashWithSalt _salt JournalS3ExportDescription' {..} =
    _salt
      `Prelude.hashWithSalt` outputFormat
      `Prelude.hashWithSalt` ledgerName
      `Prelude.hashWithSalt` exportId
      `Prelude.hashWithSalt` exportCreationTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` inclusiveStartTime
      `Prelude.hashWithSalt` exclusiveEndTime
      `Prelude.hashWithSalt` s3ExportConfiguration
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData JournalS3ExportDescription where
  rnf JournalS3ExportDescription' {..} =
    Prelude.rnf outputFormat
      `Prelude.seq` Prelude.rnf ledgerName
      `Prelude.seq` Prelude.rnf exportId
      `Prelude.seq` Prelude.rnf exportCreationTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf inclusiveStartTime
      `Prelude.seq` Prelude.rnf exclusiveEndTime
      `Prelude.seq` Prelude.rnf s3ExportConfiguration
      `Prelude.seq` Prelude.rnf roleArn
