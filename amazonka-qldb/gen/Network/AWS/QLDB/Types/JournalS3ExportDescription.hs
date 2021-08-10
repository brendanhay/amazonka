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
-- Module      : Network.AWS.QLDB.Types.JournalS3ExportDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types.JournalS3ExportDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types.ExportStatus
import Network.AWS.QLDB.Types.S3ExportConfiguration

-- | The information about a journal export job, including the ledger name,
-- export ID, when it was created, current status, and its start and end
-- time export parameters.
--
-- /See:/ 'newJournalS3ExportDescription' smart constructor.
data JournalS3ExportDescription = JournalS3ExportDescription'
  { -- | The name of the ledger.
    ledgerName :: Prelude.Text,
    -- | The unique ID of the journal export job.
    exportId :: Prelude.Text,
    -- | The date and time, in epoch time format, when the export job was
    -- created. (Epoch time format is the number of seconds elapsed since
    -- 12:00:00 AM January 1, 1970 UTC.)
    exportCreationTime :: Core.POSIX,
    -- | The current state of the journal export job.
    status :: ExportStatus,
    -- | The inclusive start date and time for the range of journal contents that
    -- are specified in the original export request.
    inclusiveStartTime :: Core.POSIX,
    -- | The exclusive end date and time for the range of journal contents that
    -- are specified in the original export request.
    exclusiveEndTime :: Core.POSIX,
    s3ExportConfiguration :: S3ExportConfiguration,
    -- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
    -- permissions for a journal export job to do the following:
    --
    -- -   Write objects into your Amazon Simple Storage Service (Amazon S3)
    --     bucket.
    --
    -- -   (Optional) Use your customer master key (CMK) in AWS Key Management
    --     Service (AWS KMS) for server-side encryption of your exported data.
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
-- 'ledgerName', 'journalS3ExportDescription_ledgerName' - The name of the ledger.
--
-- 'exportId', 'journalS3ExportDescription_exportId' - The unique ID of the journal export job.
--
-- 'exportCreationTime', 'journalS3ExportDescription_exportCreationTime' - The date and time, in epoch time format, when the export job was
-- created. (Epoch time format is the number of seconds elapsed since
-- 12:00:00 AM January 1, 1970 UTC.)
--
-- 'status', 'journalS3ExportDescription_status' - The current state of the journal export job.
--
-- 'inclusiveStartTime', 'journalS3ExportDescription_inclusiveStartTime' - The inclusive start date and time for the range of journal contents that
-- are specified in the original export request.
--
-- 'exclusiveEndTime', 'journalS3ExportDescription_exclusiveEndTime' - The exclusive end date and time for the range of journal contents that
-- are specified in the original export request.
--
-- 's3ExportConfiguration', 'journalS3ExportDescription_s3ExportConfiguration' - Undocumented member.
--
-- 'roleArn', 'journalS3ExportDescription_roleArn' - The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal export job to do the following:
--
-- -   Write objects into your Amazon Simple Storage Service (Amazon S3)
--     bucket.
--
-- -   (Optional) Use your customer master key (CMK) in AWS Key Management
--     Service (AWS KMS) for server-side encryption of your exported data.
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
      { ledgerName =
          pLedgerName_,
        exportId = pExportId_,
        exportCreationTime =
          Core._Time Lens.# pExportCreationTime_,
        status = pStatus_,
        inclusiveStartTime =
          Core._Time Lens.# pInclusiveStartTime_,
        exclusiveEndTime =
          Core._Time Lens.# pExclusiveEndTime_,
        s3ExportConfiguration = pS3ExportConfiguration_,
        roleArn = pRoleArn_
      }

-- | The name of the ledger.
journalS3ExportDescription_ledgerName :: Lens.Lens' JournalS3ExportDescription Prelude.Text
journalS3ExportDescription_ledgerName = Lens.lens (\JournalS3ExportDescription' {ledgerName} -> ledgerName) (\s@JournalS3ExportDescription' {} a -> s {ledgerName = a} :: JournalS3ExportDescription)

-- | The unique ID of the journal export job.
journalS3ExportDescription_exportId :: Lens.Lens' JournalS3ExportDescription Prelude.Text
journalS3ExportDescription_exportId = Lens.lens (\JournalS3ExportDescription' {exportId} -> exportId) (\s@JournalS3ExportDescription' {} a -> s {exportId = a} :: JournalS3ExportDescription)

-- | The date and time, in epoch time format, when the export job was
-- created. (Epoch time format is the number of seconds elapsed since
-- 12:00:00 AM January 1, 1970 UTC.)
journalS3ExportDescription_exportCreationTime :: Lens.Lens' JournalS3ExportDescription Prelude.UTCTime
journalS3ExportDescription_exportCreationTime = Lens.lens (\JournalS3ExportDescription' {exportCreationTime} -> exportCreationTime) (\s@JournalS3ExportDescription' {} a -> s {exportCreationTime = a} :: JournalS3ExportDescription) Prelude.. Core._Time

-- | The current state of the journal export job.
journalS3ExportDescription_status :: Lens.Lens' JournalS3ExportDescription ExportStatus
journalS3ExportDescription_status = Lens.lens (\JournalS3ExportDescription' {status} -> status) (\s@JournalS3ExportDescription' {} a -> s {status = a} :: JournalS3ExportDescription)

-- | The inclusive start date and time for the range of journal contents that
-- are specified in the original export request.
journalS3ExportDescription_inclusiveStartTime :: Lens.Lens' JournalS3ExportDescription Prelude.UTCTime
journalS3ExportDescription_inclusiveStartTime = Lens.lens (\JournalS3ExportDescription' {inclusiveStartTime} -> inclusiveStartTime) (\s@JournalS3ExportDescription' {} a -> s {inclusiveStartTime = a} :: JournalS3ExportDescription) Prelude.. Core._Time

-- | The exclusive end date and time for the range of journal contents that
-- are specified in the original export request.
journalS3ExportDescription_exclusiveEndTime :: Lens.Lens' JournalS3ExportDescription Prelude.UTCTime
journalS3ExportDescription_exclusiveEndTime = Lens.lens (\JournalS3ExportDescription' {exclusiveEndTime} -> exclusiveEndTime) (\s@JournalS3ExportDescription' {} a -> s {exclusiveEndTime = a} :: JournalS3ExportDescription) Prelude.. Core._Time

-- | Undocumented member.
journalS3ExportDescription_s3ExportConfiguration :: Lens.Lens' JournalS3ExportDescription S3ExportConfiguration
journalS3ExportDescription_s3ExportConfiguration = Lens.lens (\JournalS3ExportDescription' {s3ExportConfiguration} -> s3ExportConfiguration) (\s@JournalS3ExportDescription' {} a -> s {s3ExportConfiguration = a} :: JournalS3ExportDescription)

-- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal export job to do the following:
--
-- -   Write objects into your Amazon Simple Storage Service (Amazon S3)
--     bucket.
--
-- -   (Optional) Use your customer master key (CMK) in AWS Key Management
--     Service (AWS KMS) for server-side encryption of your exported data.
journalS3ExportDescription_roleArn :: Lens.Lens' JournalS3ExportDescription Prelude.Text
journalS3ExportDescription_roleArn = Lens.lens (\JournalS3ExportDescription' {roleArn} -> roleArn) (\s@JournalS3ExportDescription' {} a -> s {roleArn = a} :: JournalS3ExportDescription)

instance Core.FromJSON JournalS3ExportDescription where
  parseJSON =
    Core.withObject
      "JournalS3ExportDescription"
      ( \x ->
          JournalS3ExportDescription'
            Prelude.<$> (x Core..: "LedgerName")
            Prelude.<*> (x Core..: "ExportId")
            Prelude.<*> (x Core..: "ExportCreationTime")
            Prelude.<*> (x Core..: "Status")
            Prelude.<*> (x Core..: "InclusiveStartTime")
            Prelude.<*> (x Core..: "ExclusiveEndTime")
            Prelude.<*> (x Core..: "S3ExportConfiguration")
            Prelude.<*> (x Core..: "RoleArn")
      )

instance Prelude.Hashable JournalS3ExportDescription

instance Prelude.NFData JournalS3ExportDescription
