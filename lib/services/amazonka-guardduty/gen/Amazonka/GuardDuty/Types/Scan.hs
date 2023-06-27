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
-- Module      : Amazonka.GuardDuty.Types.Scan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Scan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.ResourceDetails
import Amazonka.GuardDuty.Types.ScanResultDetails
import Amazonka.GuardDuty.Types.ScanStatus
import Amazonka.GuardDuty.Types.ScanType
import Amazonka.GuardDuty.Types.TriggerDetails
import Amazonka.GuardDuty.Types.VolumeDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a malware scan.
--
-- /See:/ 'newScan' smart constructor.
data Scan = Scan'
  { -- | The ID for the account that belongs to the scan.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The unique detector ID of the administrator account that the request is
    -- associated with. Note that this value will be the same as the one used
    -- for @DetectorId@ if the account is an administrator.
    adminDetectorId :: Prelude.Maybe Prelude.Text,
    -- | List of volumes that were attached to the original instance to be
    -- scanned.
    attachedVolumes :: Prelude.Maybe [VolumeDetail],
    -- | The unique ID of the detector that the request is associated with.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | Represents the reason for FAILED scan status.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Represents the number of files that were scanned.
    fileCount :: Prelude.Maybe Prelude.Natural,
    -- | Represents the resources that were scanned in the scan entry.
    resourceDetails :: Prelude.Maybe ResourceDetails,
    -- | The timestamp of when the scan was finished.
    scanEndTime :: Prelude.Maybe Data.POSIX,
    -- | The unique scan ID associated with a scan entry.
    scanId :: Prelude.Maybe Prelude.Text,
    -- | Represents the result of the scan.
    scanResultDetails :: Prelude.Maybe ScanResultDetails,
    -- | The timestamp of when the scan was triggered.
    scanStartTime :: Prelude.Maybe Data.POSIX,
    -- | An enum value representing possible scan statuses.
    scanStatus :: Prelude.Maybe ScanStatus,
    -- | Specifies the scan type that invoked the malware scan.
    scanType :: Prelude.Maybe ScanType,
    -- | Represents total bytes that were scanned.
    totalBytes :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the reason why the scan was initiated.
    triggerDetails :: Prelude.Maybe TriggerDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'scan_accountId' - The ID for the account that belongs to the scan.
--
-- 'adminDetectorId', 'scan_adminDetectorId' - The unique detector ID of the administrator account that the request is
-- associated with. Note that this value will be the same as the one used
-- for @DetectorId@ if the account is an administrator.
--
-- 'attachedVolumes', 'scan_attachedVolumes' - List of volumes that were attached to the original instance to be
-- scanned.
--
-- 'detectorId', 'scan_detectorId' - The unique ID of the detector that the request is associated with.
--
-- 'failureReason', 'scan_failureReason' - Represents the reason for FAILED scan status.
--
-- 'fileCount', 'scan_fileCount' - Represents the number of files that were scanned.
--
-- 'resourceDetails', 'scan_resourceDetails' - Represents the resources that were scanned in the scan entry.
--
-- 'scanEndTime', 'scan_scanEndTime' - The timestamp of when the scan was finished.
--
-- 'scanId', 'scan_scanId' - The unique scan ID associated with a scan entry.
--
-- 'scanResultDetails', 'scan_scanResultDetails' - Represents the result of the scan.
--
-- 'scanStartTime', 'scan_scanStartTime' - The timestamp of when the scan was triggered.
--
-- 'scanStatus', 'scan_scanStatus' - An enum value representing possible scan statuses.
--
-- 'scanType', 'scan_scanType' - Specifies the scan type that invoked the malware scan.
--
-- 'totalBytes', 'scan_totalBytes' - Represents total bytes that were scanned.
--
-- 'triggerDetails', 'scan_triggerDetails' - Specifies the reason why the scan was initiated.
newScan ::
  Scan
newScan =
  Scan'
    { accountId = Prelude.Nothing,
      adminDetectorId = Prelude.Nothing,
      attachedVolumes = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      fileCount = Prelude.Nothing,
      resourceDetails = Prelude.Nothing,
      scanEndTime = Prelude.Nothing,
      scanId = Prelude.Nothing,
      scanResultDetails = Prelude.Nothing,
      scanStartTime = Prelude.Nothing,
      scanStatus = Prelude.Nothing,
      scanType = Prelude.Nothing,
      totalBytes = Prelude.Nothing,
      triggerDetails = Prelude.Nothing
    }

-- | The ID for the account that belongs to the scan.
scan_accountId :: Lens.Lens' Scan (Prelude.Maybe Prelude.Text)
scan_accountId = Lens.lens (\Scan' {accountId} -> accountId) (\s@Scan' {} a -> s {accountId = a} :: Scan)

-- | The unique detector ID of the administrator account that the request is
-- associated with. Note that this value will be the same as the one used
-- for @DetectorId@ if the account is an administrator.
scan_adminDetectorId :: Lens.Lens' Scan (Prelude.Maybe Prelude.Text)
scan_adminDetectorId = Lens.lens (\Scan' {adminDetectorId} -> adminDetectorId) (\s@Scan' {} a -> s {adminDetectorId = a} :: Scan)

-- | List of volumes that were attached to the original instance to be
-- scanned.
scan_attachedVolumes :: Lens.Lens' Scan (Prelude.Maybe [VolumeDetail])
scan_attachedVolumes = Lens.lens (\Scan' {attachedVolumes} -> attachedVolumes) (\s@Scan' {} a -> s {attachedVolumes = a} :: Scan) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the detector that the request is associated with.
scan_detectorId :: Lens.Lens' Scan (Prelude.Maybe Prelude.Text)
scan_detectorId = Lens.lens (\Scan' {detectorId} -> detectorId) (\s@Scan' {} a -> s {detectorId = a} :: Scan)

-- | Represents the reason for FAILED scan status.
scan_failureReason :: Lens.Lens' Scan (Prelude.Maybe Prelude.Text)
scan_failureReason = Lens.lens (\Scan' {failureReason} -> failureReason) (\s@Scan' {} a -> s {failureReason = a} :: Scan)

-- | Represents the number of files that were scanned.
scan_fileCount :: Lens.Lens' Scan (Prelude.Maybe Prelude.Natural)
scan_fileCount = Lens.lens (\Scan' {fileCount} -> fileCount) (\s@Scan' {} a -> s {fileCount = a} :: Scan)

-- | Represents the resources that were scanned in the scan entry.
scan_resourceDetails :: Lens.Lens' Scan (Prelude.Maybe ResourceDetails)
scan_resourceDetails = Lens.lens (\Scan' {resourceDetails} -> resourceDetails) (\s@Scan' {} a -> s {resourceDetails = a} :: Scan)

-- | The timestamp of when the scan was finished.
scan_scanEndTime :: Lens.Lens' Scan (Prelude.Maybe Prelude.UTCTime)
scan_scanEndTime = Lens.lens (\Scan' {scanEndTime} -> scanEndTime) (\s@Scan' {} a -> s {scanEndTime = a} :: Scan) Prelude.. Lens.mapping Data._Time

-- | The unique scan ID associated with a scan entry.
scan_scanId :: Lens.Lens' Scan (Prelude.Maybe Prelude.Text)
scan_scanId = Lens.lens (\Scan' {scanId} -> scanId) (\s@Scan' {} a -> s {scanId = a} :: Scan)

-- | Represents the result of the scan.
scan_scanResultDetails :: Lens.Lens' Scan (Prelude.Maybe ScanResultDetails)
scan_scanResultDetails = Lens.lens (\Scan' {scanResultDetails} -> scanResultDetails) (\s@Scan' {} a -> s {scanResultDetails = a} :: Scan)

-- | The timestamp of when the scan was triggered.
scan_scanStartTime :: Lens.Lens' Scan (Prelude.Maybe Prelude.UTCTime)
scan_scanStartTime = Lens.lens (\Scan' {scanStartTime} -> scanStartTime) (\s@Scan' {} a -> s {scanStartTime = a} :: Scan) Prelude.. Lens.mapping Data._Time

-- | An enum value representing possible scan statuses.
scan_scanStatus :: Lens.Lens' Scan (Prelude.Maybe ScanStatus)
scan_scanStatus = Lens.lens (\Scan' {scanStatus} -> scanStatus) (\s@Scan' {} a -> s {scanStatus = a} :: Scan)

-- | Specifies the scan type that invoked the malware scan.
scan_scanType :: Lens.Lens' Scan (Prelude.Maybe ScanType)
scan_scanType = Lens.lens (\Scan' {scanType} -> scanType) (\s@Scan' {} a -> s {scanType = a} :: Scan)

-- | Represents total bytes that were scanned.
scan_totalBytes :: Lens.Lens' Scan (Prelude.Maybe Prelude.Natural)
scan_totalBytes = Lens.lens (\Scan' {totalBytes} -> totalBytes) (\s@Scan' {} a -> s {totalBytes = a} :: Scan)

-- | Specifies the reason why the scan was initiated.
scan_triggerDetails :: Lens.Lens' Scan (Prelude.Maybe TriggerDetails)
scan_triggerDetails = Lens.lens (\Scan' {triggerDetails} -> triggerDetails) (\s@Scan' {} a -> s {triggerDetails = a} :: Scan)

instance Data.FromJSON Scan where
  parseJSON =
    Data.withObject
      "Scan"
      ( \x ->
          Scan'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "adminDetectorId")
            Prelude.<*> ( x
                            Data..:? "attachedVolumes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "detectorId")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "fileCount")
            Prelude.<*> (x Data..:? "resourceDetails")
            Prelude.<*> (x Data..:? "scanEndTime")
            Prelude.<*> (x Data..:? "scanId")
            Prelude.<*> (x Data..:? "scanResultDetails")
            Prelude.<*> (x Data..:? "scanStartTime")
            Prelude.<*> (x Data..:? "scanStatus")
            Prelude.<*> (x Data..:? "scanType")
            Prelude.<*> (x Data..:? "totalBytes")
            Prelude.<*> (x Data..:? "triggerDetails")
      )

instance Prelude.Hashable Scan where
  hashWithSalt _salt Scan' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` adminDetectorId
      `Prelude.hashWithSalt` attachedVolumes
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` fileCount
      `Prelude.hashWithSalt` resourceDetails
      `Prelude.hashWithSalt` scanEndTime
      `Prelude.hashWithSalt` scanId
      `Prelude.hashWithSalt` scanResultDetails
      `Prelude.hashWithSalt` scanStartTime
      `Prelude.hashWithSalt` scanStatus
      `Prelude.hashWithSalt` scanType
      `Prelude.hashWithSalt` totalBytes
      `Prelude.hashWithSalt` triggerDetails

instance Prelude.NFData Scan where
  rnf Scan' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf adminDetectorId
      `Prelude.seq` Prelude.rnf attachedVolumes
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf fileCount
      `Prelude.seq` Prelude.rnf resourceDetails
      `Prelude.seq` Prelude.rnf scanEndTime
      `Prelude.seq` Prelude.rnf scanId
      `Prelude.seq` Prelude.rnf scanResultDetails
      `Prelude.seq` Prelude.rnf scanStartTime
      `Prelude.seq` Prelude.rnf scanStatus
      `Prelude.seq` Prelude.rnf scanType
      `Prelude.seq` Prelude.rnf totalBytes
      `Prelude.seq` Prelude.rnf triggerDetails
