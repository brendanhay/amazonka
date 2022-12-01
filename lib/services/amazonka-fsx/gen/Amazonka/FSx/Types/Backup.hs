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
-- Module      : Amazonka.FSx.Types.Backup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.Backup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.ActiveDirectoryBackupAttributes
import Amazonka.FSx.Types.BackupFailureDetails
import Amazonka.FSx.Types.BackupLifecycle
import Amazonka.FSx.Types.BackupType
import Amazonka.FSx.Types.FileSystem
import Amazonka.FSx.Types.ResourceType
import Amazonka.FSx.Types.Tag
import Amazonka.FSx.Types.Volume
import qualified Amazonka.Prelude as Prelude

-- | A backup of an Amazon FSx for Windows File Server, Amazon FSx for Lustre
-- file system, Amazon FSx for NetApp ONTAP volume, or Amazon FSx for
-- OpenZFS file system.
--
-- /See:/ 'newBackup' smart constructor.
data Backup = Backup'
  { -- | The tags associated with a particular file system.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Specifies the resource type that\'s backed up.
    resourceType :: Prelude.Maybe ResourceType,
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The source Region of the backup. Specifies the Region from where this
    -- backup is copied.
    sourceBackupRegion :: Prelude.Maybe Prelude.Text,
    -- | Details explaining any failures that occurred when creating a backup.
    failureDetails :: Prelude.Maybe BackupFailureDetails,
    -- | The configuration of the self-managed Microsoft Active Directory
    -- directory to which the Windows File Server instance is joined.
    directoryInformation :: Prelude.Maybe ActiveDirectoryBackupAttributes,
    -- | The ID of the Key Management Service (KMS) key used to encrypt the
    -- backup of the Amazon FSx file system\'s data at rest.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    volume :: Prelude.Maybe Volume,
    -- | The Amazon Resource Name (ARN) for the backup resource.
    resourceARN :: Prelude.Maybe Prelude.Text,
    progressPercent :: Prelude.Maybe Prelude.Natural,
    sourceBackupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the backup.
    backupId :: Prelude.Text,
    -- | The lifecycle status of the backup.
    --
    -- -   @AVAILABLE@ - The backup is fully available.
    --
    -- -   @PENDING@ - For user-initiated backups on Lustre file systems only;
    --     Amazon FSx hasn\'t started creating the backup.
    --
    -- -   @CREATING@ - Amazon FSx is creating the backup.
    --
    -- -   @TRANSFERRING@ - For user-initiated backups on Lustre file systems
    --     only; Amazon FSx is transferring the backup to Amazon S3.
    --
    -- -   @COPYING@ - Amazon FSx is copying the backup.
    --
    -- -   @DELETED@ - Amazon FSx deleted the backup and it\'s no longer
    --     available.
    --
    -- -   @FAILED@ - Amazon FSx couldn\'t finish the backup.
    lifecycle :: BackupLifecycle,
    -- | The type of the file-system backup.
    type' :: BackupType,
    -- | The time when a particular backup was created.
    creationTime :: Core.POSIX,
    -- | The metadata of the file system associated with the backup. This
    -- metadata is persisted even if the file system is deleted.
    fileSystem :: FileSystem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Backup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'backup_tags' - The tags associated with a particular file system.
--
-- 'resourceType', 'backup_resourceType' - Specifies the resource type that\'s backed up.
--
-- 'ownerId', 'backup_ownerId' - Undocumented member.
--
-- 'sourceBackupRegion', 'backup_sourceBackupRegion' - The source Region of the backup. Specifies the Region from where this
-- backup is copied.
--
-- 'failureDetails', 'backup_failureDetails' - Details explaining any failures that occurred when creating a backup.
--
-- 'directoryInformation', 'backup_directoryInformation' - The configuration of the self-managed Microsoft Active Directory
-- directory to which the Windows File Server instance is joined.
--
-- 'kmsKeyId', 'backup_kmsKeyId' - The ID of the Key Management Service (KMS) key used to encrypt the
-- backup of the Amazon FSx file system\'s data at rest.
--
-- 'volume', 'backup_volume' - Undocumented member.
--
-- 'resourceARN', 'backup_resourceARN' - The Amazon Resource Name (ARN) for the backup resource.
--
-- 'progressPercent', 'backup_progressPercent' - Undocumented member.
--
-- 'sourceBackupId', 'backup_sourceBackupId' - Undocumented member.
--
-- 'backupId', 'backup_backupId' - The ID of the backup.
--
-- 'lifecycle', 'backup_lifecycle' - The lifecycle status of the backup.
--
-- -   @AVAILABLE@ - The backup is fully available.
--
-- -   @PENDING@ - For user-initiated backups on Lustre file systems only;
--     Amazon FSx hasn\'t started creating the backup.
--
-- -   @CREATING@ - Amazon FSx is creating the backup.
--
-- -   @TRANSFERRING@ - For user-initiated backups on Lustre file systems
--     only; Amazon FSx is transferring the backup to Amazon S3.
--
-- -   @COPYING@ - Amazon FSx is copying the backup.
--
-- -   @DELETED@ - Amazon FSx deleted the backup and it\'s no longer
--     available.
--
-- -   @FAILED@ - Amazon FSx couldn\'t finish the backup.
--
-- 'type'', 'backup_type' - The type of the file-system backup.
--
-- 'creationTime', 'backup_creationTime' - The time when a particular backup was created.
--
-- 'fileSystem', 'backup_fileSystem' - The metadata of the file system associated with the backup. This
-- metadata is persisted even if the file system is deleted.
newBackup ::
  -- | 'backupId'
  Prelude.Text ->
  -- | 'lifecycle'
  BackupLifecycle ->
  -- | 'type''
  BackupType ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'fileSystem'
  FileSystem ->
  Backup
newBackup
  pBackupId_
  pLifecycle_
  pType_
  pCreationTime_
  pFileSystem_ =
    Backup'
      { tags = Prelude.Nothing,
        resourceType = Prelude.Nothing,
        ownerId = Prelude.Nothing,
        sourceBackupRegion = Prelude.Nothing,
        failureDetails = Prelude.Nothing,
        directoryInformation = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        volume = Prelude.Nothing,
        resourceARN = Prelude.Nothing,
        progressPercent = Prelude.Nothing,
        sourceBackupId = Prelude.Nothing,
        backupId = pBackupId_,
        lifecycle = pLifecycle_,
        type' = pType_,
        creationTime = Core._Time Lens.# pCreationTime_,
        fileSystem = pFileSystem_
      }

-- | The tags associated with a particular file system.
backup_tags :: Lens.Lens' Backup (Prelude.Maybe (Prelude.NonEmpty Tag))
backup_tags = Lens.lens (\Backup' {tags} -> tags) (\s@Backup' {} a -> s {tags = a} :: Backup) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the resource type that\'s backed up.
backup_resourceType :: Lens.Lens' Backup (Prelude.Maybe ResourceType)
backup_resourceType = Lens.lens (\Backup' {resourceType} -> resourceType) (\s@Backup' {} a -> s {resourceType = a} :: Backup)

-- | Undocumented member.
backup_ownerId :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_ownerId = Lens.lens (\Backup' {ownerId} -> ownerId) (\s@Backup' {} a -> s {ownerId = a} :: Backup)

-- | The source Region of the backup. Specifies the Region from where this
-- backup is copied.
backup_sourceBackupRegion :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_sourceBackupRegion = Lens.lens (\Backup' {sourceBackupRegion} -> sourceBackupRegion) (\s@Backup' {} a -> s {sourceBackupRegion = a} :: Backup)

-- | Details explaining any failures that occurred when creating a backup.
backup_failureDetails :: Lens.Lens' Backup (Prelude.Maybe BackupFailureDetails)
backup_failureDetails = Lens.lens (\Backup' {failureDetails} -> failureDetails) (\s@Backup' {} a -> s {failureDetails = a} :: Backup)

-- | The configuration of the self-managed Microsoft Active Directory
-- directory to which the Windows File Server instance is joined.
backup_directoryInformation :: Lens.Lens' Backup (Prelude.Maybe ActiveDirectoryBackupAttributes)
backup_directoryInformation = Lens.lens (\Backup' {directoryInformation} -> directoryInformation) (\s@Backup' {} a -> s {directoryInformation = a} :: Backup)

-- | The ID of the Key Management Service (KMS) key used to encrypt the
-- backup of the Amazon FSx file system\'s data at rest.
backup_kmsKeyId :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_kmsKeyId = Lens.lens (\Backup' {kmsKeyId} -> kmsKeyId) (\s@Backup' {} a -> s {kmsKeyId = a} :: Backup)

-- | Undocumented member.
backup_volume :: Lens.Lens' Backup (Prelude.Maybe Volume)
backup_volume = Lens.lens (\Backup' {volume} -> volume) (\s@Backup' {} a -> s {volume = a} :: Backup)

-- | The Amazon Resource Name (ARN) for the backup resource.
backup_resourceARN :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_resourceARN = Lens.lens (\Backup' {resourceARN} -> resourceARN) (\s@Backup' {} a -> s {resourceARN = a} :: Backup)

-- | Undocumented member.
backup_progressPercent :: Lens.Lens' Backup (Prelude.Maybe Prelude.Natural)
backup_progressPercent = Lens.lens (\Backup' {progressPercent} -> progressPercent) (\s@Backup' {} a -> s {progressPercent = a} :: Backup)

-- | Undocumented member.
backup_sourceBackupId :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_sourceBackupId = Lens.lens (\Backup' {sourceBackupId} -> sourceBackupId) (\s@Backup' {} a -> s {sourceBackupId = a} :: Backup)

-- | The ID of the backup.
backup_backupId :: Lens.Lens' Backup Prelude.Text
backup_backupId = Lens.lens (\Backup' {backupId} -> backupId) (\s@Backup' {} a -> s {backupId = a} :: Backup)

-- | The lifecycle status of the backup.
--
-- -   @AVAILABLE@ - The backup is fully available.
--
-- -   @PENDING@ - For user-initiated backups on Lustre file systems only;
--     Amazon FSx hasn\'t started creating the backup.
--
-- -   @CREATING@ - Amazon FSx is creating the backup.
--
-- -   @TRANSFERRING@ - For user-initiated backups on Lustre file systems
--     only; Amazon FSx is transferring the backup to Amazon S3.
--
-- -   @COPYING@ - Amazon FSx is copying the backup.
--
-- -   @DELETED@ - Amazon FSx deleted the backup and it\'s no longer
--     available.
--
-- -   @FAILED@ - Amazon FSx couldn\'t finish the backup.
backup_lifecycle :: Lens.Lens' Backup BackupLifecycle
backup_lifecycle = Lens.lens (\Backup' {lifecycle} -> lifecycle) (\s@Backup' {} a -> s {lifecycle = a} :: Backup)

-- | The type of the file-system backup.
backup_type :: Lens.Lens' Backup BackupType
backup_type = Lens.lens (\Backup' {type'} -> type') (\s@Backup' {} a -> s {type' = a} :: Backup)

-- | The time when a particular backup was created.
backup_creationTime :: Lens.Lens' Backup Prelude.UTCTime
backup_creationTime = Lens.lens (\Backup' {creationTime} -> creationTime) (\s@Backup' {} a -> s {creationTime = a} :: Backup) Prelude.. Core._Time

-- | The metadata of the file system associated with the backup. This
-- metadata is persisted even if the file system is deleted.
backup_fileSystem :: Lens.Lens' Backup FileSystem
backup_fileSystem = Lens.lens (\Backup' {fileSystem} -> fileSystem) (\s@Backup' {} a -> s {fileSystem = a} :: Backup)

instance Core.FromJSON Backup where
  parseJSON =
    Core.withObject
      "Backup"
      ( \x ->
          Backup'
            Prelude.<$> (x Core..:? "Tags")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "OwnerId")
            Prelude.<*> (x Core..:? "SourceBackupRegion")
            Prelude.<*> (x Core..:? "FailureDetails")
            Prelude.<*> (x Core..:? "DirectoryInformation")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "Volume")
            Prelude.<*> (x Core..:? "ResourceARN")
            Prelude.<*> (x Core..:? "ProgressPercent")
            Prelude.<*> (x Core..:? "SourceBackupId")
            Prelude.<*> (x Core..: "BackupId")
            Prelude.<*> (x Core..: "Lifecycle")
            Prelude.<*> (x Core..: "Type")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "FileSystem")
      )

instance Prelude.Hashable Backup where
  hashWithSalt _salt Backup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` sourceBackupRegion
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` directoryInformation
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` volume
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` progressPercent
      `Prelude.hashWithSalt` sourceBackupId
      `Prelude.hashWithSalt` backupId
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` fileSystem

instance Prelude.NFData Backup where
  rnf Backup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf sourceBackupRegion
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf directoryInformation
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf volume
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf progressPercent
      `Prelude.seq` Prelude.rnf sourceBackupId
      `Prelude.seq` Prelude.rnf backupId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf fileSystem
