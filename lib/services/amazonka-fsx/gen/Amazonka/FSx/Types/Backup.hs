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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.Backup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The configuration of the self-managed Microsoft Active Directory
    -- directory to which the Windows File Server instance is joined.
    directoryInformation :: Prelude.Maybe ActiveDirectoryBackupAttributes,
    -- | Details explaining any failures that occurred when creating a backup.
    failureDetails :: Prelude.Maybe BackupFailureDetails,
    -- | The ID of the Key Management Service (KMS) key used to encrypt the
    -- backup of the Amazon FSx file system\'s data at rest.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    ownerId :: Prelude.Maybe Prelude.Text,
    progressPercent :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) for the backup resource.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies the resource type that\'s backed up.
    resourceType :: Prelude.Maybe ResourceType,
    sourceBackupId :: Prelude.Maybe Prelude.Text,
    -- | The source Region of the backup. Specifies the Region from where this
    -- backup is copied.
    sourceBackupRegion :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with a particular file system.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    volume :: Prelude.Maybe Volume,
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
    creationTime :: Data.POSIX,
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
-- 'directoryInformation', 'backup_directoryInformation' - The configuration of the self-managed Microsoft Active Directory
-- directory to which the Windows File Server instance is joined.
--
-- 'failureDetails', 'backup_failureDetails' - Details explaining any failures that occurred when creating a backup.
--
-- 'kmsKeyId', 'backup_kmsKeyId' - The ID of the Key Management Service (KMS) key used to encrypt the
-- backup of the Amazon FSx file system\'s data at rest.
--
-- 'ownerId', 'backup_ownerId' - Undocumented member.
--
-- 'progressPercent', 'backup_progressPercent' - Undocumented member.
--
-- 'resourceARN', 'backup_resourceARN' - The Amazon Resource Name (ARN) for the backup resource.
--
-- 'resourceType', 'backup_resourceType' - Specifies the resource type that\'s backed up.
--
-- 'sourceBackupId', 'backup_sourceBackupId' - Undocumented member.
--
-- 'sourceBackupRegion', 'backup_sourceBackupRegion' - The source Region of the backup. Specifies the Region from where this
-- backup is copied.
--
-- 'tags', 'backup_tags' - The tags associated with a particular file system.
--
-- 'volume', 'backup_volume' - Undocumented member.
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
      { directoryInformation = Prelude.Nothing,
        failureDetails = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        ownerId = Prelude.Nothing,
        progressPercent = Prelude.Nothing,
        resourceARN = Prelude.Nothing,
        resourceType = Prelude.Nothing,
        sourceBackupId = Prelude.Nothing,
        sourceBackupRegion = Prelude.Nothing,
        tags = Prelude.Nothing,
        volume = Prelude.Nothing,
        backupId = pBackupId_,
        lifecycle = pLifecycle_,
        type' = pType_,
        creationTime = Data._Time Lens.# pCreationTime_,
        fileSystem = pFileSystem_
      }

-- | The configuration of the self-managed Microsoft Active Directory
-- directory to which the Windows File Server instance is joined.
backup_directoryInformation :: Lens.Lens' Backup (Prelude.Maybe ActiveDirectoryBackupAttributes)
backup_directoryInformation = Lens.lens (\Backup' {directoryInformation} -> directoryInformation) (\s@Backup' {} a -> s {directoryInformation = a} :: Backup)

-- | Details explaining any failures that occurred when creating a backup.
backup_failureDetails :: Lens.Lens' Backup (Prelude.Maybe BackupFailureDetails)
backup_failureDetails = Lens.lens (\Backup' {failureDetails} -> failureDetails) (\s@Backup' {} a -> s {failureDetails = a} :: Backup)

-- | The ID of the Key Management Service (KMS) key used to encrypt the
-- backup of the Amazon FSx file system\'s data at rest.
backup_kmsKeyId :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_kmsKeyId = Lens.lens (\Backup' {kmsKeyId} -> kmsKeyId) (\s@Backup' {} a -> s {kmsKeyId = a} :: Backup)

-- | Undocumented member.
backup_ownerId :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_ownerId = Lens.lens (\Backup' {ownerId} -> ownerId) (\s@Backup' {} a -> s {ownerId = a} :: Backup)

-- | Undocumented member.
backup_progressPercent :: Lens.Lens' Backup (Prelude.Maybe Prelude.Natural)
backup_progressPercent = Lens.lens (\Backup' {progressPercent} -> progressPercent) (\s@Backup' {} a -> s {progressPercent = a} :: Backup)

-- | The Amazon Resource Name (ARN) for the backup resource.
backup_resourceARN :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_resourceARN = Lens.lens (\Backup' {resourceARN} -> resourceARN) (\s@Backup' {} a -> s {resourceARN = a} :: Backup)

-- | Specifies the resource type that\'s backed up.
backup_resourceType :: Lens.Lens' Backup (Prelude.Maybe ResourceType)
backup_resourceType = Lens.lens (\Backup' {resourceType} -> resourceType) (\s@Backup' {} a -> s {resourceType = a} :: Backup)

-- | Undocumented member.
backup_sourceBackupId :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_sourceBackupId = Lens.lens (\Backup' {sourceBackupId} -> sourceBackupId) (\s@Backup' {} a -> s {sourceBackupId = a} :: Backup)

-- | The source Region of the backup. Specifies the Region from where this
-- backup is copied.
backup_sourceBackupRegion :: Lens.Lens' Backup (Prelude.Maybe Prelude.Text)
backup_sourceBackupRegion = Lens.lens (\Backup' {sourceBackupRegion} -> sourceBackupRegion) (\s@Backup' {} a -> s {sourceBackupRegion = a} :: Backup)

-- | The tags associated with a particular file system.
backup_tags :: Lens.Lens' Backup (Prelude.Maybe (Prelude.NonEmpty Tag))
backup_tags = Lens.lens (\Backup' {tags} -> tags) (\s@Backup' {} a -> s {tags = a} :: Backup) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
backup_volume :: Lens.Lens' Backup (Prelude.Maybe Volume)
backup_volume = Lens.lens (\Backup' {volume} -> volume) (\s@Backup' {} a -> s {volume = a} :: Backup)

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
backup_creationTime = Lens.lens (\Backup' {creationTime} -> creationTime) (\s@Backup' {} a -> s {creationTime = a} :: Backup) Prelude.. Data._Time

-- | The metadata of the file system associated with the backup. This
-- metadata is persisted even if the file system is deleted.
backup_fileSystem :: Lens.Lens' Backup FileSystem
backup_fileSystem = Lens.lens (\Backup' {fileSystem} -> fileSystem) (\s@Backup' {} a -> s {fileSystem = a} :: Backup)

instance Data.FromJSON Backup where
  parseJSON =
    Data.withObject
      "Backup"
      ( \x ->
          Backup'
            Prelude.<$> (x Data..:? "DirectoryInformation")
            Prelude.<*> (x Data..:? "FailureDetails")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "ProgressPercent")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "SourceBackupId")
            Prelude.<*> (x Data..:? "SourceBackupRegion")
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "Volume")
            Prelude.<*> (x Data..: "BackupId")
            Prelude.<*> (x Data..: "Lifecycle")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "FileSystem")
      )

instance Prelude.Hashable Backup where
  hashWithSalt _salt Backup' {..} =
    _salt `Prelude.hashWithSalt` directoryInformation
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` progressPercent
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sourceBackupId
      `Prelude.hashWithSalt` sourceBackupRegion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volume
      `Prelude.hashWithSalt` backupId
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` fileSystem

instance Prelude.NFData Backup where
  rnf Backup' {..} =
    Prelude.rnf directoryInformation
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf progressPercent
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sourceBackupId
      `Prelude.seq` Prelude.rnf sourceBackupRegion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volume
      `Prelude.seq` Prelude.rnf backupId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf fileSystem
