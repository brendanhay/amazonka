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
-- Module      : Network.AWS.EFS.Types.FileSystemDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.FileSystemDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types.FileSystemSize
import Network.AWS.EFS.Types.LifeCycleState
import Network.AWS.EFS.Types.PerformanceMode
import Network.AWS.EFS.Types.Tag
import Network.AWS.EFS.Types.ThroughputMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A description of the file system.
--
-- /See:/ 'newFileSystemDescription' smart constructor.
data FileSystemDescription = FileSystemDescription'
  { -- | The throughput mode for a file system. There are two throughput modes to
    -- choose from for your file system: @bursting@ and @provisioned@. If you
    -- set @ThroughputMode@ to @provisioned@, you must also set a value for
    -- @ProvisionedThroughPutInMibps@. You can decrease your file system\'s
    -- throughput in Provisioned Throughput mode or change between the
    -- throughput modes as long as it’s been more than 24 hours since the last
    -- decrease or throughput mode change.
    throughputMode :: Prelude.Maybe ThroughputMode,
    -- | A Boolean value that, if true, indicates that the file system is
    -- encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the EFS file system, in the format
    -- @arn:aws:elasticfilesystem:region:account-id:file-system\/file-system-id @.
    -- Example with sample data:
    -- @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system\/fs-01234567@
    fileSystemArn :: Prelude.Maybe Prelude.Text,
    -- | The throughput, measured in MiB\/s, that you want to provision for a
    -- file system. Valid values are 1-1024. Required if @ThroughputMode@ is
    -- set to @provisioned@. The limit on throughput is 1024 MiB\/s. You can
    -- get these limits increased by contacting AWS Support. For more
    -- information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase>
    -- in the /Amazon EFS User Guide./
    provisionedThroughputInMibps :: Prelude.Maybe Prelude.Double,
    -- | The ID of an AWS Key Management Service (AWS KMS) customer master key
    -- (CMK) that was used to protect the encrypted file system.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | You can add tags to a file system, including a @Name@ tag. For more
    -- information, see CreateFileSystem. If the file system has a @Name@ tag,
    -- Amazon EFS returns the value in this field.
    name :: Prelude.Maybe Prelude.Text,
    -- | The AWS account that created the file system. If the file system was
    -- created by an IAM user, the parent account to which the user belongs is
    -- the owner.
    ownerId :: Prelude.Text,
    -- | The opaque string specified in the request.
    creationToken :: Prelude.Text,
    -- | The ID of the file system, assigned by Amazon EFS.
    fileSystemId :: Prelude.Text,
    -- | The time that the file system was created, in seconds (since
    -- 1970-01-01T00:00:00Z).
    creationTime :: Core.POSIX,
    -- | The lifecycle phase of the file system.
    lifeCycleState :: LifeCycleState,
    -- | The current number of mount targets that the file system has. For more
    -- information, see CreateMountTarget.
    numberOfMountTargets :: Prelude.Natural,
    -- | The latest known metered size (in bytes) of data stored in the file
    -- system, in its @Value@ field, and the time at which that size was
    -- determined in its @Timestamp@ field. The @Timestamp@ value is the
    -- integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@
    -- value doesn\'t represent the size of a consistent snapshot of the file
    -- system, but it is eventually consistent when there are no writes to the
    -- file system. That is, @SizeInBytes@ represents actual size only if the
    -- file system is not modified for a period longer than a couple of hours.
    -- Otherwise, the value is not the exact size that the file system was at
    -- any point in time.
    sizeInBytes :: FileSystemSize,
    -- | The performance mode of the file system.
    performanceMode :: PerformanceMode,
    -- | The tags associated with the file system, presented as an array of @Tag@
    -- objects.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'throughputMode', 'fileSystemDescription_throughputMode' - The throughput mode for a file system. There are two throughput modes to
-- choose from for your file system: @bursting@ and @provisioned@. If you
-- set @ThroughputMode@ to @provisioned@, you must also set a value for
-- @ProvisionedThroughPutInMibps@. You can decrease your file system\'s
-- throughput in Provisioned Throughput mode or change between the
-- throughput modes as long as it’s been more than 24 hours since the last
-- decrease or throughput mode change.
--
-- 'encrypted', 'fileSystemDescription_encrypted' - A Boolean value that, if true, indicates that the file system is
-- encrypted.
--
-- 'fileSystemArn', 'fileSystemDescription_fileSystemArn' - The Amazon Resource Name (ARN) for the EFS file system, in the format
-- @arn:aws:elasticfilesystem:region:account-id:file-system\/file-system-id @.
-- Example with sample data:
-- @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system\/fs-01234567@
--
-- 'provisionedThroughputInMibps', 'fileSystemDescription_provisionedThroughputInMibps' - The throughput, measured in MiB\/s, that you want to provision for a
-- file system. Valid values are 1-1024. Required if @ThroughputMode@ is
-- set to @provisioned@. The limit on throughput is 1024 MiB\/s. You can
-- get these limits increased by contacting AWS Support. For more
-- information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase>
-- in the /Amazon EFS User Guide./
--
-- 'kmsKeyId', 'fileSystemDescription_kmsKeyId' - The ID of an AWS Key Management Service (AWS KMS) customer master key
-- (CMK) that was used to protect the encrypted file system.
--
-- 'name', 'fileSystemDescription_name' - You can add tags to a file system, including a @Name@ tag. For more
-- information, see CreateFileSystem. If the file system has a @Name@ tag,
-- Amazon EFS returns the value in this field.
--
-- 'ownerId', 'fileSystemDescription_ownerId' - The AWS account that created the file system. If the file system was
-- created by an IAM user, the parent account to which the user belongs is
-- the owner.
--
-- 'creationToken', 'fileSystemDescription_creationToken' - The opaque string specified in the request.
--
-- 'fileSystemId', 'fileSystemDescription_fileSystemId' - The ID of the file system, assigned by Amazon EFS.
--
-- 'creationTime', 'fileSystemDescription_creationTime' - The time that the file system was created, in seconds (since
-- 1970-01-01T00:00:00Z).
--
-- 'lifeCycleState', 'fileSystemDescription_lifeCycleState' - The lifecycle phase of the file system.
--
-- 'numberOfMountTargets', 'fileSystemDescription_numberOfMountTargets' - The current number of mount targets that the file system has. For more
-- information, see CreateMountTarget.
--
-- 'sizeInBytes', 'fileSystemDescription_sizeInBytes' - The latest known metered size (in bytes) of data stored in the file
-- system, in its @Value@ field, and the time at which that size was
-- determined in its @Timestamp@ field. The @Timestamp@ value is the
-- integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@
-- value doesn\'t represent the size of a consistent snapshot of the file
-- system, but it is eventually consistent when there are no writes to the
-- file system. That is, @SizeInBytes@ represents actual size only if the
-- file system is not modified for a period longer than a couple of hours.
-- Otherwise, the value is not the exact size that the file system was at
-- any point in time.
--
-- 'performanceMode', 'fileSystemDescription_performanceMode' - The performance mode of the file system.
--
-- 'tags', 'fileSystemDescription_tags' - The tags associated with the file system, presented as an array of @Tag@
-- objects.
newFileSystemDescription ::
  -- | 'ownerId'
  Prelude.Text ->
  -- | 'creationToken'
  Prelude.Text ->
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lifeCycleState'
  LifeCycleState ->
  -- | 'numberOfMountTargets'
  Prelude.Natural ->
  -- | 'sizeInBytes'
  FileSystemSize ->
  -- | 'performanceMode'
  PerformanceMode ->
  FileSystemDescription
newFileSystemDescription
  pOwnerId_
  pCreationToken_
  pFileSystemId_
  pCreationTime_
  pLifeCycleState_
  pNumberOfMountTargets_
  pSizeInBytes_
  pPerformanceMode_ =
    FileSystemDescription'
      { throughputMode =
          Prelude.Nothing,
        encrypted = Prelude.Nothing,
        fileSystemArn = Prelude.Nothing,
        provisionedThroughputInMibps = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        name = Prelude.Nothing,
        ownerId = pOwnerId_,
        creationToken = pCreationToken_,
        fileSystemId = pFileSystemId_,
        creationTime = Core._Time Lens.# pCreationTime_,
        lifeCycleState = pLifeCycleState_,
        numberOfMountTargets = pNumberOfMountTargets_,
        sizeInBytes = pSizeInBytes_,
        performanceMode = pPerformanceMode_,
        tags = Prelude.mempty
      }

-- | The throughput mode for a file system. There are two throughput modes to
-- choose from for your file system: @bursting@ and @provisioned@. If you
-- set @ThroughputMode@ to @provisioned@, you must also set a value for
-- @ProvisionedThroughPutInMibps@. You can decrease your file system\'s
-- throughput in Provisioned Throughput mode or change between the
-- throughput modes as long as it’s been more than 24 hours since the last
-- decrease or throughput mode change.
fileSystemDescription_throughputMode :: Lens.Lens' FileSystemDescription (Prelude.Maybe ThroughputMode)
fileSystemDescription_throughputMode = Lens.lens (\FileSystemDescription' {throughputMode} -> throughputMode) (\s@FileSystemDescription' {} a -> s {throughputMode = a} :: FileSystemDescription)

-- | A Boolean value that, if true, indicates that the file system is
-- encrypted.
fileSystemDescription_encrypted :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Bool)
fileSystemDescription_encrypted = Lens.lens (\FileSystemDescription' {encrypted} -> encrypted) (\s@FileSystemDescription' {} a -> s {encrypted = a} :: FileSystemDescription)

-- | The Amazon Resource Name (ARN) for the EFS file system, in the format
-- @arn:aws:elasticfilesystem:region:account-id:file-system\/file-system-id @.
-- Example with sample data:
-- @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system\/fs-01234567@
fileSystemDescription_fileSystemArn :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Text)
fileSystemDescription_fileSystemArn = Lens.lens (\FileSystemDescription' {fileSystemArn} -> fileSystemArn) (\s@FileSystemDescription' {} a -> s {fileSystemArn = a} :: FileSystemDescription)

-- | The throughput, measured in MiB\/s, that you want to provision for a
-- file system. Valid values are 1-1024. Required if @ThroughputMode@ is
-- set to @provisioned@. The limit on throughput is 1024 MiB\/s. You can
-- get these limits increased by contacting AWS Support. For more
-- information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase>
-- in the /Amazon EFS User Guide./
fileSystemDescription_provisionedThroughputInMibps :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Double)
fileSystemDescription_provisionedThroughputInMibps = Lens.lens (\FileSystemDescription' {provisionedThroughputInMibps} -> provisionedThroughputInMibps) (\s@FileSystemDescription' {} a -> s {provisionedThroughputInMibps = a} :: FileSystemDescription)

-- | The ID of an AWS Key Management Service (AWS KMS) customer master key
-- (CMK) that was used to protect the encrypted file system.
fileSystemDescription_kmsKeyId :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Text)
fileSystemDescription_kmsKeyId = Lens.lens (\FileSystemDescription' {kmsKeyId} -> kmsKeyId) (\s@FileSystemDescription' {} a -> s {kmsKeyId = a} :: FileSystemDescription)

-- | You can add tags to a file system, including a @Name@ tag. For more
-- information, see CreateFileSystem. If the file system has a @Name@ tag,
-- Amazon EFS returns the value in this field.
fileSystemDescription_name :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Text)
fileSystemDescription_name = Lens.lens (\FileSystemDescription' {name} -> name) (\s@FileSystemDescription' {} a -> s {name = a} :: FileSystemDescription)

-- | The AWS account that created the file system. If the file system was
-- created by an IAM user, the parent account to which the user belongs is
-- the owner.
fileSystemDescription_ownerId :: Lens.Lens' FileSystemDescription Prelude.Text
fileSystemDescription_ownerId = Lens.lens (\FileSystemDescription' {ownerId} -> ownerId) (\s@FileSystemDescription' {} a -> s {ownerId = a} :: FileSystemDescription)

-- | The opaque string specified in the request.
fileSystemDescription_creationToken :: Lens.Lens' FileSystemDescription Prelude.Text
fileSystemDescription_creationToken = Lens.lens (\FileSystemDescription' {creationToken} -> creationToken) (\s@FileSystemDescription' {} a -> s {creationToken = a} :: FileSystemDescription)

-- | The ID of the file system, assigned by Amazon EFS.
fileSystemDescription_fileSystemId :: Lens.Lens' FileSystemDescription Prelude.Text
fileSystemDescription_fileSystemId = Lens.lens (\FileSystemDescription' {fileSystemId} -> fileSystemId) (\s@FileSystemDescription' {} a -> s {fileSystemId = a} :: FileSystemDescription)

-- | The time that the file system was created, in seconds (since
-- 1970-01-01T00:00:00Z).
fileSystemDescription_creationTime :: Lens.Lens' FileSystemDescription Prelude.UTCTime
fileSystemDescription_creationTime = Lens.lens (\FileSystemDescription' {creationTime} -> creationTime) (\s@FileSystemDescription' {} a -> s {creationTime = a} :: FileSystemDescription) Prelude.. Core._Time

-- | The lifecycle phase of the file system.
fileSystemDescription_lifeCycleState :: Lens.Lens' FileSystemDescription LifeCycleState
fileSystemDescription_lifeCycleState = Lens.lens (\FileSystemDescription' {lifeCycleState} -> lifeCycleState) (\s@FileSystemDescription' {} a -> s {lifeCycleState = a} :: FileSystemDescription)

-- | The current number of mount targets that the file system has. For more
-- information, see CreateMountTarget.
fileSystemDescription_numberOfMountTargets :: Lens.Lens' FileSystemDescription Prelude.Natural
fileSystemDescription_numberOfMountTargets = Lens.lens (\FileSystemDescription' {numberOfMountTargets} -> numberOfMountTargets) (\s@FileSystemDescription' {} a -> s {numberOfMountTargets = a} :: FileSystemDescription)

-- | The latest known metered size (in bytes) of data stored in the file
-- system, in its @Value@ field, and the time at which that size was
-- determined in its @Timestamp@ field. The @Timestamp@ value is the
-- integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@
-- value doesn\'t represent the size of a consistent snapshot of the file
-- system, but it is eventually consistent when there are no writes to the
-- file system. That is, @SizeInBytes@ represents actual size only if the
-- file system is not modified for a period longer than a couple of hours.
-- Otherwise, the value is not the exact size that the file system was at
-- any point in time.
fileSystemDescription_sizeInBytes :: Lens.Lens' FileSystemDescription FileSystemSize
fileSystemDescription_sizeInBytes = Lens.lens (\FileSystemDescription' {sizeInBytes} -> sizeInBytes) (\s@FileSystemDescription' {} a -> s {sizeInBytes = a} :: FileSystemDescription)

-- | The performance mode of the file system.
fileSystemDescription_performanceMode :: Lens.Lens' FileSystemDescription PerformanceMode
fileSystemDescription_performanceMode = Lens.lens (\FileSystemDescription' {performanceMode} -> performanceMode) (\s@FileSystemDescription' {} a -> s {performanceMode = a} :: FileSystemDescription)

-- | The tags associated with the file system, presented as an array of @Tag@
-- objects.
fileSystemDescription_tags :: Lens.Lens' FileSystemDescription [Tag]
fileSystemDescription_tags = Lens.lens (\FileSystemDescription' {tags} -> tags) (\s@FileSystemDescription' {} a -> s {tags = a} :: FileSystemDescription) Prelude.. Lens._Coerce

instance Core.FromJSON FileSystemDescription where
  parseJSON =
    Core.withObject
      "FileSystemDescription"
      ( \x ->
          FileSystemDescription'
            Prelude.<$> (x Core..:? "ThroughputMode")
            Prelude.<*> (x Core..:? "Encrypted")
            Prelude.<*> (x Core..:? "FileSystemArn")
            Prelude.<*> (x Core..:? "ProvisionedThroughputInMibps")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..: "OwnerId")
            Prelude.<*> (x Core..: "CreationToken")
            Prelude.<*> (x Core..: "FileSystemId")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "LifeCycleState")
            Prelude.<*> (x Core..: "NumberOfMountTargets")
            Prelude.<*> (x Core..: "SizeInBytes")
            Prelude.<*> (x Core..: "PerformanceMode")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable FileSystemDescription

instance Prelude.NFData FileSystemDescription
