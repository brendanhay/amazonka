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
-- Module      : Amazonka.EFS.Types.FileSystemDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.FileSystemDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types.FileSystemSize
import Amazonka.EFS.Types.LifeCycleState
import Amazonka.EFS.Types.PerformanceMode
import Amazonka.EFS.Types.Tag
import Amazonka.EFS.Types.ThroughputMode
import qualified Amazonka.Prelude as Prelude

-- | A description of the file system.
--
-- /See:/ 'newFileSystemDescription' smart constructor.
data FileSystemDescription = FileSystemDescription'
  { -- | The unique and consistent identifier of the Availability Zone in which
    -- the file system\'s One Zone storage classes exist. For example,
    -- @use1-az1@ is an Availability Zone ID for the us-east-1 Amazon Web
    -- Services Region, and it has the same location in every Amazon Web
    -- Services account.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | Describes the Amazon Web Services Availability Zone in which the file
    -- system is located, and is valid only for file systems using One Zone
    -- storage classes. For more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/storage-classes.html Using EFS storage classes>
    -- in the /Amazon EFS User Guide/.
    availabilityZoneName :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that, if true, indicates that the file system is
    -- encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the EFS file system, in the format
    -- @arn:aws:elasticfilesystem:region:account-id:file-system\/file-system-id @.
    -- Example with sample data:
    -- @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system\/fs-01234567@
    fileSystemArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of an KMS key used to protect the encrypted file system.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | You can add tags to a file system, including a @Name@ tag. For more
    -- information, see CreateFileSystem. If the file system has a @Name@ tag,
    -- Amazon EFS returns the value in this field.
    name :: Prelude.Maybe Prelude.Text,
    -- | The amount of provisioned throughput, measured in MiB\/s, for the file
    -- system. Valid for file systems using @ThroughputMode@ set to
    -- @provisioned@.
    provisionedThroughputInMibps :: Prelude.Maybe Prelude.Double,
    -- | Displays the file system\'s throughput mode. For more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#throughput-modes Throughput modes>
    -- in the /Amazon EFS User Guide/.
    throughputMode :: Prelude.Maybe ThroughputMode,
    -- | The Amazon Web Services account that created the file system. If the
    -- file system was created by an IAM user, the parent account to which the
    -- user belongs is the owner.
    ownerId :: Prelude.Text,
    -- | The opaque string specified in the request.
    creationToken :: Prelude.Text,
    -- | The ID of the file system, assigned by Amazon EFS.
    fileSystemId :: Prelude.Text,
    -- | The time that the file system was created, in seconds (since
    -- 1970-01-01T00:00:00Z).
    creationTime :: Data.POSIX,
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
-- 'availabilityZoneId', 'fileSystemDescription_availabilityZoneId' - The unique and consistent identifier of the Availability Zone in which
-- the file system\'s One Zone storage classes exist. For example,
-- @use1-az1@ is an Availability Zone ID for the us-east-1 Amazon Web
-- Services Region, and it has the same location in every Amazon Web
-- Services account.
--
-- 'availabilityZoneName', 'fileSystemDescription_availabilityZoneName' - Describes the Amazon Web Services Availability Zone in which the file
-- system is located, and is valid only for file systems using One Zone
-- storage classes. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/storage-classes.html Using EFS storage classes>
-- in the /Amazon EFS User Guide/.
--
-- 'encrypted', 'fileSystemDescription_encrypted' - A Boolean value that, if true, indicates that the file system is
-- encrypted.
--
-- 'fileSystemArn', 'fileSystemDescription_fileSystemArn' - The Amazon Resource Name (ARN) for the EFS file system, in the format
-- @arn:aws:elasticfilesystem:region:account-id:file-system\/file-system-id @.
-- Example with sample data:
-- @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system\/fs-01234567@
--
-- 'kmsKeyId', 'fileSystemDescription_kmsKeyId' - The ID of an KMS key used to protect the encrypted file system.
--
-- 'name', 'fileSystemDescription_name' - You can add tags to a file system, including a @Name@ tag. For more
-- information, see CreateFileSystem. If the file system has a @Name@ tag,
-- Amazon EFS returns the value in this field.
--
-- 'provisionedThroughputInMibps', 'fileSystemDescription_provisionedThroughputInMibps' - The amount of provisioned throughput, measured in MiB\/s, for the file
-- system. Valid for file systems using @ThroughputMode@ set to
-- @provisioned@.
--
-- 'throughputMode', 'fileSystemDescription_throughputMode' - Displays the file system\'s throughput mode. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#throughput-modes Throughput modes>
-- in the /Amazon EFS User Guide/.
--
-- 'ownerId', 'fileSystemDescription_ownerId' - The Amazon Web Services account that created the file system. If the
-- file system was created by an IAM user, the parent account to which the
-- user belongs is the owner.
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
      { availabilityZoneId =
          Prelude.Nothing,
        availabilityZoneName = Prelude.Nothing,
        encrypted = Prelude.Nothing,
        fileSystemArn = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        name = Prelude.Nothing,
        provisionedThroughputInMibps = Prelude.Nothing,
        throughputMode = Prelude.Nothing,
        ownerId = pOwnerId_,
        creationToken = pCreationToken_,
        fileSystemId = pFileSystemId_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lifeCycleState = pLifeCycleState_,
        numberOfMountTargets = pNumberOfMountTargets_,
        sizeInBytes = pSizeInBytes_,
        performanceMode = pPerformanceMode_,
        tags = Prelude.mempty
      }

-- | The unique and consistent identifier of the Availability Zone in which
-- the file system\'s One Zone storage classes exist. For example,
-- @use1-az1@ is an Availability Zone ID for the us-east-1 Amazon Web
-- Services Region, and it has the same location in every Amazon Web
-- Services account.
fileSystemDescription_availabilityZoneId :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Text)
fileSystemDescription_availabilityZoneId = Lens.lens (\FileSystemDescription' {availabilityZoneId} -> availabilityZoneId) (\s@FileSystemDescription' {} a -> s {availabilityZoneId = a} :: FileSystemDescription)

-- | Describes the Amazon Web Services Availability Zone in which the file
-- system is located, and is valid only for file systems using One Zone
-- storage classes. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/storage-classes.html Using EFS storage classes>
-- in the /Amazon EFS User Guide/.
fileSystemDescription_availabilityZoneName :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Text)
fileSystemDescription_availabilityZoneName = Lens.lens (\FileSystemDescription' {availabilityZoneName} -> availabilityZoneName) (\s@FileSystemDescription' {} a -> s {availabilityZoneName = a} :: FileSystemDescription)

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

-- | The ID of an KMS key used to protect the encrypted file system.
fileSystemDescription_kmsKeyId :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Text)
fileSystemDescription_kmsKeyId = Lens.lens (\FileSystemDescription' {kmsKeyId} -> kmsKeyId) (\s@FileSystemDescription' {} a -> s {kmsKeyId = a} :: FileSystemDescription)

-- | You can add tags to a file system, including a @Name@ tag. For more
-- information, see CreateFileSystem. If the file system has a @Name@ tag,
-- Amazon EFS returns the value in this field.
fileSystemDescription_name :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Text)
fileSystemDescription_name = Lens.lens (\FileSystemDescription' {name} -> name) (\s@FileSystemDescription' {} a -> s {name = a} :: FileSystemDescription)

-- | The amount of provisioned throughput, measured in MiB\/s, for the file
-- system. Valid for file systems using @ThroughputMode@ set to
-- @provisioned@.
fileSystemDescription_provisionedThroughputInMibps :: Lens.Lens' FileSystemDescription (Prelude.Maybe Prelude.Double)
fileSystemDescription_provisionedThroughputInMibps = Lens.lens (\FileSystemDescription' {provisionedThroughputInMibps} -> provisionedThroughputInMibps) (\s@FileSystemDescription' {} a -> s {provisionedThroughputInMibps = a} :: FileSystemDescription)

-- | Displays the file system\'s throughput mode. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#throughput-modes Throughput modes>
-- in the /Amazon EFS User Guide/.
fileSystemDescription_throughputMode :: Lens.Lens' FileSystemDescription (Prelude.Maybe ThroughputMode)
fileSystemDescription_throughputMode = Lens.lens (\FileSystemDescription' {throughputMode} -> throughputMode) (\s@FileSystemDescription' {} a -> s {throughputMode = a} :: FileSystemDescription)

-- | The Amazon Web Services account that created the file system. If the
-- file system was created by an IAM user, the parent account to which the
-- user belongs is the owner.
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
fileSystemDescription_creationTime = Lens.lens (\FileSystemDescription' {creationTime} -> creationTime) (\s@FileSystemDescription' {} a -> s {creationTime = a} :: FileSystemDescription) Prelude.. Data._Time

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
fileSystemDescription_tags = Lens.lens (\FileSystemDescription' {tags} -> tags) (\s@FileSystemDescription' {} a -> s {tags = a} :: FileSystemDescription) Prelude.. Lens.coerced

instance Data.FromJSON FileSystemDescription where
  parseJSON =
    Data.withObject
      "FileSystemDescription"
      ( \x ->
          FileSystemDescription'
            Prelude.<$> (x Data..:? "AvailabilityZoneId")
            Prelude.<*> (x Data..:? "AvailabilityZoneName")
            Prelude.<*> (x Data..:? "Encrypted")
            Prelude.<*> (x Data..:? "FileSystemArn")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ProvisionedThroughputInMibps")
            Prelude.<*> (x Data..:? "ThroughputMode")
            Prelude.<*> (x Data..: "OwnerId")
            Prelude.<*> (x Data..: "CreationToken")
            Prelude.<*> (x Data..: "FileSystemId")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LifeCycleState")
            Prelude.<*> (x Data..: "NumberOfMountTargets")
            Prelude.<*> (x Data..: "SizeInBytes")
            Prelude.<*> (x Data..: "PerformanceMode")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FileSystemDescription where
  hashWithSalt _salt FileSystemDescription' {..} =
    _salt `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` availabilityZoneName
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` fileSystemArn
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` provisionedThroughputInMibps
      `Prelude.hashWithSalt` throughputMode
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` creationToken
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lifeCycleState
      `Prelude.hashWithSalt` numberOfMountTargets
      `Prelude.hashWithSalt` sizeInBytes
      `Prelude.hashWithSalt` performanceMode
      `Prelude.hashWithSalt` tags

instance Prelude.NFData FileSystemDescription where
  rnf FileSystemDescription' {..} =
    Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf availabilityZoneName
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf fileSystemArn
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf provisionedThroughputInMibps
      `Prelude.seq` Prelude.rnf throughputMode
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf creationToken
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lifeCycleState
      `Prelude.seq` Prelude.rnf numberOfMountTargets
      `Prelude.seq` Prelude.rnf sizeInBytes
      `Prelude.seq` Prelude.rnf performanceMode
      `Prelude.seq` Prelude.rnf tags
