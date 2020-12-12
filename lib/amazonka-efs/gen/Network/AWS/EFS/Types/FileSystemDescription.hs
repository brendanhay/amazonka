{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.FileSystemDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.FileSystemDescription
  ( FileSystemDescription (..),

    -- * Smart constructor
    mkFileSystemDescription,

    -- * Lenses
    fsdProvisionedThroughputInMibps,
    fsdFileSystemARN,
    fsdEncrypted,
    fsdThroughputMode,
    fsdKMSKeyId,
    fsdName,
    fsdOwnerId,
    fsdCreationToken,
    fsdFileSystemId,
    fsdCreationTime,
    fsdLifeCycleState,
    fsdNumberOfMountTargets,
    fsdSizeInBytes,
    fsdPerformanceMode,
    fsdTags,
  )
where

import Network.AWS.EFS.Types.FileSystemSize
import Network.AWS.EFS.Types.LifeCycleState
import Network.AWS.EFS.Types.PerformanceMode
import Network.AWS.EFS.Types.Tag
import Network.AWS.EFS.Types.ThroughputMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A description of the file system.
--
-- /See:/ 'mkFileSystemDescription' smart constructor.
data FileSystemDescription = FileSystemDescription'
  { provisionedThroughputInMibps ::
      Lude.Maybe Lude.Double,
    fileSystemARN :: Lude.Maybe Lude.Text,
    encrypted :: Lude.Maybe Lude.Bool,
    throughputMode :: Lude.Maybe ThroughputMode,
    kmsKeyId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Text,
    creationToken :: Lude.Text,
    fileSystemId :: Lude.Text,
    creationTime :: Lude.Timestamp,
    lifeCycleState :: LifeCycleState,
    numberOfMountTargets :: Lude.Natural,
    sizeInBytes :: FileSystemSize,
    performanceMode :: PerformanceMode,
    tags :: [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileSystemDescription' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
-- * 'creationToken' - The opaque string specified in the request.
-- * 'encrypted' - A Boolean value that, if true, indicates that the file system is encrypted.
-- * 'fileSystemARN' - The Amazon Resource Name (ARN) for the EFS file system, in the format @arn:aws:elasticfilesystem:/region/ :/account-id/ :file-system//file-system-id/ @ . Example with sample data: @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system/fs-01234567@
-- * 'fileSystemId' - The ID of the file system, assigned by Amazon EFS.
-- * 'kmsKeyId' - The ID of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
-- * 'lifeCycleState' - The lifecycle phase of the file system.
-- * 'name' - You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateFileSystem' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field.
-- * 'numberOfMountTargets' - The current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
-- * 'ownerId' - The AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
-- * 'performanceMode' - The performance mode of the file system.
-- * 'provisionedThroughputInMibps' - The throughput, measured in MiB/s, that you want to provision for a file system. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The limit on throughput is 1024 MiB/s. You can get these limits increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
-- * 'sizeInBytes' - The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@ value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, @SizeInBytes@ represents actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size that the file system was at any point in time.
-- * 'tags' - The tags associated with the file system, presented as an array of @Tag@ objects.
-- * 'throughputMode' - The throughput mode for a file system. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change.
mkFileSystemDescription ::
  -- | 'ownerId'
  Lude.Text ->
  -- | 'creationToken'
  Lude.Text ->
  -- | 'fileSystemId'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'lifeCycleState'
  LifeCycleState ->
  -- | 'numberOfMountTargets'
  Lude.Natural ->
  -- | 'sizeInBytes'
  FileSystemSize ->
  -- | 'performanceMode'
  PerformanceMode ->
  FileSystemDescription
mkFileSystemDescription
  pOwnerId_
  pCreationToken_
  pFileSystemId_
  pCreationTime_
  pLifeCycleState_
  pNumberOfMountTargets_
  pSizeInBytes_
  pPerformanceMode_ =
    FileSystemDescription'
      { provisionedThroughputInMibps =
          Lude.Nothing,
        fileSystemARN = Lude.Nothing,
        encrypted = Lude.Nothing,
        throughputMode = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        name = Lude.Nothing,
        ownerId = pOwnerId_,
        creationToken = pCreationToken_,
        fileSystemId = pFileSystemId_,
        creationTime = pCreationTime_,
        lifeCycleState = pLifeCycleState_,
        numberOfMountTargets = pNumberOfMountTargets_,
        sizeInBytes = pSizeInBytes_,
        performanceMode = pPerformanceMode_,
        tags = Lude.mempty
      }

-- | The throughput, measured in MiB/s, that you want to provision for a file system. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The limit on throughput is 1024 MiB/s. You can get these limits increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
--
-- /Note:/ Consider using 'provisionedThroughputInMibps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdProvisionedThroughputInMibps :: Lens.Lens' FileSystemDescription (Lude.Maybe Lude.Double)
fsdProvisionedThroughputInMibps = Lens.lens (provisionedThroughputInMibps :: FileSystemDescription -> Lude.Maybe Lude.Double) (\s a -> s {provisionedThroughputInMibps = a} :: FileSystemDescription)
{-# DEPRECATED fsdProvisionedThroughputInMibps "Use generic-lens or generic-optics with 'provisionedThroughputInMibps' instead." #-}

-- | The Amazon Resource Name (ARN) for the EFS file system, in the format @arn:aws:elasticfilesystem:/region/ :/account-id/ :file-system//file-system-id/ @ . Example with sample data: @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system/fs-01234567@
--
-- /Note:/ Consider using 'fileSystemARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdFileSystemARN :: Lens.Lens' FileSystemDescription (Lude.Maybe Lude.Text)
fsdFileSystemARN = Lens.lens (fileSystemARN :: FileSystemDescription -> Lude.Maybe Lude.Text) (\s a -> s {fileSystemARN = a} :: FileSystemDescription)
{-# DEPRECATED fsdFileSystemARN "Use generic-lens or generic-optics with 'fileSystemARN' instead." #-}

-- | A Boolean value that, if true, indicates that the file system is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdEncrypted :: Lens.Lens' FileSystemDescription (Lude.Maybe Lude.Bool)
fsdEncrypted = Lens.lens (encrypted :: FileSystemDescription -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: FileSystemDescription)
{-# DEPRECATED fsdEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The throughput mode for a file system. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change.
--
-- /Note:/ Consider using 'throughputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdThroughputMode :: Lens.Lens' FileSystemDescription (Lude.Maybe ThroughputMode)
fsdThroughputMode = Lens.lens (throughputMode :: FileSystemDescription -> Lude.Maybe ThroughputMode) (\s a -> s {throughputMode = a} :: FileSystemDescription)
{-# DEPRECATED fsdThroughputMode "Use generic-lens or generic-optics with 'throughputMode' instead." #-}

-- | The ID of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdKMSKeyId :: Lens.Lens' FileSystemDescription (Lude.Maybe Lude.Text)
fsdKMSKeyId = Lens.lens (kmsKeyId :: FileSystemDescription -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: FileSystemDescription)
{-# DEPRECATED fsdKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateFileSystem' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdName :: Lens.Lens' FileSystemDescription (Lude.Maybe Lude.Text)
fsdName = Lens.lens (name :: FileSystemDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: FileSystemDescription)
{-# DEPRECATED fsdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdOwnerId :: Lens.Lens' FileSystemDescription Lude.Text
fsdOwnerId = Lens.lens (ownerId :: FileSystemDescription -> Lude.Text) (\s a -> s {ownerId = a} :: FileSystemDescription)
{-# DEPRECATED fsdOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The opaque string specified in the request.
--
-- /Note:/ Consider using 'creationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdCreationToken :: Lens.Lens' FileSystemDescription Lude.Text
fsdCreationToken = Lens.lens (creationToken :: FileSystemDescription -> Lude.Text) (\s a -> s {creationToken = a} :: FileSystemDescription)
{-# DEPRECATED fsdCreationToken "Use generic-lens or generic-optics with 'creationToken' instead." #-}

-- | The ID of the file system, assigned by Amazon EFS.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdFileSystemId :: Lens.Lens' FileSystemDescription Lude.Text
fsdFileSystemId = Lens.lens (fileSystemId :: FileSystemDescription -> Lude.Text) (\s a -> s {fileSystemId = a} :: FileSystemDescription)
{-# DEPRECATED fsdFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdCreationTime :: Lens.Lens' FileSystemDescription Lude.Timestamp
fsdCreationTime = Lens.lens (creationTime :: FileSystemDescription -> Lude.Timestamp) (\s a -> s {creationTime = a} :: FileSystemDescription)
{-# DEPRECATED fsdCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The lifecycle phase of the file system.
--
-- /Note:/ Consider using 'lifeCycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdLifeCycleState :: Lens.Lens' FileSystemDescription LifeCycleState
fsdLifeCycleState = Lens.lens (lifeCycleState :: FileSystemDescription -> LifeCycleState) (\s a -> s {lifeCycleState = a} :: FileSystemDescription)
{-# DEPRECATED fsdLifeCycleState "Use generic-lens or generic-optics with 'lifeCycleState' instead." #-}

-- | The current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
--
-- /Note:/ Consider using 'numberOfMountTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdNumberOfMountTargets :: Lens.Lens' FileSystemDescription Lude.Natural
fsdNumberOfMountTargets = Lens.lens (numberOfMountTargets :: FileSystemDescription -> Lude.Natural) (\s a -> s {numberOfMountTargets = a} :: FileSystemDescription)
{-# DEPRECATED fsdNumberOfMountTargets "Use generic-lens or generic-optics with 'numberOfMountTargets' instead." #-}

-- | The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@ value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, @SizeInBytes@ represents actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size that the file system was at any point in time.
--
-- /Note:/ Consider using 'sizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdSizeInBytes :: Lens.Lens' FileSystemDescription FileSystemSize
fsdSizeInBytes = Lens.lens (sizeInBytes :: FileSystemDescription -> FileSystemSize) (\s a -> s {sizeInBytes = a} :: FileSystemDescription)
{-# DEPRECATED fsdSizeInBytes "Use generic-lens or generic-optics with 'sizeInBytes' instead." #-}

-- | The performance mode of the file system.
--
-- /Note:/ Consider using 'performanceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdPerformanceMode :: Lens.Lens' FileSystemDescription PerformanceMode
fsdPerformanceMode = Lens.lens (performanceMode :: FileSystemDescription -> PerformanceMode) (\s a -> s {performanceMode = a} :: FileSystemDescription)
{-# DEPRECATED fsdPerformanceMode "Use generic-lens or generic-optics with 'performanceMode' instead." #-}

-- | The tags associated with the file system, presented as an array of @Tag@ objects.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdTags :: Lens.Lens' FileSystemDescription [Tag]
fsdTags = Lens.lens (tags :: FileSystemDescription -> [Tag]) (\s a -> s {tags = a} :: FileSystemDescription)
{-# DEPRECATED fsdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON FileSystemDescription where
  parseJSON =
    Lude.withObject
      "FileSystemDescription"
      ( \x ->
          FileSystemDescription'
            Lude.<$> (x Lude..:? "ProvisionedThroughputInMibps")
            Lude.<*> (x Lude..:? "FileSystemArn")
            Lude.<*> (x Lude..:? "Encrypted")
            Lude.<*> (x Lude..:? "ThroughputMode")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..: "OwnerId")
            Lude.<*> (x Lude..: "CreationToken")
            Lude.<*> (x Lude..: "FileSystemId")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "LifeCycleState")
            Lude.<*> (x Lude..: "NumberOfMountTargets")
            Lude.<*> (x Lude..: "SizeInBytes")
            Lude.<*> (x Lude..: "PerformanceMode")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
