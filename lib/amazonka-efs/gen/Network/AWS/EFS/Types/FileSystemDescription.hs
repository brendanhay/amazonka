{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.FileSystemDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.FileSystemDescription
  ( FileSystemDescription (..)
  -- * Smart constructor
  , mkFileSystemDescription
  -- * Lenses
  , fsdOwnerId
  , fsdCreationToken
  , fsdFileSystemId
  , fsdCreationTime
  , fsdLifeCycleState
  , fsdNumberOfMountTargets
  , fsdSizeInBytes
  , fsdPerformanceMode
  , fsdTags
  , fsdEncrypted
  , fsdFileSystemArn
  , fsdKmsKeyId
  , fsdName
  , fsdProvisionedThroughputInMibps
  , fsdThroughputMode
  ) where

import qualified Network.AWS.EFS.Types.AwsAccountId as Types
import qualified Network.AWS.EFS.Types.CreationToken as Types
import qualified Network.AWS.EFS.Types.FileSystemArn as Types
import qualified Network.AWS.EFS.Types.FileSystemId as Types
import qualified Network.AWS.EFS.Types.FileSystemSize as Types
import qualified Network.AWS.EFS.Types.KmsKeyId as Types
import qualified Network.AWS.EFS.Types.LifeCycleState as Types
import qualified Network.AWS.EFS.Types.PerformanceMode as Types
import qualified Network.AWS.EFS.Types.Tag as Types
import qualified Network.AWS.EFS.Types.TagValue as Types
import qualified Network.AWS.EFS.Types.ThroughputMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A description of the file system.
--
-- /See:/ 'mkFileSystemDescription' smart constructor.
data FileSystemDescription = FileSystemDescription'
  { ownerId :: Types.AwsAccountId
    -- ^ The AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
  , creationToken :: Types.CreationToken
    -- ^ The opaque string specified in the request.
  , fileSystemId :: Types.FileSystemId
    -- ^ The ID of the file system, assigned by Amazon EFS.
  , creationTime :: Core.NominalDiffTime
    -- ^ The time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
  , lifeCycleState :: Types.LifeCycleState
    -- ^ The lifecycle phase of the file system.
  , numberOfMountTargets :: Core.Natural
    -- ^ The current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
  , sizeInBytes :: Types.FileSystemSize
    -- ^ The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@ value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, @SizeInBytes@ represents actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size that the file system was at any point in time. 
  , performanceMode :: Types.PerformanceMode
    -- ^ The performance mode of the file system.
  , tags :: [Types.Tag]
    -- ^ The tags associated with the file system, presented as an array of @Tag@ objects.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ A Boolean value that, if true, indicates that the file system is encrypted.
  , fileSystemArn :: Core.Maybe Types.FileSystemArn
    -- ^ The Amazon Resource Name (ARN) for the EFS file system, in the format @arn:aws:elasticfilesystem:/region/ :/account-id/ :file-system//file-system-id/ @ . Example with sample data: @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system/fs-01234567@ 
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The ID of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
  , name :: Core.Maybe Types.TagValue
    -- ^ You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateFileSystem' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field. 
  , provisionedThroughputInMibps :: Core.Maybe Core.Double
    -- ^ The throughput, measured in MiB/s, that you want to provision for a file system. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The limit on throughput is 1024 MiB/s. You can get these limits increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./ 
  , throughputMode :: Core.Maybe Types.ThroughputMode
    -- ^ The throughput mode for a file system. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'FileSystemDescription' value with any optional fields omitted.
mkFileSystemDescription
    :: Types.AwsAccountId -- ^ 'ownerId'
    -> Types.CreationToken -- ^ 'creationToken'
    -> Types.FileSystemId -- ^ 'fileSystemId'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.LifeCycleState -- ^ 'lifeCycleState'
    -> Core.Natural -- ^ 'numberOfMountTargets'
    -> Types.FileSystemSize -- ^ 'sizeInBytes'
    -> Types.PerformanceMode -- ^ 'performanceMode'
    -> FileSystemDescription
mkFileSystemDescription ownerId creationToken fileSystemId
  creationTime lifeCycleState numberOfMountTargets sizeInBytes
  performanceMode
  = FileSystemDescription'{ownerId, creationToken, fileSystemId,
                           creationTime, lifeCycleState, numberOfMountTargets, sizeInBytes,
                           performanceMode, tags = Core.mempty, encrypted = Core.Nothing,
                           fileSystemArn = Core.Nothing, kmsKeyId = Core.Nothing,
                           name = Core.Nothing, provisionedThroughputInMibps = Core.Nothing,
                           throughputMode = Core.Nothing}

-- | The AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdOwnerId :: Lens.Lens' FileSystemDescription Types.AwsAccountId
fsdOwnerId = Lens.field @"ownerId"
{-# INLINEABLE fsdOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The opaque string specified in the request.
--
-- /Note:/ Consider using 'creationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdCreationToken :: Lens.Lens' FileSystemDescription Types.CreationToken
fsdCreationToken = Lens.field @"creationToken"
{-# INLINEABLE fsdCreationToken #-}
{-# DEPRECATED creationToken "Use generic-lens or generic-optics with 'creationToken' instead"  #-}

-- | The ID of the file system, assigned by Amazon EFS.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdFileSystemId :: Lens.Lens' FileSystemDescription Types.FileSystemId
fsdFileSystemId = Lens.field @"fileSystemId"
{-# INLINEABLE fsdFileSystemId #-}
{-# DEPRECATED fileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead"  #-}

-- | The time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdCreationTime :: Lens.Lens' FileSystemDescription Core.NominalDiffTime
fsdCreationTime = Lens.field @"creationTime"
{-# INLINEABLE fsdCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The lifecycle phase of the file system.
--
-- /Note:/ Consider using 'lifeCycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdLifeCycleState :: Lens.Lens' FileSystemDescription Types.LifeCycleState
fsdLifeCycleState = Lens.field @"lifeCycleState"
{-# INLINEABLE fsdLifeCycleState #-}
{-# DEPRECATED lifeCycleState "Use generic-lens or generic-optics with 'lifeCycleState' instead"  #-}

-- | The current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
--
-- /Note:/ Consider using 'numberOfMountTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdNumberOfMountTargets :: Lens.Lens' FileSystemDescription Core.Natural
fsdNumberOfMountTargets = Lens.field @"numberOfMountTargets"
{-# INLINEABLE fsdNumberOfMountTargets #-}
{-# DEPRECATED numberOfMountTargets "Use generic-lens or generic-optics with 'numberOfMountTargets' instead"  #-}

-- | The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@ value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, @SizeInBytes@ represents actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size that the file system was at any point in time. 
--
-- /Note:/ Consider using 'sizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdSizeInBytes :: Lens.Lens' FileSystemDescription Types.FileSystemSize
fsdSizeInBytes = Lens.field @"sizeInBytes"
{-# INLINEABLE fsdSizeInBytes #-}
{-# DEPRECATED sizeInBytes "Use generic-lens or generic-optics with 'sizeInBytes' instead"  #-}

-- | The performance mode of the file system.
--
-- /Note:/ Consider using 'performanceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdPerformanceMode :: Lens.Lens' FileSystemDescription Types.PerformanceMode
fsdPerformanceMode = Lens.field @"performanceMode"
{-# INLINEABLE fsdPerformanceMode #-}
{-# DEPRECATED performanceMode "Use generic-lens or generic-optics with 'performanceMode' instead"  #-}

-- | The tags associated with the file system, presented as an array of @Tag@ objects.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdTags :: Lens.Lens' FileSystemDescription [Types.Tag]
fsdTags = Lens.field @"tags"
{-# INLINEABLE fsdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A Boolean value that, if true, indicates that the file system is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdEncrypted :: Lens.Lens' FileSystemDescription (Core.Maybe Core.Bool)
fsdEncrypted = Lens.field @"encrypted"
{-# INLINEABLE fsdEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The Amazon Resource Name (ARN) for the EFS file system, in the format @arn:aws:elasticfilesystem:/region/ :/account-id/ :file-system//file-system-id/ @ . Example with sample data: @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system/fs-01234567@ 
--
-- /Note:/ Consider using 'fileSystemArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdFileSystemArn :: Lens.Lens' FileSystemDescription (Core.Maybe Types.FileSystemArn)
fsdFileSystemArn = Lens.field @"fileSystemArn"
{-# INLINEABLE fsdFileSystemArn #-}
{-# DEPRECATED fileSystemArn "Use generic-lens or generic-optics with 'fileSystemArn' instead"  #-}

-- | The ID of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdKmsKeyId :: Lens.Lens' FileSystemDescription (Core.Maybe Types.KmsKeyId)
fsdKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE fsdKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateFileSystem' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdName :: Lens.Lens' FileSystemDescription (Core.Maybe Types.TagValue)
fsdName = Lens.field @"name"
{-# INLINEABLE fsdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The throughput, measured in MiB/s, that you want to provision for a file system. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The limit on throughput is 1024 MiB/s. You can get these limits increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./ 
--
-- /Note:/ Consider using 'provisionedThroughputInMibps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdProvisionedThroughputInMibps :: Lens.Lens' FileSystemDescription (Core.Maybe Core.Double)
fsdProvisionedThroughputInMibps = Lens.field @"provisionedThroughputInMibps"
{-# INLINEABLE fsdProvisionedThroughputInMibps #-}
{-# DEPRECATED provisionedThroughputInMibps "Use generic-lens or generic-optics with 'provisionedThroughputInMibps' instead"  #-}

-- | The throughput mode for a file system. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change. 
--
-- /Note:/ Consider using 'throughputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsdThroughputMode :: Lens.Lens' FileSystemDescription (Core.Maybe Types.ThroughputMode)
fsdThroughputMode = Lens.field @"throughputMode"
{-# INLINEABLE fsdThroughputMode #-}
{-# DEPRECATED throughputMode "Use generic-lens or generic-optics with 'throughputMode' instead"  #-}

instance Core.FromJSON FileSystemDescription where
        parseJSON
          = Core.withObject "FileSystemDescription" Core.$
              \ x ->
                FileSystemDescription' Core.<$>
                  (x Core..: "OwnerId") Core.<*> x Core..: "CreationToken" Core.<*>
                    x Core..: "FileSystemId"
                    Core.<*> x Core..: "CreationTime"
                    Core.<*> x Core..: "LifeCycleState"
                    Core.<*> x Core..: "NumberOfMountTargets"
                    Core.<*> x Core..: "SizeInBytes"
                    Core.<*> x Core..: "PerformanceMode"
                    Core.<*> x Core..:? "Tags" Core..!= Core.mempty
                    Core.<*> x Core..:? "Encrypted"
                    Core.<*> x Core..:? "FileSystemArn"
                    Core.<*> x Core..:? "KmsKeyId"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "ProvisionedThroughputInMibps"
                    Core.<*> x Core..:? "ThroughputMode"
