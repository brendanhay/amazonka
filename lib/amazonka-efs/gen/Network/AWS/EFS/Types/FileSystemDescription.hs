{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.FileSystemDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.FileSystemDescription where

import Network.AWS.EFS.Types.FileSystemSize
import Network.AWS.EFS.Types.LifeCycleState
import Network.AWS.EFS.Types.PerformanceMode
import Network.AWS.EFS.Types.Tag
import Network.AWS.EFS.Types.ThroughputMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A description of the file system.
--
--
--
-- /See:/ 'fileSystemDescription' smart constructor.
data FileSystemDescription = FileSystemDescription'
  { _fsdProvisionedThroughputInMibps ::
      !(Maybe Double),
    _fsdFileSystemARN :: !(Maybe Text),
    _fsdEncrypted :: !(Maybe Bool),
    _fsdThroughputMode :: !(Maybe ThroughputMode),
    _fsdKMSKeyId :: !(Maybe Text),
    _fsdName :: !(Maybe Text),
    _fsdOwnerId :: !Text,
    _fsdCreationToken :: !Text,
    _fsdFileSystemId :: !Text,
    _fsdCreationTime :: !POSIX,
    _fsdLifeCycleState :: !LifeCycleState,
    _fsdNumberOfMountTargets :: !Nat,
    _fsdSizeInBytes :: !FileSystemSize,
    _fsdPerformanceMode :: !PerformanceMode,
    _fsdTags :: ![Tag]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileSystemDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsdProvisionedThroughputInMibps' - The throughput, measured in MiB/s, that you want to provision for a file system. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The limit on throughput is 1024 MiB/s. You can get these limits increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
--
-- * 'fsdFileSystemARN' - The Amazon Resource Name (ARN) for the EFS file system, in the format @arn:aws:elasticfilesystem:/region/ :/account-id/ :file-system//file-system-id/ @ . Example with sample data: @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system/fs-01234567@
--
-- * 'fsdEncrypted' - A Boolean value that, if true, indicates that the file system is encrypted.
--
-- * 'fsdThroughputMode' - The throughput mode for a file system. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change.
--
-- * 'fsdKMSKeyId' - The ID of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
--
-- * 'fsdName' - You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateFileSystem' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field.
--
-- * 'fsdOwnerId' - The AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
--
-- * 'fsdCreationToken' - The opaque string specified in the request.
--
-- * 'fsdFileSystemId' - The ID of the file system, assigned by Amazon EFS.
--
-- * 'fsdCreationTime' - The time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
--
-- * 'fsdLifeCycleState' - The lifecycle phase of the file system.
--
-- * 'fsdNumberOfMountTargets' - The current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
--
-- * 'fsdSizeInBytes' - The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@ value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, @SizeInBytes@ represents actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size that the file system was at any point in time.
--
-- * 'fsdPerformanceMode' - The performance mode of the file system.
--
-- * 'fsdTags' - The tags associated with the file system, presented as an array of @Tag@ objects.
fileSystemDescription ::
  -- | 'fsdOwnerId'
  Text ->
  -- | 'fsdCreationToken'
  Text ->
  -- | 'fsdFileSystemId'
  Text ->
  -- | 'fsdCreationTime'
  UTCTime ->
  -- | 'fsdLifeCycleState'
  LifeCycleState ->
  -- | 'fsdNumberOfMountTargets'
  Natural ->
  -- | 'fsdSizeInBytes'
  FileSystemSize ->
  -- | 'fsdPerformanceMode'
  PerformanceMode ->
  FileSystemDescription
fileSystemDescription
  pOwnerId_
  pCreationToken_
  pFileSystemId_
  pCreationTime_
  pLifeCycleState_
  pNumberOfMountTargets_
  pSizeInBytes_
  pPerformanceMode_ =
    FileSystemDescription'
      { _fsdProvisionedThroughputInMibps =
          Nothing,
        _fsdFileSystemARN = Nothing,
        _fsdEncrypted = Nothing,
        _fsdThroughputMode = Nothing,
        _fsdKMSKeyId = Nothing,
        _fsdName = Nothing,
        _fsdOwnerId = pOwnerId_,
        _fsdCreationToken = pCreationToken_,
        _fsdFileSystemId = pFileSystemId_,
        _fsdCreationTime = _Time # pCreationTime_,
        _fsdLifeCycleState = pLifeCycleState_,
        _fsdNumberOfMountTargets = _Nat # pNumberOfMountTargets_,
        _fsdSizeInBytes = pSizeInBytes_,
        _fsdPerformanceMode = pPerformanceMode_,
        _fsdTags = mempty
      }

-- | The throughput, measured in MiB/s, that you want to provision for a file system. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The limit on throughput is 1024 MiB/s. You can get these limits increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
fsdProvisionedThroughputInMibps :: Lens' FileSystemDescription (Maybe Double)
fsdProvisionedThroughputInMibps = lens _fsdProvisionedThroughputInMibps (\s a -> s {_fsdProvisionedThroughputInMibps = a})

-- | The Amazon Resource Name (ARN) for the EFS file system, in the format @arn:aws:elasticfilesystem:/region/ :/account-id/ :file-system//file-system-id/ @ . Example with sample data: @arn:aws:elasticfilesystem:us-west-2:1111333322228888:file-system/fs-01234567@
fsdFileSystemARN :: Lens' FileSystemDescription (Maybe Text)
fsdFileSystemARN = lens _fsdFileSystemARN (\s a -> s {_fsdFileSystemARN = a})

-- | A Boolean value that, if true, indicates that the file system is encrypted.
fsdEncrypted :: Lens' FileSystemDescription (Maybe Bool)
fsdEncrypted = lens _fsdEncrypted (\s a -> s {_fsdEncrypted = a})

-- | The throughput mode for a file system. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change.
fsdThroughputMode :: Lens' FileSystemDescription (Maybe ThroughputMode)
fsdThroughputMode = lens _fsdThroughputMode (\s a -> s {_fsdThroughputMode = a})

-- | The ID of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
fsdKMSKeyId :: Lens' FileSystemDescription (Maybe Text)
fsdKMSKeyId = lens _fsdKMSKeyId (\s a -> s {_fsdKMSKeyId = a})

-- | You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateFileSystem' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field.
fsdName :: Lens' FileSystemDescription (Maybe Text)
fsdName = lens _fsdName (\s a -> s {_fsdName = a})

-- | The AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
fsdOwnerId :: Lens' FileSystemDescription Text
fsdOwnerId = lens _fsdOwnerId (\s a -> s {_fsdOwnerId = a})

-- | The opaque string specified in the request.
fsdCreationToken :: Lens' FileSystemDescription Text
fsdCreationToken = lens _fsdCreationToken (\s a -> s {_fsdCreationToken = a})

-- | The ID of the file system, assigned by Amazon EFS.
fsdFileSystemId :: Lens' FileSystemDescription Text
fsdFileSystemId = lens _fsdFileSystemId (\s a -> s {_fsdFileSystemId = a})

-- | The time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
fsdCreationTime :: Lens' FileSystemDescription UTCTime
fsdCreationTime = lens _fsdCreationTime (\s a -> s {_fsdCreationTime = a}) . _Time

-- | The lifecycle phase of the file system.
fsdLifeCycleState :: Lens' FileSystemDescription LifeCycleState
fsdLifeCycleState = lens _fsdLifeCycleState (\s a -> s {_fsdLifeCycleState = a})

-- | The current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
fsdNumberOfMountTargets :: Lens' FileSystemDescription Natural
fsdNumberOfMountTargets = lens _fsdNumberOfMountTargets (\s a -> s {_fsdNumberOfMountTargets = a}) . _Nat

-- | The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@ value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, @SizeInBytes@ represents actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size that the file system was at any point in time.
fsdSizeInBytes :: Lens' FileSystemDescription FileSystemSize
fsdSizeInBytes = lens _fsdSizeInBytes (\s a -> s {_fsdSizeInBytes = a})

-- | The performance mode of the file system.
fsdPerformanceMode :: Lens' FileSystemDescription PerformanceMode
fsdPerformanceMode = lens _fsdPerformanceMode (\s a -> s {_fsdPerformanceMode = a})

-- | The tags associated with the file system, presented as an array of @Tag@ objects.
fsdTags :: Lens' FileSystemDescription [Tag]
fsdTags = lens _fsdTags (\s a -> s {_fsdTags = a}) . _Coerce

instance FromJSON FileSystemDescription where
  parseJSON =
    withObject
      "FileSystemDescription"
      ( \x ->
          FileSystemDescription'
            <$> (x .:? "ProvisionedThroughputInMibps")
            <*> (x .:? "FileSystemArn")
            <*> (x .:? "Encrypted")
            <*> (x .:? "ThroughputMode")
            <*> (x .:? "KmsKeyId")
            <*> (x .:? "Name")
            <*> (x .: "OwnerId")
            <*> (x .: "CreationToken")
            <*> (x .: "FileSystemId")
            <*> (x .: "CreationTime")
            <*> (x .: "LifeCycleState")
            <*> (x .: "NumberOfMountTargets")
            <*> (x .: "SizeInBytes")
            <*> (x .: "PerformanceMode")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable FileSystemDescription

instance NFData FileSystemDescription
