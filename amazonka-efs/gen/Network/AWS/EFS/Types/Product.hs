{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.Product where

import Network.AWS.EFS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Description of the file system.
--
--
--
-- /See:/ 'fileSystemDescription' smart constructor.
data FileSystemDescription = FileSystemDescription'
  { _fsdEncrypted            :: !(Maybe Bool)
  , _fsdKMSKeyId             :: !(Maybe Text)
  , _fsdName                 :: !(Maybe Text)
  , _fsdOwnerId              :: !Text
  , _fsdCreationToken        :: !Text
  , _fsdFileSystemId         :: !Text
  , _fsdCreationTime         :: !POSIX
  , _fsdLifeCycleState       :: !LifeCycleState
  , _fsdNumberOfMountTargets :: !Nat
  , _fsdSizeInBytes          :: !FileSystemSize
  , _fsdPerformanceMode      :: !PerformanceMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FileSystemDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsdEncrypted' - A boolean value that, if true, indicates that the file system is encrypted.
--
-- * 'fsdKMSKeyId' - The id of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
--
-- * 'fsdName' - You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateTags' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field.
--
-- * 'fsdOwnerId' - AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
--
-- * 'fsdCreationToken' - Opaque string specified in the request.
--
-- * 'fsdFileSystemId' - ID of the file system, assigned by Amazon EFS.
--
-- * 'fsdCreationTime' - Time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
--
-- * 'fsdLifeCycleState' - Lifecycle phase of the file system.
--
-- * 'fsdNumberOfMountTargets' - Current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
--
-- * 'fsdSizeInBytes' - Latest known metered size (in bytes) of data stored in the file system, in bytes, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. Note that the value does not represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, the value will represent actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size the file system was at any instant in time.
--
-- * 'fsdPerformanceMode' - The @PerformanceMode@ of the file system.
fileSystemDescription
    :: Text -- ^ 'fsdOwnerId'
    -> Text -- ^ 'fsdCreationToken'
    -> Text -- ^ 'fsdFileSystemId'
    -> UTCTime -- ^ 'fsdCreationTime'
    -> LifeCycleState -- ^ 'fsdLifeCycleState'
    -> Natural -- ^ 'fsdNumberOfMountTargets'
    -> FileSystemSize -- ^ 'fsdSizeInBytes'
    -> PerformanceMode -- ^ 'fsdPerformanceMode'
    -> FileSystemDescription
fileSystemDescription pOwnerId_ pCreationToken_ pFileSystemId_ pCreationTime_ pLifeCycleState_ pNumberOfMountTargets_ pSizeInBytes_ pPerformanceMode_ =
  FileSystemDescription'
    { _fsdEncrypted = Nothing
    , _fsdKMSKeyId = Nothing
    , _fsdName = Nothing
    , _fsdOwnerId = pOwnerId_
    , _fsdCreationToken = pCreationToken_
    , _fsdFileSystemId = pFileSystemId_
    , _fsdCreationTime = _Time # pCreationTime_
    , _fsdLifeCycleState = pLifeCycleState_
    , _fsdNumberOfMountTargets = _Nat # pNumberOfMountTargets_
    , _fsdSizeInBytes = pSizeInBytes_
    , _fsdPerformanceMode = pPerformanceMode_
    }


-- | A boolean value that, if true, indicates that the file system is encrypted.
fsdEncrypted :: Lens' FileSystemDescription (Maybe Bool)
fsdEncrypted = lens _fsdEncrypted (\ s a -> s{_fsdEncrypted = a})

-- | The id of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
fsdKMSKeyId :: Lens' FileSystemDescription (Maybe Text)
fsdKMSKeyId = lens _fsdKMSKeyId (\ s a -> s{_fsdKMSKeyId = a})

-- | You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateTags' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field.
fsdName :: Lens' FileSystemDescription (Maybe Text)
fsdName = lens _fsdName (\ s a -> s{_fsdName = a})

-- | AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
fsdOwnerId :: Lens' FileSystemDescription Text
fsdOwnerId = lens _fsdOwnerId (\ s a -> s{_fsdOwnerId = a})

-- | Opaque string specified in the request.
fsdCreationToken :: Lens' FileSystemDescription Text
fsdCreationToken = lens _fsdCreationToken (\ s a -> s{_fsdCreationToken = a})

-- | ID of the file system, assigned by Amazon EFS.
fsdFileSystemId :: Lens' FileSystemDescription Text
fsdFileSystemId = lens _fsdFileSystemId (\ s a -> s{_fsdFileSystemId = a})

-- | Time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
fsdCreationTime :: Lens' FileSystemDescription UTCTime
fsdCreationTime = lens _fsdCreationTime (\ s a -> s{_fsdCreationTime = a}) . _Time

-- | Lifecycle phase of the file system.
fsdLifeCycleState :: Lens' FileSystemDescription LifeCycleState
fsdLifeCycleState = lens _fsdLifeCycleState (\ s a -> s{_fsdLifeCycleState = a})

-- | Current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
fsdNumberOfMountTargets :: Lens' FileSystemDescription Natural
fsdNumberOfMountTargets = lens _fsdNumberOfMountTargets (\ s a -> s{_fsdNumberOfMountTargets = a}) . _Nat

-- | Latest known metered size (in bytes) of data stored in the file system, in bytes, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. Note that the value does not represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, the value will represent actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size the file system was at any instant in time.
fsdSizeInBytes :: Lens' FileSystemDescription FileSystemSize
fsdSizeInBytes = lens _fsdSizeInBytes (\ s a -> s{_fsdSizeInBytes = a})

-- | The @PerformanceMode@ of the file system.
fsdPerformanceMode :: Lens' FileSystemDescription PerformanceMode
fsdPerformanceMode = lens _fsdPerformanceMode (\ s a -> s{_fsdPerformanceMode = a})

instance FromJSON FileSystemDescription where
        parseJSON
          = withObject "FileSystemDescription"
              (\ x ->
                 FileSystemDescription' <$>
                   (x .:? "Encrypted") <*> (x .:? "KmsKeyId") <*>
                     (x .:? "Name")
                     <*> (x .: "OwnerId")
                     <*> (x .: "CreationToken")
                     <*> (x .: "FileSystemId")
                     <*> (x .: "CreationTime")
                     <*> (x .: "LifeCycleState")
                     <*> (x .: "NumberOfMountTargets")
                     <*> (x .: "SizeInBytes")
                     <*> (x .: "PerformanceMode"))

instance Hashable FileSystemDescription where

instance NFData FileSystemDescription where

-- | Latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. Note that the value does not represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, the value will represent the actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not necessarily the exact size the file system was at any instant in time.
--
--
--
-- /See:/ 'fileSystemSize' smart constructor.
data FileSystemSize = FileSystemSize'
  { _fssTimestamp :: !(Maybe POSIX)
  , _fssValue     :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FileSystemSize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fssTimestamp' - Time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
--
-- * 'fssValue' - Latest known metered size (in bytes) of data stored in the file system.
fileSystemSize
    :: Natural -- ^ 'fssValue'
    -> FileSystemSize
fileSystemSize pValue_ =
  FileSystemSize' {_fssTimestamp = Nothing, _fssValue = _Nat # pValue_}


-- | Time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
fssTimestamp :: Lens' FileSystemSize (Maybe UTCTime)
fssTimestamp = lens _fssTimestamp (\ s a -> s{_fssTimestamp = a}) . mapping _Time

-- | Latest known metered size (in bytes) of data stored in the file system.
fssValue :: Lens' FileSystemSize Natural
fssValue = lens _fssValue (\ s a -> s{_fssValue = a}) . _Nat

instance FromJSON FileSystemSize where
        parseJSON
          = withObject "FileSystemSize"
              (\ x ->
                 FileSystemSize' <$>
                   (x .:? "Timestamp") <*> (x .: "Value"))

instance Hashable FileSystemSize where

instance NFData FileSystemSize where

-- | Provides a description of a mount target.
--
--
--
-- /See:/ 'mountTargetDescription' smart constructor.
data MountTargetDescription = MountTargetDescription'
  { _mtdIPAddress          :: !(Maybe Text)
  , _mtdNetworkInterfaceId :: !(Maybe Text)
  , _mtdOwnerId            :: !(Maybe Text)
  , _mtdMountTargetId      :: !Text
  , _mtdFileSystemId       :: !Text
  , _mtdSubnetId           :: !Text
  , _mtdLifeCycleState     :: !LifeCycleState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MountTargetDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtdIPAddress' - Address at which the file system may be mounted via the mount target.
--
-- * 'mtdNetworkInterfaceId' - ID of the network interface that Amazon EFS created when it created the mount target.
--
-- * 'mtdOwnerId' - AWS account ID that owns the resource.
--
-- * 'mtdMountTargetId' - System-assigned mount target ID.
--
-- * 'mtdFileSystemId' - ID of the file system for which the mount target is intended.
--
-- * 'mtdSubnetId' - ID of the mount target's subnet.
--
-- * 'mtdLifeCycleState' - Lifecycle state of the mount target.
mountTargetDescription
    :: Text -- ^ 'mtdMountTargetId'
    -> Text -- ^ 'mtdFileSystemId'
    -> Text -- ^ 'mtdSubnetId'
    -> LifeCycleState -- ^ 'mtdLifeCycleState'
    -> MountTargetDescription
mountTargetDescription pMountTargetId_ pFileSystemId_ pSubnetId_ pLifeCycleState_ =
  MountTargetDescription'
    { _mtdIPAddress = Nothing
    , _mtdNetworkInterfaceId = Nothing
    , _mtdOwnerId = Nothing
    , _mtdMountTargetId = pMountTargetId_
    , _mtdFileSystemId = pFileSystemId_
    , _mtdSubnetId = pSubnetId_
    , _mtdLifeCycleState = pLifeCycleState_
    }


-- | Address at which the file system may be mounted via the mount target.
mtdIPAddress :: Lens' MountTargetDescription (Maybe Text)
mtdIPAddress = lens _mtdIPAddress (\ s a -> s{_mtdIPAddress = a})

-- | ID of the network interface that Amazon EFS created when it created the mount target.
mtdNetworkInterfaceId :: Lens' MountTargetDescription (Maybe Text)
mtdNetworkInterfaceId = lens _mtdNetworkInterfaceId (\ s a -> s{_mtdNetworkInterfaceId = a})

-- | AWS account ID that owns the resource.
mtdOwnerId :: Lens' MountTargetDescription (Maybe Text)
mtdOwnerId = lens _mtdOwnerId (\ s a -> s{_mtdOwnerId = a})

-- | System-assigned mount target ID.
mtdMountTargetId :: Lens' MountTargetDescription Text
mtdMountTargetId = lens _mtdMountTargetId (\ s a -> s{_mtdMountTargetId = a})

-- | ID of the file system for which the mount target is intended.
mtdFileSystemId :: Lens' MountTargetDescription Text
mtdFileSystemId = lens _mtdFileSystemId (\ s a -> s{_mtdFileSystemId = a})

-- | ID of the mount target's subnet.
mtdSubnetId :: Lens' MountTargetDescription Text
mtdSubnetId = lens _mtdSubnetId (\ s a -> s{_mtdSubnetId = a})

-- | Lifecycle state of the mount target.
mtdLifeCycleState :: Lens' MountTargetDescription LifeCycleState
mtdLifeCycleState = lens _mtdLifeCycleState (\ s a -> s{_mtdLifeCycleState = a})

instance FromJSON MountTargetDescription where
        parseJSON
          = withObject "MountTargetDescription"
              (\ x ->
                 MountTargetDescription' <$>
                   (x .:? "IpAddress") <*> (x .:? "NetworkInterfaceId")
                     <*> (x .:? "OwnerId")
                     <*> (x .: "MountTargetId")
                     <*> (x .: "FileSystemId")
                     <*> (x .: "SubnetId")
                     <*> (x .: "LifeCycleState"))

instance Hashable MountTargetDescription where

instance NFData MountTargetDescription where

-- | A tag is a key-value pair. Allowed characters: letters, whitespace, and numbers, representable in UTF-8, and the following characters:@+ - = . _ : /@
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - Tag key (String). The key can't start with @aws:@ .
--
-- * 'tagValue' - Value of the tag key.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | Tag key (String). The key can't start with @aws:@ .
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | Value of the tag key.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])
