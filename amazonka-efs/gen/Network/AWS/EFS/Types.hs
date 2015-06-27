{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EFS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.EFS.Types
    (
    -- * Service
      EFS

    -- * Errors
    , _MountTargetNotFound
    , _SecurityGroupLimitExceeded
    , _MountTargetConflict
    , _UnsupportedAvailabilityZone
    , _SecurityGroupNotFound
    , _FileSystemAlreadyExists
    , _FileSystemLimitExceeded
    , _NetworkInterfaceLimitExceeded
    , _FileSystemNotFound
    , _SubnetNotFound
    , _IncorrectFileSystemLifeCycleState
    , _BadRequest
    , _NoFreeAddressesInSubnet
    , _DependencyTimeout
    , _FileSystemInUse
    , _IncorrectMountTargetState
    , _InternalServerError
    , _IPAddressInUse

    -- * LifeCycleState
    , LifeCycleState (..)

    -- * FileSystemDescription
    , FileSystemDescription
    , fileSystemDescription
    , fsdName
    , fsdOwnerId
    , fsdCreationToken
    , fsdFileSystemId
    , fsdCreationTime
    , fsdLifeCycleState
    , fsdNumberOfMountTargets
    , fsdSizeInBytes

    -- * FileSystemSize
    , FileSystemSize
    , fileSystemSize
    , fssTimestamp
    , fssValue

    -- * MountTargetDescription
    , MountTargetDescription
    , mountTargetDescription
    , mtdIPAddress
    , mtdNetworkInterfaceId
    , mtdOwnerId
    , mtdMountTargetId
    , mtdFileSystemId
    , mtdSubnetId
    , mtdLifeCycleState

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2015-02-01@ of the Amazon Elastic File System SDK.
data EFS

instance AWSService EFS where
    type Sg EFS = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "EFS"
            , _svcPrefix = "elasticfilesystem"
            , _svcVersion = "2015-02-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = 80000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Returned if there is no mount target with the specified ID is found in
-- the caller\'s account.
_MountTargetNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_MountTargetNotFound =
    _ServiceError . hasStatus 404 . hasCode "MountTargetNotFound"

-- | Returned if the size of @SecurityGroups@ specified in the request is
-- greater than five.
_SecurityGroupLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_SecurityGroupLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "SecurityGroupLimitExceeded"

-- | Returned if the mount target would violate one of the specified
-- restrictions based on the file system\'s existing mount targets.
_MountTargetConflict :: AWSError a => Getting (First ServiceError) a ServiceError
_MountTargetConflict =
    _ServiceError . hasStatus 409 . hasCode "MountTargetConflict"

-- | Prism for UnsupportedAvailabilityZone' errors.
_UnsupportedAvailabilityZone :: AWSError a => Getting (First ServiceError) a ServiceError
_UnsupportedAvailabilityZone =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedAvailabilityZone"

-- | Returned if one of the specified security groups does not exist in the
-- subnet\'s VPC.
_SecurityGroupNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_SecurityGroupNotFound =
    _ServiceError . hasStatus 400 . hasCode "SecurityGroupNotFound"

-- | Returned if the file system you are trying to create already exists,
-- with the creation token you provided.
_FileSystemAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_FileSystemAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "FileSystemAlreadyExists"

-- | Returned if the AWS account has already created maximum number of file
-- systems allowed per account.
_FileSystemLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_FileSystemLimitExceeded =
    _ServiceError . hasStatus 403 . hasCode "FileSystemLimitExceeded"

-- | The calling account has reached the ENI limit for the specific AWS
-- region. Client should try to delete some ENIs or get its account limit
-- raised. For more information, go to
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html Amazon VPC Limits>
-- in the Amazon Virtual Private Cloud User Guide (see the Network
-- interfaces per VPC entry in the table).
_NetworkInterfaceLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_NetworkInterfaceLimitExceeded =
    _ServiceError . hasStatus 409 . hasCode "NetworkInterfaceLimitExceeded"

-- | Returned if the specified @FileSystemId@ does not exist in the
-- requester\'s AWS account.
_FileSystemNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_FileSystemNotFound =
    _ServiceError . hasStatus 404 . hasCode "FileSystemNotFound"

-- | Returned if there is no subnet with ID @SubnetId@ provided in the
-- request.
_SubnetNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_SubnetNotFound = _ServiceError . hasStatus 400 . hasCode "SubnetNotFound"

-- | Returned if the file system\'s life cycle state is not \"created\".
_IncorrectFileSystemLifeCycleState :: AWSError a => Getting (First ServiceError) a ServiceError
_IncorrectFileSystemLifeCycleState =
    _ServiceError . hasStatus 409 . hasCode "IncorrectFileSystemLifeCycleState"

-- | Returned if the request is malformed or contains an error such as an
-- invalid parameter value or a missing required parameter.
_BadRequest :: AWSError a => Getting (First ServiceError) a ServiceError
_BadRequest = _ServiceError . hasStatus 400 . hasCode "BadRequest"

-- | Returned if @IpAddress@ was not specified in the request and there are
-- no free IP addresses in the subnet.
_NoFreeAddressesInSubnet :: AWSError a => Getting (First ServiceError) a ServiceError
_NoFreeAddressesInSubnet =
    _ServiceError . hasStatus 409 . hasCode "NoFreeAddressesInSubnet"

-- | The service timed out trying to fulfill the request, and the client
-- should try the call again.
_DependencyTimeout :: AWSError a => Getting (First ServiceError) a ServiceError
_DependencyTimeout =
    _ServiceError . hasStatus 504 . hasCode "DependencyTimeout"

-- | Returned if a file system has mount targets.
_FileSystemInUse :: AWSError a => Getting (First ServiceError) a ServiceError
_FileSystemInUse = _ServiceError . hasStatus 409 . hasCode "FileSystemInUse"

-- | Returned if the mount target is not in the correct state for the
-- operation.
_IncorrectMountTargetState :: AWSError a => Getting (First ServiceError) a ServiceError
_IncorrectMountTargetState =
    _ServiceError . hasStatus 409 . hasCode "IncorrectMountTargetState"

-- | Returned if an error occurred on the server side.
_InternalServerError :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServerError =
    _ServiceError . hasStatus 500 . hasCode "InternalServerError"

-- | Returned if the request specified an @IpAddress@ that is already in use
-- in the subnet.
_IPAddressInUse :: AWSError a => Getting (First ServiceError) a ServiceError
_IPAddressInUse = _ServiceError . hasStatus 409 . hasCode "IpAddressInUse"

data LifeCycleState
    = Deleting
    | Creating
    | Deleted
    | Available
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText LifeCycleState where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "creating" -> pure Creating
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        e -> fail ("Failure parsing LifeCycleState from " ++ show e)

instance ToText LifeCycleState where
    toText = \case
        Available -> "available"
        Creating -> "creating"
        Deleted -> "deleted"
        Deleting -> "deleting"

instance Hashable LifeCycleState
instance ToQuery LifeCycleState
instance ToHeader LifeCycleState

instance FromJSON LifeCycleState where
    parseJSON = parseJSONText "LifeCycleState"

-- | This object provides description of a file system.
--
-- /See:/ 'fileSystemDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fsdName'
--
-- * 'fsdOwnerId'
--
-- * 'fsdCreationToken'
--
-- * 'fsdFileSystemId'
--
-- * 'fsdCreationTime'
--
-- * 'fsdLifeCycleState'
--
-- * 'fsdNumberOfMountTargets'
--
-- * 'fsdSizeInBytes'
data FileSystemDescription = FileSystemDescription'
    { _fsdName                 :: Maybe Text
    , _fsdOwnerId              :: Text
    , _fsdCreationToken        :: Text
    , _fsdFileSystemId         :: Text
    , _fsdCreationTime         :: POSIX
    , _fsdLifeCycleState       :: LifeCycleState
    , _fsdNumberOfMountTargets :: !Nat
    , _fsdSizeInBytes          :: FileSystemSize
    } deriving (Eq,Read,Show)

-- | 'FileSystemDescription' smart constructor.
fileSystemDescription :: Text -> Text -> Text -> UTCTime -> LifeCycleState -> Natural -> FileSystemSize -> FileSystemDescription
fileSystemDescription pOwnerId pCreationToken pFileSystemId pCreationTime pLifeCycleState pNumberOfMountTargets pSizeInBytes =
    FileSystemDescription'
    { _fsdName = Nothing
    , _fsdOwnerId = pOwnerId
    , _fsdCreationToken = pCreationToken
    , _fsdFileSystemId = pFileSystemId
    , _fsdCreationTime = _Time # pCreationTime
    , _fsdLifeCycleState = pLifeCycleState
    , _fsdNumberOfMountTargets = _Nat # pNumberOfMountTargets
    , _fsdSizeInBytes = pSizeInBytes
    }

-- | You can add tags to a file system (see CreateTags) including a \"Name\"
-- tag. If the file system has a \"Name\" tag, Amazon EFS returns the value
-- in this field.
fsdName :: Lens' FileSystemDescription (Maybe Text)
fsdName = lens _fsdName (\ s a -> s{_fsdName = a});

-- | The AWS account that created the file system. If the file system was
-- created by an IAM user, the parent account to which the user belongs is
-- the owner.
fsdOwnerId :: Lens' FileSystemDescription Text
fsdOwnerId = lens _fsdOwnerId (\ s a -> s{_fsdOwnerId = a});

-- | Opaque string specified in the request.
fsdCreationToken :: Lens' FileSystemDescription Text
fsdCreationToken = lens _fsdCreationToken (\ s a -> s{_fsdCreationToken = a});

-- | The file system ID assigned by Amazon EFS.
fsdFileSystemId :: Lens' FileSystemDescription Text
fsdFileSystemId = lens _fsdFileSystemId (\ s a -> s{_fsdFileSystemId = a});

-- | The time at which the file system was created, in seconds, since
-- 1970-01-01T00:00:00Z.
fsdCreationTime :: Lens' FileSystemDescription UTCTime
fsdCreationTime = lens _fsdCreationTime (\ s a -> s{_fsdCreationTime = a}) . _Time;

-- | A predefined string value that indicates the lifecycle phase of the file
-- system.
fsdLifeCycleState :: Lens' FileSystemDescription LifeCycleState
fsdLifeCycleState = lens _fsdLifeCycleState (\ s a -> s{_fsdLifeCycleState = a});

-- | The current number of mount targets (see CreateMountTarget) the file
-- system has.
fsdNumberOfMountTargets :: Lens' FileSystemDescription Natural
fsdNumberOfMountTargets = lens _fsdNumberOfMountTargets (\ s a -> s{_fsdNumberOfMountTargets = a}) . _Nat;

-- | This object provides the latest known metered size of data stored in the
-- file system, in bytes, in its @Value@ field, and the time at which that
-- size was determined in its @Timestamp@ field. The @Timestamp@ value is
-- the integer number of seconds since 1970-01-01T00:00:00Z. Note that the
-- value does not represent the size of a consistent snapshot of the file
-- system, but it is eventually consistent when there are no writes to the
-- file system. That is, the value will represent actual size only if the
-- file system is not modified for a period longer than a couple of hours.
-- Otherwise, the value is not the exact size the file system was at any
-- instant in time.
fsdSizeInBytes :: Lens' FileSystemDescription FileSystemSize
fsdSizeInBytes = lens _fsdSizeInBytes (\ s a -> s{_fsdSizeInBytes = a});

instance FromJSON FileSystemDescription where
        parseJSON
          = withObject "FileSystemDescription"
              (\ x ->
                 FileSystemDescription' <$>
                   (x .:? "Name") <*> (x .: "OwnerId") <*>
                     (x .: "CreationToken")
                     <*> (x .: "FileSystemId")
                     <*> (x .: "CreationTime")
                     <*> (x .: "LifeCycleState")
                     <*> (x .: "NumberOfMountTargets")
                     <*> (x .: "SizeInBytes"))

-- | This object provides the latest known metered size, in bytes, of data
-- stored in the file system, in its @Value@ field, and the time at which
-- that size was determined in its @Timestamp@ field. Note that the value
-- does not represent the size of a consistent snapshot of the file system,
-- but it is eventually consistent when there are no writes to the file
-- system. That is, the value will represent the actual size only if the
-- file system is not modified for a period longer than a couple of hours.
-- Otherwise, the value is not necessarily the exact size the file system
-- was at any instant in time.
--
-- /See:/ 'fileSystemSize' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fssTimestamp'
--
-- * 'fssValue'
data FileSystemSize = FileSystemSize'
    { _fssTimestamp :: Maybe POSIX
    , _fssValue     :: !Nat
    } deriving (Eq,Read,Show)

-- | 'FileSystemSize' smart constructor.
fileSystemSize :: Natural -> FileSystemSize
fileSystemSize pValue =
    FileSystemSize'
    { _fssTimestamp = Nothing
    , _fssValue = _Nat # pValue
    }

-- | The time at which the size of data, returned in the @Value@ field, was
-- determined. The value is the integer number of seconds since
-- 1970-01-01T00:00:00Z.
fssTimestamp :: Lens' FileSystemSize (Maybe UTCTime)
fssTimestamp = lens _fssTimestamp (\ s a -> s{_fssTimestamp = a}) . mapping _Time;

-- | The latest known metered size, in bytes, of data stored in the file
-- system.
fssValue :: Lens' FileSystemSize Natural
fssValue = lens _fssValue (\ s a -> s{_fssValue = a}) . _Nat;

instance FromJSON FileSystemSize where
        parseJSON
          = withObject "FileSystemSize"
              (\ x ->
                 FileSystemSize' <$>
                   (x .:? "Timestamp") <*> (x .: "Value"))

-- | This object provides description of a mount target.
--
-- /See:/ 'mountTargetDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mtdIPAddress'
--
-- * 'mtdNetworkInterfaceId'
--
-- * 'mtdOwnerId'
--
-- * 'mtdMountTargetId'
--
-- * 'mtdFileSystemId'
--
-- * 'mtdSubnetId'
--
-- * 'mtdLifeCycleState'
data MountTargetDescription = MountTargetDescription'
    { _mtdIPAddress          :: Maybe Text
    , _mtdNetworkInterfaceId :: Maybe Text
    , _mtdOwnerId            :: Maybe Text
    , _mtdMountTargetId      :: Text
    , _mtdFileSystemId       :: Text
    , _mtdSubnetId           :: Text
    , _mtdLifeCycleState     :: LifeCycleState
    } deriving (Eq,Read,Show)

-- | 'MountTargetDescription' smart constructor.
mountTargetDescription :: Text -> Text -> Text -> LifeCycleState -> MountTargetDescription
mountTargetDescription pMountTargetId pFileSystemId pSubnetId pLifeCycleState =
    MountTargetDescription'
    { _mtdIPAddress = Nothing
    , _mtdNetworkInterfaceId = Nothing
    , _mtdOwnerId = Nothing
    , _mtdMountTargetId = pMountTargetId
    , _mtdFileSystemId = pFileSystemId
    , _mtdSubnetId = pSubnetId
    , _mtdLifeCycleState = pLifeCycleState
    }

-- | The address at which the file system may be mounted via the mount
-- target.
mtdIPAddress :: Lens' MountTargetDescription (Maybe Text)
mtdIPAddress = lens _mtdIPAddress (\ s a -> s{_mtdIPAddress = a});

-- | The ID of the network interface that Amazon EFS created when it created
-- the mount target.
mtdNetworkInterfaceId :: Lens' MountTargetDescription (Maybe Text)
mtdNetworkInterfaceId = lens _mtdNetworkInterfaceId (\ s a -> s{_mtdNetworkInterfaceId = a});

-- | The AWS account ID that owns the resource.
mtdOwnerId :: Lens' MountTargetDescription (Maybe Text)
mtdOwnerId = lens _mtdOwnerId (\ s a -> s{_mtdOwnerId = a});

-- | The system-assigned mount target ID.
mtdMountTargetId :: Lens' MountTargetDescription Text
mtdMountTargetId = lens _mtdMountTargetId (\ s a -> s{_mtdMountTargetId = a});

-- | The ID of the file system for which the mount target is intended.
mtdFileSystemId :: Lens' MountTargetDescription Text
mtdFileSystemId = lens _mtdFileSystemId (\ s a -> s{_mtdFileSystemId = a});

-- | The ID of the subnet that the mount target is in.
mtdSubnetId :: Lens' MountTargetDescription Text
mtdSubnetId = lens _mtdSubnetId (\ s a -> s{_mtdSubnetId = a});

-- | The lifecycle state the mount target is in.
mtdLifeCycleState :: Lens' MountTargetDescription LifeCycleState
mtdLifeCycleState = lens _mtdLifeCycleState (\ s a -> s{_mtdLifeCycleState = a});

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

-- | A tag is a pair of key and value. The allowed characters in keys and
-- values are letters, whitespace, and numbers, representable in UTF-8, and
-- the characters \'+\', \'-\', \'=\', \'.\', \'_\', \':\', and \'\/\'.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey'
--
-- * 'tagValue'
data Tag = Tag'
    { _tagKey   :: Text
    , _tagValue :: Text
    } deriving (Eq,Read,Show)

-- | 'Tag' smart constructor.
tag :: Text -> Text -> Tag
tag pKey pValue =
    Tag'
    { _tagKey = pKey
    , _tagValue = pValue
    }

-- | Tag key, a string. The key must not start with \"aws:\".
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | Value of the tag key.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance ToJSON Tag where
        toJSON Tag'{..}
          = object ["Key" .= _tagKey, "Value" .= _tagValue]
