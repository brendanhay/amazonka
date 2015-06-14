{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.CognitoSync.Types
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

module Network.AWS.CognitoSync.Types
    (
    -- * Service
      CognitoSync
    -- ** Errors
    , JSONError

    -- * BulkPublishStatus
    , BulkPublishStatus (..)

    -- * CognitoStreams
    , CognitoStreams
    , cognitoStreams
    , csStreamingStatus
    , csStreamName
    , csRoleARN

    -- * Dataset
    , Dataset
    , dataset
    , datLastModifiedDate
    , datNumRecords
    , datDataStorage
    , datDatasetName
    , datCreationDate
    , datLastModifiedBy
    , datIdentityId

    -- * IdentityPoolUsage
    , IdentityPoolUsage
    , identityPoolUsage
    , ipuLastModifiedDate
    , ipuIdentityPoolId
    , ipuDataStorage
    , ipuSyncSessionsCount

    -- * IdentityUsage
    , IdentityUsage
    , identityUsage
    , iuLastModifiedDate
    , iuIdentityPoolId
    , iuDatasetCount
    , iuDataStorage
    , iuIdentityId

    -- * Operation
    , Operation (..)

    -- * Platform
    , Platform (..)

    -- * PushSync
    , PushSync
    , pushSync
    , psApplicationARNs
    , psRoleARN

    -- * Record
    , Record
    , record
    , recSyncCount
    , recLastModifiedDate
    , recDeviceLastModifiedDate
    , recValue
    , recKey
    , recLastModifiedBy

    -- * RecordPatch
    , RecordPatch
    , recordPatch
    , rpDeviceLastModifiedDate
    , rpValue
    , rpOp
    , rpKey
    , rpSyncCount

    -- * StreamingStatus
    , StreamingStatus (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2014-06-30@ of the Amazon Cognito Sync SDK.
data CognitoSync

instance AWSService CognitoSync where
    type Sg CognitoSync = V4
    type Er CognitoSync = JSONError

    service = service'
      where
        service' :: Service CognitoSync
        service' = Service
            { _svcAbbrev  = "CognitoSync"
            , _svcPrefix  = "cognito-sync"
            , _svcVersion = "2014-06-30"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry CognitoSync
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

data BulkPublishStatus = NotStarted | INProgress | Succeeded | Failed deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText BulkPublishStatus where
    parser = takeLowerText >>= \case
        "FAILED" -> pure Failed
        "IN_PROGRESS" -> pure INProgress
        "NOT_STARTED" -> pure NotStarted
        "SUCCEEDED" -> pure Succeeded
        e -> fail ("Failure parsing BulkPublishStatus from " ++ show e)

instance ToText BulkPublishStatus where
    toText = \case
        Failed -> "FAILED"
        INProgress -> "IN_PROGRESS"
        NotStarted -> "NOT_STARTED"
        Succeeded -> "SUCCEEDED"

instance Hashable BulkPublishStatus
instance ToQuery BulkPublishStatus
instance ToHeader BulkPublishStatus

instance FromJSON BulkPublishStatus where
    parseJSON = parseJSONText "BulkPublishStatus"

-- | /See:/ 'cognitoStreams' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csStreamingStatus'
--
-- * 'csStreamName'
--
-- * 'csRoleARN'
data CognitoStreams = CognitoStreams'{_csStreamingStatus :: Maybe StreamingStatus, _csStreamName :: Maybe Text, _csRoleARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CognitoStreams' smart constructor.
cognitoStreams :: CognitoStreams
cognitoStreams = CognitoStreams'{_csStreamingStatus = Nothing, _csStreamName = Nothing, _csRoleARN = Nothing};

-- | Status of the Cognito streams. Valid values are:
--
-- ENABLED - Streaming of updates to identity pool is enabled.
--
-- DISABLED - Streaming of updates to identity pool is disabled. Bulk
-- publish will also fail if StreamingStatus is DISABLED.
csStreamingStatus :: Lens' CognitoStreams (Maybe StreamingStatus)
csStreamingStatus = lens _csStreamingStatus (\ s a -> s{_csStreamingStatus = a});

-- | The name of the Cognito stream to receive updates. This stream must be
-- in the developers account and in the same region as the identity pool.
csStreamName :: Lens' CognitoStreams (Maybe Text)
csStreamName = lens _csStreamName (\ s a -> s{_csStreamName = a});

-- | The ARN of the role Amazon Cognito can assume in order to publish to the
-- stream. This role must grant access to Amazon Cognito (cognito-sync) to
-- invoke PutRecord on your Cognito stream.
csRoleARN :: Lens' CognitoStreams (Maybe Text)
csRoleARN = lens _csRoleARN (\ s a -> s{_csRoleARN = a});

instance FromJSON CognitoStreams where
        parseJSON
          = withObject "CognitoStreams"
              (\ x ->
                 CognitoStreams' <$>
                   x .:? "StreamingStatus" <*> x .:? "StreamName" <*>
                     x .:? "RoleArn")

instance ToJSON CognitoStreams where
        toJSON CognitoStreams'{..}
          = object
              ["StreamingStatus" .= _csStreamingStatus,
               "StreamName" .= _csStreamName,
               "RoleArn" .= _csRoleARN]

-- | /See:/ 'dataset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datLastModifiedDate'
--
-- * 'datNumRecords'
--
-- * 'datDataStorage'
--
-- * 'datDatasetName'
--
-- * 'datCreationDate'
--
-- * 'datLastModifiedBy'
--
-- * 'datIdentityId'
data Dataset = Dataset'{_datLastModifiedDate :: Maybe POSIX, _datNumRecords :: Maybe Integer, _datDataStorage :: Maybe Integer, _datDatasetName :: Maybe Text, _datCreationDate :: Maybe POSIX, _datLastModifiedBy :: Maybe Text, _datIdentityId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Dataset' smart constructor.
dataset :: Dataset
dataset = Dataset'{_datLastModifiedDate = Nothing, _datNumRecords = Nothing, _datDataStorage = Nothing, _datDatasetName = Nothing, _datCreationDate = Nothing, _datLastModifiedBy = Nothing, _datIdentityId = Nothing};

-- | Date when the dataset was last modified.
datLastModifiedDate :: Lens' Dataset (Maybe UTCTime)
datLastModifiedDate = lens _datLastModifiedDate (\ s a -> s{_datLastModifiedDate = a}) . mapping _Time;

-- | Number of records in this dataset.
datNumRecords :: Lens' Dataset (Maybe Integer)
datNumRecords = lens _datNumRecords (\ s a -> s{_datNumRecords = a});

-- | Total size in bytes of the records in this dataset.
datDataStorage :: Lens' Dataset (Maybe Integer)
datDataStorage = lens _datDataStorage (\ s a -> s{_datDataStorage = a});

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9,
-- \'_\' (underscore), \'-\' (dash), and \'.\' (dot).
datDatasetName :: Lens' Dataset (Maybe Text)
datDatasetName = lens _datDatasetName (\ s a -> s{_datDatasetName = a});

-- | Date on which the dataset was created.
datCreationDate :: Lens' Dataset (Maybe UTCTime)
datCreationDate = lens _datCreationDate (\ s a -> s{_datCreationDate = a}) . mapping _Time;

-- | The device that made the last change to this dataset.
datLastModifiedBy :: Lens' Dataset (Maybe Text)
datLastModifiedBy = lens _datLastModifiedBy (\ s a -> s{_datLastModifiedBy = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
datIdentityId :: Lens' Dataset (Maybe Text)
datIdentityId = lens _datIdentityId (\ s a -> s{_datIdentityId = a});

instance FromJSON Dataset where
        parseJSON
          = withObject "Dataset"
              (\ x ->
                 Dataset' <$>
                   x .:? "LastModifiedDate" <*> x .:? "NumRecords" <*>
                     x .:? "DataStorage"
                     <*> x .:? "DatasetName"
                     <*> x .:? "CreationDate"
                     <*> x .:? "LastModifiedBy"
                     <*> x .:? "IdentityId")

-- | /See:/ 'identityPoolUsage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipuLastModifiedDate'
--
-- * 'ipuIdentityPoolId'
--
-- * 'ipuDataStorage'
--
-- * 'ipuSyncSessionsCount'
data IdentityPoolUsage = IdentityPoolUsage'{_ipuLastModifiedDate :: Maybe POSIX, _ipuIdentityPoolId :: Maybe Text, _ipuDataStorage :: Maybe Integer, _ipuSyncSessionsCount :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'IdentityPoolUsage' smart constructor.
identityPoolUsage :: IdentityPoolUsage
identityPoolUsage = IdentityPoolUsage'{_ipuLastModifiedDate = Nothing, _ipuIdentityPoolId = Nothing, _ipuDataStorage = Nothing, _ipuSyncSessionsCount = Nothing};

-- | Date on which the identity pool was last modified.
ipuLastModifiedDate :: Lens' IdentityPoolUsage (Maybe UTCTime)
ipuLastModifiedDate = lens _ipuLastModifiedDate (\ s a -> s{_ipuLastModifiedDate = a}) . mapping _Time;

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
ipuIdentityPoolId :: Lens' IdentityPoolUsage (Maybe Text)
ipuIdentityPoolId = lens _ipuIdentityPoolId (\ s a -> s{_ipuIdentityPoolId = a});

-- | Data storage information for the identity pool.
ipuDataStorage :: Lens' IdentityPoolUsage (Maybe Integer)
ipuDataStorage = lens _ipuDataStorage (\ s a -> s{_ipuDataStorage = a});

-- | Number of sync sessions for the identity pool.
ipuSyncSessionsCount :: Lens' IdentityPoolUsage (Maybe Integer)
ipuSyncSessionsCount = lens _ipuSyncSessionsCount (\ s a -> s{_ipuSyncSessionsCount = a});

instance FromJSON IdentityPoolUsage where
        parseJSON
          = withObject "IdentityPoolUsage"
              (\ x ->
                 IdentityPoolUsage' <$>
                   x .:? "LastModifiedDate" <*> x .:? "IdentityPoolId"
                     <*> x .:? "DataStorage"
                     <*> x .:? "SyncSessionsCount")

-- | /See:/ 'identityUsage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iuLastModifiedDate'
--
-- * 'iuIdentityPoolId'
--
-- * 'iuDatasetCount'
--
-- * 'iuDataStorage'
--
-- * 'iuIdentityId'
data IdentityUsage = IdentityUsage'{_iuLastModifiedDate :: Maybe POSIX, _iuIdentityPoolId :: Maybe Text, _iuDatasetCount :: Maybe Int, _iuDataStorage :: Maybe Integer, _iuIdentityId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'IdentityUsage' smart constructor.
identityUsage :: IdentityUsage
identityUsage = IdentityUsage'{_iuLastModifiedDate = Nothing, _iuIdentityPoolId = Nothing, _iuDatasetCount = Nothing, _iuDataStorage = Nothing, _iuIdentityId = Nothing};

-- | Date on which the identity was last modified.
iuLastModifiedDate :: Lens' IdentityUsage (Maybe UTCTime)
iuLastModifiedDate = lens _iuLastModifiedDate (\ s a -> s{_iuLastModifiedDate = a}) . mapping _Time;

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
iuIdentityPoolId :: Lens' IdentityUsage (Maybe Text)
iuIdentityPoolId = lens _iuIdentityPoolId (\ s a -> s{_iuIdentityPoolId = a});

-- | Number of datasets for the identity.
iuDatasetCount :: Lens' IdentityUsage (Maybe Int)
iuDatasetCount = lens _iuDatasetCount (\ s a -> s{_iuDatasetCount = a});

-- | Total data storage for this identity.
iuDataStorage :: Lens' IdentityUsage (Maybe Integer)
iuDataStorage = lens _iuDataStorage (\ s a -> s{_iuDataStorage = a});

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
iuIdentityId :: Lens' IdentityUsage (Maybe Text)
iuIdentityId = lens _iuIdentityId (\ s a -> s{_iuIdentityId = a});

instance FromJSON IdentityUsage where
        parseJSON
          = withObject "IdentityUsage"
              (\ x ->
                 IdentityUsage' <$>
                   x .:? "LastModifiedDate" <*> x .:? "IdentityPoolId"
                     <*> x .:? "DatasetCount"
                     <*> x .:? "DataStorage"
                     <*> x .:? "IdentityId")

data Operation = Replace | Remove deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText Operation where
    parser = takeLowerText >>= \case
        "remove" -> pure Remove
        "replace" -> pure Replace
        e -> fail ("Failure parsing Operation from " ++ show e)

instance ToText Operation where
    toText = \case
        Remove -> "remove"
        Replace -> "replace"

instance Hashable Operation
instance ToQuery Operation
instance ToHeader Operation

instance ToJSON Operation where
    toJSON = toJSONText

data Platform = GCM | APNS | ADM | APNSSandbox deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText Platform where
    parser = takeLowerText >>= \case
        "ADM" -> pure ADM
        "APNS" -> pure APNS
        "APNS_SANDBOX" -> pure APNSSandbox
        "GCM" -> pure GCM
        e -> fail ("Failure parsing Platform from " ++ show e)

instance ToText Platform where
    toText = \case
        ADM -> "ADM"
        APNS -> "APNS"
        APNSSandbox -> "APNS_SANDBOX"
        GCM -> "GCM"

instance Hashable Platform
instance ToQuery Platform
instance ToHeader Platform

instance ToJSON Platform where
    toJSON = toJSONText

-- | /See:/ 'pushSync' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psApplicationARNs'
--
-- * 'psRoleARN'
data PushSync = PushSync'{_psApplicationARNs :: Maybe [Text], _psRoleARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'PushSync' smart constructor.
pushSync :: PushSync
pushSync = PushSync'{_psApplicationARNs = Nothing, _psRoleARN = Nothing};

-- | List of SNS platform application ARNs that could be used by clients.
psApplicationARNs :: Lens' PushSync (Maybe [Text])
psApplicationARNs = lens _psApplicationARNs (\ s a -> s{_psApplicationARNs = a});

-- | A role configured to allow Cognito to call SNS on behalf of the
-- developer.
psRoleARN :: Lens' PushSync (Maybe Text)
psRoleARN = lens _psRoleARN (\ s a -> s{_psRoleARN = a});

instance FromJSON PushSync where
        parseJSON
          = withObject "PushSync"
              (\ x ->
                 PushSync' <$>
                   x .:? "ApplicationArns" .!= mempty <*>
                     x .:? "RoleArn")

instance ToJSON PushSync where
        toJSON PushSync'{..}
          = object
              ["ApplicationArns" .= _psApplicationARNs,
               "RoleArn" .= _psRoleARN]

-- | /See:/ 'record' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'recSyncCount'
--
-- * 'recLastModifiedDate'
--
-- * 'recDeviceLastModifiedDate'
--
-- * 'recValue'
--
-- * 'recKey'
--
-- * 'recLastModifiedBy'
data Record = Record'{_recSyncCount :: Maybe Integer, _recLastModifiedDate :: Maybe POSIX, _recDeviceLastModifiedDate :: Maybe POSIX, _recValue :: Maybe Text, _recKey :: Maybe Text, _recLastModifiedBy :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Record' smart constructor.
record :: Record
record = Record'{_recSyncCount = Nothing, _recLastModifiedDate = Nothing, _recDeviceLastModifiedDate = Nothing, _recValue = Nothing, _recKey = Nothing, _recLastModifiedBy = Nothing};

-- | The server sync count for this record.
recSyncCount :: Lens' Record (Maybe Integer)
recSyncCount = lens _recSyncCount (\ s a -> s{_recSyncCount = a});

-- | The date on which the record was last modified.
recLastModifiedDate :: Lens' Record (Maybe UTCTime)
recLastModifiedDate = lens _recLastModifiedDate (\ s a -> s{_recLastModifiedDate = a}) . mapping _Time;

-- | The last modified date of the client device.
recDeviceLastModifiedDate :: Lens' Record (Maybe UTCTime)
recDeviceLastModifiedDate = lens _recDeviceLastModifiedDate (\ s a -> s{_recDeviceLastModifiedDate = a}) . mapping _Time;

-- | The value for the record.
recValue :: Lens' Record (Maybe Text)
recValue = lens _recValue (\ s a -> s{_recValue = a});

-- | The key for the record.
recKey :: Lens' Record (Maybe Text)
recKey = lens _recKey (\ s a -> s{_recKey = a});

-- | The user\/device that made the last change to this record.
recLastModifiedBy :: Lens' Record (Maybe Text)
recLastModifiedBy = lens _recLastModifiedBy (\ s a -> s{_recLastModifiedBy = a});

instance FromJSON Record where
        parseJSON
          = withObject "Record"
              (\ x ->
                 Record' <$>
                   x .:? "SyncCount" <*> x .:? "LastModifiedDate" <*>
                     x .:? "DeviceLastModifiedDate"
                     <*> x .:? "Value"
                     <*> x .:? "Key"
                     <*> x .:? "LastModifiedBy")

-- | /See:/ 'recordPatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpDeviceLastModifiedDate'
--
-- * 'rpValue'
--
-- * 'rpOp'
--
-- * 'rpKey'
--
-- * 'rpSyncCount'
data RecordPatch = RecordPatch'{_rpDeviceLastModifiedDate :: Maybe POSIX, _rpValue :: Maybe Text, _rpOp :: Operation, _rpKey :: Text, _rpSyncCount :: Integer} deriving (Eq, Read, Show)

-- | 'RecordPatch' smart constructor.
recordPatch :: Operation -> Text -> Integer -> RecordPatch
recordPatch pOp pKey pSyncCount = RecordPatch'{_rpDeviceLastModifiedDate = Nothing, _rpValue = Nothing, _rpOp = pOp, _rpKey = pKey, _rpSyncCount = pSyncCount};

-- | The last modified date of the client device.
rpDeviceLastModifiedDate :: Lens' RecordPatch (Maybe UTCTime)
rpDeviceLastModifiedDate = lens _rpDeviceLastModifiedDate (\ s a -> s{_rpDeviceLastModifiedDate = a}) . mapping _Time;

-- | The value associated with the record patch.
rpValue :: Lens' RecordPatch (Maybe Text)
rpValue = lens _rpValue (\ s a -> s{_rpValue = a});

-- | An operation, either replace or remove.
rpOp :: Lens' RecordPatch Operation
rpOp = lens _rpOp (\ s a -> s{_rpOp = a});

-- | The key associated with the record patch.
rpKey :: Lens' RecordPatch Text
rpKey = lens _rpKey (\ s a -> s{_rpKey = a});

-- | Last known server sync count for this record. Set to 0 if unknown.
rpSyncCount :: Lens' RecordPatch Integer
rpSyncCount = lens _rpSyncCount (\ s a -> s{_rpSyncCount = a});

instance ToJSON RecordPatch where
        toJSON RecordPatch'{..}
          = object
              ["DeviceLastModifiedDate" .=
                 _rpDeviceLastModifiedDate,
               "Value" .= _rpValue, "Op" .= _rpOp, "Key" .= _rpKey,
               "SyncCount" .= _rpSyncCount]

data StreamingStatus = Enabled | Disabled deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText StreamingStatus where
    parser = takeLowerText >>= \case
        "DISABLED" -> pure Disabled
        "ENABLED" -> pure Enabled
        e -> fail ("Failure parsing StreamingStatus from " ++ show e)

instance ToText StreamingStatus where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable StreamingStatus
instance ToQuery StreamingStatus
instance ToHeader StreamingStatus

instance ToJSON StreamingStatus where
    toJSON = toJSONText

instance FromJSON StreamingStatus where
    parseJSON = parseJSONText "StreamingStatus"
