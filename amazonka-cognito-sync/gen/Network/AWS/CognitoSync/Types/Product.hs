{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.Product where

import           Network.AWS.CognitoSync.Types.Sum
import           Network.AWS.Prelude

-- | Configuration options for configure Cognito streams.
--
-- /See:/ 'cognitoStreams' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csStreamingStatus'
--
-- * 'csStreamName'
--
-- * 'csRoleARN'
data CognitoStreams = CognitoStreams'
    { _csStreamingStatus :: !(Maybe StreamingStatus)
    , _csStreamName      :: !(Maybe Text)
    , _csRoleARN         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CognitoStreams' smart constructor.
cognitoStreams :: CognitoStreams
cognitoStreams =
    CognitoStreams'
    { _csStreamingStatus = Nothing
    , _csStreamName = Nothing
    , _csRoleARN = Nothing
    }

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
                   (x .:? "StreamingStatus") <*> (x .:? "StreamName")
                     <*> (x .:? "RoleArn"))

instance ToJSON CognitoStreams where
        toJSON CognitoStreams'{..}
          = object
              ["StreamingStatus" .= _csStreamingStatus,
               "StreamName" .= _csStreamName,
               "RoleArn" .= _csRoleARN]

-- | A collection of data for an identity pool. An identity pool can have
-- multiple datasets. A dataset is per identity and can be general or
-- associated with a particular entity in an application (like a saved
-- game). Datasets are automatically created if they don\'t exist. Data is
-- synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /See:/ 'dataset' smart constructor.
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
data Dataset = Dataset'
    { _datLastModifiedDate :: !(Maybe POSIX)
    , _datNumRecords       :: !(Maybe Integer)
    , _datDataStorage      :: !(Maybe Integer)
    , _datDatasetName      :: !(Maybe Text)
    , _datCreationDate     :: !(Maybe POSIX)
    , _datLastModifiedBy   :: !(Maybe Text)
    , _datIdentityId       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Dataset' smart constructor.
dataset :: Dataset
dataset =
    Dataset'
    { _datLastModifiedDate = Nothing
    , _datNumRecords = Nothing
    , _datDataStorage = Nothing
    , _datDatasetName = Nothing
    , _datCreationDate = Nothing
    , _datLastModifiedBy = Nothing
    , _datIdentityId = Nothing
    }

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
                   (x .:? "LastModifiedDate") <*> (x .:? "NumRecords")
                     <*> (x .:? "DataStorage")
                     <*> (x .:? "DatasetName")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "IdentityId"))

-- | Usage information for the identity pool.
--
-- /See:/ 'identityPoolUsage' smart constructor.
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
data IdentityPoolUsage = IdentityPoolUsage'
    { _ipuLastModifiedDate  :: !(Maybe POSIX)
    , _ipuIdentityPoolId    :: !(Maybe Text)
    , _ipuDataStorage       :: !(Maybe Integer)
    , _ipuSyncSessionsCount :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'IdentityPoolUsage' smart constructor.
identityPoolUsage :: IdentityPoolUsage
identityPoolUsage =
    IdentityPoolUsage'
    { _ipuLastModifiedDate = Nothing
    , _ipuIdentityPoolId = Nothing
    , _ipuDataStorage = Nothing
    , _ipuSyncSessionsCount = Nothing
    }

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
                   (x .:? "LastModifiedDate") <*>
                     (x .:? "IdentityPoolId")
                     <*> (x .:? "DataStorage")
                     <*> (x .:? "SyncSessionsCount"))

-- | Usage information for the identity.
--
-- /See:/ 'identityUsage' smart constructor.
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
data IdentityUsage = IdentityUsage'
    { _iuLastModifiedDate :: !(Maybe POSIX)
    , _iuIdentityPoolId   :: !(Maybe Text)
    , _iuDatasetCount     :: !(Maybe Int)
    , _iuDataStorage      :: !(Maybe Integer)
    , _iuIdentityId       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'IdentityUsage' smart constructor.
identityUsage :: IdentityUsage
identityUsage =
    IdentityUsage'
    { _iuLastModifiedDate = Nothing
    , _iuIdentityPoolId = Nothing
    , _iuDatasetCount = Nothing
    , _iuDataStorage = Nothing
    , _iuIdentityId = Nothing
    }

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
                   (x .:? "LastModifiedDate") <*>
                     (x .:? "IdentityPoolId")
                     <*> (x .:? "DatasetCount")
                     <*> (x .:? "DataStorage")
                     <*> (x .:? "IdentityId"))

-- | Configuration options to be applied to the identity pool.
--
-- /See:/ 'pushSync' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psApplicationARNs'
--
-- * 'psRoleARN'
data PushSync = PushSync'
    { _psApplicationARNs :: !(Maybe [Text])
    , _psRoleARN         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PushSync' smart constructor.
pushSync :: PushSync
pushSync =
    PushSync'
    { _psApplicationARNs = Nothing
    , _psRoleARN = Nothing
    }

-- | List of SNS platform application ARNs that could be used by clients.
psApplicationARNs :: Lens' PushSync [Text]
psApplicationARNs = lens _psApplicationARNs (\ s a -> s{_psApplicationARNs = a}) . _Default;

-- | A role configured to allow Cognito to call SNS on behalf of the
-- developer.
psRoleARN :: Lens' PushSync (Maybe Text)
psRoleARN = lens _psRoleARN (\ s a -> s{_psRoleARN = a});

instance FromJSON PushSync where
        parseJSON
          = withObject "PushSync"
              (\ x ->
                 PushSync' <$>
                   (x .:? "ApplicationArns" .!= mempty) <*>
                     (x .:? "RoleArn"))

instance ToJSON PushSync where
        toJSON PushSync'{..}
          = object
              ["ApplicationArns" .= _psApplicationARNs,
               "RoleArn" .= _psRoleARN]

-- | The basic data structure of a dataset.
--
-- /See:/ 'record' smart constructor.
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
data Record = Record'
    { _recSyncCount              :: !(Maybe Integer)
    , _recLastModifiedDate       :: !(Maybe POSIX)
    , _recDeviceLastModifiedDate :: !(Maybe POSIX)
    , _recValue                  :: !(Maybe Text)
    , _recKey                    :: !(Maybe Text)
    , _recLastModifiedBy         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Record' smart constructor.
record :: Record
record =
    Record'
    { _recSyncCount = Nothing
    , _recLastModifiedDate = Nothing
    , _recDeviceLastModifiedDate = Nothing
    , _recValue = Nothing
    , _recKey = Nothing
    , _recLastModifiedBy = Nothing
    }

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
                   (x .:? "SyncCount") <*> (x .:? "LastModifiedDate")
                     <*> (x .:? "DeviceLastModifiedDate")
                     <*> (x .:? "Value")
                     <*> (x .:? "Key")
                     <*> (x .:? "LastModifiedBy"))

-- | An update operation for a record.
--
-- /See:/ 'recordPatch' smart constructor.
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
data RecordPatch = RecordPatch'
    { _rpDeviceLastModifiedDate :: !(Maybe POSIX)
    , _rpValue                  :: !(Maybe Text)
    , _rpOp                     :: !Operation
    , _rpKey                    :: !Text
    , _rpSyncCount              :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RecordPatch' smart constructor.
recordPatch :: Operation -> Text -> Integer -> RecordPatch
recordPatch pOp pKey pSyncCount =
    RecordPatch'
    { _rpDeviceLastModifiedDate = Nothing
    , _rpValue = Nothing
    , _rpOp = pOp
    , _rpKey = pKey
    , _rpSyncCount = pSyncCount
    }

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
