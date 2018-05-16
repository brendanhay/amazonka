{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.Product where

import Network.AWS.CognitoSync.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration options for configure Cognito streams.
--
-- /See:/ 'cognitoStreams' smart constructor.
data CognitoStreams = CognitoStreams'
  { _csStreamingStatus :: !(Maybe StreamingStatus)
  , _csStreamName      :: !(Maybe Text)
  , _csRoleARN         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CognitoStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csStreamingStatus' - Status of the Cognito streams. Valid values are: ENABLED - Streaming of updates to identity pool is enabled. DISABLED - Streaming of updates to identity pool is disabled. Bulk publish will also fail if StreamingStatus is DISABLED.
--
-- * 'csStreamName' - The name of the Cognito stream to receive updates. This stream must be in the developers account and in the same region as the identity pool.
--
-- * 'csRoleARN' - The ARN of the role Amazon Cognito can assume in order to publish to the stream. This role must grant access to Amazon Cognito (cognito-sync) to invoke PutRecord on your Cognito stream.
cognitoStreams
    :: CognitoStreams
cognitoStreams =
  CognitoStreams'
    { _csStreamingStatus = Nothing
    , _csStreamName = Nothing
    , _csRoleARN = Nothing
    }


-- | Status of the Cognito streams. Valid values are: ENABLED - Streaming of updates to identity pool is enabled. DISABLED - Streaming of updates to identity pool is disabled. Bulk publish will also fail if StreamingStatus is DISABLED.
csStreamingStatus :: Lens' CognitoStreams (Maybe StreamingStatus)
csStreamingStatus = lens _csStreamingStatus (\ s a -> s{_csStreamingStatus = a})

-- | The name of the Cognito stream to receive updates. This stream must be in the developers account and in the same region as the identity pool.
csStreamName :: Lens' CognitoStreams (Maybe Text)
csStreamName = lens _csStreamName (\ s a -> s{_csStreamName = a})

-- | The ARN of the role Amazon Cognito can assume in order to publish to the stream. This role must grant access to Amazon Cognito (cognito-sync) to invoke PutRecord on your Cognito stream.
csRoleARN :: Lens' CognitoStreams (Maybe Text)
csRoleARN = lens _csRoleARN (\ s a -> s{_csRoleARN = a})

instance FromJSON CognitoStreams where
        parseJSON
          = withObject "CognitoStreams"
              (\ x ->
                 CognitoStreams' <$>
                   (x .:? "StreamingStatus") <*> (x .:? "StreamName")
                     <*> (x .:? "RoleArn"))

instance Hashable CognitoStreams where

instance NFData CognitoStreams where

instance ToJSON CognitoStreams where
        toJSON CognitoStreams'{..}
          = object
              (catMaybes
                 [("StreamingStatus" .=) <$> _csStreamingStatus,
                  ("StreamName" .=) <$> _csStreamName,
                  ("RoleArn" .=) <$> _csRoleARN])

-- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /See:/ 'dataset' smart constructor.
data Dataset = Dataset'
  { _dLastModifiedDate :: !(Maybe POSIX)
  , _dNumRecords       :: !(Maybe Integer)
  , _dDataStorage      :: !(Maybe Integer)
  , _dDatasetName      :: !(Maybe Text)
  , _dCreationDate     :: !(Maybe POSIX)
  , _dLastModifiedBy   :: !(Maybe Text)
  , _dIdentityId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Dataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLastModifiedDate' - Date when the dataset was last modified.
--
-- * 'dNumRecords' - Number of records in this dataset.
--
-- * 'dDataStorage' - Total size in bytes of the records in this dataset.
--
-- * 'dDatasetName' - A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- * 'dCreationDate' - Date on which the dataset was created.
--
-- * 'dLastModifiedBy' - The device that made the last change to this dataset.
--
-- * 'dIdentityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
dataset
    :: Dataset
dataset =
  Dataset'
    { _dLastModifiedDate = Nothing
    , _dNumRecords = Nothing
    , _dDataStorage = Nothing
    , _dDatasetName = Nothing
    , _dCreationDate = Nothing
    , _dLastModifiedBy = Nothing
    , _dIdentityId = Nothing
    }


-- | Date when the dataset was last modified.
dLastModifiedDate :: Lens' Dataset (Maybe UTCTime)
dLastModifiedDate = lens _dLastModifiedDate (\ s a -> s{_dLastModifiedDate = a}) . mapping _Time

-- | Number of records in this dataset.
dNumRecords :: Lens' Dataset (Maybe Integer)
dNumRecords = lens _dNumRecords (\ s a -> s{_dNumRecords = a})

-- | Total size in bytes of the records in this dataset.
dDataStorage :: Lens' Dataset (Maybe Integer)
dDataStorage = lens _dDataStorage (\ s a -> s{_dDataStorage = a})

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
dDatasetName :: Lens' Dataset (Maybe Text)
dDatasetName = lens _dDatasetName (\ s a -> s{_dDatasetName = a})

-- | Date on which the dataset was created.
dCreationDate :: Lens' Dataset (Maybe UTCTime)
dCreationDate = lens _dCreationDate (\ s a -> s{_dCreationDate = a}) . mapping _Time

-- | The device that made the last change to this dataset.
dLastModifiedBy :: Lens' Dataset (Maybe Text)
dLastModifiedBy = lens _dLastModifiedBy (\ s a -> s{_dLastModifiedBy = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
dIdentityId :: Lens' Dataset (Maybe Text)
dIdentityId = lens _dIdentityId (\ s a -> s{_dIdentityId = a})

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

instance Hashable Dataset where

instance NFData Dataset where

-- | Usage information for the identity pool.
--
-- /See:/ 'identityPoolUsage' smart constructor.
data IdentityPoolUsage = IdentityPoolUsage'
  { _ipuLastModifiedDate  :: !(Maybe POSIX)
  , _ipuIdentityPoolId    :: !(Maybe Text)
  , _ipuDataStorage       :: !(Maybe Integer)
  , _ipuSyncSessionsCount :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdentityPoolUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipuLastModifiedDate' - Date on which the identity pool was last modified.
--
-- * 'ipuIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'ipuDataStorage' - Data storage information for the identity pool.
--
-- * 'ipuSyncSessionsCount' - Number of sync sessions for the identity pool.
identityPoolUsage
    :: IdentityPoolUsage
identityPoolUsage =
  IdentityPoolUsage'
    { _ipuLastModifiedDate = Nothing
    , _ipuIdentityPoolId = Nothing
    , _ipuDataStorage = Nothing
    , _ipuSyncSessionsCount = Nothing
    }


-- | Date on which the identity pool was last modified.
ipuLastModifiedDate :: Lens' IdentityPoolUsage (Maybe UTCTime)
ipuLastModifiedDate = lens _ipuLastModifiedDate (\ s a -> s{_ipuLastModifiedDate = a}) . mapping _Time

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
ipuIdentityPoolId :: Lens' IdentityPoolUsage (Maybe Text)
ipuIdentityPoolId = lens _ipuIdentityPoolId (\ s a -> s{_ipuIdentityPoolId = a})

-- | Data storage information for the identity pool.
ipuDataStorage :: Lens' IdentityPoolUsage (Maybe Integer)
ipuDataStorage = lens _ipuDataStorage (\ s a -> s{_ipuDataStorage = a})

-- | Number of sync sessions for the identity pool.
ipuSyncSessionsCount :: Lens' IdentityPoolUsage (Maybe Integer)
ipuSyncSessionsCount = lens _ipuSyncSessionsCount (\ s a -> s{_ipuSyncSessionsCount = a})

instance FromJSON IdentityPoolUsage where
        parseJSON
          = withObject "IdentityPoolUsage"
              (\ x ->
                 IdentityPoolUsage' <$>
                   (x .:? "LastModifiedDate") <*>
                     (x .:? "IdentityPoolId")
                     <*> (x .:? "DataStorage")
                     <*> (x .:? "SyncSessionsCount"))

instance Hashable IdentityPoolUsage where

instance NFData IdentityPoolUsage where

-- | Usage information for the identity.
--
-- /See:/ 'identityUsage' smart constructor.
data IdentityUsage = IdentityUsage'
  { _iuLastModifiedDate :: !(Maybe POSIX)
  , _iuIdentityPoolId   :: !(Maybe Text)
  , _iuDatasetCount     :: !(Maybe Int)
  , _iuDataStorage      :: !(Maybe Integer)
  , _iuIdentityId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IdentityUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iuLastModifiedDate' - Date on which the identity was last modified.
--
-- * 'iuIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'iuDatasetCount' - Number of datasets for the identity.
--
-- * 'iuDataStorage' - Total data storage for this identity.
--
-- * 'iuIdentityId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
identityUsage
    :: IdentityUsage
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
iuLastModifiedDate = lens _iuLastModifiedDate (\ s a -> s{_iuLastModifiedDate = a}) . mapping _Time

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
iuIdentityPoolId :: Lens' IdentityUsage (Maybe Text)
iuIdentityPoolId = lens _iuIdentityPoolId (\ s a -> s{_iuIdentityPoolId = a})

-- | Number of datasets for the identity.
iuDatasetCount :: Lens' IdentityUsage (Maybe Int)
iuDatasetCount = lens _iuDatasetCount (\ s a -> s{_iuDatasetCount = a})

-- | Total data storage for this identity.
iuDataStorage :: Lens' IdentityUsage (Maybe Integer)
iuDataStorage = lens _iuDataStorage (\ s a -> s{_iuDataStorage = a})

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
iuIdentityId :: Lens' IdentityUsage (Maybe Text)
iuIdentityId = lens _iuIdentityId (\ s a -> s{_iuIdentityId = a})

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

instance Hashable IdentityUsage where

instance NFData IdentityUsage where

-- | Configuration options to be applied to the identity pool.
--
--
--
-- /See:/ 'pushSync' smart constructor.
data PushSync = PushSync'
  { _psApplicationARNs :: !(Maybe [Text])
  , _psRoleARN         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PushSync' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psApplicationARNs' - List of SNS platform application ARNs that could be used by clients.
--
-- * 'psRoleARN' - A role configured to allow Cognito to call SNS on behalf of the developer.
pushSync
    :: PushSync
pushSync = PushSync' {_psApplicationARNs = Nothing, _psRoleARN = Nothing}


-- | List of SNS platform application ARNs that could be used by clients.
psApplicationARNs :: Lens' PushSync [Text]
psApplicationARNs = lens _psApplicationARNs (\ s a -> s{_psApplicationARNs = a}) . _Default . _Coerce

-- | A role configured to allow Cognito to call SNS on behalf of the developer.
psRoleARN :: Lens' PushSync (Maybe Text)
psRoleARN = lens _psRoleARN (\ s a -> s{_psRoleARN = a})

instance FromJSON PushSync where
        parseJSON
          = withObject "PushSync"
              (\ x ->
                 PushSync' <$>
                   (x .:? "ApplicationArns" .!= mempty) <*>
                     (x .:? "RoleArn"))

instance Hashable PushSync where

instance NFData PushSync where

instance ToJSON PushSync where
        toJSON PushSync'{..}
          = object
              (catMaybes
                 [("ApplicationArns" .=) <$> _psApplicationARNs,
                  ("RoleArn" .=) <$> _psRoleARN])

-- | The basic data structure of a dataset.
--
-- /See:/ 'record' smart constructor.
data Record = Record'
  { _rSyncCount              :: !(Maybe Integer)
  , _rDeviceLastModifiedDate :: !(Maybe POSIX)
  , _rLastModifiedDate       :: !(Maybe POSIX)
  , _rValue                  :: !(Maybe Text)
  , _rKey                    :: !(Maybe Text)
  , _rLastModifiedBy         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rSyncCount' - The server sync count for this record.
--
-- * 'rDeviceLastModifiedDate' - The last modified date of the client device.
--
-- * 'rLastModifiedDate' - The date on which the record was last modified.
--
-- * 'rValue' - The value for the record.
--
-- * 'rKey' - The key for the record.
--
-- * 'rLastModifiedBy' - The user/device that made the last change to this record.
record
    :: Record
record =
  Record'
    { _rSyncCount = Nothing
    , _rDeviceLastModifiedDate = Nothing
    , _rLastModifiedDate = Nothing
    , _rValue = Nothing
    , _rKey = Nothing
    , _rLastModifiedBy = Nothing
    }


-- | The server sync count for this record.
rSyncCount :: Lens' Record (Maybe Integer)
rSyncCount = lens _rSyncCount (\ s a -> s{_rSyncCount = a})

-- | The last modified date of the client device.
rDeviceLastModifiedDate :: Lens' Record (Maybe UTCTime)
rDeviceLastModifiedDate = lens _rDeviceLastModifiedDate (\ s a -> s{_rDeviceLastModifiedDate = a}) . mapping _Time

-- | The date on which the record was last modified.
rLastModifiedDate :: Lens' Record (Maybe UTCTime)
rLastModifiedDate = lens _rLastModifiedDate (\ s a -> s{_rLastModifiedDate = a}) . mapping _Time

-- | The value for the record.
rValue :: Lens' Record (Maybe Text)
rValue = lens _rValue (\ s a -> s{_rValue = a})

-- | The key for the record.
rKey :: Lens' Record (Maybe Text)
rKey = lens _rKey (\ s a -> s{_rKey = a})

-- | The user/device that made the last change to this record.
rLastModifiedBy :: Lens' Record (Maybe Text)
rLastModifiedBy = lens _rLastModifiedBy (\ s a -> s{_rLastModifiedBy = a})

instance FromJSON Record where
        parseJSON
          = withObject "Record"
              (\ x ->
                 Record' <$>
                   (x .:? "SyncCount") <*>
                     (x .:? "DeviceLastModifiedDate")
                     <*> (x .:? "LastModifiedDate")
                     <*> (x .:? "Value")
                     <*> (x .:? "Key")
                     <*> (x .:? "LastModifiedBy"))

instance Hashable Record where

instance NFData Record where

-- | An update operation for a record.
--
-- /See:/ 'recordPatch' smart constructor.
data RecordPatch = RecordPatch'
  { _rpDeviceLastModifiedDate :: !(Maybe POSIX)
  , _rpValue                  :: !(Maybe Text)
  , _rpOp                     :: !Operation
  , _rpKey                    :: !Text
  , _rpSyncCount              :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordPatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpDeviceLastModifiedDate' - The last modified date of the client device.
--
-- * 'rpValue' - The value associated with the record patch.
--
-- * 'rpOp' - An operation, either replace or remove.
--
-- * 'rpKey' - The key associated with the record patch.
--
-- * 'rpSyncCount' - Last known server sync count for this record. Set to 0 if unknown.
recordPatch
    :: Operation -- ^ 'rpOp'
    -> Text -- ^ 'rpKey'
    -> Integer -- ^ 'rpSyncCount'
    -> RecordPatch
recordPatch pOp_ pKey_ pSyncCount_ =
  RecordPatch'
    { _rpDeviceLastModifiedDate = Nothing
    , _rpValue = Nothing
    , _rpOp = pOp_
    , _rpKey = pKey_
    , _rpSyncCount = pSyncCount_
    }


-- | The last modified date of the client device.
rpDeviceLastModifiedDate :: Lens' RecordPatch (Maybe UTCTime)
rpDeviceLastModifiedDate = lens _rpDeviceLastModifiedDate (\ s a -> s{_rpDeviceLastModifiedDate = a}) . mapping _Time

-- | The value associated with the record patch.
rpValue :: Lens' RecordPatch (Maybe Text)
rpValue = lens _rpValue (\ s a -> s{_rpValue = a})

-- | An operation, either replace or remove.
rpOp :: Lens' RecordPatch Operation
rpOp = lens _rpOp (\ s a -> s{_rpOp = a})

-- | The key associated with the record patch.
rpKey :: Lens' RecordPatch Text
rpKey = lens _rpKey (\ s a -> s{_rpKey = a})

-- | Last known server sync count for this record. Set to 0 if unknown.
rpSyncCount :: Lens' RecordPatch Integer
rpSyncCount = lens _rpSyncCount (\ s a -> s{_rpSyncCount = a})

instance Hashable RecordPatch where

instance NFData RecordPatch where

instance ToJSON RecordPatch where
        toJSON RecordPatch'{..}
          = object
              (catMaybes
                 [("DeviceLastModifiedDate" .=) <$>
                    _rpDeviceLastModifiedDate,
                  ("Value" .=) <$> _rpValue, Just ("Op" .= _rpOp),
                  Just ("Key" .= _rpKey),
                  Just ("SyncCount" .= _rpSyncCount)])
