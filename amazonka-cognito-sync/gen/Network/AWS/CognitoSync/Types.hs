{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Error
    , JSONError

    -- * IdentityPoolUsage
    , IdentityPoolUsage
    , identityPoolUsage
    , ipuDataStorage
    , ipuIdentityPoolId
    , ipuLastModifiedDate
    , ipuSyncSessionsCount

    -- * Platform
    , Platform (..)

    -- * Dataset
    , Dataset
    , dataset
    , dCreationDate
    , dDataStorage
    , dDatasetName
    , dIdentityId
    , dLastModifiedBy
    , dLastModifiedDate
    , dNumRecords

    -- * Operation
    , Operation (..)

    -- * Record
    , Record
    , record
    , rDeviceLastModifiedDate
    , rKey
    , rLastModifiedBy
    , rLastModifiedDate
    , rSyncCount
    , rValue

    -- * IdentityUsage
    , IdentityUsage
    , identityUsage
    , iuDataStorage
    , iuDatasetCount
    , iuIdentityId
    , iuIdentityPoolId
    , iuLastModifiedDate

    -- * RecordPatch
    , RecordPatch
    , recordPatch
    , rpDeviceLastModifiedDate
    , rpKey
    , rpOp
    , rpSyncCount
    , rpValue

    -- * PushSync
    , PushSync
    , pushSync
    , psApplicationArns
    , psRoleArn
    ) where

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-06-30@ of the Amazon Cognito Sync service.
data CognitoSync

instance AWSService CognitoSync where
    type Sg CognitoSync = V4
    type Er CognitoSync = JSONError

    service = Service
        { _svcAbbrev       = "CognitoSync"
        , _svcPrefix       = "cognito-sync"
        , _svcVersion      = "2014-06-30"
        , _svcTargetPrefix = Nothing
        , _svcJSONVersion  = Just "1.1"
        }

    handle = jsonError statusSuccess

data IdentityPoolUsage = IdentityPoolUsage
    { _ipuDataStorage       :: Maybe Integer
    , _ipuIdentityPoolId    :: Maybe Text
    , _ipuLastModifiedDate  :: Maybe ISO8601
    , _ipuSyncSessionsCount :: Maybe Integer
    } deriving (Eq, Ord, Show)

-- | 'IdentityPoolUsage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ipuDataStorage' @::@ 'Maybe' 'Integer'
--
-- * 'ipuIdentityPoolId' @::@ 'Maybe' 'Text'
--
-- * 'ipuLastModifiedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'ipuSyncSessionsCount' @::@ 'Maybe' 'Integer'
--
identityPoolUsage :: IdentityPoolUsage
identityPoolUsage = IdentityPoolUsage
    { _ipuIdentityPoolId    = Nothing
    , _ipuSyncSessionsCount = Nothing
    , _ipuDataStorage       = Nothing
    , _ipuLastModifiedDate  = Nothing
    }

-- | Data storage information for the identity pool.
ipuDataStorage :: Lens' IdentityPoolUsage (Maybe Integer)
ipuDataStorage = lens _ipuDataStorage (\s a -> s { _ipuDataStorage = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
ipuIdentityPoolId :: Lens' IdentityPoolUsage (Maybe Text)
ipuIdentityPoolId =
    lens _ipuIdentityPoolId (\s a -> s { _ipuIdentityPoolId = a })

-- | Date on which the identity pool was last modified.
ipuLastModifiedDate :: Lens' IdentityPoolUsage (Maybe UTCTime)
ipuLastModifiedDate =
    lens _ipuLastModifiedDate (\s a -> s { _ipuLastModifiedDate = a })
        . mapping _Time

-- | Number of sync sessions for the identity pool.
ipuSyncSessionsCount :: Lens' IdentityPoolUsage (Maybe Integer)
ipuSyncSessionsCount =
    lens _ipuSyncSessionsCount (\s a -> s { _ipuSyncSessionsCount = a })

instance FromJSON IdentityPoolUsage where
    parseJSON = withObject "IdentityPoolUsage" $ \o -> IdentityPoolUsage
        <$> o .:? "DataStorage"
        <*> o .:? "IdentityPoolId"
        <*> o .:? "LastModifiedDate"
        <*> o .:? "SyncSessionsCount"

instance ToJSON IdentityPoolUsage where
    toJSON IdentityPoolUsage{..} = object
        [ "IdentityPoolId"    .= _ipuIdentityPoolId
        , "SyncSessionsCount" .= _ipuSyncSessionsCount
        , "DataStorage"       .= _ipuDataStorage
        , "LastModifiedDate"  .= _ipuLastModifiedDate
        ]

data Platform
    = Adm         -- ^ ADM
    | Apns        -- ^ APNS
    | ApnsSandbox -- ^ APNS_SANDBOX
    | Gcm         -- ^ GCM
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable Platform

instance FromText Platform where
    parser = takeText >>= \case
        "ADM"          -> pure Adm
        "APNS"         -> pure Apns
        "APNS_SANDBOX" -> pure ApnsSandbox
        "GCM"          -> pure Gcm
        e              -> fail $
            "Failure parsing Platform from " ++ show e

instance ToText Platform where
    toText = \case
        Adm         -> "ADM"
        Apns        -> "APNS"
        ApnsSandbox -> "APNS_SANDBOX"
        Gcm         -> "GCM"

instance ToByteString Platform
instance ToHeader     Platform
instance ToQuery      Platform

instance FromJSON Platform where
    parseJSON = parseJSONText "Platform"

instance ToJSON Platform where
    toJSON = toJSONText

data Dataset = Dataset
    { _dCreationDate     :: Maybe ISO8601
    , _dDataStorage      :: Maybe Integer
    , _dDatasetName      :: Maybe Text
    , _dIdentityId       :: Maybe Text
    , _dLastModifiedBy   :: Maybe Text
    , _dLastModifiedDate :: Maybe ISO8601
    , _dNumRecords       :: Maybe Integer
    } deriving (Eq, Ord, Show)

-- | 'Dataset' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dCreationDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'dDataStorage' @::@ 'Maybe' 'Integer'
--
-- * 'dDatasetName' @::@ 'Maybe' 'Text'
--
-- * 'dIdentityId' @::@ 'Maybe' 'Text'
--
-- * 'dLastModifiedBy' @::@ 'Maybe' 'Text'
--
-- * 'dLastModifiedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'dNumRecords' @::@ 'Maybe' 'Integer'
--
dataset :: Dataset
dataset = Dataset
    { _dIdentityId       = Nothing
    , _dDatasetName      = Nothing
    , _dCreationDate     = Nothing
    , _dLastModifiedDate = Nothing
    , _dLastModifiedBy   = Nothing
    , _dDataStorage      = Nothing
    , _dNumRecords       = Nothing
    }

-- | Date on which the dataset was created.
dCreationDate :: Lens' Dataset (Maybe UTCTime)
dCreationDate = lens _dCreationDate (\s a -> s { _dCreationDate = a }) . mapping _Time

-- | Total size in bytes of the records in this dataset.
dDataStorage :: Lens' Dataset (Maybe Integer)
dDataStorage = lens _dDataStorage (\s a -> s { _dDataStorage = a })

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_'
-- (underscore), '-' (dash), and '.' (dot).
dDatasetName :: Lens' Dataset (Maybe Text)
dDatasetName = lens _dDatasetName (\s a -> s { _dDatasetName = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
dIdentityId :: Lens' Dataset (Maybe Text)
dIdentityId = lens _dIdentityId (\s a -> s { _dIdentityId = a })

-- | The device that made the last change to this dataset.
dLastModifiedBy :: Lens' Dataset (Maybe Text)
dLastModifiedBy = lens _dLastModifiedBy (\s a -> s { _dLastModifiedBy = a })

-- | Date when the dataset was last modified.
dLastModifiedDate :: Lens' Dataset (Maybe UTCTime)
dLastModifiedDate =
    lens _dLastModifiedDate (\s a -> s { _dLastModifiedDate = a })
        . mapping _Time

-- | Number of records in this dataset.
dNumRecords :: Lens' Dataset (Maybe Integer)
dNumRecords = lens _dNumRecords (\s a -> s { _dNumRecords = a })

instance FromJSON Dataset where
    parseJSON = withObject "Dataset" $ \o -> Dataset
        <$> o .:? "CreationDate"
        <*> o .:? "DataStorage"
        <*> o .:? "DatasetName"
        <*> o .:? "IdentityId"
        <*> o .:? "LastModifiedBy"
        <*> o .:? "LastModifiedDate"
        <*> o .:? "NumRecords"

instance ToJSON Dataset where
    toJSON Dataset{..} = object
        [ "IdentityId"       .= _dIdentityId
        , "DatasetName"      .= _dDatasetName
        , "CreationDate"     .= _dCreationDate
        , "LastModifiedDate" .= _dLastModifiedDate
        , "LastModifiedBy"   .= _dLastModifiedBy
        , "DataStorage"      .= _dDataStorage
        , "NumRecords"       .= _dNumRecords
        ]

data Operation
    = Remove  -- ^ remove
    | Replace -- ^ replace
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable Operation

instance FromText Operation where
    parser = takeText >>= \case
        "remove"  -> pure Remove
        "replace" -> pure Replace
        e         -> fail $
            "Failure parsing Operation from " ++ show e

instance ToText Operation where
    toText = \case
        Remove  -> "remove"
        Replace -> "replace"

instance ToByteString Operation
instance ToHeader     Operation
instance ToQuery      Operation

instance FromJSON Operation where
    parseJSON = parseJSONText "Operation"

instance ToJSON Operation where
    toJSON = toJSONText

data Record = Record
    { _rDeviceLastModifiedDate :: Maybe ISO8601
    , _rKey                    :: Maybe Text
    , _rLastModifiedBy         :: Maybe Text
    , _rLastModifiedDate       :: Maybe ISO8601
    , _rSyncCount              :: Maybe Integer
    , _rValue                  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Record' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rDeviceLastModifiedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rKey' @::@ 'Maybe' 'Text'
--
-- * 'rLastModifiedBy' @::@ 'Maybe' 'Text'
--
-- * 'rLastModifiedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rSyncCount' @::@ 'Maybe' 'Integer'
--
-- * 'rValue' @::@ 'Maybe' 'Text'
--
record :: Record
record = Record
    { _rKey                    = Nothing
    , _rValue                  = Nothing
    , _rSyncCount              = Nothing
    , _rLastModifiedDate       = Nothing
    , _rLastModifiedBy         = Nothing
    , _rDeviceLastModifiedDate = Nothing
    }

-- | The last modified date of the client device.
rDeviceLastModifiedDate :: Lens' Record (Maybe UTCTime)
rDeviceLastModifiedDate =
    lens _rDeviceLastModifiedDate (\s a -> s { _rDeviceLastModifiedDate = a })
        . mapping _Time

-- | The key for the record.
rKey :: Lens' Record (Maybe Text)
rKey = lens _rKey (\s a -> s { _rKey = a })

-- | The user/device that made the last change to this record.
rLastModifiedBy :: Lens' Record (Maybe Text)
rLastModifiedBy = lens _rLastModifiedBy (\s a -> s { _rLastModifiedBy = a })

-- | The date on which the record was last modified.
rLastModifiedDate :: Lens' Record (Maybe UTCTime)
rLastModifiedDate =
    lens _rLastModifiedDate (\s a -> s { _rLastModifiedDate = a })
        . mapping _Time

-- | The server sync count for this record.
rSyncCount :: Lens' Record (Maybe Integer)
rSyncCount = lens _rSyncCount (\s a -> s { _rSyncCount = a })

-- | The value for the record.
rValue :: Lens' Record (Maybe Text)
rValue = lens _rValue (\s a -> s { _rValue = a })

instance FromJSON Record where
    parseJSON = withObject "Record" $ \o -> Record
        <$> o .:? "DeviceLastModifiedDate"
        <*> o .:? "Key"
        <*> o .:? "LastModifiedBy"
        <*> o .:? "LastModifiedDate"
        <*> o .:? "SyncCount"
        <*> o .:? "Value"

instance ToJSON Record where
    toJSON Record{..} = object
        [ "Key"                    .= _rKey
        , "Value"                  .= _rValue
        , "SyncCount"              .= _rSyncCount
        , "LastModifiedDate"       .= _rLastModifiedDate
        , "LastModifiedBy"         .= _rLastModifiedBy
        , "DeviceLastModifiedDate" .= _rDeviceLastModifiedDate
        ]

data IdentityUsage = IdentityUsage
    { _iuDataStorage      :: Maybe Integer
    , _iuDatasetCount     :: Maybe Int
    , _iuIdentityId       :: Maybe Text
    , _iuIdentityPoolId   :: Maybe Text
    , _iuLastModifiedDate :: Maybe ISO8601
    } deriving (Eq, Ord, Show)

-- | 'IdentityUsage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iuDataStorage' @::@ 'Maybe' 'Integer'
--
-- * 'iuDatasetCount' @::@ 'Maybe' 'Int'
--
-- * 'iuIdentityId' @::@ 'Maybe' 'Text'
--
-- * 'iuIdentityPoolId' @::@ 'Maybe' 'Text'
--
-- * 'iuLastModifiedDate' @::@ 'Maybe' 'UTCTime'
--
identityUsage :: IdentityUsage
identityUsage = IdentityUsage
    { _iuIdentityId       = Nothing
    , _iuIdentityPoolId   = Nothing
    , _iuLastModifiedDate = Nothing
    , _iuDatasetCount     = Nothing
    , _iuDataStorage      = Nothing
    }

-- | Total data storage for this identity.
iuDataStorage :: Lens' IdentityUsage (Maybe Integer)
iuDataStorage = lens _iuDataStorage (\s a -> s { _iuDataStorage = a })

-- | Number of datasets for the identity.
iuDatasetCount :: Lens' IdentityUsage (Maybe Int)
iuDatasetCount = lens _iuDatasetCount (\s a -> s { _iuDatasetCount = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
iuIdentityId :: Lens' IdentityUsage (Maybe Text)
iuIdentityId = lens _iuIdentityId (\s a -> s { _iuIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- GUID generation is unique within a region.
iuIdentityPoolId :: Lens' IdentityUsage (Maybe Text)
iuIdentityPoolId = lens _iuIdentityPoolId (\s a -> s { _iuIdentityPoolId = a })

-- | Date on which the identity was last modified.
iuLastModifiedDate :: Lens' IdentityUsage (Maybe UTCTime)
iuLastModifiedDate =
    lens _iuLastModifiedDate (\s a -> s { _iuLastModifiedDate = a })
        . mapping _Time

instance FromJSON IdentityUsage where
    parseJSON = withObject "IdentityUsage" $ \o -> IdentityUsage
        <$> o .:? "DataStorage"
        <*> o .:? "DatasetCount"
        <*> o .:? "IdentityId"
        <*> o .:? "IdentityPoolId"
        <*> o .:? "LastModifiedDate"

instance ToJSON IdentityUsage where
    toJSON IdentityUsage{..} = object
        [ "IdentityId"       .= _iuIdentityId
        , "IdentityPoolId"   .= _iuIdentityPoolId
        , "LastModifiedDate" .= _iuLastModifiedDate
        , "DatasetCount"     .= _iuDatasetCount
        , "DataStorage"      .= _iuDataStorage
        ]

data RecordPatch = RecordPatch
    { _rpDeviceLastModifiedDate :: Maybe ISO8601
    , _rpKey                    :: Text
    , _rpOp                     :: Operation
    , _rpSyncCount              :: Integer
    , _rpValue                  :: Maybe Text
    } deriving (Eq, Show)

-- | 'RecordPatch' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpDeviceLastModifiedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'rpKey' @::@ 'Text'
--
-- * 'rpOp' @::@ 'Operation'
--
-- * 'rpSyncCount' @::@ 'Integer'
--
-- * 'rpValue' @::@ 'Maybe' 'Text'
--
recordPatch :: Operation -- ^ 'rpOp'
            -> Text -- ^ 'rpKey'
            -> Integer -- ^ 'rpSyncCount'
            -> RecordPatch
recordPatch p1 p2 p3 = RecordPatch
    { _rpOp                     = p1
    , _rpKey                    = p2
    , _rpSyncCount              = p3
    , _rpValue                  = Nothing
    , _rpDeviceLastModifiedDate = Nothing
    }

-- | The last modified date of the client device.
rpDeviceLastModifiedDate :: Lens' RecordPatch (Maybe UTCTime)
rpDeviceLastModifiedDate =
    lens _rpDeviceLastModifiedDate
        (\s a -> s { _rpDeviceLastModifiedDate = a })
            . mapping _Time

-- | The key associated with the record patch.
rpKey :: Lens' RecordPatch Text
rpKey = lens _rpKey (\s a -> s { _rpKey = a })

-- | An operation, either replace or remove.
rpOp :: Lens' RecordPatch Operation
rpOp = lens _rpOp (\s a -> s { _rpOp = a })

-- | Last known server sync count for this record. Set to 0 if unknown.
rpSyncCount :: Lens' RecordPatch Integer
rpSyncCount = lens _rpSyncCount (\s a -> s { _rpSyncCount = a })

-- | The value associated with the record patch.
rpValue :: Lens' RecordPatch (Maybe Text)
rpValue = lens _rpValue (\s a -> s { _rpValue = a })

instance FromJSON RecordPatch where
    parseJSON = withObject "RecordPatch" $ \o -> RecordPatch
        <$> o .:? "DeviceLastModifiedDate"
        <*> o .:  "Key"
        <*> o .:  "Op"
        <*> o .:  "SyncCount"
        <*> o .:? "Value"

instance ToJSON RecordPatch where
    toJSON RecordPatch{..} = object
        [ "Op"                     .= _rpOp
        , "Key"                    .= _rpKey
        , "Value"                  .= _rpValue
        , "SyncCount"              .= _rpSyncCount
        , "DeviceLastModifiedDate" .= _rpDeviceLastModifiedDate
        ]

data PushSync = PushSync
    { _psApplicationArns :: List "ApplicationArns" Text
    , _psRoleArn         :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'PushSync' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psApplicationArns' @::@ ['Text']
--
-- * 'psRoleArn' @::@ 'Maybe' 'Text'
--
pushSync :: PushSync
pushSync = PushSync
    { _psApplicationArns = mempty
    , _psRoleArn         = Nothing
    }

-- | List of SNS platform application ARNs that could be used by clients.
psApplicationArns :: Lens' PushSync [Text]
psApplicationArns =
    lens _psApplicationArns (\s a -> s { _psApplicationArns = a })
        . _List

-- | A role configured to allow Cognito to call SNS on behalf of the developer.
psRoleArn :: Lens' PushSync (Maybe Text)
psRoleArn = lens _psRoleArn (\s a -> s { _psRoleArn = a })

instance FromJSON PushSync where
    parseJSON = withObject "PushSync" $ \o -> PushSync
        <$> o .:  "ApplicationArns"
        <*> o .:? "RoleArn"

instance ToJSON PushSync where
    toJSON PushSync{..} = object
        [ "ApplicationArns" .= _psApplicationArns
        , "RoleArn"         .= _psRoleArn
        ]
