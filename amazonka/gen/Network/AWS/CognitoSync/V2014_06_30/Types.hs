{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CognitoSync.V2014_06_30.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Cognito Sync provides an AWS service and client library that enable
-- cross-device syncing of application-related user data. High-level client
-- libraries are available for both iOS and Android. You can use these
-- libraries to persist data locally so that it's available even if the device
-- is offline. Developer credentials don't need to be stored on the mobile
-- device to access the service. You can use Amazon Cognito to obtain a
-- normalized user ID and credentials. User data is persisted in a dataset
-- that can store up to 1 MB of key-value pairs, and you can have up to 20
-- datasets per user identity.
module Network.AWS.CognitoSync.V2014_06_30.Types where

import Control.Lens.TH
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-06-30@) of the
-- @Amazon Cognito Service@ service.
data CognitoSync deriving (Typeable)

instance AWSService CognitoSync where
    type Sg CognitoSync = V4
    data Er CognitoSync

        = CognitoSyncClient HttpException
        | CognitoSyncSerializer String
        | CognitoSyncService String
        | InternalErrorException
            { _ieeMessage :: Text
            }

        | InvalidParameterException
            { _ipeMessage :: Text
            }

        | LimitExceededException
            { _leeMessage :: Text
            }

        | NotAuthorizedException
            { _naeMessage :: Text
            }

        | ResourceConflictException
            { _rceMessage :: Text
            }

        | ResourceNotFoundException
            { _rnfeMessage :: Text
            }

        | TooManyRequestsException
            { _tmreMessage :: Text
            }

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "cognito-sync"
        , _svcVersion  = "2014-06-30"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CognitoSync)
deriving instance Generic (Er CognitoSync)

instance AWSError (Er CognitoSync) where
    awsError = const "CognitoSyncError"

instance ServiceError (Er CognitoSync) where
    serviceError    = CognitoSyncService
    clientError     = CognitoSyncClient
    serializerError = CognitoSyncSerializer

instance Exception (Er CognitoSync)

-- | An operation, either replace or remove.
data Operation
    = OperationRemove -- ^ remove
    | OperationReplace -- ^ replace
      deriving (Eq, Show, Generic)

instance Hashable Operation

instance FromText Operation where
    parser = match "remove" OperationRemove
         <|> match "replace" OperationReplace

instance ToText Operation where
    toText OperationRemove = "remove"
    toText OperationReplace = "replace"

instance ToByteString Operation

instance FromJSON Operation

instance ToJSON Operation

-- | Metadata for a collection of data for an identity. An identity can have
-- multiple datasets. A dataset can be general or associated with a particular
-- entity in an application (like a saved game). Datasets are automatically
-- created if they don't exist. Data is synced by dataset, and a dataset can
-- hold up to 1MB of key-value pairs.
data Dataset = Dataset
    { _dLastModifiedDate :: Maybe ISO8601
      -- ^ Date when the dataset was last modified.
    , _dNumRecords :: Maybe Integer
      -- ^ Number of records in this dataset.
    , _dDataStorage :: Maybe Integer
      -- ^ Total size in bytes of the records in this dataset.
    , _dDatasetName :: Maybe Text
      -- ^ A string of up to 128 characters. Allowed characters are a-z,
      -- A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    , _dCreationDate :: Maybe ISO8601
      -- ^ Date.
    , _dLastModifiedBy :: Maybe Text
      -- ^ The device that made the last change to this dataset.
    , _dIdentityId :: Maybe Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    } deriving (Generic)

instance FromJSON Dataset

-- | Information about the usage of the identity pool.
data IdentityPoolUsage = IdentityPoolUsage
    { _ipuLastModifiedDate :: Maybe ISO8601
      -- ^ Date on which the identity pool was last modified.
    , _ipuIdentityPoolId :: Maybe Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _ipuDataStorage :: Maybe Integer
      -- ^ Data storage information for the identity pool.
    , _ipuSyncSessionsCount :: Maybe Integer
      -- ^ Number of sync sessions for the identity pool.
    } deriving (Generic)

instance FromJSON IdentityPoolUsage

-- | Usage information for the identity.
data IdentityUsage = IdentityUsage
    { _iuLastModifiedDate :: Maybe ISO8601
      -- ^ Date on which the identity was last modified.
    , _iuIdentityPoolId :: Maybe Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    , _iuDatasetCount :: Maybe Integer
      -- ^ Number of datasets for the identity.
    , _iuDataStorage :: Maybe Integer
      -- ^ Total data storage for this identity.
    , _iuIdentityId :: Maybe Text
      -- ^ A name-spaced GUID (for example,
      -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
      -- Cognito. GUID generation is unique within a region.
    } deriving (Generic)

instance FromJSON IdentityUsage

-- | The basic data structure of a dataset.
data Record = Record
    { _rdSyncCount :: Maybe Integer
      -- ^ The server sync count for this record.
    , _rdDeviceLastModifiedDate :: Maybe ISO8601
      -- ^ The last modified date of the client device.
    , _rdLastModifiedDate :: Maybe ISO8601
      -- ^ The date on which the record was last modified.
    , _rdValue :: Maybe Text
      -- ^ The value for the record.
    , _rdKey :: Maybe Text
      -- ^ The key for the record.
    , _rdLastModifiedBy :: Maybe Text
      -- ^ The user/device that made the last change to this record.
    } deriving (Generic)

instance FromJSON Record

-- | An update operation for a record.
data RecordPatch = RecordPatch
    { _rpSyncCount :: Integer
      -- ^ Last known server sync count for this record. Set to 0 if
      -- unknown.
    , _rpDeviceLastModifiedDate :: Maybe ISO8601
      -- ^ The last modified date of the client device.
    , _rpOp :: Operation
      -- ^ An operation, either replace or remove.
    , _rpValue :: Maybe Text
      -- ^ The value associated with the record patch.
    , _rpKey :: Text
      -- ^ The key associated with the record patch.
    } deriving (Generic)

instance ToJSON RecordPatch

-- Newtypes

-- Products
makeLenses ''Dataset
makeLenses ''IdentityPoolUsage
makeLenses ''IdentityUsage
makeLenses ''Record
makeLenses ''RecordPatch
