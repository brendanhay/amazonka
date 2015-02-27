{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SSM.Types
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

module Network.AWS.SSM.Types
    (
    -- * Service
      SSM
    -- ** Error
    , JSONError

    -- * CreateAssociationBatchRequestEntry
    , CreateAssociationBatchRequestEntry
    , createAssociationBatchRequestEntry
    , cabreInstanceId
    , cabreName

    -- * DocumentFilter
    , DocumentFilter
    , documentFilter
    , dfKey
    , dfValue

    -- * AssociationDescription
    , AssociationDescription
    , associationDescription
    , adDate
    , adInstanceId
    , adName
    , adStatus

    -- * AssociationStatusName
    , AssociationStatusName (..)

    -- * DocumentFilterKey
    , DocumentFilterKey (..)

    -- * DocumentDescription
    , DocumentDescription
    , documentDescription
    , dd1CreatedDate
    , dd1Name
    , dd1Sha1
    , dd1Status

    -- * AssociationFilter
    , AssociationFilter
    , associationFilter
    , afKey
    , afValue

    -- * DocumentIdentifier
    , DocumentIdentifier
    , documentIdentifier
    , diName

    -- * Fault
    , Fault (..)

    -- * AssociationStatus
    , AssociationStatus
    , associationStatus
    , asAdditionalInfo
    , asDate
    , asMessage
    , asName

    -- * DocumentStatus
    , DocumentStatus (..)

    -- * AssociationFilterKey
    , AssociationFilterKey (..)

    -- * FailedCreateAssociation
    , FailedCreateAssociation
    , failedCreateAssociation
    , fcaEntry
    , fcaFault
    , fcaMessage

    -- * Association
    , Association
    , association
    , aInstanceId
    , aName
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-11-06@ of the Amazon Simple Systems Management Service service.
data SSM

instance AWSService SSM where
    type Sg SSM = V4
    type Er SSM = JSONError

    service = service'
      where
        service' :: Service SSM
        service' = Service
            { _svcAbbrev       = "SSM"
            , _svcPrefix       = "ssm"
            , _svcVersion      = "2014-11-06"
            , _svcTargetPrefix = Just "AmazonSSM"
            , _svcJSONVersion  = Just "1.1"
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry SSM
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry
    { _cabreInstanceId :: Maybe Text
    , _cabreName       :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateAssociationBatchRequestEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cabreInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'cabreName' @::@ 'Maybe' 'Text'
--
createAssociationBatchRequestEntry :: CreateAssociationBatchRequestEntry
createAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry
    { _cabreName       = Nothing
    , _cabreInstanceId = Nothing
    }

-- | The ID of the instance.
cabreInstanceId :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreInstanceId = lens _cabreInstanceId (\s a -> s { _cabreInstanceId = a })

-- | The name of the configuration document.
cabreName :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreName = lens _cabreName (\s a -> s { _cabreName = a })

instance FromJSON CreateAssociationBatchRequestEntry where
    parseJSON = withObject "CreateAssociationBatchRequestEntry" $ \o -> CreateAssociationBatchRequestEntry
        <$> o .:? "InstanceId"
        <*> o .:? "Name"

instance ToJSON CreateAssociationBatchRequestEntry where
    toJSON CreateAssociationBatchRequestEntry{..} = object
        [ "Name"       .= _cabreName
        , "InstanceId" .= _cabreInstanceId
        ]

data DocumentFilter = DocumentFilter
    { _dfKey   :: DocumentFilterKey
    , _dfValue :: Text
    } deriving (Eq, Read, Show)

-- | 'DocumentFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfKey' @::@ 'DocumentFilterKey'
--
-- * 'dfValue' @::@ 'Text'
--
documentFilter :: DocumentFilterKey -- ^ 'dfKey'
               -> Text -- ^ 'dfValue'
               -> DocumentFilter
documentFilter p1 p2 = DocumentFilter
    { _dfKey   = p1
    , _dfValue = p2
    }

-- | The name of the filter.
dfKey :: Lens' DocumentFilter DocumentFilterKey
dfKey = lens _dfKey (\s a -> s { _dfKey = a })

-- | The value of the filter.
dfValue :: Lens' DocumentFilter Text
dfValue = lens _dfValue (\s a -> s { _dfValue = a })

instance FromJSON DocumentFilter where
    parseJSON = withObject "DocumentFilter" $ \o -> DocumentFilter
        <$> o .:  "key"
        <*> o .:  "value"

instance ToJSON DocumentFilter where
    toJSON DocumentFilter{..} = object
        [ "key"   .= _dfKey
        , "value" .= _dfValue
        ]

data AssociationDescription = AssociationDescription
    { _adDate       :: Maybe POSIX
    , _adInstanceId :: Maybe Text
    , _adName       :: Maybe Text
    , _adStatus     :: Maybe AssociationStatus
    } deriving (Eq, Read, Show)

-- | 'AssociationDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'adInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'adName' @::@ 'Maybe' 'Text'
--
-- * 'adStatus' @::@ 'Maybe' 'AssociationStatus'
--
associationDescription :: AssociationDescription
associationDescription = AssociationDescription
    { _adName       = Nothing
    , _adInstanceId = Nothing
    , _adDate       = Nothing
    , _adStatus     = Nothing
    }

-- | The date when the association was made.
adDate :: Lens' AssociationDescription (Maybe UTCTime)
adDate = lens _adDate (\s a -> s { _adDate = a }) . mapping _Time

-- | The ID of the instance.
adInstanceId :: Lens' AssociationDescription (Maybe Text)
adInstanceId = lens _adInstanceId (\s a -> s { _adInstanceId = a })

-- | The name of the configuration document.
adName :: Lens' AssociationDescription (Maybe Text)
adName = lens _adName (\s a -> s { _adName = a })

-- | The association status.
adStatus :: Lens' AssociationDescription (Maybe AssociationStatus)
adStatus = lens _adStatus (\s a -> s { _adStatus = a })

instance FromJSON AssociationDescription where
    parseJSON = withObject "AssociationDescription" $ \o -> AssociationDescription
        <$> o .:? "Date"
        <*> o .:? "InstanceId"
        <*> o .:? "Name"
        <*> o .:? "Status"

instance ToJSON AssociationDescription where
    toJSON AssociationDescription{..} = object
        [ "Name"       .= _adName
        , "InstanceId" .= _adInstanceId
        , "Date"       .= _adDate
        , "Status"     .= _adStatus
        ]

data AssociationStatusName
    = Failed  -- ^ Failed
    | Pending -- ^ Pending
    | Success -- ^ Success
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable AssociationStatusName

instance FromText AssociationStatusName where
    parser = takeLowerText >>= \case
        "failed"  -> pure Failed
        "pending" -> pure Pending
        "success" -> pure Success
        e         -> fail $
            "Failure parsing AssociationStatusName from " ++ show e

instance ToText AssociationStatusName where
    toText = \case
        Failed  -> "Failed"
        Pending -> "Pending"
        Success -> "Success"

instance ToByteString AssociationStatusName
instance ToHeader     AssociationStatusName
instance ToQuery      AssociationStatusName

instance FromJSON AssociationStatusName where
    parseJSON = parseJSONText "AssociationStatusName"

instance ToJSON AssociationStatusName where
    toJSON = toJSONText

data DocumentFilterKey
    = Name -- ^ Name
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DocumentFilterKey

instance FromText DocumentFilterKey where
    parser = takeLowerText >>= \case
        "name" -> pure Name
        e      -> fail $
            "Failure parsing DocumentFilterKey from " ++ show e

instance ToText DocumentFilterKey where
    toText Name = "Name"

instance ToByteString DocumentFilterKey
instance ToHeader     DocumentFilterKey
instance ToQuery      DocumentFilterKey

instance FromJSON DocumentFilterKey where
    parseJSON = parseJSONText "DocumentFilterKey"

instance ToJSON DocumentFilterKey where
    toJSON = toJSONText

data DocumentDescription = DocumentDescription
    { _dd1CreatedDate :: Maybe POSIX
    , _dd1Name        :: Maybe Text
    , _dd1Sha1        :: Maybe Text
    , _dd1Status      :: Maybe DocumentStatus
    } deriving (Eq, Read, Show)

-- | 'DocumentDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dd1CreatedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'dd1Name' @::@ 'Maybe' 'Text'
--
-- * 'dd1Sha1' @::@ 'Maybe' 'Text'
--
-- * 'dd1Status' @::@ 'Maybe' 'DocumentStatus'
--
documentDescription :: DocumentDescription
documentDescription = DocumentDescription
    { _dd1Sha1        = Nothing
    , _dd1Name        = Nothing
    , _dd1CreatedDate = Nothing
    , _dd1Status      = Nothing
    }

-- | The date when the configuration document was created.
dd1CreatedDate :: Lens' DocumentDescription (Maybe UTCTime)
dd1CreatedDate = lens _dd1CreatedDate (\s a -> s { _dd1CreatedDate = a }) . mapping _Time

-- | The name of the configuration document.
dd1Name :: Lens' DocumentDescription (Maybe Text)
dd1Name = lens _dd1Name (\s a -> s { _dd1Name = a })

-- | The SHA1 hash of the document, which you can use for verification purposes.
dd1Sha1 :: Lens' DocumentDescription (Maybe Text)
dd1Sha1 = lens _dd1Sha1 (\s a -> s { _dd1Sha1 = a })

-- | The status of the configuration document.
dd1Status :: Lens' DocumentDescription (Maybe DocumentStatus)
dd1Status = lens _dd1Status (\s a -> s { _dd1Status = a })

instance FromJSON DocumentDescription where
    parseJSON = withObject "DocumentDescription" $ \o -> DocumentDescription
        <$> o .:? "CreatedDate"
        <*> o .:? "Name"
        <*> o .:? "Sha1"
        <*> o .:? "Status"

instance ToJSON DocumentDescription where
    toJSON DocumentDescription{..} = object
        [ "Sha1"        .= _dd1Sha1
        , "Name"        .= _dd1Name
        , "CreatedDate" .= _dd1CreatedDate
        , "Status"      .= _dd1Status
        ]

data AssociationFilter = AssociationFilter
    { _afKey   :: AssociationFilterKey
    , _afValue :: Text
    } deriving (Eq, Read, Show)

-- | 'AssociationFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'afKey' @::@ 'AssociationFilterKey'
--
-- * 'afValue' @::@ 'Text'
--
associationFilter :: AssociationFilterKey -- ^ 'afKey'
                  -> Text -- ^ 'afValue'
                  -> AssociationFilter
associationFilter p1 p2 = AssociationFilter
    { _afKey   = p1
    , _afValue = p2
    }

-- | The name of the filter.
afKey :: Lens' AssociationFilter AssociationFilterKey
afKey = lens _afKey (\s a -> s { _afKey = a })

-- | The filter value.
afValue :: Lens' AssociationFilter Text
afValue = lens _afValue (\s a -> s { _afValue = a })

instance FromJSON AssociationFilter where
    parseJSON = withObject "AssociationFilter" $ \o -> AssociationFilter
        <$> o .:  "key"
        <*> o .:  "value"

instance ToJSON AssociationFilter where
    toJSON AssociationFilter{..} = object
        [ "key"   .= _afKey
        , "value" .= _afValue
        ]

newtype DocumentIdentifier = DocumentIdentifier
    { _diName :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DocumentIdentifier' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diName' @::@ 'Maybe' 'Text'
--
documentIdentifier :: DocumentIdentifier
documentIdentifier = DocumentIdentifier
    { _diName = Nothing
    }

-- | The name of the configuration document.
diName :: Lens' DocumentIdentifier (Maybe Text)
diName = lens _diName (\s a -> s { _diName = a })

instance FromJSON DocumentIdentifier where
    parseJSON = withObject "DocumentIdentifier" $ \o -> DocumentIdentifier
        <$> o .:? "Name"

instance ToJSON DocumentIdentifier where
    toJSON DocumentIdentifier{..} = object
        [ "Name" .= _diName
        ]

data Fault
    = Client  -- ^ Client
    | Server  -- ^ Server
    | Unknown -- ^ Unknown
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable Fault

instance FromText Fault where
    parser = takeLowerText >>= \case
        "client"  -> pure Client
        "server"  -> pure Server
        "unknown" -> pure Unknown
        e         -> fail $
            "Failure parsing Fault from " ++ show e

instance ToText Fault where
    toText = \case
        Client  -> "Client"
        Server  -> "Server"
        Unknown -> "Unknown"

instance ToByteString Fault
instance ToHeader     Fault
instance ToQuery      Fault

instance FromJSON Fault where
    parseJSON = parseJSONText "Fault"

instance ToJSON Fault where
    toJSON = toJSONText

data AssociationStatus = AssociationStatus
    { _asAdditionalInfo :: Maybe Text
    , _asDate           :: POSIX
    , _asMessage        :: Text
    , _asName           :: AssociationStatusName
    } deriving (Eq, Read, Show)

-- | 'AssociationStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asAdditionalInfo' @::@ 'Maybe' 'Text'
--
-- * 'asDate' @::@ 'UTCTime'
--
-- * 'asMessage' @::@ 'Text'
--
-- * 'asName' @::@ 'AssociationStatusName'
--
associationStatus :: UTCTime -- ^ 'asDate'
                  -> AssociationStatusName -- ^ 'asName'
                  -> Text -- ^ 'asMessage'
                  -> AssociationStatus
associationStatus p1 p2 p3 = AssociationStatus
    { _asDate           = withIso _Time (const id) p1
    , _asName           = p2
    , _asMessage        = p3
    , _asAdditionalInfo = Nothing
    }

-- | A user-defined string.
asAdditionalInfo :: Lens' AssociationStatus (Maybe Text)
asAdditionalInfo = lens _asAdditionalInfo (\s a -> s { _asAdditionalInfo = a })

-- | The date when the status changed.
asDate :: Lens' AssociationStatus UTCTime
asDate = lens _asDate (\s a -> s { _asDate = a }) . _Time

-- | The reason for the status.
asMessage :: Lens' AssociationStatus Text
asMessage = lens _asMessage (\s a -> s { _asMessage = a })

-- | The status.
asName :: Lens' AssociationStatus AssociationStatusName
asName = lens _asName (\s a -> s { _asName = a })

instance FromJSON AssociationStatus where
    parseJSON = withObject "AssociationStatus" $ \o -> AssociationStatus
        <$> o .:? "AdditionalInfo"
        <*> o .:  "Date"
        <*> o .:  "Message"
        <*> o .:  "Name"

instance ToJSON AssociationStatus where
    toJSON AssociationStatus{..} = object
        [ "Date"           .= _asDate
        , "Name"           .= _asName
        , "Message"        .= _asMessage
        , "AdditionalInfo" .= _asAdditionalInfo
        ]

data DocumentStatus
    = Active   -- ^ Active
    | Creating -- ^ Creating
    | Deleting -- ^ Deleting
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DocumentStatus

instance FromText DocumentStatus where
    parser = takeLowerText >>= \case
        "active"   -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        e          -> fail $
            "Failure parsing DocumentStatus from " ++ show e

instance ToText DocumentStatus where
    toText = \case
        Active   -> "Active"
        Creating -> "Creating"
        Deleting -> "Deleting"

instance ToByteString DocumentStatus
instance ToHeader     DocumentStatus
instance ToQuery      DocumentStatus

instance FromJSON DocumentStatus where
    parseJSON = parseJSONText "DocumentStatus"

instance ToJSON DocumentStatus where
    toJSON = toJSONText

data AssociationFilterKey
    = AFKInstanceId -- ^ InstanceId
    | AFKName       -- ^ Name
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable AssociationFilterKey

instance FromText AssociationFilterKey where
    parser = takeLowerText >>= \case
        "instanceid" -> pure AFKInstanceId
        "name"       -> pure AFKName
        e            -> fail $
            "Failure parsing AssociationFilterKey from " ++ show e

instance ToText AssociationFilterKey where
    toText = \case
        AFKInstanceId -> "InstanceId"
        AFKName       -> "Name"

instance ToByteString AssociationFilterKey
instance ToHeader     AssociationFilterKey
instance ToQuery      AssociationFilterKey

instance FromJSON AssociationFilterKey where
    parseJSON = parseJSONText "AssociationFilterKey"

instance ToJSON AssociationFilterKey where
    toJSON = toJSONText

data FailedCreateAssociation = FailedCreateAssociation
    { _fcaEntry   :: Maybe CreateAssociationBatchRequestEntry
    , _fcaFault   :: Maybe Fault
    , _fcaMessage :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'FailedCreateAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcaEntry' @::@ 'Maybe' 'CreateAssociationBatchRequestEntry'
--
-- * 'fcaFault' @::@ 'Maybe' 'Fault'
--
-- * 'fcaMessage' @::@ 'Maybe' 'Text'
--
failedCreateAssociation :: FailedCreateAssociation
failedCreateAssociation = FailedCreateAssociation
    { _fcaEntry   = Nothing
    , _fcaMessage = Nothing
    , _fcaFault   = Nothing
    }

-- | The association.
fcaEntry :: Lens' FailedCreateAssociation (Maybe CreateAssociationBatchRequestEntry)
fcaEntry = lens _fcaEntry (\s a -> s { _fcaEntry = a })

-- | The source of the failure.
fcaFault :: Lens' FailedCreateAssociation (Maybe Fault)
fcaFault = lens _fcaFault (\s a -> s { _fcaFault = a })

-- | A description of the failure.
fcaMessage :: Lens' FailedCreateAssociation (Maybe Text)
fcaMessage = lens _fcaMessage (\s a -> s { _fcaMessage = a })

instance FromJSON FailedCreateAssociation where
    parseJSON = withObject "FailedCreateAssociation" $ \o -> FailedCreateAssociation
        <$> o .:? "Entry"
        <*> o .:? "Fault"
        <*> o .:? "Message"

instance ToJSON FailedCreateAssociation where
    toJSON FailedCreateAssociation{..} = object
        [ "Entry"   .= _fcaEntry
        , "Message" .= _fcaMessage
        , "Fault"   .= _fcaFault
        ]

data Association = Association
    { _aInstanceId :: Maybe Text
    , _aName       :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Association' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'aName' @::@ 'Maybe' 'Text'
--
association :: Association
association = Association
    { _aName       = Nothing
    , _aInstanceId = Nothing
    }

-- | The ID of the instance.
aInstanceId :: Lens' Association (Maybe Text)
aInstanceId = lens _aInstanceId (\s a -> s { _aInstanceId = a })

-- | The name of the configuration document.
aName :: Lens' Association (Maybe Text)
aName = lens _aName (\s a -> s { _aName = a })

instance FromJSON Association where
    parseJSON = withObject "Association" $ \o -> Association
        <$> o .:? "InstanceId"
        <*> o .:? "Name"

instance ToJSON Association where
    toJSON Association{..} = object
        [ "Name"       .= _aName
        , "InstanceId" .= _aInstanceId
        ]
