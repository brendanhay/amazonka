{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SSM.Types
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

module Network.AWS.SSM.Types
    (
    -- * Service
      SSM

    -- * Errors
    , _AssociatedInstances
    , _InvalidNextToken
    , _InvalidInstanceId
    , _StatusUnchanged
    , _DuplicateInstanceId
    , _InvalidDocument
    , _AssociationLimitExceeded
    , _InvalidDocumentContent
    , _AssociationAlreadyExists
    , _AssociationDoesNotExist
    , _InternalServerError
    , _MaxDocumentSizeExceeded
    , _TooManyUpdates
    , _DocumentAlreadyExists
    , _DocumentLimitExceeded

    -- * AssociationFilterKey
    , AssociationFilterKey (..)

    -- * AssociationStatusName
    , AssociationStatusName (..)

    -- * DocumentFilterKey
    , DocumentFilterKey (..)

    -- * DocumentStatus
    , DocumentStatus (..)

    -- * Fault
    , Fault (..)

    -- * Association
    , Association
    , association
    , assInstanceId
    , assName

    -- * AssociationDescription
    , AssociationDescription
    , associationDescription
    , adInstanceId
    , adStatus
    , adDate
    , adName

    -- * AssociationFilter
    , AssociationFilter
    , associationFilter
    , afKey
    , afValue

    -- * AssociationStatus
    , AssociationStatus
    , associationStatus
    , asAdditionalInfo
    , asDate
    , asName
    , asMessage

    -- * CreateAssociationBatchRequestEntry
    , CreateAssociationBatchRequestEntry
    , createAssociationBatchRequestEntry
    , cabreInstanceId
    , cabreName

    -- * DocumentDescription
    , DocumentDescription
    , documentDescription
    , docStatus
    , docSha1
    , docCreatedDate
    , docName

    -- * DocumentFilter
    , DocumentFilter
    , documentFilter
    , dfKey
    , dfValue

    -- * DocumentIdentifier
    , DocumentIdentifier
    , documentIdentifier
    , diName

    -- * FailedCreateAssociation
    , FailedCreateAssociation
    , failedCreateAssociation
    , fcaEntry
    , fcaFault
    , fcaMessage
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-11-06@ of the Amazon Simple Systems Management Service SDK.
data SSM

instance AWSService SSM where
    type Sg SSM = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "SSM"
            , _svcPrefix = "ssm"
            , _svcVersion = "2014-11-06"
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

-- | You must disassociate a configuration document from all instances before
-- you can delete it.
_AssociatedInstances :: AWSError a => Getting (First ServiceError) a ServiceError
_AssociatedInstances =
    _ServiceError . hasStatus 400 . hasCode "AssociatedInstances"

-- | The specified token is not valid.
_InvalidNextToken :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken = _ServiceError . hasStatus 400 . hasCode "InvalidNextToken"

-- | You must specify the ID of a running instance.
_InvalidInstanceId :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceId =
    _ServiceError . hasStatus 404 . hasCode "InvalidInstanceId"

-- | The updated status is the same as the current status.
_StatusUnchanged :: AWSError a => Getting (First ServiceError) a ServiceError
_StatusUnchanged = _ServiceError . hasStatus 400 . hasCode "StatusUnchanged"

-- | You cannot specify an instance ID in more than one association.
_DuplicateInstanceId :: AWSError a => Getting (First ServiceError) a ServiceError
_DuplicateInstanceId =
    _ServiceError . hasStatus 404 . hasCode "DuplicateInstanceId"

-- | The configuration document is not valid.
_InvalidDocument :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDocument = _ServiceError . hasStatus 404 . hasCode "InvalidDocument"

-- | You can have at most 2,000 active associations.
_AssociationLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_AssociationLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "AssociationLimitExceeded"

-- | The content for the configuration document is not valid.
_InvalidDocumentContent :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDocumentContent =
    _ServiceError . hasStatus 400 . hasCode "InvalidDocumentContent"

-- | The specified association already exists.
_AssociationAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_AssociationAlreadyExists =
    _ServiceError . hasStatus 400 . hasCode "AssociationAlreadyExists"

-- | The specified association does not exist.
_AssociationDoesNotExist :: AWSError a => Getting (First ServiceError) a ServiceError
_AssociationDoesNotExist =
    _ServiceError . hasStatus 404 . hasCode "AssociationDoesNotExist"

-- | An error occurred on the server side.
_InternalServerError :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServerError =
    _ServiceError . hasStatus 500 . hasCode "InternalServerError"

-- | The size limit of a configuration document is 64 KB.
_MaxDocumentSizeExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_MaxDocumentSizeExceeded =
    _ServiceError . hasStatus 400 . hasCode "MaxDocumentSizeExceeded"

-- | There are concurrent updates for a resource that supports one update at
-- a time.
_TooManyUpdates :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyUpdates = _ServiceError . hasStatus 429 . hasCode "TooManyUpdates"

-- | The specified configuration document already exists.
_DocumentAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_DocumentAlreadyExists =
    _ServiceError . hasStatus 400 . hasCode "DocumentAlreadyExists"

-- | You can have at most 100 active configuration documents.
_DocumentLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_DocumentLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "DocumentLimitExceeded"

data AssociationFilterKey
    = AFKInstanceId
    | AFKName
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText AssociationFilterKey where
    parser = takeLowerText >>= \case
        "InstanceId" -> pure AFKInstanceId
        "Name" -> pure AFKName
        e -> fail ("Failure parsing AssociationFilterKey from " ++ show e)

instance ToText AssociationFilterKey where
    toText = \case
        AFKInstanceId -> "InstanceId"
        AFKName -> "Name"

instance Hashable AssociationFilterKey
instance ToQuery AssociationFilterKey
instance ToHeader AssociationFilterKey

instance ToJSON AssociationFilterKey where
    toJSON = toJSONText

data AssociationStatusName
    = Pending
    | Success
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText AssociationStatusName where
    parser = takeLowerText >>= \case
        "Failed" -> pure Failed
        "Pending" -> pure Pending
        "Success" -> pure Success
        e -> fail ("Failure parsing AssociationStatusName from " ++ show e)

instance ToText AssociationStatusName where
    toText = \case
        Failed -> "Failed"
        Pending -> "Pending"
        Success -> "Success"

instance Hashable AssociationStatusName
instance ToQuery AssociationStatusName
instance ToHeader AssociationStatusName

instance ToJSON AssociationStatusName where
    toJSON = toJSONText

instance FromJSON AssociationStatusName where
    parseJSON = parseJSONText "AssociationStatusName"

data DocumentFilterKey =
    Name
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DocumentFilterKey where
    parser = takeLowerText >>= \case
        "Name" -> pure Name
        e -> fail ("Failure parsing DocumentFilterKey from " ++ show e)

instance ToText DocumentFilterKey where
    toText = \case
        Name -> "Name"

instance Hashable DocumentFilterKey
instance ToQuery DocumentFilterKey
instance ToHeader DocumentFilterKey

instance ToJSON DocumentFilterKey where
    toJSON = toJSONText

data DocumentStatus
    = Deleting
    | Creating
    | Active
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DocumentStatus where
    parser = takeLowerText >>= \case
        "Active" -> pure Active
        "Creating" -> pure Creating
        "Deleting" -> pure Deleting
        e -> fail ("Failure parsing DocumentStatus from " ++ show e)

instance ToText DocumentStatus where
    toText = \case
        Active -> "Active"
        Creating -> "Creating"
        Deleting -> "Deleting"

instance Hashable DocumentStatus
instance ToQuery DocumentStatus
instance ToHeader DocumentStatus

instance FromJSON DocumentStatus where
    parseJSON = parseJSONText "DocumentStatus"

data Fault
    = Unknown
    | Server
    | Client
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText Fault where
    parser = takeLowerText >>= \case
        "Client" -> pure Client
        "Server" -> pure Server
        "Unknown" -> pure Unknown
        e -> fail ("Failure parsing Fault from " ++ show e)

instance ToText Fault where
    toText = \case
        Client -> "Client"
        Server -> "Server"
        Unknown -> "Unknown"

instance Hashable Fault
instance ToQuery Fault
instance ToHeader Fault

instance FromJSON Fault where
    parseJSON = parseJSONText "Fault"

-- | Describes an association of a configuration document and an instance.
--
-- /See:/ 'association' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'assInstanceId'
--
-- * 'assName'
data Association = Association'
    { _assInstanceId :: Maybe Text
    , _assName       :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'Association' smart constructor.
association :: Association
association =
    Association'
    { _assInstanceId = Nothing
    , _assName = Nothing
    }

-- | The ID of the instance.
assInstanceId :: Lens' Association (Maybe Text)
assInstanceId = lens _assInstanceId (\ s a -> s{_assInstanceId = a});

-- | The name of the configuration document.
assName :: Lens' Association (Maybe Text)
assName = lens _assName (\ s a -> s{_assName = a});

instance FromJSON Association where
        parseJSON
          = withObject "Association"
              (\ x ->
                 Association' <$>
                   (x .:? "InstanceId") <*> (x .:? "Name"))

-- | Describes an association.
--
-- /See:/ 'associationDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adInstanceId'
--
-- * 'adStatus'
--
-- * 'adDate'
--
-- * 'adName'
data AssociationDescription = AssociationDescription'
    { _adInstanceId :: Maybe Text
    , _adStatus     :: Maybe AssociationStatus
    , _adDate       :: Maybe POSIX
    , _adName       :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'AssociationDescription' smart constructor.
associationDescription :: AssociationDescription
associationDescription =
    AssociationDescription'
    { _adInstanceId = Nothing
    , _adStatus = Nothing
    , _adDate = Nothing
    , _adName = Nothing
    }

-- | The ID of the instance.
adInstanceId :: Lens' AssociationDescription (Maybe Text)
adInstanceId = lens _adInstanceId (\ s a -> s{_adInstanceId = a});

-- | The association status.
adStatus :: Lens' AssociationDescription (Maybe AssociationStatus)
adStatus = lens _adStatus (\ s a -> s{_adStatus = a});

-- | The date when the association was made.
adDate :: Lens' AssociationDescription (Maybe UTCTime)
adDate = lens _adDate (\ s a -> s{_adDate = a}) . mapping _Time;

-- | The name of the configuration document.
adName :: Lens' AssociationDescription (Maybe Text)
adName = lens _adName (\ s a -> s{_adName = a});

instance FromJSON AssociationDescription where
        parseJSON
          = withObject "AssociationDescription"
              (\ x ->
                 AssociationDescription' <$>
                   (x .:? "InstanceId") <*> (x .:? "Status") <*>
                     (x .:? "Date")
                     <*> (x .:? "Name"))

-- | Describes a filter.
--
-- /See:/ 'associationFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'afKey'
--
-- * 'afValue'
data AssociationFilter = AssociationFilter'
    { _afKey   :: AssociationFilterKey
    , _afValue :: Text
    } deriving (Eq,Read,Show)

-- | 'AssociationFilter' smart constructor.
associationFilter :: AssociationFilterKey -> Text -> AssociationFilter
associationFilter pKey pValue =
    AssociationFilter'
    { _afKey = pKey
    , _afValue = pValue
    }

-- | The name of the filter.
afKey :: Lens' AssociationFilter AssociationFilterKey
afKey = lens _afKey (\ s a -> s{_afKey = a});

-- | The filter value.
afValue :: Lens' AssociationFilter Text
afValue = lens _afValue (\ s a -> s{_afValue = a});

instance ToJSON AssociationFilter where
        toJSON AssociationFilter'{..}
          = object ["key" .= _afKey, "value" .= _afValue]

-- | Describes an association status.
--
-- /See:/ 'associationStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asAdditionalInfo'
--
-- * 'asDate'
--
-- * 'asName'
--
-- * 'asMessage'
data AssociationStatus = AssociationStatus'
    { _asAdditionalInfo :: Maybe Text
    , _asDate           :: POSIX
    , _asName           :: AssociationStatusName
    , _asMessage        :: Text
    } deriving (Eq,Read,Show)

-- | 'AssociationStatus' smart constructor.
associationStatus :: UTCTime -> AssociationStatusName -> Text -> AssociationStatus
associationStatus pDate pName pMessage =
    AssociationStatus'
    { _asAdditionalInfo = Nothing
    , _asDate = _Time # pDate
    , _asName = pName
    , _asMessage = pMessage
    }

-- | A user-defined string.
asAdditionalInfo :: Lens' AssociationStatus (Maybe Text)
asAdditionalInfo = lens _asAdditionalInfo (\ s a -> s{_asAdditionalInfo = a});

-- | The date when the status changed.
asDate :: Lens' AssociationStatus UTCTime
asDate = lens _asDate (\ s a -> s{_asDate = a}) . _Time;

-- | The status.
asName :: Lens' AssociationStatus AssociationStatusName
asName = lens _asName (\ s a -> s{_asName = a});

-- | The reason for the status.
asMessage :: Lens' AssociationStatus Text
asMessage = lens _asMessage (\ s a -> s{_asMessage = a});

instance FromJSON AssociationStatus where
        parseJSON
          = withObject "AssociationStatus"
              (\ x ->
                 AssociationStatus' <$>
                   (x .:? "AdditionalInfo") <*> (x .: "Date") <*>
                     (x .: "Name")
                     <*> (x .: "Message"))

instance ToJSON AssociationStatus where
        toJSON AssociationStatus'{..}
          = object
              ["AdditionalInfo" .= _asAdditionalInfo,
               "Date" .= _asDate, "Name" .= _asName,
               "Message" .= _asMessage]

-- | Describes the association of a configuration document and an instance.
--
-- /See:/ 'createAssociationBatchRequestEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cabreInstanceId'
--
-- * 'cabreName'
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
    { _cabreInstanceId :: Maybe Text
    , _cabreName       :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'CreateAssociationBatchRequestEntry' smart constructor.
createAssociationBatchRequestEntry :: CreateAssociationBatchRequestEntry
createAssociationBatchRequestEntry =
    CreateAssociationBatchRequestEntry'
    { _cabreInstanceId = Nothing
    , _cabreName = Nothing
    }

-- | The ID of the instance.
cabreInstanceId :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreInstanceId = lens _cabreInstanceId (\ s a -> s{_cabreInstanceId = a});

-- | The name of the configuration document.
cabreName :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreName = lens _cabreName (\ s a -> s{_cabreName = a});

instance FromJSON CreateAssociationBatchRequestEntry
         where
        parseJSON
          = withObject "CreateAssociationBatchRequestEntry"
              (\ x ->
                 CreateAssociationBatchRequestEntry' <$>
                   (x .:? "InstanceId") <*> (x .:? "Name"))

instance ToJSON CreateAssociationBatchRequestEntry
         where
        toJSON CreateAssociationBatchRequestEntry'{..}
          = object
              ["InstanceId" .= _cabreInstanceId,
               "Name" .= _cabreName]

-- | Describes a configuration document.
--
-- /See:/ 'documentDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'docStatus'
--
-- * 'docSha1'
--
-- * 'docCreatedDate'
--
-- * 'docName'
data DocumentDescription = DocumentDescription'
    { _docStatus      :: Maybe DocumentStatus
    , _docSha1        :: Maybe Text
    , _docCreatedDate :: Maybe POSIX
    , _docName        :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'DocumentDescription' smart constructor.
documentDescription :: DocumentDescription
documentDescription =
    DocumentDescription'
    { _docStatus = Nothing
    , _docSha1 = Nothing
    , _docCreatedDate = Nothing
    , _docName = Nothing
    }

-- | The status of the configuration document.
docStatus :: Lens' DocumentDescription (Maybe DocumentStatus)
docStatus = lens _docStatus (\ s a -> s{_docStatus = a});

-- | The SHA1 hash of the document, which you can use for verification
-- purposes.
docSha1 :: Lens' DocumentDescription (Maybe Text)
docSha1 = lens _docSha1 (\ s a -> s{_docSha1 = a});

-- | The date when the configuration document was created.
docCreatedDate :: Lens' DocumentDescription (Maybe UTCTime)
docCreatedDate = lens _docCreatedDate (\ s a -> s{_docCreatedDate = a}) . mapping _Time;

-- | The name of the configuration document.
docName :: Lens' DocumentDescription (Maybe Text)
docName = lens _docName (\ s a -> s{_docName = a});

instance FromJSON DocumentDescription where
        parseJSON
          = withObject "DocumentDescription"
              (\ x ->
                 DocumentDescription' <$>
                   (x .:? "Status") <*> (x .:? "Sha1") <*>
                     (x .:? "CreatedDate")
                     <*> (x .:? "Name"))

-- | Describes a filter.
--
-- /See:/ 'documentFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfKey'
--
-- * 'dfValue'
data DocumentFilter = DocumentFilter'
    { _dfKey   :: DocumentFilterKey
    , _dfValue :: Text
    } deriving (Eq,Read,Show)

-- | 'DocumentFilter' smart constructor.
documentFilter :: DocumentFilterKey -> Text -> DocumentFilter
documentFilter pKey pValue =
    DocumentFilter'
    { _dfKey = pKey
    , _dfValue = pValue
    }

-- | The name of the filter.
dfKey :: Lens' DocumentFilter DocumentFilterKey
dfKey = lens _dfKey (\ s a -> s{_dfKey = a});

-- | The value of the filter.
dfValue :: Lens' DocumentFilter Text
dfValue = lens _dfValue (\ s a -> s{_dfValue = a});

instance ToJSON DocumentFilter where
        toJSON DocumentFilter'{..}
          = object ["key" .= _dfKey, "value" .= _dfValue]

-- | Describes the name of a configuration document.
--
-- /See:/ 'documentIdentifier' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diName'
newtype DocumentIdentifier = DocumentIdentifier'
    { _diName :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'DocumentIdentifier' smart constructor.
documentIdentifier :: DocumentIdentifier
documentIdentifier =
    DocumentIdentifier'
    { _diName = Nothing
    }

-- | The name of the configuration document.
diName :: Lens' DocumentIdentifier (Maybe Text)
diName = lens _diName (\ s a -> s{_diName = a});

instance FromJSON DocumentIdentifier where
        parseJSON
          = withObject "DocumentIdentifier"
              (\ x -> DocumentIdentifier' <$> (x .:? "Name"))

-- | Describes a failed association.
--
-- /See:/ 'failedCreateAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcaEntry'
--
-- * 'fcaFault'
--
-- * 'fcaMessage'
data FailedCreateAssociation = FailedCreateAssociation'
    { _fcaEntry   :: Maybe CreateAssociationBatchRequestEntry
    , _fcaFault   :: Maybe Fault
    , _fcaMessage :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'FailedCreateAssociation' smart constructor.
failedCreateAssociation :: FailedCreateAssociation
failedCreateAssociation =
    FailedCreateAssociation'
    { _fcaEntry = Nothing
    , _fcaFault = Nothing
    , _fcaMessage = Nothing
    }

-- | The association.
fcaEntry :: Lens' FailedCreateAssociation (Maybe CreateAssociationBatchRequestEntry)
fcaEntry = lens _fcaEntry (\ s a -> s{_fcaEntry = a});

-- | The source of the failure.
fcaFault :: Lens' FailedCreateAssociation (Maybe Fault)
fcaFault = lens _fcaFault (\ s a -> s{_fcaFault = a});

-- | A description of the failure.
fcaMessage :: Lens' FailedCreateAssociation (Maybe Text)
fcaMessage = lens _fcaMessage (\ s a -> s{_fcaMessage = a});

instance FromJSON FailedCreateAssociation where
        parseJSON
          = withObject "FailedCreateAssociation"
              (\ x ->
                 FailedCreateAssociation' <$>
                   (x .:? "Entry") <*> (x .:? "Fault") <*>
                     (x .:? "Message"))
