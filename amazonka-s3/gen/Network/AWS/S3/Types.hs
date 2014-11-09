{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.Types
    (
    -- * Service
      S3
    -- ** Errors
    , S3Error (..)
    , _S3Http
    , _S3Serializer
    , _S3Service
    -- ** XML
    , xmlOptions

    -- * Event
    , Event (..)

    -- * NoncurrentVersionExpiration
    , NoncurrentVersionExpiration
    , noncurrentVersionExpiration
    , nveNoncurrentDays

    -- * Transition
    , Transition
    , transition
    , tDate
    , tDays
    , tStorageClass

    -- * DeleteMarkerEntry
    , DeleteMarkerEntry
    , deleteMarkerEntry
    , dmeIsLatest
    , dmeKey
    , dmeLastModified
    , dmeOwner
    , dmeVersionId

    -- * ExpirationStatus
    , ExpirationStatus (..)

    -- * Part
    , Part
    , part
    , pETag
    , pLastModified
    , pPartNumber
    , pSize

    -- * VersioningConfiguration
    , VersioningConfiguration
    , versioningConfiguration
    , vcMFADelete
    , vcStatus

    -- * Tag
    , Tag
    , tag
    , tKey
    , tValue

    -- * ObjectStorageClass
    , ObjectStorageClass (..)

    -- * MetadataDirective
    , MetadataDirective (..)

    -- * RedirectAllRequestsTo
    , RedirectAllRequestsTo
    , redirectAllRequestsTo
    , rartHostName
    , rartProtocol

    -- * RoutingRule
    , RoutingRule
    , routingRule
    , rrCondition
    , rrRedirect

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicConfiguration

    -- * S3ServiceError
    , S3ServiceError
    , s3ServiceError
    , sseCode
    , sseKey
    , sseMessage
    , sseVersionId

    -- * ObjectCannedACL
    , ObjectCannedACL (..)

    -- * BucketVersioningStatus
    , BucketVersioningStatus (..)

    -- * DeletedObject
    , DeletedObject
    , deletedObject
    , doDeleteMarker
    , doDeleteMarkerVersionId
    , doKey
    , doVersionId

    -- * ObjectVersionStorageClass
    , ObjectVersionStorageClass (..)

    -- * CopyPartResult
    , CopyPartResult
    , copyPartResult
    , cprETag
    , cprLastModified

    -- * EncodingType
    , EncodingType (..)

    -- * RequestPaymentConfiguration
    , RequestPaymentConfiguration
    , requestPaymentConfiguration
    , rpcPayer

    -- * CORSRule
    , CORSRule
    , corsrule
    , corsrAllowedHeaders
    , corsrAllowedMethods
    , corsrAllowedOrigins
    , corsrExposeHeaders
    , corsrMaxAgeSeconds

    -- * WebsiteConfiguration
    , WebsiteConfiguration
    , websiteConfiguration
    , wcErrorDocument
    , wcIndexDocument
    , wcRedirectAllRequestsTo
    , wcRoutingRules

    -- * NoncurrentVersionTransition
    , NoncurrentVersionTransition
    , noncurrentVersionTransition
    , nvtNoncurrentDays
    , nvtStorageClass

    -- * Initiator
    , Initiator
    , initiator
    , iDisplayName
    , iID

    -- * ObjectIdentifier
    , ObjectIdentifier
    , objectIdentifier
    , oiKey
    , oiVersionId

    -- * Bucket
    , Bucket
    , bucket
    , bCreationDate
    , bName

    -- * Protocol
    , Protocol (..)

    -- * Grant
    , Grant
    , grant
    , gGrantee
    , gPermission

    -- * Rule
    , Rule
    , rule
    , rExpiration
    , rID
    , rNoncurrentVersionExpiration
    , rNoncurrentVersionTransition
    , rPrefix
    , rStatus
    , rTransition

    -- * TopicConfiguration
    , TopicConfiguration
    , topicConfiguration
    , tcEvent
    , tcTopic

    -- * Owner
    , Owner
    , owner
    , oDisplayName
    , oID

    -- * BucketLoggingStatus
    , BucketLoggingStatus
    , bucketLoggingStatus
    , blsLoggingEnabled

    -- * ErrorDocument
    , ErrorDocument
    , errorDocument
    , edKey

    -- * StorageClass
    , StorageClass (..)

    -- * ObjectVersion
    , ObjectVersion
    , objectVersion
    , ovETag
    , ovIsLatest
    , ovKey
    , ovLastModified
    , ovOwner
    , ovSize
    , ovStorageClass
    , ovVersionId

    -- * TargetGrant
    , TargetGrant
    , targetGrant
    , tgGrantee
    , tgPermission

    -- * MFADeleteStatus
    , MFADeleteStatus (..)

    -- * Payer
    , Payer (..)

    -- * Redirect
    , Redirect
    , redirect
    , rHostName
    , rHttpRedirectCode
    , rProtocol
    , rReplaceKeyPrefixWith
    , rReplaceKeyWith

    -- * BucketLogsPermission
    , BucketLogsPermission (..)

    -- * CompletedPart
    , CompletedPart
    , completedPart
    , cpETag
    , cpPartNumber

    -- * CreateBucketConfiguration
    , CreateBucketConfiguration
    , createBucketConfiguration
    , cbcLocationConstraint

    -- * Tagging
    , Tagging
    , tagging
    , tTagSet

    -- * LifecycleExpiration
    , LifecycleExpiration
    , lifecycleExpiration
    , leDate
    , leDays

    -- * CORSConfiguration
    , CORSConfiguration
    , corsconfiguration
    , corscCORSRules

    -- * Object
    , Object
    , object
    , oETag
    , oKey
    , oLastModified
    , oOwner
    , oSize
    , oStorageClass

    -- * CommonPrefix
    , CommonPrefix
    , commonPrefix
    , cpPrefix

    -- * MultipartUpload
    , MultipartUpload
    , multipartUpload
    , muInitiated
    , muInitiator
    , muKey
    , muOwner
    , muStorageClass
    , muUploadId

    -- * Type
    , Type (..)

    -- * TransitionStorageClass
    , TransitionStorageClass (..)

    -- * CompletedMultipartUpload
    , CompletedMultipartUpload
    , completedMultipartUpload
    , cmuParts

    -- * Condition
    , Condition
    , condition
    , cHttpErrorCodeReturnedEquals
    , cKeyPrefixEquals

    -- * Permission
    , Permission (..)

    -- * AccessControlPolicy
    , AccessControlPolicy
    , accessControlPolicy
    , acpGrants
    , acpOwner

    -- * BucketCannedACL
    , BucketCannedACL (..)

    -- * MFADelete
    , MFADelete (..)

    -- * Grantee
    , Grantee
    , grantee
    , gDisplayName
    , gEmailAddress
    , gID
    , gType
    , gURI

    -- * LifecycleConfiguration
    , LifecycleConfiguration
    , lifecycleConfiguration
    , lcRules

    -- * LoggingEnabled
    , LoggingEnabled
    , loggingEnabled
    , leTargetBucket
    , leTargetGrants
    , leTargetPrefix

    -- * ServerSideEncryption
    , ServerSideEncryption (..)

    -- * IndexDocument
    , IndexDocument
    , indexDocument
    , idSuffix

    -- * CopyObjectResult
    , CopyObjectResult
    , copyObjectResult
    , corETag
    , corLastModified

    -- * Delete
    , Delete
    , delete'
    , dObjects
    , dQuiet

    -- * RestoreRequest
    , RestoreRequest
    , restoreRequest
    , rDays

    -- * Common
    , module Network.AWS.S3.Internal
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4
import Network.AWS.S3.Internal

-- | Supported version (@2006-03-01@) of the Amazon Simple Storage Service.
data S3 deriving (Typeable)

instance AWSService S3 where
    type Sg S3 = V4
    type Er S3 = S3Error

    service = Service
        { _svcEndpoint = Global
        , _svcPrefix   = "s3"
        , _svcVersion  = "2006-03-01"
        , _svcTarget   = Nothing
        }

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

-- | A sum type representing possible 'S3' errors returned by the
-- Amazon Simple Storage Service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data S3Error
    = S3Http       HttpException
    | S3Serializer String
    | S3Service    Status S3ServiceError
      deriving (Show, Typeable, Generic)

instance Exception S3Error

instance AWSError S3Error where
    awsError = \case
        S3Http       ex  -> HttpError       ex
        S3Serializer e   -> SerializerError "S3" e
        S3Service    s x -> ServiceError    "S3" s (show x)

instance AWSServiceError S3Error where
    httpError       = S3Http
    serializerError = S3Serializer
    serviceError    = xmlError httpStatus S3Service

_S3Http :: Prism' S3Error HttpException
_S3Http = prism S3Http $ \case
    S3Http ex -> Right ex
    x -> Left x

_S3Serializer :: Prism' S3Error String
_S3Serializer = prism S3Serializer $ \case
    S3Serializer e -> Right e
    x -> Left x

_S3Service :: Prism' S3Error (Status, S3ServiceError)
_S3Service = prism (uncurry S3Service) $ \case
    S3Service s x -> Right (s, x)
    x -> Left x

data Event
    = S3ReducedRedundancyLostObject -- ^ s3:ReducedRedundancyLostObject
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable Event

instance FromText Event where
    parser = match "s3:ReducedRedundancyLostObject" S3ReducedRedundancyLostObject

instance ToText Event where
    toText S3ReducedRedundancyLostObject = "s3:ReducedRedundancyLostObject"

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

instance ToXML Event where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Event"

newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration
    { _nveNoncurrentDays :: Int
    } deriving (Eq, Ord, Show, Generic, Enum, Num)

-- | 'NoncurrentVersionExpiration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nveNoncurrentDays' @::@ 'Int'
--
noncurrentVersionExpiration :: Int -- ^ 'nveNoncurrentDays'
                            -> NoncurrentVersionExpiration
noncurrentVersionExpiration p1 = NoncurrentVersionExpiration
    { _nveNoncurrentDays = p1
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3 can
-- perform the associated action. For information about the noncurrent days
-- calculations, see How Amazon S3 Calculates When an Object Became
-- Noncurrent in the Amazon Simple Storage Service Developer Guide.
nveNoncurrentDays :: Lens' NoncurrentVersionExpiration Int
nveNoncurrentDays =
    lens _nveNoncurrentDays (\s a -> s { _nveNoncurrentDays = a })

instance FromXML NoncurrentVersionExpiration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NoncurrentVersionExpiration"

instance ToXML NoncurrentVersionExpiration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NoncurrentVersionExpiration"

data Transition = Transition
    { _tDate         :: Maybe ISO8601
    , _tDays         :: Maybe Int
    , _tStorageClass :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Transition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'tDays' @::@ 'Maybe' 'Int'
--
-- * 'tStorageClass' @::@ 'Maybe' 'Text'
--
transition :: Transition
transition = Transition
    { _tDate         = Nothing
    , _tDays         = Nothing
    , _tStorageClass = Nothing
    }

-- | Indicates at what date the object is to be moved or deleted. Should be in
-- GMT ISO 8601 Format.
tDate :: Lens' Transition (Maybe UTCTime)
tDate = lens _tDate (\s a -> s { _tDate = a })
    . mapping _Time

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
tDays :: Lens' Transition (Maybe Int)
tDays = lens _tDays (\s a -> s { _tDays = a })

-- | The class of storage used to store the object.
tStorageClass :: Lens' Transition (Maybe Text)
tStorageClass = lens _tStorageClass (\s a -> s { _tStorageClass = a })

instance FromXML Transition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Transition"

instance ToXML Transition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Transition"

data DeleteMarkerEntry = DeleteMarkerEntry
    { _dmeIsLatest     :: Maybe Bool
    , _dmeKey          :: Maybe Text
    , _dmeLastModified :: Maybe RFC822
    , _dmeOwner        :: Maybe Owner
    , _dmeVersionId    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DeleteMarkerEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmeIsLatest' @::@ 'Maybe' 'Bool'
--
-- * 'dmeKey' @::@ 'Maybe' 'Text'
--
-- * 'dmeLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'dmeOwner' @::@ 'Maybe' 'Owner'
--
-- * 'dmeVersionId' @::@ 'Maybe' 'Text'
--
deleteMarkerEntry :: DeleteMarkerEntry
deleteMarkerEntry = DeleteMarkerEntry
    { _dmeOwner        = Nothing
    , _dmeKey          = Nothing
    , _dmeVersionId    = Nothing
    , _dmeIsLatest     = Nothing
    , _dmeLastModified = Nothing
    }

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
dmeIsLatest :: Lens' DeleteMarkerEntry (Maybe Bool)
dmeIsLatest = lens _dmeIsLatest (\s a -> s { _dmeIsLatest = a })

-- | The object key.
dmeKey :: Lens' DeleteMarkerEntry (Maybe Text)
dmeKey = lens _dmeKey (\s a -> s { _dmeKey = a })

-- | Date and time the object was last modified.
dmeLastModified :: Lens' DeleteMarkerEntry (Maybe UTCTime)
dmeLastModified = lens _dmeLastModified (\s a -> s { _dmeLastModified = a })
    . mapping _Time

dmeOwner :: Lens' DeleteMarkerEntry (Maybe Owner)
dmeOwner = lens _dmeOwner (\s a -> s { _dmeOwner = a })

-- | Version ID of an object.
dmeVersionId :: Lens' DeleteMarkerEntry (Maybe Text)
dmeVersionId = lens _dmeVersionId (\s a -> s { _dmeVersionId = a })

instance FromXML DeleteMarkerEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteMarkerEntry"

instance ToXML DeleteMarkerEntry where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteMarkerEntry"

data ExpirationStatus
    = Disabled -- ^ Disabled
    | Enabled  -- ^ Enabled
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ExpirationStatus

instance FromText ExpirationStatus where
    parser = match "Disabled" Disabled
         <|> match "Enabled"  Enabled

instance ToText ExpirationStatus where
    toText = \case
        Disabled -> "Disabled"
        Enabled  -> "Enabled"

instance FromXML ExpirationStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ExpirationStatus"

instance ToXML ExpirationStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ExpirationStatus"

data Part = Part
    { _pETag         :: Maybe Text
    , _pLastModified :: Maybe RFC822
    , _pPartNumber   :: Maybe Int
    , _pSize         :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'Part' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pETag' @::@ 'Maybe' 'Text'
--
-- * 'pLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'pPartNumber' @::@ 'Maybe' 'Int'
--
-- * 'pSize' @::@ 'Maybe' 'Int'
--
part :: Part
part = Part
    { _pPartNumber   = Nothing
    , _pLastModified = Nothing
    , _pETag         = Nothing
    , _pSize         = Nothing
    }

-- | Entity tag returned when the part was uploaded.
pETag :: Lens' Part (Maybe Text)
pETag = lens _pETag (\s a -> s { _pETag = a })

-- | Date and time at which the part was uploaded.
pLastModified :: Lens' Part (Maybe UTCTime)
pLastModified = lens _pLastModified (\s a -> s { _pLastModified = a })
    . mapping _Time

-- | Part number identifying the part.
pPartNumber :: Lens' Part (Maybe Int)
pPartNumber = lens _pPartNumber (\s a -> s { _pPartNumber = a })

-- | Size of the uploaded part data.
pSize :: Lens' Part (Maybe Int)
pSize = lens _pSize (\s a -> s { _pSize = a })

instance FromXML Part where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Part"

instance ToXML Part where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Part"

data VersioningConfiguration = VersioningConfiguration
    { _vcMFADelete :: Maybe Text
    , _vcStatus    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VersioningConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcMFADelete' @::@ 'Maybe' 'Text'
--
-- * 'vcStatus' @::@ 'Maybe' 'Text'
--
versioningConfiguration :: VersioningConfiguration
versioningConfiguration = VersioningConfiguration
    { _vcMFADelete = Nothing
    , _vcStatus    = Nothing
    }

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
vcMFADelete :: Lens' VersioningConfiguration (Maybe Text)
vcMFADelete = lens _vcMFADelete (\s a -> s { _vcMFADelete = a })

-- | The versioning state of the bucket.
vcStatus :: Lens' VersioningConfiguration (Maybe Text)
vcStatus = lens _vcStatus (\s a -> s { _vcStatus = a })

instance FromXML VersioningConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VersioningConfiguration"

instance ToXML VersioningConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "VersioningConfiguration"

data Tag = Tag
    { _tKey   :: Text
    , _tValue :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tKey' @::@ 'Text'
--
-- * 'tValue' @::@ 'Text'
--
tag :: Text -- ^ 'tKey'
    -> Text -- ^ 'tValue'
    -> Tag
tag p1 p2 = Tag
    { _tKey   = p1
    , _tValue = p2
    }

-- | Name of the tag.
tKey :: Lens' Tag Text
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | Value of the tag.
tValue :: Lens' Tag Text
tValue = lens _tValue (\s a -> s { _tValue = a })

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToXML Tag where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tag"

data ObjectStorageClass
    = Glacier           -- ^ GLACIER
    | ReducedRedundancy -- ^ REDUCED_REDUNDANCY
    | Standard          -- ^ STANDARD
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ObjectStorageClass

instance FromText ObjectStorageClass where
    parser = match "GLACIER"            Glacier
         <|> match "REDUCED_REDUNDANCY" ReducedRedundancy
         <|> match "STANDARD"           Standard

instance ToText ObjectStorageClass where
    toText = \case
        Glacier           -> "GLACIER"
        ReducedRedundancy -> "REDUCED_REDUNDANCY"
        Standard          -> "STANDARD"

instance FromXML ObjectStorageClass where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectStorageClass"

instance ToXML ObjectStorageClass where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectStorageClass"

data MetadataDirective
    = Copy    -- ^ COPY
    | Replace -- ^ REPLACE
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable MetadataDirective

instance FromText MetadataDirective where
    parser = match "COPY"    Copy
         <|> match "REPLACE" Replace

instance ToText MetadataDirective where
    toText = \case
        Copy    -> "COPY"
        Replace -> "REPLACE"

instance FromXML MetadataDirective where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MetadataDirective"

instance ToXML MetadataDirective where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "MetadataDirective"

data RedirectAllRequestsTo = RedirectAllRequestsTo
    { _rartHostName :: Text
    , _rartProtocol :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RedirectAllRequestsTo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rartHostName' @::@ 'Text'
--
-- * 'rartProtocol' @::@ 'Maybe' 'Text'
--
redirectAllRequestsTo :: Text -- ^ 'rartHostName'
                      -> RedirectAllRequestsTo
redirectAllRequestsTo p1 = RedirectAllRequestsTo
    { _rartHostName = p1
    , _rartProtocol = Nothing
    }

-- | Name of the host where requests will be redirected.
rartHostName :: Lens' RedirectAllRequestsTo Text
rartHostName = lens _rartHostName (\s a -> s { _rartHostName = a })

-- | Protocol to use (http, https) when redirecting requests. The default is
-- the protocol that is used in the original request.
rartProtocol :: Lens' RedirectAllRequestsTo (Maybe Text)
rartProtocol = lens _rartProtocol (\s a -> s { _rartProtocol = a })

instance FromXML RedirectAllRequestsTo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RedirectAllRequestsTo"

instance ToXML RedirectAllRequestsTo where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RedirectAllRequestsTo"

data RoutingRule = RoutingRule
    { _rrCondition :: Maybe Condition
    , _rrRedirect  :: Redirect
    } deriving (Eq, Show, Generic)

-- | 'RoutingRule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrCondition' @::@ 'Maybe' 'Condition'
--
-- * 'rrRedirect' @::@ 'Redirect'
--
routingRule :: Redirect -- ^ 'rrRedirect'
            -> RoutingRule
routingRule p1 = RoutingRule
    { _rrRedirect  = p1
    , _rrCondition = Nothing
    }

-- | A container for describing a condition that must be met for the specified
-- redirect to apply. For example, 1. If request is for pages in the /docs
-- folder, redirect to the /documents folder. 2. If request results in HTTP
-- error 4xx, redirect request to another host where you might process the
-- error.
rrCondition :: Lens' RoutingRule (Maybe Condition)
rrCondition = lens _rrCondition (\s a -> s { _rrCondition = a })

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an
-- error, you can can specify a different error code to return.
rrRedirect :: Lens' RoutingRule Redirect
rrRedirect = lens _rrRedirect (\s a -> s { _rrRedirect = a })

instance FromXML RoutingRule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RoutingRule"

instance ToXML RoutingRule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RoutingRule"

newtype NotificationConfiguration = NotificationConfiguration
    { _ncTopicConfiguration :: TopicConfiguration
    } deriving (Eq, Show, Generic)

-- | 'NotificationConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ncTopicConfiguration' @::@ 'TopicConfiguration'
--
notificationConfiguration :: TopicConfiguration -- ^ 'ncTopicConfiguration'
                          -> NotificationConfiguration
notificationConfiguration p1 = NotificationConfiguration
    { _ncTopicConfiguration = p1
    }

ncTopicConfiguration :: Lens' NotificationConfiguration TopicConfiguration
ncTopicConfiguration =
    lens _ncTopicConfiguration (\s a -> s { _ncTopicConfiguration = a })

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NotificationConfiguration"

instance ToXML NotificationConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NotificationConfiguration"

data S3ServiceError = S3ServiceError
    { _sseCode      :: Maybe Text
    , _sseKey       :: Maybe Text
    , _sseMessage   :: Maybe Text
    , _sseVersionId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'S3ServiceError' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sseCode' @::@ 'Maybe' 'Text'
--
-- * 'sseKey' @::@ 'Maybe' 'Text'
--
-- * 'sseMessage' @::@ 'Maybe' 'Text'
--
-- * 'sseVersionId' @::@ 'Maybe' 'Text'
--
s3ServiceError :: S3ServiceError
s3ServiceError = S3ServiceError
    { _sseKey       = Nothing
    , _sseVersionId = Nothing
    , _sseCode      = Nothing
    , _sseMessage   = Nothing
    }

sseCode :: Lens' S3ServiceError (Maybe Text)
sseCode = lens _sseCode (\s a -> s { _sseCode = a })

sseKey :: Lens' S3ServiceError (Maybe Text)
sseKey = lens _sseKey (\s a -> s { _sseKey = a })

sseMessage :: Lens' S3ServiceError (Maybe Text)
sseMessage = lens _sseMessage (\s a -> s { _sseMessage = a })

sseVersionId :: Lens' S3ServiceError (Maybe Text)
sseVersionId = lens _sseVersionId (\s a -> s { _sseVersionId = a })

instance FromXML S3ServiceError where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3ServiceError"

instance ToXML S3ServiceError where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "S3ServiceError"

data ObjectCannedACL
    = AuthenticatedRead      -- ^ authenticated-read
    | BucketOwnerFullControl -- ^ bucket-owner-full-control
    | BucketOwnerRead        -- ^ bucket-owner-read
    | Private                -- ^ private
    | PublicRead             -- ^ public-read
    | PublicReadWrite        -- ^ public-read-write
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ObjectCannedACL

instance FromText ObjectCannedACL where
    parser = match "authenticated-read"        AuthenticatedRead
         <|> match "bucket-owner-full-control" BucketOwnerFullControl
         <|> match "bucket-owner-read"         BucketOwnerRead
         <|> match "private"                   Private
         <|> match "public-read"               PublicRead
         <|> match "public-read-write"         PublicReadWrite

instance ToText ObjectCannedACL where
    toText = \case
        AuthenticatedRead      -> "authenticated-read"
        BucketOwnerFullControl -> "bucket-owner-full-control"
        BucketOwnerRead        -> "bucket-owner-read"
        Private                -> "private"
        PublicRead             -> "public-read"
        PublicReadWrite        -> "public-read-write"

instance FromXML ObjectCannedACL where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectCannedACL"

instance ToXML ObjectCannedACL where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectCannedACL"

data BucketVersioningStatus
    = BVSEnabled   -- ^ Enabled
    | BVSSuspended -- ^ Suspended
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable BucketVersioningStatus

instance FromText BucketVersioningStatus where
    parser = match "Enabled"   BVSEnabled
         <|> match "Suspended" BVSSuspended

instance ToText BucketVersioningStatus where
    toText = \case
        BVSEnabled   -> "Enabled"
        BVSSuspended -> "Suspended"

instance FromXML BucketVersioningStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BucketVersioningStatus"

instance ToXML BucketVersioningStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketVersioningStatus"

data DeletedObject = DeletedObject
    { _doDeleteMarker          :: Maybe Bool
    , _doDeleteMarkerVersionId :: Maybe Text
    , _doKey                   :: Maybe Text
    , _doVersionId             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeletedObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doDeleteMarker' @::@ 'Maybe' 'Bool'
--
-- * 'doDeleteMarkerVersionId' @::@ 'Maybe' 'Text'
--
-- * 'doKey' @::@ 'Maybe' 'Text'
--
-- * 'doVersionId' @::@ 'Maybe' 'Text'
--
deletedObject :: DeletedObject
deletedObject = DeletedObject
    { _doKey                   = Nothing
    , _doVersionId             = Nothing
    , _doDeleteMarker          = Nothing
    , _doDeleteMarkerVersionId = Nothing
    }

doDeleteMarker :: Lens' DeletedObject (Maybe Bool)
doDeleteMarker = lens _doDeleteMarker (\s a -> s { _doDeleteMarker = a })

doDeleteMarkerVersionId :: Lens' DeletedObject (Maybe Text)
doDeleteMarkerVersionId =
    lens _doDeleteMarkerVersionId (\s a -> s { _doDeleteMarkerVersionId = a })

doKey :: Lens' DeletedObject (Maybe Text)
doKey = lens _doKey (\s a -> s { _doKey = a })

doVersionId :: Lens' DeletedObject (Maybe Text)
doVersionId = lens _doVersionId (\s a -> s { _doVersionId = a })

instance FromXML DeletedObject where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletedObject"

instance ToXML DeletedObject where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeletedObject"

data ObjectVersionStorageClass
    = OVSCStandard -- ^ STANDARD
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ObjectVersionStorageClass

instance FromText ObjectVersionStorageClass where
    parser = match "STANDARD" OVSCStandard

instance ToText ObjectVersionStorageClass where
    toText OVSCStandard = "STANDARD"

instance FromXML ObjectVersionStorageClass where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectVersionStorageClass"

instance ToXML ObjectVersionStorageClass where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectVersionStorageClass"

data CopyPartResult = CopyPartResult
    { _cprETag         :: Maybe Text
    , _cprLastModified :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'CopyPartResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprETag' @::@ 'Maybe' 'Text'
--
-- * 'cprLastModified' @::@ 'Maybe' 'UTCTime'
--
copyPartResult :: CopyPartResult
copyPartResult = CopyPartResult
    { _cprETag         = Nothing
    , _cprLastModified = Nothing
    }

-- | Entity tag of the object.
cprETag :: Lens' CopyPartResult (Maybe Text)
cprETag = lens _cprETag (\s a -> s { _cprETag = a })

-- | Date and time at which the object was uploaded.
cprLastModified :: Lens' CopyPartResult (Maybe UTCTime)
cprLastModified = lens _cprLastModified (\s a -> s { _cprLastModified = a })
    . mapping _Time

instance FromXML CopyPartResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyPartResult"

instance ToXML CopyPartResult where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CopyPartResult"

data EncodingType
    = Url -- ^ url
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable EncodingType

instance FromText EncodingType where
    parser = match "url" Url

instance ToText EncodingType where
    toText Url = "url"

instance FromXML EncodingType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EncodingType"

instance ToXML EncodingType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "EncodingType"

newtype RequestPaymentConfiguration = RequestPaymentConfiguration
    { _rpcPayer :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'RequestPaymentConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpcPayer' @::@ 'Text'
--
requestPaymentConfiguration :: Text -- ^ 'rpcPayer'
                            -> RequestPaymentConfiguration
requestPaymentConfiguration p1 = RequestPaymentConfiguration
    { _rpcPayer = p1
    }

-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration Text
rpcPayer = lens _rpcPayer (\s a -> s { _rpcPayer = a })

instance FromXML RequestPaymentConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RequestPaymentConfiguration"

instance ToXML RequestPaymentConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RequestPaymentConfiguration"

data CORSRule = CORSRule
    { _corsrAllowedHeaders :: [Text]
    , _corsrAllowedMethods :: [Text]
    , _corsrAllowedOrigins :: [Text]
    , _corsrExposeHeaders  :: [Text]
    , _corsrMaxAgeSeconds  :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'CORSRule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'corsrAllowedHeaders' @::@ ['Text']
--
-- * 'corsrAllowedMethods' @::@ ['Text']
--
-- * 'corsrAllowedOrigins' @::@ ['Text']
--
-- * 'corsrExposeHeaders' @::@ ['Text']
--
-- * 'corsrMaxAgeSeconds' @::@ 'Maybe' 'Int'
--
corsrule :: CORSRule
corsrule = CORSRule
    { _corsrAllowedHeaders = mempty
    , _corsrAllowedMethods = mempty
    , _corsrAllowedOrigins = mempty
    , _corsrExposeHeaders  = mempty
    , _corsrMaxAgeSeconds  = Nothing
    }

-- | Specifies which headers are allowed in a pre-flight OPTIONS request.
corsrAllowedHeaders :: Lens' CORSRule [Text]
corsrAllowedHeaders =
    lens _corsrAllowedHeaders (\s a -> s { _corsrAllowedHeaders = a })

-- | Identifies HTTP methods that the domain/origin specified in the rule is
-- allowed to execute.
corsrAllowedMethods :: Lens' CORSRule [Text]
corsrAllowedMethods =
    lens _corsrAllowedMethods (\s a -> s { _corsrAllowedMethods = a })

-- | One or more origins you want customers to be able to access the bucket
-- from.
corsrAllowedOrigins :: Lens' CORSRule [Text]
corsrAllowedOrigins =
    lens _corsrAllowedOrigins (\s a -> s { _corsrAllowedOrigins = a })

-- | One or more headers in the response that you want customers to be able to
-- access from their applications (for example, from a JavaScript
-- XMLHttpRequest object).
corsrExposeHeaders :: Lens' CORSRule [Text]
corsrExposeHeaders =
    lens _corsrExposeHeaders (\s a -> s { _corsrExposeHeaders = a })

-- | The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
corsrMaxAgeSeconds :: Lens' CORSRule (Maybe Int)
corsrMaxAgeSeconds =
    lens _corsrMaxAgeSeconds (\s a -> s { _corsrMaxAgeSeconds = a })

instance FromXML CORSRule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CORSRule"

instance ToXML CORSRule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CORSRule"

data WebsiteConfiguration = WebsiteConfiguration
    { _wcErrorDocument         :: Maybe ErrorDocument
    , _wcIndexDocument         :: Maybe IndexDocument
    , _wcRedirectAllRequestsTo :: Maybe RedirectAllRequestsTo
    , _wcRoutingRules          :: [RoutingRule]
    } deriving (Eq, Show, Generic)

-- | 'WebsiteConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wcErrorDocument' @::@ 'Maybe' 'ErrorDocument'
--
-- * 'wcIndexDocument' @::@ 'Maybe' 'IndexDocument'
--
-- * 'wcRedirectAllRequestsTo' @::@ 'Maybe' 'RedirectAllRequestsTo'
--
-- * 'wcRoutingRules' @::@ ['RoutingRule']
--
websiteConfiguration :: WebsiteConfiguration
websiteConfiguration = WebsiteConfiguration
    { _wcErrorDocument         = Nothing
    , _wcIndexDocument         = Nothing
    , _wcRedirectAllRequestsTo = Nothing
    , _wcRoutingRules          = mempty
    }

wcErrorDocument :: Lens' WebsiteConfiguration (Maybe ErrorDocument)
wcErrorDocument = lens _wcErrorDocument (\s a -> s { _wcErrorDocument = a })

wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument = lens _wcIndexDocument (\s a -> s { _wcIndexDocument = a })

wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo =
    lens _wcRedirectAllRequestsTo (\s a -> s { _wcRedirectAllRequestsTo = a })

wcRoutingRules :: Lens' WebsiteConfiguration [RoutingRule]
wcRoutingRules = lens _wcRoutingRules (\s a -> s { _wcRoutingRules = a })

instance FromXML WebsiteConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "WebsiteConfiguration"

instance ToXML WebsiteConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "WebsiteConfiguration"

data NoncurrentVersionTransition = NoncurrentVersionTransition
    { _nvtNoncurrentDays :: Int
    , _nvtStorageClass   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'NoncurrentVersionTransition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nvtNoncurrentDays' @::@ 'Int'
--
-- * 'nvtStorageClass' @::@ 'Text'
--
noncurrentVersionTransition :: Int -- ^ 'nvtNoncurrentDays'
                            -> Text -- ^ 'nvtStorageClass'
                            -> NoncurrentVersionTransition
noncurrentVersionTransition p1 p2 = NoncurrentVersionTransition
    { _nvtNoncurrentDays = p1
    , _nvtStorageClass   = p2
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3 can
-- perform the associated action. For information about the noncurrent days
-- calculations, see How Amazon S3 Calculates When an Object Became
-- Noncurrent in the Amazon Simple Storage Service Developer Guide.
nvtNoncurrentDays :: Lens' NoncurrentVersionTransition Int
nvtNoncurrentDays =
    lens _nvtNoncurrentDays (\s a -> s { _nvtNoncurrentDays = a })

-- | The class of storage used to store the object.
nvtStorageClass :: Lens' NoncurrentVersionTransition Text
nvtStorageClass = lens _nvtStorageClass (\s a -> s { _nvtStorageClass = a })

instance FromXML NoncurrentVersionTransition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NoncurrentVersionTransition"

instance ToXML NoncurrentVersionTransition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "NoncurrentVersionTransition"

data Initiator = Initiator
    { _iDisplayName :: Maybe Text
    , _iID          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Initiator' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iDisplayName' @::@ 'Maybe' 'Text'
--
-- * 'iID' @::@ 'Maybe' 'Text'
--
initiator :: Initiator
initiator = Initiator
    { _iID          = Nothing
    , _iDisplayName = Nothing
    }

-- | Name of the Principal.
iDisplayName :: Lens' Initiator (Maybe Text)
iDisplayName = lens _iDisplayName (\s a -> s { _iDisplayName = a })

-- | If the principal is an AWS account, it provides the Canonical User ID. If
-- the principal is an IAM User, it provides a user ARN value.
iID :: Lens' Initiator (Maybe Text)
iID = lens _iID (\s a -> s { _iID = a })

instance FromXML Initiator where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Initiator"

instance ToXML Initiator where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Initiator"

data ObjectIdentifier = ObjectIdentifier
    { _oiKey       :: Text
    , _oiVersionId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ObjectIdentifier' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oiKey' @::@ 'Text'
--
-- * 'oiVersionId' @::@ 'Maybe' 'Text'
--
objectIdentifier :: Text -- ^ 'oiKey'
                 -> ObjectIdentifier
objectIdentifier p1 = ObjectIdentifier
    { _oiKey       = p1
    , _oiVersionId = Nothing
    }

-- | Key name of the object to delete.
oiKey :: Lens' ObjectIdentifier Text
oiKey = lens _oiKey (\s a -> s { _oiKey = a })

-- | VersionId for the specific version of the object to delete.
oiVersionId :: Lens' ObjectIdentifier (Maybe Text)
oiVersionId = lens _oiVersionId (\s a -> s { _oiVersionId = a })

instance FromXML ObjectIdentifier where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectIdentifier"

instance ToXML ObjectIdentifier where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectIdentifier"

data Bucket = Bucket
    { _bCreationDate :: Maybe RFC822
    , _bName         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Bucket' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bCreationDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'bName' @::@ 'Maybe' 'Text'
--
bucket :: Bucket
bucket = Bucket
    { _bName         = Nothing
    , _bCreationDate = Nothing
    }

-- | Date the bucket was created.
bCreationDate :: Lens' Bucket (Maybe UTCTime)
bCreationDate = lens _bCreationDate (\s a -> s { _bCreationDate = a })
    . mapping _Time

-- | The name of the bucket.
bName :: Lens' Bucket (Maybe Text)
bName = lens _bName (\s a -> s { _bName = a })

instance FromXML Bucket where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Bucket"

instance ToXML Bucket where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Bucket"

data Protocol
    = Http  -- ^ http
    | Https -- ^ https
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable Protocol

instance FromText Protocol where
    parser = match "http"  Http
         <|> match "https" Https

instance ToText Protocol where
    toText = \case
        Http  -> "http"
        Https -> "https"

instance FromXML Protocol where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Protocol"

instance ToXML Protocol where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Protocol"

data Grant = Grant
    { _gGrantee    :: Maybe Grantee
    , _gPermission :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Grant' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gGrantee' @::@ 'Maybe' 'Grantee'
--
-- * 'gPermission' @::@ 'Maybe' 'Text'
--
grant :: Grant
grant = Grant
    { _gGrantee    = Nothing
    , _gPermission = Nothing
    }

gGrantee :: Lens' Grant (Maybe Grantee)
gGrantee = lens _gGrantee (\s a -> s { _gGrantee = a })

-- | Specifies the permission given to the grantee.
gPermission :: Lens' Grant (Maybe Text)
gPermission = lens _gPermission (\s a -> s { _gPermission = a })

instance FromXML Grant where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grant"

instance ToXML Grant where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grant"

data Rule = Rule
    { _rExpiration                  :: Maybe LifecycleExpiration
    , _rID                          :: Maybe Text
    , _rNoncurrentVersionExpiration :: Maybe NoncurrentVersionExpiration
    , _rNoncurrentVersionTransition :: Maybe NoncurrentVersionTransition
    , _rPrefix                      :: Text
    , _rStatus                      :: Text
    , _rTransition                  :: Maybe Transition
    } deriving (Eq, Show, Generic)

-- | 'Rule' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rExpiration' @::@ 'Maybe' 'LifecycleExpiration'
--
-- * 'rID' @::@ 'Maybe' 'Text'
--
-- * 'rNoncurrentVersionExpiration' @::@ 'Maybe' 'NoncurrentVersionExpiration'
--
-- * 'rNoncurrentVersionTransition' @::@ 'Maybe' 'NoncurrentVersionTransition'
--
-- * 'rPrefix' @::@ 'Text'
--
-- * 'rStatus' @::@ 'Text'
--
-- * 'rTransition' @::@ 'Maybe' 'Transition'
--
rule :: Text -- ^ 'rPrefix'
     -> Text -- ^ 'rStatus'
     -> Rule
rule p1 p2 = Rule
    { _rPrefix                      = p1
    , _rStatus                      = p2
    , _rExpiration                  = Nothing
    , _rID                          = Nothing
    , _rTransition                  = Nothing
    , _rNoncurrentVersionTransition = Nothing
    , _rNoncurrentVersionExpiration = Nothing
    }

rExpiration :: Lens' Rule (Maybe LifecycleExpiration)
rExpiration = lens _rExpiration (\s a -> s { _rExpiration = a })

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
rID :: Lens' Rule (Maybe Text)
rID = lens _rID (\s a -> s { _rID = a })

rNoncurrentVersionExpiration :: Lens' Rule (Maybe NoncurrentVersionExpiration)
rNoncurrentVersionExpiration =
    lens _rNoncurrentVersionExpiration
        (\s a -> s { _rNoncurrentVersionExpiration = a })

rNoncurrentVersionTransition :: Lens' Rule (Maybe NoncurrentVersionTransition)
rNoncurrentVersionTransition =
    lens _rNoncurrentVersionTransition
        (\s a -> s { _rNoncurrentVersionTransition = a })

-- | Prefix identifying one or more objects to which the rule applies.
rPrefix :: Lens' Rule Text
rPrefix = lens _rPrefix (\s a -> s { _rPrefix = a })

-- | If 'Enabled', the rule is currently being applied. If 'Disabled', the
-- rule is not currently being applied.
rStatus :: Lens' Rule Text
rStatus = lens _rStatus (\s a -> s { _rStatus = a })

rTransition :: Lens' Rule (Maybe Transition)
rTransition = lens _rTransition (\s a -> s { _rTransition = a })

instance FromXML Rule where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Rule"

instance ToXML Rule where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Rule"

data TopicConfiguration = TopicConfiguration
    { _tcEvent :: Maybe Text
    , _tcTopic :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'TopicConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tcEvent' @::@ 'Maybe' 'Text'
--
-- * 'tcTopic' @::@ 'Maybe' 'Text'
--
topicConfiguration :: TopicConfiguration
topicConfiguration = TopicConfiguration
    { _tcEvent = Nothing
    , _tcTopic = Nothing
    }

-- | Bucket event for which to send notifications.
tcEvent :: Lens' TopicConfiguration (Maybe Text)
tcEvent = lens _tcEvent (\s a -> s { _tcEvent = a })

-- | Amazon SNS topic to which Amazon S3 will publish a message to report the
-- specified events for the bucket.
tcTopic :: Lens' TopicConfiguration (Maybe Text)
tcTopic = lens _tcTopic (\s a -> s { _tcTopic = a })

instance FromXML TopicConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TopicConfiguration"

instance ToXML TopicConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TopicConfiguration"

data Owner = Owner
    { _oDisplayName :: Maybe Text
    , _oID          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Owner' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oDisplayName' @::@ 'Maybe' 'Text'
--
-- * 'oID' @::@ 'Maybe' 'Text'
--
owner :: Owner
owner = Owner
    { _oDisplayName = Nothing
    , _oID          = Nothing
    }

oDisplayName :: Lens' Owner (Maybe Text)
oDisplayName = lens _oDisplayName (\s a -> s { _oDisplayName = a })

oID :: Lens' Owner (Maybe Text)
oID = lens _oID (\s a -> s { _oID = a })

instance FromXML Owner where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Owner"

instance ToXML Owner where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Owner"

newtype BucketLoggingStatus = BucketLoggingStatus
    { _blsLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq, Show, Generic)

-- | 'BucketLoggingStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'blsLoggingEnabled' @::@ 'Maybe' 'LoggingEnabled'
--
bucketLoggingStatus :: BucketLoggingStatus
bucketLoggingStatus = BucketLoggingStatus
    { _blsLoggingEnabled = Nothing
    }

blsLoggingEnabled :: Lens' BucketLoggingStatus (Maybe LoggingEnabled)
blsLoggingEnabled =
    lens _blsLoggingEnabled (\s a -> s { _blsLoggingEnabled = a })

instance FromXML BucketLoggingStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BucketLoggingStatus"

instance ToXML BucketLoggingStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketLoggingStatus"

newtype ErrorDocument = ErrorDocument
    { _edKey :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ErrorDocument' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edKey' @::@ 'Text'
--
errorDocument :: Text -- ^ 'edKey'
              -> ErrorDocument
errorDocument p1 = ErrorDocument
    { _edKey = p1
    }

-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument Text
edKey = lens _edKey (\s a -> s { _edKey = a })

instance FromXML ErrorDocument where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ErrorDocument"

instance ToXML ErrorDocument where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ErrorDocument"

data StorageClass
    = SCReducedRedundancy -- ^ REDUCED_REDUNDANCY
    | SCStandard          -- ^ STANDARD
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable StorageClass

instance FromText StorageClass where
    parser = match "REDUCED_REDUNDANCY" SCReducedRedundancy
         <|> match "STANDARD"           SCStandard

instance ToText StorageClass where
    toText = \case
        SCReducedRedundancy -> "REDUCED_REDUNDANCY"
        SCStandard          -> "STANDARD"

instance FromXML StorageClass where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StorageClass"

instance ToXML StorageClass where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "StorageClass"

data ObjectVersion = ObjectVersion
    { _ovETag         :: Maybe Text
    , _ovIsLatest     :: Maybe Bool
    , _ovKey          :: Maybe Text
    , _ovLastModified :: Maybe RFC822
    , _ovOwner        :: Maybe Owner
    , _ovSize         :: Maybe Int
    , _ovStorageClass :: Maybe Text
    , _ovVersionId    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ObjectVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ovETag' @::@ 'Maybe' 'Text'
--
-- * 'ovIsLatest' @::@ 'Maybe' 'Bool'
--
-- * 'ovKey' @::@ 'Maybe' 'Text'
--
-- * 'ovLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'ovOwner' @::@ 'Maybe' 'Owner'
--
-- * 'ovSize' @::@ 'Maybe' 'Int'
--
-- * 'ovStorageClass' @::@ 'Maybe' 'Text'
--
-- * 'ovVersionId' @::@ 'Maybe' 'Text'
--
objectVersion :: ObjectVersion
objectVersion = ObjectVersion
    { _ovETag         = Nothing
    , _ovSize         = Nothing
    , _ovStorageClass = Nothing
    , _ovKey          = Nothing
    , _ovVersionId    = Nothing
    , _ovIsLatest     = Nothing
    , _ovLastModified = Nothing
    , _ovOwner        = Nothing
    }

ovETag :: Lens' ObjectVersion (Maybe Text)
ovETag = lens _ovETag (\s a -> s { _ovETag = a })

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
ovIsLatest :: Lens' ObjectVersion (Maybe Bool)
ovIsLatest = lens _ovIsLatest (\s a -> s { _ovIsLatest = a })

-- | The object key.
ovKey :: Lens' ObjectVersion (Maybe Text)
ovKey = lens _ovKey (\s a -> s { _ovKey = a })

-- | Date and time the object was last modified.
ovLastModified :: Lens' ObjectVersion (Maybe UTCTime)
ovLastModified = lens _ovLastModified (\s a -> s { _ovLastModified = a })
    . mapping _Time

ovOwner :: Lens' ObjectVersion (Maybe Owner)
ovOwner = lens _ovOwner (\s a -> s { _ovOwner = a })

-- | Size in bytes of the object.
ovSize :: Lens' ObjectVersion (Maybe Int)
ovSize = lens _ovSize (\s a -> s { _ovSize = a })

-- | The class of storage used to store the object.
ovStorageClass :: Lens' ObjectVersion (Maybe Text)
ovStorageClass = lens _ovStorageClass (\s a -> s { _ovStorageClass = a })

-- | Version ID of an object.
ovVersionId :: Lens' ObjectVersion (Maybe Text)
ovVersionId = lens _ovVersionId (\s a -> s { _ovVersionId = a })

instance FromXML ObjectVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ObjectVersion"

instance ToXML ObjectVersion where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ObjectVersion"

data TargetGrant = TargetGrant
    { _tgGrantee    :: Maybe Grantee
    , _tgPermission :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'TargetGrant' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tgGrantee' @::@ 'Maybe' 'Grantee'
--
-- * 'tgPermission' @::@ 'Maybe' 'Text'
--
targetGrant :: TargetGrant
targetGrant = TargetGrant
    { _tgGrantee    = Nothing
    , _tgPermission = Nothing
    }

tgGrantee :: Lens' TargetGrant (Maybe Grantee)
tgGrantee = lens _tgGrantee (\s a -> s { _tgGrantee = a })

-- | Logging permissions assigned to the Grantee for the bucket.
tgPermission :: Lens' TargetGrant (Maybe Text)
tgPermission = lens _tgPermission (\s a -> s { _tgPermission = a })

instance FromXML TargetGrant where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TargetGrant"

instance ToXML TargetGrant where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TargetGrant"

data MFADeleteStatus
    = MFADSDisabled -- ^ Disabled
    | MFADSEnabled  -- ^ Enabled
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable MFADeleteStatus

instance FromText MFADeleteStatus where
    parser = match "Disabled" MFADSDisabled
         <|> match "Enabled"  MFADSEnabled

instance ToText MFADeleteStatus where
    toText = \case
        MFADSDisabled -> "Disabled"
        MFADSEnabled  -> "Enabled"

instance FromXML MFADeleteStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MFADeleteStatus"

instance ToXML MFADeleteStatus where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "MFADeleteStatus"

data Payer
    = BucketOwner -- ^ BucketOwner
    | Requester   -- ^ Requester
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable Payer

instance FromText Payer where
    parser = match "BucketOwner" BucketOwner
         <|> match "Requester"   Requester

instance ToText Payer where
    toText = \case
        BucketOwner -> "BucketOwner"
        Requester   -> "Requester"

instance FromXML Payer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Payer"

instance ToXML Payer where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Payer"

data Redirect = Redirect
    { _rHostName             :: Maybe Text
    , _rHttpRedirectCode     :: Maybe Text
    , _rProtocol             :: Maybe Text
    , _rReplaceKeyPrefixWith :: Maybe Text
    , _rReplaceKeyWith       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Redirect' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rHostName' @::@ 'Maybe' 'Text'
--
-- * 'rHttpRedirectCode' @::@ 'Maybe' 'Text'
--
-- * 'rProtocol' @::@ 'Maybe' 'Text'
--
-- * 'rReplaceKeyPrefixWith' @::@ 'Maybe' 'Text'
--
-- * 'rReplaceKeyWith' @::@ 'Maybe' 'Text'
--
redirect :: Redirect
redirect = Redirect
    { _rHostName             = Nothing
    , _rHttpRedirectCode     = Nothing
    , _rProtocol             = Nothing
    , _rReplaceKeyPrefixWith = Nothing
    , _rReplaceKeyWith       = Nothing
    }

-- | The host name to use in the redirect request.
rHostName :: Lens' Redirect (Maybe Text)
rHostName = lens _rHostName (\s a -> s { _rHostName = a })

-- | The HTTP redirect code to use on the response. Not required if one of the
-- siblings is present.
rHttpRedirectCode :: Lens' Redirect (Maybe Text)
rHttpRedirectCode =
    lens _rHttpRedirectCode (\s a -> s { _rHttpRedirectCode = a })

-- | Protocol to use (http, https) when redirecting requests. The default is
-- the protocol that is used in the original request.
rProtocol :: Lens' Redirect (Maybe Text)
rProtocol = lens _rProtocol (\s a -> s { _rProtocol = a })

-- | The object key prefix to use in the redirect request. For example, to
-- redirect requests for all pages with prefix docs/ (objects in the docs/
-- folder) to documents/, you can set a condition block with KeyPrefixEquals
-- set to docs/ and in the Redirect set ReplaceKeyPrefixWith to /documents.
-- Not required if one of the siblings is present. Can be present only if
-- ReplaceKeyWith is not provided.
rReplaceKeyPrefixWith :: Lens' Redirect (Maybe Text)
rReplaceKeyPrefixWith =
    lens _rReplaceKeyPrefixWith (\s a -> s { _rReplaceKeyPrefixWith = a })

-- | The specific object key to use in the redirect request. For example,
-- redirect request to error.html. Not required if one of the sibling is
-- present. Can be present only if ReplaceKeyPrefixWith is not provided.
rReplaceKeyWith :: Lens' Redirect (Maybe Text)
rReplaceKeyWith = lens _rReplaceKeyWith (\s a -> s { _rReplaceKeyWith = a })

instance FromXML Redirect where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Redirect"

instance ToXML Redirect where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Redirect"

data BucketLogsPermission
    = FullControl -- ^ FULL_CONTROL
    | Read        -- ^ READ
    | Write       -- ^ WRITE
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable BucketLogsPermission

instance FromText BucketLogsPermission where
    parser = match "FULL_CONTROL" FullControl
         <|> match "READ"         Read
         <|> match "WRITE"        Write

instance ToText BucketLogsPermission where
    toText = \case
        FullControl -> "FULL_CONTROL"
        Read        -> "READ"
        Write       -> "WRITE"

instance FromXML BucketLogsPermission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BucketLogsPermission"

instance ToXML BucketLogsPermission where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketLogsPermission"

data CompletedPart = CompletedPart
    { _cpETag       :: Maybe Text
    , _cpPartNumber :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'CompletedPart' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpETag' @::@ 'Maybe' 'Text'
--
-- * 'cpPartNumber' @::@ 'Maybe' 'Int'
--
completedPart :: CompletedPart
completedPart = CompletedPart
    { _cpETag       = Nothing
    , _cpPartNumber = Nothing
    }

-- | Entity tag returned when the part was uploaded.
cpETag :: Lens' CompletedPart (Maybe Text)
cpETag = lens _cpETag (\s a -> s { _cpETag = a })

-- | Part number that identifies the part.
cpPartNumber :: Lens' CompletedPart (Maybe Int)
cpPartNumber = lens _cpPartNumber (\s a -> s { _cpPartNumber = a })

instance FromXML CompletedPart where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CompletedPart"

instance ToXML CompletedPart where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CompletedPart"

newtype CreateBucketConfiguration = CreateBucketConfiguration
    { _cbcLocationConstraint :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateBucketConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbcLocationConstraint' @::@ 'Maybe' 'Text'
--
createBucketConfiguration :: CreateBucketConfiguration
createBucketConfiguration = CreateBucketConfiguration
    { _cbcLocationConstraint = Nothing
    }

-- | Specifies the region where the bucket will be created.
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe Text)
cbcLocationConstraint =
    lens _cbcLocationConstraint (\s a -> s { _cbcLocationConstraint = a })

instance FromXML CreateBucketConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateBucketConfiguration"

instance ToXML CreateBucketConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateBucketConfiguration"

newtype Tagging = Tagging
    { _tTagSet :: [Tag]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'Tagging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tTagSet' @::@ ['Tag']
--
tagging :: Tagging
tagging = Tagging
    { _tTagSet = mempty
    }

tTagSet :: Lens' Tagging [Tag]
tTagSet = lens _tTagSet (\s a -> s { _tTagSet = a })

instance FromXML Tagging where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tagging"

instance ToXML Tagging where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tagging"

data LifecycleExpiration = LifecycleExpiration
    { _leDate :: Maybe ISO8601
    , _leDays :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'LifecycleExpiration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'leDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'leDays' @::@ 'Maybe' 'Int'
--
lifecycleExpiration :: LifecycleExpiration
lifecycleExpiration = LifecycleExpiration
    { _leDate = Nothing
    , _leDays = Nothing
    }

-- | Indicates at what date the object is to be moved or deleted. Should be in
-- GMT ISO 8601 Format.
leDate :: Lens' LifecycleExpiration (Maybe UTCTime)
leDate = lens _leDate (\s a -> s { _leDate = a })
    . mapping _Time

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
leDays :: Lens' LifecycleExpiration (Maybe Int)
leDays = lens _leDays (\s a -> s { _leDays = a })

instance FromXML LifecycleExpiration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LifecycleExpiration"

instance ToXML LifecycleExpiration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LifecycleExpiration"

newtype CORSConfiguration = CORSConfiguration
    { _corscCORSRules :: [CORSRule]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'CORSConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'corscCORSRules' @::@ ['CORSRule']
--
corsconfiguration :: CORSConfiguration
corsconfiguration = CORSConfiguration
    { _corscCORSRules = mempty
    }

corscCORSRules :: Lens' CORSConfiguration [CORSRule]
corscCORSRules = lens _corscCORSRules (\s a -> s { _corscCORSRules = a })

instance FromXML CORSConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CORSConfiguration"

instance ToXML CORSConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CORSConfiguration"

data Object = Object
    { _oETag         :: Text
    , _oKey          :: Text
    , _oLastModified :: RFC822
    , _oOwner        :: Owner
    , _oSize         :: Int
    , _oStorageClass :: Text
    } deriving (Eq, Show, Generic)

-- | 'Object' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oETag' @::@ 'Text'
--
-- * 'oKey' @::@ 'Text'
--
-- * 'oLastModified' @::@ 'UTCTime'
--
-- * 'oOwner' @::@ 'Owner'
--
-- * 'oSize' @::@ 'Int'
--
-- * 'oStorageClass' @::@ 'Text'
--
object :: Text -- ^ 'oKey'
       -> UTCTime -- ^ 'oLastModified'
       -> Text -- ^ 'oETag'
       -> Int -- ^ 'oSize'
       -> Text -- ^ 'oStorageClass'
       -> Owner -- ^ 'oOwner'
       -> Object
object p1 p2 p3 p4 p5 p6 = Object
    { _oKey          = p1
    , _oLastModified = withIso _Time (const id) p2
    , _oETag         = p3
    , _oSize         = p4
    , _oStorageClass = p5
    , _oOwner        = p6
    }

oETag :: Lens' Object Text
oETag = lens _oETag (\s a -> s { _oETag = a })

oKey :: Lens' Object Text
oKey = lens _oKey (\s a -> s { _oKey = a })

oLastModified :: Lens' Object UTCTime
oLastModified = lens _oLastModified (\s a -> s { _oLastModified = a })
    . _Time

oOwner :: Lens' Object Owner
oOwner = lens _oOwner (\s a -> s { _oOwner = a })

oSize :: Lens' Object Int
oSize = lens _oSize (\s a -> s { _oSize = a })

-- | The class of storage used to store the object.
oStorageClass :: Lens' Object Text
oStorageClass = lens _oStorageClass (\s a -> s { _oStorageClass = a })

instance FromXML Object where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Object"

instance ToXML Object where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Object"

newtype CommonPrefix = CommonPrefix
    { _cpPrefix :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CommonPrefix' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpPrefix' @::@ 'Maybe' 'Text'
--
commonPrefix :: CommonPrefix
commonPrefix = CommonPrefix
    { _cpPrefix = Nothing
    }

cpPrefix :: Lens' CommonPrefix (Maybe Text)
cpPrefix = lens _cpPrefix (\s a -> s { _cpPrefix = a })

instance FromXML CommonPrefix where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CommonPrefix"

instance ToXML CommonPrefix where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CommonPrefix"

data MultipartUpload = MultipartUpload
    { _muInitiated    :: Maybe RFC822
    , _muInitiator    :: Maybe Initiator
    , _muKey          :: Maybe Text
    , _muOwner        :: Maybe Owner
    , _muStorageClass :: Maybe Text
    , _muUploadId     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'MultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'muInitiated' @::@ 'Maybe' 'UTCTime'
--
-- * 'muInitiator' @::@ 'Maybe' 'Initiator'
--
-- * 'muKey' @::@ 'Maybe' 'Text'
--
-- * 'muOwner' @::@ 'Maybe' 'Owner'
--
-- * 'muStorageClass' @::@ 'Maybe' 'Text'
--
-- * 'muUploadId' @::@ 'Maybe' 'Text'
--
multipartUpload :: MultipartUpload
multipartUpload = MultipartUpload
    { _muUploadId     = Nothing
    , _muKey          = Nothing
    , _muInitiated    = Nothing
    , _muStorageClass = Nothing
    , _muOwner        = Nothing
    , _muInitiator    = Nothing
    }

-- | Date and time at which the multipart upload was initiated.
muInitiated :: Lens' MultipartUpload (Maybe UTCTime)
muInitiated = lens _muInitiated (\s a -> s { _muInitiated = a })
    . mapping _Time

-- | Identifies who initiated the multipart upload.
muInitiator :: Lens' MultipartUpload (Maybe Initiator)
muInitiator = lens _muInitiator (\s a -> s { _muInitiator = a })

-- | Key of the object for which the multipart upload was initiated.
muKey :: Lens' MultipartUpload (Maybe Text)
muKey = lens _muKey (\s a -> s { _muKey = a })

muOwner :: Lens' MultipartUpload (Maybe Owner)
muOwner = lens _muOwner (\s a -> s { _muOwner = a })

-- | The class of storage used to store the object.
muStorageClass :: Lens' MultipartUpload (Maybe Text)
muStorageClass = lens _muStorageClass (\s a -> s { _muStorageClass = a })

-- | Upload ID that identifies the multipart upload.
muUploadId :: Lens' MultipartUpload (Maybe Text)
muUploadId = lens _muUploadId (\s a -> s { _muUploadId = a })

instance FromXML MultipartUpload where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MultipartUpload"

instance ToXML MultipartUpload where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "MultipartUpload"

data Type
    = AmazonCustomerByEmail -- ^ AmazonCustomerByEmail
    | CanonicalUser         -- ^ CanonicalUser
    | Group                 -- ^ Group
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable Type

instance FromText Type where
    parser = match "AmazonCustomerByEmail" AmazonCustomerByEmail
         <|> match "CanonicalUser"         CanonicalUser
         <|> match "Group"                 Group

instance ToText Type where
    toText = \case
        AmazonCustomerByEmail -> "AmazonCustomerByEmail"
        CanonicalUser         -> "CanonicalUser"
        Group                 -> "Group"

instance FromXML Type where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Type"

instance ToXML Type where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Type"

data TransitionStorageClass
    = TSCGlacier -- ^ GLACIER
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable TransitionStorageClass

instance FromText TransitionStorageClass where
    parser = match "GLACIER" TSCGlacier

instance ToText TransitionStorageClass where
    toText TSCGlacier = "GLACIER"

instance FromXML TransitionStorageClass where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TransitionStorageClass"

instance ToXML TransitionStorageClass where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TransitionStorageClass"

newtype CompletedMultipartUpload = CompletedMultipartUpload
    { _cmuParts :: [CompletedPart]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'CompletedMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmuParts' @::@ ['CompletedPart']
--
completedMultipartUpload :: CompletedMultipartUpload
completedMultipartUpload = CompletedMultipartUpload
    { _cmuParts = mempty
    }

cmuParts :: Lens' CompletedMultipartUpload [CompletedPart]
cmuParts = lens _cmuParts (\s a -> s { _cmuParts = a })

instance FromXML CompletedMultipartUpload where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CompletedMultipartUpload"

instance ToXML CompletedMultipartUpload where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CompletedMultipartUpload"

data Condition = Condition
    { _cHttpErrorCodeReturnedEquals :: Maybe Text
    , _cKeyPrefixEquals             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Condition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cHttpErrorCodeReturnedEquals' @::@ 'Maybe' 'Text'
--
-- * 'cKeyPrefixEquals' @::@ 'Maybe' 'Text'
--
condition :: Condition
condition = Condition
    { _cHttpErrorCodeReturnedEquals = Nothing
    , _cKeyPrefixEquals             = Nothing
    }

-- | The HTTP error code when the redirect is applied. In the event of an
-- error, if the error code equals this value, then the specified redirect
-- is applied. Required when parent element Condition is specified and
-- sibling KeyPrefixEquals is not specified. If both are specified, then
-- both must be true for the redirect to be applied.
cHttpErrorCodeReturnedEquals :: Lens' Condition (Maybe Text)
cHttpErrorCodeReturnedEquals =
    lens _cHttpErrorCodeReturnedEquals
        (\s a -> s { _cHttpErrorCodeReturnedEquals = a })

-- | The object key name prefix when the redirect is applied. For example, to
-- redirect requests for ExamplePage.html, the key prefix will be
-- ExamplePage.html. To redirect request for all pages with the prefix
-- docs/, the key prefix will be /docs, which identifies all objects in the
-- docs/ folder. Required when the parent element Condition is specified and
-- sibling HttpErrorCodeReturnedEquals is not specified. If both conditions
-- are specified, both must be true for the redirect to be applied.
cKeyPrefixEquals :: Lens' Condition (Maybe Text)
cKeyPrefixEquals = lens _cKeyPrefixEquals (\s a -> s { _cKeyPrefixEquals = a })

instance FromXML Condition where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Condition"

instance ToXML Condition where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Condition"

data Permission
    = PFullControl -- ^ FULL_CONTROL
    | PRead        -- ^ READ
    | PReadAcp     -- ^ READ_ACP
    | PWrite       -- ^ WRITE
    | PWriteAcp    -- ^ WRITE_ACP
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable Permission

instance FromText Permission where
    parser = match "FULL_CONTROL" PFullControl
         <|> match "READ"         PRead
         <|> match "READ_ACP"     PReadAcp
         <|> match "WRITE"        PWrite
         <|> match "WRITE_ACP"    PWriteAcp

instance ToText Permission where
    toText = \case
        PFullControl -> "FULL_CONTROL"
        PRead        -> "READ"
        PReadAcp     -> "READ_ACP"
        PWrite       -> "WRITE"
        PWriteAcp    -> "WRITE_ACP"

instance FromXML Permission where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Permission"

instance ToXML Permission where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Permission"

data AccessControlPolicy = AccessControlPolicy
    { _acpGrants :: [Grant]
    , _acpOwner  :: Maybe Owner
    } deriving (Eq, Show, Generic)

-- | 'AccessControlPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acpGrants' @::@ ['Grant']
--
-- * 'acpOwner' @::@ 'Maybe' 'Owner'
--
accessControlPolicy :: AccessControlPolicy
accessControlPolicy = AccessControlPolicy
    { _acpGrants = mempty
    , _acpOwner  = Nothing
    }

-- | A list of grants.
acpGrants :: Lens' AccessControlPolicy [Grant]
acpGrants = lens _acpGrants (\s a -> s { _acpGrants = a })

acpOwner :: Lens' AccessControlPolicy (Maybe Owner)
acpOwner = lens _acpOwner (\s a -> s { _acpOwner = a })

instance FromXML AccessControlPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessControlPolicy"

instance ToXML AccessControlPolicy where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "AccessControlPolicy"

data BucketCannedACL
    = CannedAuthenticatedRead -- ^ authenticated-read
    | CannedPrivate           -- ^ private
    | CannedPublicRead        -- ^ public-read
    | CannedPublicReadWrite   -- ^ public-read-write
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable BucketCannedACL

instance FromText BucketCannedACL where
    parser = match "authenticated-read" CannedAuthenticatedRead
         <|> match "private"            CannedPrivate
         <|> match "public-read"        CannedPublicRead
         <|> match "public-read-write"  CannedPublicReadWrite

instance ToText BucketCannedACL where
    toText = \case
        CannedAuthenticatedRead -> "authenticated-read"
        CannedPrivate           -> "private"
        CannedPublicRead        -> "public-read"
        CannedPublicReadWrite   -> "public-read-write"

instance FromXML BucketCannedACL where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BucketCannedACL"

instance ToXML BucketCannedACL where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "BucketCannedACL"

data MFADelete
    = MFADDisabled -- ^ Disabled
    | MFADEnabled  -- ^ Enabled
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable MFADelete

instance FromText MFADelete where
    parser = match "Disabled" MFADDisabled
         <|> match "Enabled"  MFADEnabled

instance ToText MFADelete where
    toText = \case
        MFADDisabled -> "Disabled"
        MFADEnabled  -> "Enabled"

instance FromXML MFADelete where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MFADelete"

instance ToXML MFADelete where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "MFADelete"

data Grantee = Grantee
    { _gDisplayName  :: Maybe Text
    , _gEmailAddress :: Maybe Text
    , _gID           :: Maybe Text
    , _gType         :: Text
    , _gURI          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Grantee' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gDisplayName' @::@ 'Maybe' 'Text'
--
-- * 'gEmailAddress' @::@ 'Maybe' 'Text'
--
-- * 'gID' @::@ 'Maybe' 'Text'
--
-- * 'gType' @::@ 'Text'
--
-- * 'gURI' @::@ 'Maybe' 'Text'
--
grantee :: Text -- ^ 'gType'
        -> Grantee
grantee p1 = Grantee
    { _gType         = p1
    , _gDisplayName  = Nothing
    , _gEmailAddress = Nothing
    , _gID           = Nothing
    , _gURI          = Nothing
    }

-- | Screen name of the grantee.
gDisplayName :: Lens' Grantee (Maybe Text)
gDisplayName = lens _gDisplayName (\s a -> s { _gDisplayName = a })

-- | Email address of the grantee.
gEmailAddress :: Lens' Grantee (Maybe Text)
gEmailAddress = lens _gEmailAddress (\s a -> s { _gEmailAddress = a })

-- | The canonical user ID of the grantee.
gID :: Lens' Grantee (Maybe Text)
gID = lens _gID (\s a -> s { _gID = a })

-- | Type of grantee.
gType :: Lens' Grantee Text
gType = lens _gType (\s a -> s { _gType = a })

-- | URI of the grantee group.
gURI :: Lens' Grantee (Maybe Text)
gURI = lens _gURI (\s a -> s { _gURI = a })

instance FromXML Grantee where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Grantee"

instance ToXML Grantee where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Grantee"

newtype LifecycleConfiguration = LifecycleConfiguration
    { _lcRules :: [Rule]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'LifecycleConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcRules' @::@ ['Rule']
--
lifecycleConfiguration :: LifecycleConfiguration
lifecycleConfiguration = LifecycleConfiguration
    { _lcRules = mempty
    }

lcRules :: Lens' LifecycleConfiguration [Rule]
lcRules = lens _lcRules (\s a -> s { _lcRules = a })

instance FromXML LifecycleConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LifecycleConfiguration"

instance ToXML LifecycleConfiguration where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LifecycleConfiguration"

data LoggingEnabled = LoggingEnabled
    { _leTargetBucket :: Maybe Text
    , _leTargetGrants :: [TargetGrant]
    , _leTargetPrefix :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'LoggingEnabled' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'leTargetBucket' @::@ 'Maybe' 'Text'
--
-- * 'leTargetGrants' @::@ ['TargetGrant']
--
-- * 'leTargetPrefix' @::@ 'Maybe' 'Text'
--
loggingEnabled :: LoggingEnabled
loggingEnabled = LoggingEnabled
    { _leTargetBucket = Nothing
    , _leTargetGrants = mempty
    , _leTargetPrefix = Nothing
    }

-- | Specifies the bucket where you want Amazon S3 to store server access
-- logs. You can have your logs delivered to any bucket that you own,
-- including the same bucket that is being logged. You can also configure
-- multiple buckets to deliver their logs to the same target bucket. In this
-- case you should choose a different TargetPrefix for each source bucket so
-- that the delivered log files can be distinguished by key.
leTargetBucket :: Lens' LoggingEnabled (Maybe Text)
leTargetBucket = lens _leTargetBucket (\s a -> s { _leTargetBucket = a })

leTargetGrants :: Lens' LoggingEnabled [TargetGrant]
leTargetGrants = lens _leTargetGrants (\s a -> s { _leTargetGrants = a })

-- | This element lets you specify a prefix for the keys that the log files
-- will be stored under.
leTargetPrefix :: Lens' LoggingEnabled (Maybe Text)
leTargetPrefix = lens _leTargetPrefix (\s a -> s { _leTargetPrefix = a })

instance FromXML LoggingEnabled where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoggingEnabled"

instance ToXML LoggingEnabled where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LoggingEnabled"

data ServerSideEncryption
    = AES256 -- ^ AES256
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ServerSideEncryption

instance FromText ServerSideEncryption where
    parser = match "AES256" AES256

instance ToText ServerSideEncryption where
    toText AES256 = "AES256"

instance FromXML ServerSideEncryption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ServerSideEncryption"

instance ToXML ServerSideEncryption where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ServerSideEncryption"

newtype IndexDocument = IndexDocument
    { _idSuffix :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'IndexDocument' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idSuffix' @::@ 'Text'
--
indexDocument :: Text -- ^ 'idSuffix'
              -> IndexDocument
indexDocument p1 = IndexDocument
    { _idSuffix = p1
    }

-- | A suffix that is appended to a request that is for a directory on the
-- website endpoint (e.g. if the suffix is index.html and you make a request
-- to samplebucket/images/ the data that is returned will be for the object
-- with the key name images/index.html) The suffix must not be empty and
-- must not include a slash character.
idSuffix :: Lens' IndexDocument Text
idSuffix = lens _idSuffix (\s a -> s { _idSuffix = a })

instance FromXML IndexDocument where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IndexDocument"

instance ToXML IndexDocument where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "IndexDocument"

data CopyObjectResult = CopyObjectResult
    { _corETag         :: Maybe Text
    , _corLastModified :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'CopyObjectResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'corETag' @::@ 'Maybe' 'Text'
--
-- * 'corLastModified' @::@ 'Maybe' 'UTCTime'
--
copyObjectResult :: CopyObjectResult
copyObjectResult = CopyObjectResult
    { _corETag         = Nothing
    , _corLastModified = Nothing
    }

corETag :: Lens' CopyObjectResult (Maybe Text)
corETag = lens _corETag (\s a -> s { _corETag = a })

corLastModified :: Lens' CopyObjectResult (Maybe UTCTime)
corLastModified = lens _corLastModified (\s a -> s { _corLastModified = a })
    . mapping _Time

instance FromXML CopyObjectResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyObjectResult"

instance ToXML CopyObjectResult where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CopyObjectResult"

data Delete = Delete
    { _dObjects :: [ObjectIdentifier]
    , _dQuiet   :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'Delete' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dObjects' @::@ ['ObjectIdentifier']
--
-- * 'dQuiet' @::@ 'Maybe' 'Bool'
--
delete' :: Delete
delete' = Delete
    { _dObjects = mempty
    , _dQuiet   = Nothing
    }

dObjects :: Lens' Delete [ObjectIdentifier]
dObjects = lens _dObjects (\s a -> s { _dObjects = a })

-- | Element to enable quiet mode for the request. When you add this element,
-- you must set its value to true.
dQuiet :: Lens' Delete (Maybe Bool)
dQuiet = lens _dQuiet (\s a -> s { _dQuiet = a })

instance FromXML Delete where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Delete"

instance ToXML Delete where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Delete"

newtype RestoreRequest = RestoreRequest
    { _rDays :: Int
    } deriving (Eq, Ord, Show, Generic, Enum, Num)

-- | 'RestoreRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rDays' @::@ 'Int'
--
restoreRequest :: Int -- ^ 'rDays'
               -> RestoreRequest
restoreRequest p1 = RestoreRequest
    { _rDays = p1
    }

-- | Lifetime of the active copy in days.
rDays :: Lens' RestoreRequest Int
rDays = lens _rDays (\s a -> s { _rDays = a })

instance FromXML RestoreRequest where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RestoreRequest"

instance ToXML RestoreRequest where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "RestoreRequest"
