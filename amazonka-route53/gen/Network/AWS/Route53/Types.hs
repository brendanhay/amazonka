{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Route 53 is a scalable Domain Name System (DNS) web service. It
-- provides secure and reliable routing to your infrastructure that uses
-- Amazon Web Services (AWS) products, such as Amazon Elastic Compute Cloud
-- (Amazon EC2), Elastic Load Balancing, or Amazon Simple Storage Service
-- (Amazon S3). You can also use Amazon Route 53 to route users to your
-- infrastructure outside of AWS.
module Network.AWS.Route53.Types
    (
    -- * Service
      Route53
    -- ** Errors
    , Route53Error (..)
    , _DelegationSetNotAvailable
    , _HealthCheckAlreadyExists
    , _HealthCheckInUse
    , _HealthCheckVersionMismatch
    , _HostedZoneAlreadyExists
    , _HostedZoneNotEmpty
    , _IncompatibleVersion
    , _InvalidChangeBatch
    , _InvalidDomainName
    , _InvalidInput
    , _NoSuchChange
    , _NoSuchGeoLocation
    , _NoSuchHealthCheck
    , _NoSuchHostedZone
    , _PriorRequestNotComplete
    , _Route53Client
    , _Route53Serializer
    , _Route53Service
    , _ThrottlingException
    , _TooManyHealthChecks
    , _TooManyHostedZones
    -- ** XML
    , xmlOptions

    -- * ChangeAction
    , ChangeAction (..)

    -- * ChangeStatus
    , ChangeStatus (..)

    -- * Failover
    , Failover (..)

    -- * HealthCheckType
    , HealthCheckType (..)

    -- * TagResourceType
    , TagResourceType (..)

    -- * DelegationSet
    , DelegationSet
    , delegationSet
    , dsNameServer

    -- * HostedZoneConfig
    , HostedZoneConfig
    , hostedZoneConfig
    , hzcComment

    -- * ResourceRecord
    , ResourceRecord
    , resourceRecord
    , rrValue

    -- * AliasTarget
    , AliasTarget
    , aliasTarget
    , atHostedZoneId
    , atDNSName
    , atEvaluateTargetHealth

    -- * Change
    , Change
    , change
    , cAction
    , cResourceRecordSet

    -- * ChangeBatch
    , ChangeBatch
    , changeBatch
    , cbComment
    , cbChange

    -- * ChangeInfo
    , ChangeInfo
    , changeInfo
    , ciId
    , ciStatus
    , ciSubmittedAt
    , ciComment

    -- * GeoLocation
    , GeoLocation
    , geoLocation
    , glContinentCode
    , glCountryCode
    , glSubdivisionCode

    -- * GeoLocationDetails
    , GeoLocationDetails
    , geoLocationDetails
    , gldContinentCode
    , gldContinentName
    , gldCountryCode
    , gldCountryName
    , gldSubdivisionCode
    , gldSubdivisionName

    -- * HealthCheck
    , HealthCheck
    , healthCheck
    , hcId
    , hcCallerReference
    , hcHealthCheckConfig
    , hcHealthCheckVersion

    -- * HealthCheckConfig
    , HealthCheckConfig
    , healthCheckConfig
    , hccIPAddress
    , hccPort
    , hccType
    , hccResourcePath
    , hccFullyQualifiedDomainName
    , hccSearchString
    , hccRequestInterval
    , hccFailureThreshold

    -- * HostedZone
    , HostedZone
    , hostedZone
    , hzId
    , hzName
    , hzCallerReference
    , hzConfig
    , hzResourceRecordSetCount

    -- * ResourceRecordSet
    , ResourceRecordSet
    , resourceRecordSet
    , rrsName
    , rrsType
    , rrsSetIdentifier
    , rrsWeight
    , rrsRegion
    , rrsGeoLocation
    , rrsFailover
    , rrsTTL
    , rrsResourceRecord
    , rrsAliasTarget
    , rrsHealthCheckId

    -- * ResourceTagSet
    , ResourceTagSet
    , resourceTagSet
    , rtsResourceType
    , rtsResourceId
    , rtsTag

    -- * Tag
    , Tag
    , tag
    , tKey
    , tValue

    -- * Common
    , module Network.AWS.Route53.Internal.Types
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V3
import Network.AWS.Types (Region)
import Network.AWS.Route53.Internal.Types

-- | Supported version (@2013-04-01@) of the
-- @Amazon Route 53@ service.
data Route53 deriving (Typeable)

instance AWSService Route53 where
    type Sg Route53 = V3
    type Er Route53 = Route53Error

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "route53"
        , _svcVersion  = "2013-04-01"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'Route53' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data Route53Error
      -- | Route 53 allows some duplicate domain names, but there is a
      -- maximum number of duplicate names. This error indicates that you
      -- have reached that maximum. If you want to create another hosted
      -- zone with the same name and Route 53 generates this error, you
      -- can request an increase to the limit on the Contact Us page.
    = DelegationSetNotAvailable
        { _dsnaMessage :: Maybe Text
        }
      -- | The health check you are trying to create already exists. Route
      -- 53 returns this error when a health check has already been
      -- created with the specified CallerReference.
    | HealthCheckAlreadyExists
        { _hcaeMessage :: Maybe Text
        }
      -- | There are resource records associated with this health check.
      -- Before you can delete the health check, you must disassociate it
      -- from the resource record sets.
    | HealthCheckInUse
        { _hciuMessage :: Maybe Text
        }
    | HealthCheckVersionMismatch
        { _hcvmMessage :: Maybe Text
        }
      -- | The hosted zone you are trying to create already exists. Route 53
      -- returns this error when a hosted zone has already been created
      -- with the specified CallerReference.
    | HostedZoneAlreadyExists
        { _hzaeMessage :: Maybe Text
        }
      -- | The hosted zone contains resource record sets in addition to the
      -- default NS and SOA resource record sets. Before you can delete
      -- the hosted zone, you must delete the additional resource record
      -- sets.
    | HostedZoneNotEmpty
        { _hzneMessage :: Maybe Text
        }
      -- | The resource you are trying to access is unsupported on this
      -- Route 53 endpoint. Please consider using a newer endpoint or a
      -- tool that does so.
    | IncompatibleVersion
        { _ivMessage :: Maybe Text
        }
      -- | This error contains a list of one or more error messages. Each
      -- error message indicates one error in the change batch. For more
      -- information, see Example InvalidChangeBatch Errors.
    | InvalidChangeBatch
        { _icbMessage :: [Text]
        }
      -- | This error indicates that the specified domain name is not valid.
    | InvalidDomainName
        { _idnMessage :: Maybe Text
        }
      -- | Some value specified in the request is invalid or the XML
      -- document is malformed.
    | InvalidInput
        { _iiMessage :: Maybe Text
        }
    | NoSuchChange
        { _nscMessage :: Maybe Text
        }
      -- | The geo location you are trying to get does not exist.
    | NoSuchGeoLocation
        { _nsglMessage :: Maybe Text
        }
      -- | The health check you are trying to get or delete does not exist.
    | NoSuchHealthCheck
        { _nshcMessage :: Maybe Text
        }
    | NoSuchHostedZone
        { _nshzMessage :: Maybe Text
        }
      -- | The request was rejected because Route 53 was still processing a
      -- prior request.
    | PriorRequestNotComplete
        { _prncMessage :: Maybe Text
        }
    | Route53Client HttpException
    | Route53Serializer String
    | Route53Service String
    | ThrottlingException
        { _teMessage :: Maybe Text
        }
    | TooManyHealthChecks
        { _tmhcMessage :: Maybe Text
        }
      -- | This error indicates that you've reached the maximum number of
      -- hosted zones that can be created for the current AWS account. You
      -- can request an increase to the limit on the Contact Us page.
    | TooManyHostedZones
        { _tmhzMessage :: Maybe Text
        }
      deriving (Show, Typeable, Generic)

instance AWSError Route53Error where
    awsError = const "Route53Error"

instance AWSServiceError Route53Error where
    serviceError    = Route53Service
    clientError     = Route53Client
    serializerError = Route53Serializer

instance Exception Route53Error

-- | Route 53 allows some duplicate domain names, but there is a maximum number
-- of duplicate names. This error indicates that you have reached that
-- maximum. If you want to create another hosted zone with the same name and
-- Route 53 generates this error, you can request an increase to the limit on
-- the Contact Us page.
--
-- See: 'DelegationSetNotAvailable'
_DelegationSetNotAvailable :: Prism' Route53Error (Maybe Text)
_DelegationSetNotAvailable = prism
    DelegationSetNotAvailable
    (\case
        DelegationSetNotAvailable p1 -> Right p1
        x -> Left x)

-- | The health check you are trying to create already exists. Route 53 returns
-- this error when a health check has already been created with the specified
-- CallerReference.
--
-- See: 'HealthCheckAlreadyExists'
_HealthCheckAlreadyExists :: Prism' Route53Error (Maybe Text)
_HealthCheckAlreadyExists = prism
    HealthCheckAlreadyExists
    (\case
        HealthCheckAlreadyExists p1 -> Right p1
        x -> Left x)

-- | There are resource records associated with this health check. Before you
-- can delete the health check, you must disassociate it from the resource
-- record sets.
--
-- See: 'HealthCheckInUse'
_HealthCheckInUse :: Prism' Route53Error (Maybe Text)
_HealthCheckInUse = prism
    HealthCheckInUse
    (\case
        HealthCheckInUse p1 -> Right p1
        x -> Left x)

-- | See: 'HealthCheckVersionMismatch'
_HealthCheckVersionMismatch :: Prism' Route53Error (Maybe Text)
_HealthCheckVersionMismatch = prism
    HealthCheckVersionMismatch
    (\case
        HealthCheckVersionMismatch p1 -> Right p1
        x -> Left x)

-- | The hosted zone you are trying to create already exists. Route 53 returns
-- this error when a hosted zone has already been created with the specified
-- CallerReference.
--
-- See: 'HostedZoneAlreadyExists'
_HostedZoneAlreadyExists :: Prism' Route53Error (Maybe Text)
_HostedZoneAlreadyExists = prism
    HostedZoneAlreadyExists
    (\case
        HostedZoneAlreadyExists p1 -> Right p1
        x -> Left x)

-- | The hosted zone contains resource record sets in addition to the default NS
-- and SOA resource record sets. Before you can delete the hosted zone, you
-- must delete the additional resource record sets.
--
-- See: 'HostedZoneNotEmpty'
_HostedZoneNotEmpty :: Prism' Route53Error (Maybe Text)
_HostedZoneNotEmpty = prism
    HostedZoneNotEmpty
    (\case
        HostedZoneNotEmpty p1 -> Right p1
        x -> Left x)

-- | The resource you are trying to access is unsupported on this Route 53
-- endpoint. Please consider using a newer endpoint or a tool that does so.
--
-- See: 'IncompatibleVersion'
_IncompatibleVersion :: Prism' Route53Error (Maybe Text)
_IncompatibleVersion = prism
    IncompatibleVersion
    (\case
        IncompatibleVersion p1 -> Right p1
        x -> Left x)

-- | This error contains a list of one or more error messages. Each error
-- message indicates one error in the change batch. For more information, see
-- Example InvalidChangeBatch Errors.
--
-- See: 'InvalidChangeBatch'
_InvalidChangeBatch :: Prism' Route53Error [Text]
_InvalidChangeBatch = prism
    InvalidChangeBatch
    (\case
        InvalidChangeBatch p1 -> Right p1
        x -> Left x)

-- | This error indicates that the specified domain name is not valid.
--
-- See: 'InvalidDomainName'
_InvalidDomainName :: Prism' Route53Error (Maybe Text)
_InvalidDomainName = prism
    InvalidDomainName
    (\case
        InvalidDomainName p1 -> Right p1
        x -> Left x)

-- | Some value specified in the request is invalid or the XML document is
-- malformed.
--
-- See: 'InvalidInput'
_InvalidInput :: Prism' Route53Error (Maybe Text)
_InvalidInput = prism
    InvalidInput
    (\case
        InvalidInput p1 -> Right p1
        x -> Left x)

-- | See: 'NoSuchChange'
_NoSuchChange :: Prism' Route53Error (Maybe Text)
_NoSuchChange = prism
    NoSuchChange
    (\case
        NoSuchChange p1 -> Right p1
        x -> Left x)

-- | The geo location you are trying to get does not exist.
--
-- See: 'NoSuchGeoLocation'
_NoSuchGeoLocation :: Prism' Route53Error (Maybe Text)
_NoSuchGeoLocation = prism
    NoSuchGeoLocation
    (\case
        NoSuchGeoLocation p1 -> Right p1
        x -> Left x)

-- | The health check you are trying to get or delete does not exist.
--
-- See: 'NoSuchHealthCheck'
_NoSuchHealthCheck :: Prism' Route53Error (Maybe Text)
_NoSuchHealthCheck = prism
    NoSuchHealthCheck
    (\case
        NoSuchHealthCheck p1 -> Right p1
        x -> Left x)

-- | See: 'NoSuchHostedZone'
_NoSuchHostedZone :: Prism' Route53Error (Maybe Text)
_NoSuchHostedZone = prism
    NoSuchHostedZone
    (\case
        NoSuchHostedZone p1 -> Right p1
        x -> Left x)

-- | The request was rejected because Route 53 was still processing a prior
-- request.
--
-- See: 'PriorRequestNotComplete'
_PriorRequestNotComplete :: Prism' Route53Error (Maybe Text)
_PriorRequestNotComplete = prism
    PriorRequestNotComplete
    (\case
        PriorRequestNotComplete p1 -> Right p1
        x -> Left x)

-- | See: 'Route53Client'
_Route53Client :: Prism' Route53Error HttpException
_Route53Client = prism
    Route53Client
    (\case
        Route53Client p1 -> Right p1
        x -> Left x)

-- | See: 'Route53Serializer'
_Route53Serializer :: Prism' Route53Error String
_Route53Serializer = prism
    Route53Serializer
    (\case
        Route53Serializer p1 -> Right p1
        x -> Left x)

-- | See: 'Route53Service'
_Route53Service :: Prism' Route53Error String
_Route53Service = prism
    Route53Service
    (\case
        Route53Service p1 -> Right p1
        x -> Left x)

-- | See: 'ThrottlingException'
_ThrottlingException :: Prism' Route53Error (Maybe Text)
_ThrottlingException = prism
    ThrottlingException
    (\case
        ThrottlingException p1 -> Right p1
        x -> Left x)

-- | See: 'TooManyHealthChecks'
_TooManyHealthChecks :: Prism' Route53Error (Maybe Text)
_TooManyHealthChecks = prism
    TooManyHealthChecks
    (\case
        TooManyHealthChecks p1 -> Right p1
        x -> Left x)

-- | This error indicates that you've reached the maximum number of hosted zones
-- that can be created for the current AWS account. You can request an
-- increase to the limit on the Contact Us page.
--
-- See: 'TooManyHostedZones'
_TooManyHostedZones :: Prism' Route53Error (Maybe Text)
_TooManyHostedZones = prism
    TooManyHostedZones
    (\case
        TooManyHostedZones p1 -> Right p1
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

data ChangeAction
    = Create -- ^ CREATE
    | Delete -- ^ DELETE
    | Upsert -- ^ UPSERT
      deriving (Eq, Ord, Show, Generic)

instance Hashable ChangeAction

instance FromText ChangeAction where
    parser = match "CREATE" Create
         <|> match "DELETE" Delete
         <|> match "UPSERT" Upsert

instance ToText ChangeAction where
    toText Create = "CREATE"
    toText Delete = "DELETE"
    toText Upsert = "UPSERT"

instance ToByteString ChangeAction

instance FromXML ChangeAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeAction"

instance ToXML ChangeAction where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ChangeAction"

instance ToQuery ChangeAction where
      toQuery = toQuery . toBS

data ChangeStatus
    = Insync -- ^ INSYNC
    | Pending -- ^ PENDING
      deriving (Eq, Ord, Show, Generic)

instance Hashable ChangeStatus

instance FromText ChangeStatus where
    parser = match "INSYNC" Insync
         <|> match "PENDING" Pending

instance ToText ChangeStatus where
    toText Insync = "INSYNC"
    toText Pending = "PENDING"

instance ToByteString ChangeStatus

instance FromXML ChangeStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeStatus"

instance ToQuery ChangeStatus where
      toQuery = toQuery . toBS

data Failover
    = Primary -- ^ PRIMARY
    | Secondary -- ^ SECONDARY
      deriving (Eq, Ord, Show, Generic)

instance Hashable Failover

instance FromText Failover where
    parser = match "PRIMARY" Primary
         <|> match "SECONDARY" Secondary

instance ToText Failover where
    toText Primary = "PRIMARY"
    toText Secondary = "SECONDARY"

instance ToByteString Failover

instance FromXML Failover where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Failover"

instance ToXML Failover where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Failover"

instance ToQuery Failover where
      toQuery = toQuery . toBS

data HealthCheckType
    = Http -- ^ HTTP
    | HttpStrMatch -- ^ HTTP_STR_MATCH
    | Https -- ^ HTTPS
    | HttpsStrMatch -- ^ HTTPS_STR_MATCH
    | Tcp -- ^ TCP
      deriving (Eq, Ord, Show, Generic)

instance Hashable HealthCheckType

instance FromText HealthCheckType where
    parser = match "HTTP" Http
         <|> match "HTTP_STR_MATCH" HttpStrMatch
         <|> match "HTTPS" Https
         <|> match "HTTPS_STR_MATCH" HttpsStrMatch
         <|> match "TCP" Tcp

instance ToText HealthCheckType where
    toText Http = "HTTP"
    toText HttpStrMatch = "HTTP_STR_MATCH"
    toText Https = "HTTPS"
    toText HttpsStrMatch = "HTTPS_STR_MATCH"
    toText Tcp = "TCP"

instance ToByteString HealthCheckType

instance FromXML HealthCheckType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheckType"

instance ToXML HealthCheckType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "HealthCheckType"

instance ToQuery HealthCheckType where
      toQuery = toQuery . toBS

data TagResourceType
    = Healthcheck -- ^ healthcheck
      deriving (Eq, Ord, Show, Generic)

instance Hashable TagResourceType

instance FromText TagResourceType where
    parser = match "healthcheck" Healthcheck

instance ToText TagResourceType where
    toText Healthcheck = "healthcheck"

instance ToByteString TagResourceType

instance FromXML TagResourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagResourceType"

instance ToXML TagResourceType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TagResourceType"

instance ToQuery TagResourceType where
      toQuery = toQuery . toBS

-- | A complex type that contains information about the name servers for the
-- specified hosted zone.
newtype DelegationSet = DelegationSet
    { _dsNameServer :: List1 Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DelegationSet' data type.
--
-- 'DelegationSet' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NameServer ::@ @List1 Text@
--
delegationSet :: List1 Text -- ^ 'dsNameServer'
              -> DelegationSet
delegationSet p1 = DelegationSet
    { _dsNameServer = p1
    }

-- | A complex type that contains the authoritative name servers for the hosted
-- zone. Use the method provided by your domain registrar to add an NS record
-- to your domain for each NameServer that is assigned to your hosted zone.
dsNameServer :: Lens' DelegationSet (List1 Text)
dsNameServer = lens _dsNameServer (\s a -> s { _dsNameServer = a })

instance FromXML DelegationSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DelegationSet"

-- | A complex type that contains an optional comment about your hosted zone.
newtype HostedZoneConfig = HostedZoneConfig
    { _hzcComment :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HostedZoneConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Comment ::@ @Maybe Text@
--
hostedZoneConfig :: HostedZoneConfig
hostedZoneConfig = HostedZoneConfig
    { _hzcComment = Nothing
    }

-- | An optional comment about your hosted zone. If you don't want to specify a
-- comment, you can omit the HostedZoneConfig and Comment elements from the
-- XML document.
hzcComment :: Lens' HostedZoneConfig (Maybe Text)
hzcComment = lens _hzcComment (\s a -> s { _hzcComment = a })

instance FromXML HostedZoneConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HostedZoneConfig"

instance ToXML HostedZoneConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "HostedZoneConfig"

-- | A complex type that contains the value of the Value element for the current
-- resource record set.
newtype ResourceRecord = ResourceRecord
    { _rrValue :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ResourceRecord' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Value ::@ @Text@
--
resourceRecord :: Text -- ^ 'rrValue'
               -> ResourceRecord
resourceRecord p1 = ResourceRecord
    { _rrValue = p1
    }

-- | The value of the Value element for the current resource record set.
rrValue :: Lens' ResourceRecord Text
rrValue = lens _rrValue (\s a -> s { _rrValue = a })

instance FromXML ResourceRecord where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceRecord"

instance ToXML ResourceRecord where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ResourceRecord"

-- | Alias resource record sets only: Information about the AWS resource to
-- which you are redirecting traffic.
data AliasTarget = AliasTarget
    { _atHostedZoneId :: ResourceId
    , _atDNSName :: Text
    , _atEvaluateTargetHealth :: !Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AliasTarget' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HostedZoneId ::@ @ResourceId@
--
-- * @DNSName ::@ @Text@
--
-- * @EvaluateTargetHealth ::@ @Bool@
--
aliasTarget :: ResourceId -- ^ 'atHostedZoneId'
            -> Text -- ^ 'atDNSName'
            -> Bool -- ^ 'atEvaluateTargetHealth'
            -> AliasTarget
aliasTarget p1 p2 p3 = AliasTarget
    { _atHostedZoneId = p1
    , _atDNSName = p2
    , _atEvaluateTargetHealth = p3
    }

-- | Alias resource record sets only: The value of the hosted zone ID for the
-- AWS resource. For more information and an example, see Creating Alias
-- Resource Record Sets in the Amazon Route 53 Developer Guide.
atHostedZoneId :: Lens' AliasTarget ResourceId
atHostedZoneId = lens _atHostedZoneId (\s a -> s { _atHostedZoneId = a })

-- | Alias resource record sets only: The external DNS name associated with the
-- AWS Resource. For more information and an example, see Creating Alias
-- Resource Record Sets in the Amazon Route 53 Developer Guide.
atDNSName :: Lens' AliasTarget Text
atDNSName = lens _atDNSName (\s a -> s { _atDNSName = a })

-- | Alias resource record sets only: A boolean value that indicates whether
-- this Resource Record Set should respect the health status of any health
-- checks associated with the ALIAS target record which it is linked to. For
-- more information and an example, see Creating Alias Resource Record Sets in
-- the Amazon Route 53 Developer Guide.
atEvaluateTargetHealth :: Lens' AliasTarget Bool
atEvaluateTargetHealth =
    lens _atEvaluateTargetHealth (\s a -> s { _atEvaluateTargetHealth = a })

instance FromXML AliasTarget where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AliasTarget"

instance ToXML AliasTarget where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "AliasTarget"

-- | A complex type that contains the information for each change in a change
-- batch request.
data Change = Change
    { _cAction :: ChangeAction
    , _cResourceRecordSet :: ResourceRecordSet
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Change' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Action ::@ @ChangeAction@
--
-- * @ResourceRecordSet ::@ @ResourceRecordSet@
--
change :: ChangeAction -- ^ 'cAction'
       -> ResourceRecordSet -- ^ 'cResourceRecordSet'
       -> Change
change p1 p2 = Change
    { _cAction = p1
    , _cResourceRecordSet = p2
    }

-- | The action to perform. Valid values: CREATE | DELETE | UPSERT.
cAction :: Lens' Change ChangeAction
cAction = lens _cAction (\s a -> s { _cAction = a })

-- | Information about the resource record set to create or delete.
cResourceRecordSet :: Lens' Change ResourceRecordSet
cResourceRecordSet =
    lens _cResourceRecordSet (\s a -> s { _cResourceRecordSet = a })

instance ToXML Change where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Change"

-- | A complex type that contains an optional comment and the Changes element.
data ChangeBatch = ChangeBatch
    { _cbComment :: Maybe Text
    , _cbChange :: List1 Change
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChangeBatch' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Comment ::@ @Maybe Text@
--
-- * @Change ::@ @List1 Change@
--
changeBatch :: List1 Change -- ^ 'cbChange'
            -> ChangeBatch
changeBatch p2 = ChangeBatch
    { _cbComment = Nothing
    , _cbChange = p2
    }

-- | Optional: Any comments you want to include about a change batch request.
cbComment :: Lens' ChangeBatch (Maybe Text)
cbComment = lens _cbComment (\s a -> s { _cbComment = a })

-- | A complex type that contains one Change element for each resource record
-- set that you want to create or delete.
cbChange :: Lens' ChangeBatch (List1 Change)
cbChange = lens _cbChange (\s a -> s { _cbChange = a })

instance ToXML ChangeBatch where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ChangeBatch"

-- | A complex type that contains information about changes made to your hosted
-- zone. This element contains an ID that you use when performing a GetChange
-- action to get detailed information about the change.
data ChangeInfo = ChangeInfo
    { _ciId :: ResourceId
    , _ciStatus :: ChangeStatus
    , _ciSubmittedAt :: ISO8601
    , _ciComment :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChangeInfo' data type.
--
-- 'ChangeInfo' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @ResourceId@
--
-- * @Status ::@ @ChangeStatus@
--
-- * @SubmittedAt ::@ @ISO8601@
--
-- * @Comment ::@ @Maybe Text@
--
changeInfo :: ResourceId -- ^ 'ciId'
           -> ChangeStatus -- ^ 'ciStatus'
           -> ISO8601 -- ^ 'ciSubmittedAt'
           -> ChangeInfo
changeInfo p1 p2 p3 = ChangeInfo
    { _ciId = p1
    , _ciStatus = p2
    , _ciSubmittedAt = p3
    , _ciComment = Nothing
    }

-- | The ID of the request. Use this ID to track when the change has completed
-- across all Amazon Route 53 DNS servers.
ciId :: Lens' ChangeInfo ResourceId
ciId = lens _ciId (\s a -> s { _ciId = a })

-- | The current state of the request. PENDING indicates that this request has
-- not yet been applied to all Amazon Route 53 DNS servers. Valid Values:
-- PENDING | INSYNC.
ciStatus :: Lens' ChangeInfo ChangeStatus
ciStatus = lens _ciStatus (\s a -> s { _ciStatus = a })

-- | The date and time the change was submitted, in the format
-- YYYY-MM-DDThh:mm:ssZ, as specified in the ISO 8601 standard (for example,
-- 2009-11-19T19:37:58Z). The Z after the time indicates that the time is
-- listed in Coordinated Universal Time (UTC), which is synonymous with
-- Greenwich Mean Time in this context.
ciSubmittedAt :: Lens' ChangeInfo ISO8601
ciSubmittedAt = lens _ciSubmittedAt (\s a -> s { _ciSubmittedAt = a })

-- | A complex type that describes change information about changes made to your
-- hosted zone. This element contains an ID that you use when performing a
-- GetChange action to get detailed information about the change.
ciComment :: Lens' ChangeInfo (Maybe Text)
ciComment = lens _ciComment (\s a -> s { _ciComment = a })

instance FromXML ChangeInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeInfo"

-- | Geo location resource record sets only: Among resource record sets that
-- have the same combination of DNS name and type, a value that specifies the
-- geo location for the current resource record set.
data GeoLocation = GeoLocation
    { _glContinentCode :: Maybe Text
    , _glCountryCode :: Maybe Text
    , _glSubdivisionCode :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GeoLocation' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ContinentCode ::@ @Maybe Text@
--
-- * @CountryCode ::@ @Maybe Text@
--
-- * @SubdivisionCode ::@ @Maybe Text@
--
geoLocation :: GeoLocation
geoLocation = GeoLocation
    { _glContinentCode = Nothing
    , _glCountryCode = Nothing
    , _glSubdivisionCode = Nothing
    }

-- | The code for a continent geo location. Note: only continent locations have
-- a continent code. Valid values: AF | AN | AS | EU | OC | NA | SA
-- Constraint: Specifying ContinentCode with either CountryCode or
-- SubdivisionCode returns an InvalidInput error.
glContinentCode :: Lens' GeoLocation (Maybe Text)
glContinentCode = lens _glContinentCode (\s a -> s { _glContinentCode = a })

-- | The code for a country geo location. The default location uses '*' for the
-- country code and will match all locations that are not matched by a geo
-- location. The default geo location uses a * for the country code. All other
-- country codes follow the ISO 3166 two-character code.
glCountryCode :: Lens' GeoLocation (Maybe Text)
glCountryCode = lens _glCountryCode (\s a -> s { _glCountryCode = a })

-- | The code for a country's subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
-- Constraint: Specifying SubdivisionCode without CountryCode returns an
-- InvalidInput error.
glSubdivisionCode :: Lens' GeoLocation (Maybe Text)
glSubdivisionCode =
    lens _glSubdivisionCode (\s a -> s { _glSubdivisionCode = a })

instance FromXML GeoLocation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GeoLocation"

instance ToXML GeoLocation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GeoLocation"

-- | A complex type that contains the information about the specified geo
-- location.
data GeoLocationDetails = GeoLocationDetails
    { _gldContinentCode :: Maybe Text
    , _gldContinentName :: Maybe Text
    , _gldCountryCode :: Maybe Text
    , _gldCountryName :: Maybe Text
    , _gldSubdivisionCode :: Maybe Text
    , _gldSubdivisionName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GeoLocationDetails' data type.
--
-- 'GeoLocationDetails' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ContinentCode ::@ @Maybe Text@
--
-- * @ContinentName ::@ @Maybe Text@
--
-- * @CountryCode ::@ @Maybe Text@
--
-- * @CountryName ::@ @Maybe Text@
--
-- * @SubdivisionCode ::@ @Maybe Text@
--
-- * @SubdivisionName ::@ @Maybe Text@
--
geoLocationDetails :: GeoLocationDetails
geoLocationDetails = GeoLocationDetails
    { _gldContinentCode = Nothing
    , _gldContinentName = Nothing
    , _gldCountryCode = Nothing
    , _gldCountryName = Nothing
    , _gldSubdivisionCode = Nothing
    , _gldSubdivisionName = Nothing
    }

-- | The code for a continent geo location. Note: only continent locations have
-- a continent code.
gldContinentCode :: Lens' GeoLocationDetails (Maybe Text)
gldContinentCode =
    lens _gldContinentCode (\s a -> s { _gldContinentCode = a })

-- | The name of the continent. This element is only present if ContinentCode is
-- also present.
gldContinentName :: Lens' GeoLocationDetails (Maybe Text)
gldContinentName =
    lens _gldContinentName (\s a -> s { _gldContinentName = a })

-- | The code for a country geo location. The default location uses '*' for the
-- country code and will match all locations that are not matched by a geo
-- location. The default geo location uses a * for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gldCountryCode :: Lens' GeoLocationDetails (Maybe Text)
gldCountryCode = lens _gldCountryCode (\s a -> s { _gldCountryCode = a })

-- | The name of the country. This element is only present if CountryCode is
-- also present.
gldCountryName :: Lens' GeoLocationDetails (Maybe Text)
gldCountryName = lens _gldCountryName (\s a -> s { _gldCountryName = a })

-- | The code for a country's subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
gldSubdivisionCode :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionCode =
    lens _gldSubdivisionCode (\s a -> s { _gldSubdivisionCode = a })

-- | The name of the subdivision. This element is only present if
-- SubdivisionCode is also present.
gldSubdivisionName :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionName =
    lens _gldSubdivisionName (\s a -> s { _gldSubdivisionName = a })

instance FromXML GeoLocationDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GeoLocationDetails"

-- | A complex type that contains the information about the specified health
-- check.
data HealthCheck = HealthCheck
    { _hcId :: Text
    , _hcCallerReference :: Text
    , _hcHealthCheckConfig :: HealthCheckConfig
    , _hcHealthCheckVersion :: !Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HealthCheck' data type.
--
-- 'HealthCheck' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @CallerReference ::@ @Text@
--
-- * @HealthCheckConfig ::@ @HealthCheckConfig@
--
-- * @HealthCheckVersion ::@ @Integer@
--
healthCheck :: Text -- ^ 'hcId'
            -> Text -- ^ 'hcCallerReference'
            -> HealthCheckConfig -- ^ 'hcHealthCheckConfig'
            -> Integer -- ^ 'hcHealthCheckVersion'
            -> HealthCheck
healthCheck p1 p2 p3 p4 = HealthCheck
    { _hcId = p1
    , _hcCallerReference = p2
    , _hcHealthCheckConfig = p3
    , _hcHealthCheckVersion = p4
    }

-- | The ID of the specified health check.
hcId :: Lens' HealthCheck Text
hcId = lens _hcId (\s a -> s { _hcId = a })

-- | A unique string that identifies the request to create the health check.
hcCallerReference :: Lens' HealthCheck Text
hcCallerReference =
    lens _hcCallerReference (\s a -> s { _hcCallerReference = a })

-- | A complex type that contains the health check configuration.
hcHealthCheckConfig :: Lens' HealthCheck HealthCheckConfig
hcHealthCheckConfig =
    lens _hcHealthCheckConfig (\s a -> s { _hcHealthCheckConfig = a })

-- | The version of the health check. You can optionally pass this value in a
-- call to UpdateHealthCheck to prevent overwriting another change to the
-- health check.
hcHealthCheckVersion :: Lens' HealthCheck Integer
hcHealthCheckVersion =
    lens _hcHealthCheckVersion (\s a -> s { _hcHealthCheckVersion = a })

instance FromXML HealthCheck where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheck"

-- | A complex type that contains the health check configuration.
data HealthCheckConfig = HealthCheckConfig
    { _hccIPAddress :: Maybe Text
    , _hccPort :: Maybe Integer
    , _hccType :: HealthCheckType
    , _hccResourcePath :: Maybe Text
    , _hccFullyQualifiedDomainName :: Maybe Text
    , _hccSearchString :: Maybe Text
    , _hccRequestInterval :: Maybe Integer
    , _hccFailureThreshold :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HealthCheckConfig' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IPAddress ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @Type ::@ @HealthCheckType@
--
-- * @ResourcePath ::@ @Maybe Text@
--
-- * @FullyQualifiedDomainName ::@ @Maybe Text@
--
-- * @SearchString ::@ @Maybe Text@
--
-- * @RequestInterval ::@ @Maybe Integer@
--
-- * @FailureThreshold ::@ @Maybe Integer@
--
healthCheckConfig :: HealthCheckType -- ^ 'hccType'
                  -> HealthCheckConfig
healthCheckConfig p3 = HealthCheckConfig
    { _hccIPAddress = Nothing
    , _hccPort = Nothing
    , _hccType = p3
    , _hccResourcePath = Nothing
    , _hccFullyQualifiedDomainName = Nothing
    , _hccSearchString = Nothing
    , _hccRequestInterval = Nothing
    , _hccFailureThreshold = Nothing
    }

-- | IP Address of the instance being checked.
hccIPAddress :: Lens' HealthCheckConfig (Maybe Text)
hccIPAddress = lens _hccIPAddress (\s a -> s { _hccIPAddress = a })

-- | Port on which connection will be opened to the instance to health check.
-- For HTTP and HTTP_STR_MATCH this defaults to 80 if the port is not
-- specified. For HTTPS and HTTPS_STR_MATCH this defaults to 443 if the port
-- is not specified.
hccPort :: Lens' HealthCheckConfig (Maybe Integer)
hccPort = lens _hccPort (\s a -> s { _hccPort = a })

-- | The type of health check to be performed. Currently supported types are
-- TCP, HTTP, HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH.
hccType :: Lens' HealthCheckConfig HealthCheckType
hccType = lens _hccType (\s a -> s { _hccType = a })

-- | Path to ping on the instance to check the health. Required for HTTP, HTTPS,
-- HTTP_STR_MATCH, and HTTPS_STR_MATCH health checks, HTTP request is issued
-- to the instance on the given port and path.
hccResourcePath :: Lens' HealthCheckConfig (Maybe Text)
hccResourcePath = lens _hccResourcePath (\s a -> s { _hccResourcePath = a })

-- | Fully qualified domain name of the instance to be health checked.
hccFullyQualifiedDomainName :: Lens' HealthCheckConfig (Maybe Text)
hccFullyQualifiedDomainName =
    lens _hccFullyQualifiedDomainName
         (\s a -> s { _hccFullyQualifiedDomainName = a })

-- | A string to search for in the body of a health check response. Required for
-- HTTP_STR_MATCH and HTTPS_STR_MATCH health checks.
hccSearchString :: Lens' HealthCheckConfig (Maybe Text)
hccSearchString = lens _hccSearchString (\s a -> s { _hccSearchString = a })

-- | The number of seconds between the time that Route 53 gets a response from
-- your endpoint and the time that it sends the next health-check request.
-- Each Route 53 health checker makes requests at this interval. Valid values
-- are 10 and 30. The default value is 30.
hccRequestInterval :: Lens' HealthCheckConfig (Maybe Integer)
hccRequestInterval =
    lens _hccRequestInterval (\s a -> s { _hccRequestInterval = a })

-- | The number of consecutive health checks that an endpoint must pass or fail
-- for Route 53 to change the current status of the endpoint from unhealthy to
-- healthy or vice versa. Valid values are integers between 1 and 10. For more
-- information, see "How Amazon Route 53 Determines Whether an Endpoint Is
-- Healthy" in the Amazon Route 53 Developer Guide.
hccFailureThreshold :: Lens' HealthCheckConfig (Maybe Integer)
hccFailureThreshold =
    lens _hccFailureThreshold (\s a -> s { _hccFailureThreshold = a })

instance FromXML HealthCheckConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheckConfig"

instance ToXML HealthCheckConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "HealthCheckConfig"

-- | A complex type that contains the information about the specified hosted
-- zone.
data HostedZone = HostedZone
    { _hzId :: ResourceId
    , _hzName :: Text
    , _hzCallerReference :: Text
    , _hzConfig :: Maybe HostedZoneConfig
    , _hzResourceRecordSetCount :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HostedZone' data type.
--
-- 'HostedZone' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @ResourceId@
--
-- * @Name ::@ @Text@
--
-- * @CallerReference ::@ @Text@
--
-- * @Config ::@ @Maybe HostedZoneConfig@
--
-- * @ResourceRecordSetCount ::@ @Maybe Integer@
--
hostedZone :: ResourceId -- ^ 'hzId'
           -> Text -- ^ 'hzName'
           -> Text -- ^ 'hzCallerReference'
           -> HostedZone
hostedZone p1 p2 p3 = HostedZone
    { _hzId = p1
    , _hzName = p2
    , _hzCallerReference = p3
    , _hzConfig = Nothing
    , _hzResourceRecordSetCount = Nothing
    }

-- | The ID of the specified hosted zone.
hzId :: Lens' HostedZone ResourceId
hzId = lens _hzId (\s a -> s { _hzId = a })

-- | The name of the domain. This must be a fully-specified domain, for example,
-- www.example.com. The trailing dot is optional; Route 53 assumes that the
-- domain name is fully qualified. This means that Route 53 treats
-- www.example.com (without a trailing dot) and www.example.com. (with a
-- trailing dot) as identical. This is the name you have registered with your
-- DNS registrar. You should ask your registrar to change the authoritative
-- name servers for your domain to the set of NameServers elements returned in
-- DelegationSet.
hzName :: Lens' HostedZone Text
hzName = lens _hzName (\s a -> s { _hzName = a })

-- | A unique string that identifies the request to create the hosted zone.
hzCallerReference :: Lens' HostedZone Text
hzCallerReference =
    lens _hzCallerReference (\s a -> s { _hzCallerReference = a })

-- | A complex type that contains the Comment element.
hzConfig :: Lens' HostedZone (Maybe HostedZoneConfig)
hzConfig = lens _hzConfig (\s a -> s { _hzConfig = a })

-- | Total number of resource record sets in the hosted zone.
hzResourceRecordSetCount :: Lens' HostedZone (Maybe Integer)
hzResourceRecordSetCount =
    lens _hzResourceRecordSetCount
         (\s a -> s { _hzResourceRecordSetCount = a })

instance FromXML HostedZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HostedZone"

-- | A complex type that contains information about the current resource record
-- set.
data ResourceRecordSet = ResourceRecordSet
    { _rrsName :: Text
    , _rrsType :: RecordType
    , _rrsSetIdentifier :: Maybe Text
    , _rrsWeight :: Maybe Integer
    , _rrsRegion :: Maybe Region
    , _rrsGeoLocation :: Maybe GeoLocation
    , _rrsFailover :: Maybe Failover
    , _rrsTTL :: Maybe Integer
    , _rrsResourceRecord :: List1 ResourceRecord
    , _rrsAliasTarget :: Maybe AliasTarget
    , _rrsHealthCheckId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ResourceRecordSet' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @Type ::@ @RecordType@
--
-- * @SetIdentifier ::@ @Maybe Text@
--
-- * @Weight ::@ @Maybe Integer@
--
-- * @Region ::@ @Maybe Region@
--
-- * @GeoLocation ::@ @Maybe GeoLocation@
--
-- * @Failover ::@ @Maybe Failover@
--
-- * @TTL ::@ @Maybe Integer@
--
-- * @ResourceRecord ::@ @List1 ResourceRecord@
--
-- * @AliasTarget ::@ @Maybe AliasTarget@
--
-- * @HealthCheckId ::@ @Maybe Text@
--
resourceRecordSet :: Text -- ^ 'rrsName'
                  -> RecordType -- ^ 'rrsType'
                  -> List1 ResourceRecord -- ^ 'rrsResourceRecord'
                  -> ResourceRecordSet
resourceRecordSet p1 p2 p9 = ResourceRecordSet
    { _rrsName = p1
    , _rrsType = p2
    , _rrsSetIdentifier = Nothing
    , _rrsWeight = Nothing
    , _rrsRegion = Nothing
    , _rrsGeoLocation = Nothing
    , _rrsFailover = Nothing
    , _rrsTTL = Nothing
    , _rrsResourceRecord = p9
    , _rrsAliasTarget = Nothing
    , _rrsHealthCheckId = Nothing
    }

-- | The domain name of the current resource record set.
rrsName :: Lens' ResourceRecordSet Text
rrsName = lens _rrsName (\s a -> s { _rrsName = a })

-- | The type of the current resource record set.
rrsType :: Lens' ResourceRecordSet RecordType
rrsType = lens _rrsType (\s a -> s { _rrsType = a })

-- | Weighted, Latency, Geo, and Failover resource record sets only: An
-- identifier that differentiates among multiple resource record sets that
-- have the same combination of DNS name and type.
rrsSetIdentifier :: Lens' ResourceRecordSet (Maybe Text)
rrsSetIdentifier =
    lens _rrsSetIdentifier (\s a -> s { _rrsSetIdentifier = a })

-- | Weighted resource record sets only: Among resource record sets that have
-- the same combination of DNS name and type, a value that determines what
-- portion of traffic for the current resource record set is routed to the
-- associated location.
rrsWeight :: Lens' ResourceRecordSet (Maybe Integer)
rrsWeight = lens _rrsWeight (\s a -> s { _rrsWeight = a })

-- | Latency-based resource record sets only: Among resource record sets that
-- have the same combination of DNS name and type, a value that specifies the
-- AWS region for the current resource record set.
rrsRegion :: Lens' ResourceRecordSet (Maybe Region)
rrsRegion = lens _rrsRegion (\s a -> s { _rrsRegion = a })

-- | Geo location resource record sets only: Among resource record sets that
-- have the same combination of DNS name and type, a value that specifies the
-- geo location for the current resource record set.
rrsGeoLocation :: Lens' ResourceRecordSet (Maybe GeoLocation)
rrsGeoLocation = lens _rrsGeoLocation (\s a -> s { _rrsGeoLocation = a })

-- | Failover resource record sets only: Among resource record sets that have
-- the same combination of DNS name and type, a value that indicates whether
-- the current resource record set is a primary or secondary resource record
-- set. A failover set may contain at most one resource record set marked as
-- primary and one resource record set marked as secondary. A resource record
-- set marked as primary will be returned if any of the following are true:
-- (1) an associated health check is passing, (2) if the resource record set
-- is an alias with the evaluate target health and at least one target
-- resource record set is healthy, (3) both the primary and secondary resource
-- record set are failing health checks or (4) there is no secondary resource
-- record set. A secondary resource record set will be returned if: (1) the
-- primary is failing a health check and either the secondary is passing a
-- health check or has no associated health check, or (2) there is no primary
-- resource record set. Valid values: PRIMARY | SECONDARY.
rrsFailover :: Lens' ResourceRecordSet (Maybe Failover)
rrsFailover = lens _rrsFailover (\s a -> s { _rrsFailover = a })

-- | The cache time to live for the current resource record set.
rrsTTL :: Lens' ResourceRecordSet (Maybe Integer)
rrsTTL = lens _rrsTTL (\s a -> s { _rrsTTL = a })

-- | A complex type that contains the resource records for the current resource
-- record set.
rrsResourceRecord :: Lens' ResourceRecordSet (List1 ResourceRecord)
rrsResourceRecord =
    lens _rrsResourceRecord (\s a -> s { _rrsResourceRecord = a })

-- | Alias resource record sets only: Information about the AWS resource to
-- which you are redirecting traffic.
rrsAliasTarget :: Lens' ResourceRecordSet (Maybe AliasTarget)
rrsAliasTarget = lens _rrsAliasTarget (\s a -> s { _rrsAliasTarget = a })

-- | Health Check resource record sets only, not required for alias resource
-- record sets: An identifier that is used to identify health check associated
-- with the resource record set.
rrsHealthCheckId :: Lens' ResourceRecordSet (Maybe Text)
rrsHealthCheckId =
    lens _rrsHealthCheckId (\s a -> s { _rrsHealthCheckId = a })

instance FromXML ResourceRecordSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceRecordSet"

instance ToXML ResourceRecordSet where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ResourceRecordSet"

-- | A ResourceTagSet containing tags associated with the specified resource.
data ResourceTagSet = ResourceTagSet
    { _rtsResourceType :: Maybe TagResourceType
    , _rtsResourceId :: Maybe Text
    , _rtsTag :: Maybe (List1 Tag)
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ResourceTagSet' data type.
--
-- 'ResourceTagSet' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceType ::@ @Maybe TagResourceType@
--
-- * @ResourceId ::@ @Maybe Text@
--
-- * @Tag ::@ @Maybe (List1 Tag)@
--
resourceTagSet :: ResourceTagSet
resourceTagSet = ResourceTagSet
    { _rtsResourceType = Nothing
    , _rtsResourceId = Nothing
    , _rtsTag = Nothing
    }

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
rtsResourceType :: Lens' ResourceTagSet (Maybe TagResourceType)
rtsResourceType = lens _rtsResourceType (\s a -> s { _rtsResourceType = a })

-- | The ID for the specified resource.
rtsResourceId :: Lens' ResourceTagSet (Maybe Text)
rtsResourceId = lens _rtsResourceId (\s a -> s { _rtsResourceId = a })

-- | The tags associated with the specified resource.
rtsTag :: Lens' ResourceTagSet (Maybe (List1 Tag))
rtsTag = lens _rtsTag (\s a -> s { _rtsTag = a })

instance FromXML ResourceTagSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceTagSet"

-- | A single tag containing a key and value.
data Tag = Tag
    { _tKey :: Maybe Text
    , _tValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
tag :: Tag
tag = Tag
    { _tKey = Nothing
    , _tValue = Nothing
    }

-- | The key for a Tag.
tKey :: Lens' Tag (Maybe Text)
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | The value for a Tag.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToXML Tag where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tag"
