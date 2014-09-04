{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.Types
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
module Network.AWS.Route53.V2013_04_01.Types
    (
    -- * Service
      Route53
    -- ** Errors
    , Er (..)
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
    , dsNameServers

    -- * HostedZoneConfig
    , HostedZoneConfig
    , mkHostedZoneConfig
    , hzcComment

    -- * ResourceRecord
    , ResourceRecord
    , mkResourceRecord
    , rsValue

    -- * AliasTarget
    , AliasTarget
    , mkAliasTarget
    , atHostedZoneId
    , atDNSName
    , atEvaluateTargetHealth

    -- * Change
    , Change
    , mkChange
    , dAction
    , dResourceRecordSet

    -- * ChangeBatch
    , ChangeBatch
    , mkChangeBatch
    , cbComment
    , cbChanges

    -- * ChangeInfo
    , ChangeInfo
    , ciId
    , ciStatus
    , ciSubmittedAt
    , ciComment

    -- * GeoLocation
    , GeoLocation
    , mkGeoLocation
    , glContinentCode
    , glCountryCode
    , glSubdivisionCode

    -- * GeoLocationDetails
    , GeoLocationDetails
    , gldContinentCode
    , gldContinentName
    , gldCountryCode
    , gldCountryName
    , gldSubdivisionCode
    , gldSubdivisionName

    -- * HealthCheck
    , HealthCheck
    , hcId
    , hcCallerReference
    , hcHealthCheckConfig
    , hcHealthCheckVersion

    -- * HealthCheckConfig
    , HealthCheckConfig
    , mkHealthCheckConfig
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
    , hzId
    , hzName
    , hzCallerReference
    , hzConfig
    , hzResourceRecordSetCount

    -- * ResourceRecordSet
    , ResourceRecordSet
    , mkResourceRecordSet
    , rrsName
    , rrsType
    , rrsSetIdentifier
    , rrsWeight
    , rrsRegion
    , rrsGeoLocation
    , rrsFailover
    , rrsTTL
    , rrsResourceRecords
    , rrsAliasTarget
    , rrsHealthCheckId

    -- * ResourceTagSet
    , ResourceTagSet
    , rtsResourceType
    , rtsResourceId
    , rtsTags

    -- * Tag
    , Tag
    , mkTag
    , tKey
    , tValue

    -- * Common
    , module Network.AWS.Route53.Internal.Types
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Signing.V3
import           Network.AWS.Types     (Region)
import qualified Network.AWS.Types.Map as Map
import           Network.AWS.Route53.Internal.Types

-- | Supported version (@2013-04-01@) of the
-- @Amazon Route 53@ service.
data Route53 deriving (Typeable)

instance AWSService Route53 where
    type Sg Route53 = V3
    data Er Route53
        = DelegationSetNotAvailable
            { _dsnaMessage :: Maybe Text
            }
        | HealthCheckAlreadyExists
            { _hcaeMessage :: Maybe Text
            }
        | HealthCheckInUse
            { _hciuMessage :: Maybe Text
            }
        | HealthCheckVersionMismatch
            { _hcvmMessage :: Maybe Text
            }
        | HostedZoneAlreadyExists
            { _hzaeMessage :: Maybe Text
            }
        | HostedZoneNotEmpty
            { _hzneMessage :: Maybe Text
            }
        | IncompatibleVersion
            { _ivMessage :: Maybe Text
            }
        | InvalidChangeBatch
            { _icbMessages :: [Text]
            }
        | InvalidDomainName
            { _idnMessage :: Maybe Text
            }
        | InvalidInput
            { _iiMessage :: Maybe Text
            }
        | NoSuchChange
            { _nscMessage :: Maybe Text
            }
        | NoSuchGeoLocation
            { _nsglMessage :: Maybe Text
            }
        | NoSuchHealthCheck
            { _nshcMessage :: Maybe Text
            }
        | NoSuchHostedZone
            { _nshzMessage :: Maybe Text
            }
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
        | TooManyHostedZones
            { _tmhzMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "route53"
        , _svcVersion  = "2013-04-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er Route53)
deriving instance Generic (Er Route53)

instance AWSError (Er Route53) where
    awsError = const "Route53Error"

instance AWSServiceError (Er Route53) where
    serviceError    = Route53Service
    clientError     = Route53Client
    serializerError = Route53Serializer

instance Exception (Er Route53)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "https://route53.amazonaws.com/doc/2013-04-01/"
    }

-- | The action to perform. Valid values: CREATE | DELETE | UPSERT.
data ChangeAction
    = ChangeActionCreate -- ^ CREATE
    | ChangeActionDelete -- ^ DELETE
    | ChangeActionUpsert -- ^ UPSERT
      deriving (Eq, Show, Generic)

instance Hashable ChangeAction

instance FromText ChangeAction where
    parser = match "CREATE" ChangeActionCreate
         <|> match "DELETE" ChangeActionDelete
         <|> match "UPSERT" ChangeActionUpsert

instance ToText ChangeAction where
    toText ChangeActionCreate = "CREATE"
    toText ChangeActionDelete = "DELETE"
    toText ChangeActionUpsert = "UPSERT"

instance ToByteString ChangeAction

instance FromXML ChangeAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeAction"

instance ToXML ChangeAction where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ChangeAction"

instance ToQuery ChangeAction where
      toQuery = toQuery . toBS

-- | The current state of the request. PENDING indicates that this request has
-- not yet been applied to all Amazon Route 53 DNS servers. Valid Values:
-- PENDING | INSYNC.
data ChangeStatus
    = ChangeStatusInsync -- ^ INSYNC
    | ChangeStatusPending -- ^ PENDING
      deriving (Eq, Show, Generic)

instance Hashable ChangeStatus

instance FromText ChangeStatus where
    parser = match "INSYNC" ChangeStatusInsync
         <|> match "PENDING" ChangeStatusPending

instance ToText ChangeStatus where
    toText ChangeStatusInsync = "INSYNC"
    toText ChangeStatusPending = "PENDING"

instance ToByteString ChangeStatus

instance FromXML ChangeStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeStatus"

instance ToQuery ChangeStatus where
      toQuery = toQuery . toBS

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
data Failover
    = Primary -- ^ PRIMARY
    | Secondary -- ^ SECONDARY
      deriving (Eq, Show, Generic)

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
    fromXMLRoot    = fromRoot "ResourceRecordSetFailover"

instance ToXML Failover where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ResourceRecordSetFailover"

instance ToQuery Failover where
      toQuery = toQuery . toBS

-- | The type of health check to be performed. Currently supported types are
-- TCP, HTTP, HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH.
data HealthCheckType
    = HealthCheckTypeHttp -- ^ HTTP
    | HealthCheckTypeHttpStrMatch -- ^ HTTP_STR_MATCH
    | HealthCheckTypeHttps -- ^ HTTPS
    | HealthCheckTypeHttpsStrMatch -- ^ HTTPS_STR_MATCH
    | HealthCheckTypeTcp -- ^ TCP
      deriving (Eq, Show, Generic)

instance Hashable HealthCheckType

instance FromText HealthCheckType where
    parser = match "HTTP" HealthCheckTypeHttp
         <|> match "HTTP_STR_MATCH" HealthCheckTypeHttpStrMatch
         <|> match "HTTPS" HealthCheckTypeHttps
         <|> match "HTTPS_STR_MATCH" HealthCheckTypeHttpsStrMatch
         <|> match "TCP" HealthCheckTypeTcp

instance ToText HealthCheckType where
    toText HealthCheckTypeHttp = "HTTP"
    toText HealthCheckTypeHttpStrMatch = "HTTP_STR_MATCH"
    toText HealthCheckTypeHttps = "HTTPS"
    toText HealthCheckTypeHttpsStrMatch = "HTTPS_STR_MATCH"
    toText HealthCheckTypeTcp = "TCP"

instance ToByteString HealthCheckType

instance FromXML HealthCheckType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheckType"

instance ToXML HealthCheckType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "HealthCheckType"

instance ToQuery HealthCheckType where
      toQuery = toQuery . toBS

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
data TagResourceType
    = TagResourceTypeHealthcheck -- ^ healthcheck
      deriving (Eq, Show, Generic)

instance Hashable TagResourceType

instance FromText TagResourceType where
    parser = match "healthcheck" TagResourceTypeHealthcheck

instance ToText TagResourceType where
    toText TagResourceTypeHealthcheck = "healthcheck"

instance ToByteString TagResourceType

instance FromXML TagResourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagResourceType"

instance ToXML TagResourceType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TagResourceType"

instance ToQuery TagResourceType where
      toQuery = toQuery . toBS

-- | A complex type that contains name server information.
newtype DelegationSet = DelegationSet
    { _dsNameServers :: [Text]
      -- ^ A complex type that contains the authoritative name servers for
      -- the hosted zone. Use the method provided by your domain registrar
      -- to add an NS record to your domain for each NameServer that is
      -- assigned to your hosted zone.
    } deriving (Show, Generic)

-- | A complex type that contains the authoritative name servers for the hosted
-- zone. Use the method provided by your domain registrar to add an NS record
-- to your domain for each NameServer that is assigned to your hosted zone.
dsNameServers :: Lens' DelegationSet ([Text])
dsNameServers = lens _dsNameServers (\s a -> s { _dsNameServers = a })
{-# INLINE dsNameServers #-}

instance FromXML DelegationSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DelegationSet"

-- | A complex type that contains an optional comment about your hosted zone.
newtype HostedZoneConfig = HostedZoneConfig
    { _hzcComment :: Maybe Text
      -- ^ An optional comment about your hosted zone. If you don't want to
      -- specify a comment, you can omit the HostedZoneConfig and Comment
      -- elements from the XML document.
    } deriving (Show, Generic)

-- | An optional comment about your hosted zone. If you don't want to specify a
-- comment, you can omit the HostedZoneConfig and Comment elements from the
-- XML document.
hzcComment :: Lens' HostedZoneConfig (Maybe Text)
hzcComment = lens _hzcComment (\s a -> s { _hzcComment = a })
{-# INLINE hzcComment #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HostedZoneConfig' data type to populate a request.
mkHostedZoneConfig :: HostedZoneConfig
mkHostedZoneConfig = HostedZoneConfig
    { _hzcComment = Nothing
    }
{-# INLINE mkHostedZoneConfig #-}

instance FromXML HostedZoneConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HostedZoneConfig"

instance ToXML HostedZoneConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "HostedZoneConfig"

-- | A complex type that contains the value of the Value element for the current
-- resource record set.
newtype ResourceRecord = ResourceRecord
    { _rsValue :: Text
      -- ^ The value of the Value element for the current resource record
      -- set.
    } deriving (Show, Generic)

-- | The value of the Value element for the current resource record set.
rsValue :: Lens' ResourceRecord (Text)
rsValue = lens _rsValue (\s a -> s { _rsValue = a })
{-# INLINE rsValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ResourceRecord' data type to populate a request.
mkResourceRecord :: Text -- ^ 'rsValue'
                 -> ResourceRecord
mkResourceRecord p1 = ResourceRecord
    { _rsValue = p1
    }
{-# INLINE mkResourceRecord #-}

instance FromXML ResourceRecord where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceRecord"

instance ToXML ResourceRecord where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ResourceRecord"

-- | Alias resource record sets only: Information about the AWS resource to
-- which you are redirecting traffic.
data AliasTarget = AliasTarget
    { _atHostedZoneId :: Text
      -- ^ Alias resource record sets only: The value of the hosted zone ID
      -- for the AWS resource. For more information and an example, see
      -- Creating Alias Resource Record Sets in the Amazon Route 53
      -- Developer Guide.
    , _atDNSName :: Text
      -- ^ Alias resource record sets only: The external DNS name associated
      -- with the AWS Resource. For more information and an example, see
      -- Creating Alias Resource Record Sets in the Amazon Route 53
      -- Developer Guide.
    , _atEvaluateTargetHealth :: Bool
      -- ^ Alias resource record sets only: A boolean value that indicates
      -- whether this Resource Record Set should respect the health status
      -- of any health checks associated with the ALIAS target record
      -- which it is linked to. For more information and an example, see
      -- Creating Alias Resource Record Sets in the Amazon Route 53
      -- Developer Guide.
    } deriving (Show, Generic)

-- | Alias resource record sets only: The value of the hosted zone ID for the
-- AWS resource. For more information and an example, see Creating Alias
-- Resource Record Sets in the Amazon Route 53 Developer Guide.
atHostedZoneId :: Lens' AliasTarget (Text)
atHostedZoneId = lens _atHostedZoneId (\s a -> s { _atHostedZoneId = a })
{-# INLINE atHostedZoneId #-}

-- | Alias resource record sets only: The external DNS name associated with the
-- AWS Resource. For more information and an example, see Creating Alias
-- Resource Record Sets in the Amazon Route 53 Developer Guide.
atDNSName :: Lens' AliasTarget (Text)
atDNSName = lens _atDNSName (\s a -> s { _atDNSName = a })
{-# INLINE atDNSName #-}

-- | Alias resource record sets only: A boolean value that indicates whether
-- this Resource Record Set should respect the health status of any health
-- checks associated with the ALIAS target record which it is linked to. For
-- more information and an example, see Creating Alias Resource Record Sets in
-- the Amazon Route 53 Developer Guide.
atEvaluateTargetHealth :: Lens' AliasTarget (Bool)
atEvaluateTargetHealth = lens _atEvaluateTargetHealth (\s a -> s { _atEvaluateTargetHealth = a })
{-# INLINE atEvaluateTargetHealth #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AliasTarget' data type to populate a request.
mkAliasTarget :: Text -- ^ 'atHostedZoneId'
              -> Text -- ^ 'atDNSName'
              -> Bool -- ^ 'atEvaluateTargetHealth'
              -> AliasTarget
mkAliasTarget p1 p2 p3 = AliasTarget
    { _atHostedZoneId = p1
    , _atDNSName = p2
    , _atEvaluateTargetHealth = p3
    }
{-# INLINE mkAliasTarget #-}

instance FromXML AliasTarget where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AliasTarget"

instance ToXML AliasTarget where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "AliasTarget"

-- | A complex type that contains the information for each change in a change
-- batch request.
data Change = Change
    { _dAction :: ChangeAction
      -- ^ The action to perform. Valid values: CREATE | DELETE | UPSERT.
    , _dResourceRecordSet :: ResourceRecordSet
      -- ^ Information about the resource record set to create or delete.
    } deriving (Show, Generic)

-- | The action to perform. Valid values: CREATE | DELETE | UPSERT.
dAction :: Lens' Change (ChangeAction)
dAction = lens _dAction (\s a -> s { _dAction = a })
{-# INLINE dAction #-}

-- | Information about the resource record set to create or delete.
dResourceRecordSet :: Lens' Change (ResourceRecordSet)
dResourceRecordSet = lens _dResourceRecordSet (\s a -> s { _dResourceRecordSet = a })
{-# INLINE dResourceRecordSet #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Change' data type to populate a request.
mkChange :: ChangeAction -- ^ 'dAction'
         -> ResourceRecordSet -- ^ 'dResourceRecordSet'
         -> Change
mkChange p1 p2 = Change
    { _dAction = p1
    , _dResourceRecordSet = p2
    }
{-# INLINE mkChange #-}

instance ToXML Change where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Change"

-- | A complex type that contains an optional comment and the Changes element.
data ChangeBatch = ChangeBatch
    { _cbComment :: Maybe Text
      -- ^ Optional: Any comments you want to include about a change batch
      -- request.
    , _cbChanges :: [Change]
      -- ^ A complex type that contains one Change element for each resource
      -- record set that you want to create or delete.
    } deriving (Show, Generic)

-- | Optional: Any comments you want to include about a change batch request.
cbComment :: Lens' ChangeBatch (Maybe Text)
cbComment = lens _cbComment (\s a -> s { _cbComment = a })
{-# INLINE cbComment #-}

-- | A complex type that contains one Change element for each resource record
-- set that you want to create or delete.
cbChanges :: Lens' ChangeBatch ([Change])
cbChanges = lens _cbChanges (\s a -> s { _cbChanges = a })
{-# INLINE cbChanges #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChangeBatch' data type to populate a request.
mkChangeBatch :: [Change] -- ^ 'cbChanges'
              -> ChangeBatch
mkChangeBatch p1 = ChangeBatch
    { _cbComment = Nothing
    , _cbChanges = p2
    }
{-# INLINE mkChangeBatch #-}

instance ToXML ChangeBatch where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ChangeBatch"

-- | A complex type that contains information about changes made to your hosted
-- zone. This element contains an ID that you use when performing a GetChange
-- action to get detailed information about the change.
data ChangeInfo = ChangeInfo
    { _ciId :: Text
      -- ^ The ID of the request. Use this ID to track when the change has
      -- completed across all Amazon Route 53 DNS servers.
    , _ciStatus :: ChangeStatus
      -- ^ The current state of the request. PENDING indicates that this
      -- request has not yet been applied to all Amazon Route 53 DNS
      -- servers. Valid Values: PENDING | INSYNC.
    , _ciSubmittedAt :: ISO8601
      -- ^ The date and time the change was submitted, in the format
      -- YYYY-MM-DDThh:mm:ssZ, as specified in the ISO 8601 standard (for
      -- example, 2009-11-19T19:37:58Z). The Z after the time indicates
      -- that the time is listed in Coordinated Universal Time (UTC),
      -- which is synonymous with Greenwich Mean Time in this context.
    , _ciComment :: Maybe Text
      -- ^ A complex type that describes change information about changes
      -- made to your hosted zone. This element contains an ID that you
      -- use when performing a GetChange action to get detailed
      -- information about the change.
    } deriving (Show, Generic)

-- | The ID of the request. Use this ID to track when the change has completed
-- across all Amazon Route 53 DNS servers.
ciId :: Lens' ChangeInfo (Text)
ciId = lens _ciId (\s a -> s { _ciId = a })
{-# INLINE ciId #-}

-- | The current state of the request. PENDING indicates that this request has
-- not yet been applied to all Amazon Route 53 DNS servers. Valid Values:
-- PENDING | INSYNC.
ciStatus :: Lens' ChangeInfo (ChangeStatus)
ciStatus = lens _ciStatus (\s a -> s { _ciStatus = a })
{-# INLINE ciStatus #-}

-- | The date and time the change was submitted, in the format
-- YYYY-MM-DDThh:mm:ssZ, as specified in the ISO 8601 standard (for example,
-- 2009-11-19T19:37:58Z). The Z after the time indicates that the time is
-- listed in Coordinated Universal Time (UTC), which is synonymous with
-- Greenwich Mean Time in this context.
ciSubmittedAt :: Lens' ChangeInfo (ISO8601)
ciSubmittedAt = lens _ciSubmittedAt (\s a -> s { _ciSubmittedAt = a })
{-# INLINE ciSubmittedAt #-}

-- | A complex type that describes change information about changes made to your
-- hosted zone. This element contains an ID that you use when performing a
-- GetChange action to get detailed information about the change.
ciComment :: Lens' ChangeInfo (Maybe Text)
ciComment = lens _ciComment (\s a -> s { _ciComment = a })
{-# INLINE ciComment #-}

instance FromXML ChangeInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeInfo"

-- | Geo location resource record sets only: Among resource record sets that
-- have the same combination of DNS name and type, a value that specifies the
-- geo location for the current resource record set.
data GeoLocation = GeoLocation
    { _glContinentCode :: Maybe Text
      -- ^ The code for a continent geo location. Note: only continent
      -- locations have a continent code. Valid values: AF | AN | AS | EU
      -- | OC | NA | SA Constraint: Specifying ContinentCode with either
      -- CountryCode or SubdivisionCode returns an InvalidInput error.
    , _glCountryCode :: Maybe Text
      -- ^ The code for a country geo location. The default location uses
      -- '*' for the country code and will match all locations that are
      -- not matched by a geo location. The default geo location uses a *
      -- for the country code. All other country codes follow the ISO 3166
      -- two-character code.
    , _glSubdivisionCode :: Maybe Text
      -- ^ The code for a country's subdivision (e.g., a province of
      -- Canada). A subdivision code is only valid with the appropriate
      -- country code. Constraint: Specifying SubdivisionCode without
      -- CountryCode returns an InvalidInput error.
    } deriving (Show, Generic)

-- | The code for a continent geo location. Note: only continent locations have
-- a continent code. Valid values: AF | AN | AS | EU | OC | NA | SA
-- Constraint: Specifying ContinentCode with either CountryCode or
-- SubdivisionCode returns an InvalidInput error.
glContinentCode :: Lens' GeoLocation (Maybe Text)
glContinentCode = lens _glContinentCode (\s a -> s { _glContinentCode = a })
{-# INLINE glContinentCode #-}

-- | The code for a country geo location. The default location uses '*' for the
-- country code and will match all locations that are not matched by a geo
-- location. The default geo location uses a * for the country code. All other
-- country codes follow the ISO 3166 two-character code.
glCountryCode :: Lens' GeoLocation (Maybe Text)
glCountryCode = lens _glCountryCode (\s a -> s { _glCountryCode = a })
{-# INLINE glCountryCode #-}

-- | The code for a country's subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
-- Constraint: Specifying SubdivisionCode without CountryCode returns an
-- InvalidInput error.
glSubdivisionCode :: Lens' GeoLocation (Maybe Text)
glSubdivisionCode = lens _glSubdivisionCode (\s a -> s { _glSubdivisionCode = a })
{-# INLINE glSubdivisionCode #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GeoLocation' data type to populate a request.
mkGeoLocation :: GeoLocation
mkGeoLocation = GeoLocation
    { _glContinentCode = Nothing
    , _glCountryCode = Nothing
    , _glSubdivisionCode = Nothing
    }
{-# INLINE mkGeoLocation #-}

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
      -- ^ The code for a continent geo location. Note: only continent
      -- locations have a continent code.
    , _gldContinentName :: Maybe Text
      -- ^ The name of the continent. This element is only present if
      -- ContinentCode is also present.
    , _gldCountryCode :: Maybe Text
      -- ^ The code for a country geo location. The default location uses
      -- '*' for the country code and will match all locations that are
      -- not matched by a geo location. The default geo location uses a *
      -- for the country code. All other country codes follow the ISO 3166
      -- two-character code.
    , _gldCountryName :: Maybe Text
      -- ^ The name of the country. This element is only present if
      -- CountryCode is also present.
    , _gldSubdivisionCode :: Maybe Text
      -- ^ The code for a country's subdivision (e.g., a province of
      -- Canada). A subdivision code is only valid with the appropriate
      -- country code.
    , _gldSubdivisionName :: Maybe Text
      -- ^ The name of the subdivision. This element is only present if
      -- SubdivisionCode is also present.
    } deriving (Show, Generic)

-- | The code for a continent geo location. Note: only continent locations have
-- a continent code.
gldContinentCode :: Lens' GeoLocationDetails (Maybe Text)
gldContinentCode = lens _gldContinentCode (\s a -> s { _gldContinentCode = a })
{-# INLINE gldContinentCode #-}

-- | The name of the continent. This element is only present if ContinentCode is
-- also present.
gldContinentName :: Lens' GeoLocationDetails (Maybe Text)
gldContinentName = lens _gldContinentName (\s a -> s { _gldContinentName = a })
{-# INLINE gldContinentName #-}

-- | The code for a country geo location. The default location uses '*' for the
-- country code and will match all locations that are not matched by a geo
-- location. The default geo location uses a * for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gldCountryCode :: Lens' GeoLocationDetails (Maybe Text)
gldCountryCode = lens _gldCountryCode (\s a -> s { _gldCountryCode = a })
{-# INLINE gldCountryCode #-}

-- | The name of the country. This element is only present if CountryCode is
-- also present.
gldCountryName :: Lens' GeoLocationDetails (Maybe Text)
gldCountryName = lens _gldCountryName (\s a -> s { _gldCountryName = a })
{-# INLINE gldCountryName #-}

-- | The code for a country's subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
gldSubdivisionCode :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionCode = lens _gldSubdivisionCode (\s a -> s { _gldSubdivisionCode = a })
{-# INLINE gldSubdivisionCode #-}

-- | The name of the subdivision. This element is only present if
-- SubdivisionCode is also present.
gldSubdivisionName :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionName = lens _gldSubdivisionName (\s a -> s { _gldSubdivisionName = a })
{-# INLINE gldSubdivisionName #-}

instance FromXML GeoLocationDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GeoLocationDetails"

-- | A complex type that contains identifying information about the health
-- check.
data HealthCheck = HealthCheck
    { _hcId :: Text
      -- ^ The ID of the specified health check.
    , _hcCallerReference :: Text
      -- ^ A unique string that identifies the request to create the health
      -- check.
    , _hcHealthCheckConfig :: HealthCheckConfig
      -- ^ A complex type that contains the health check configuration.
    , _hcHealthCheckVersion :: Integer
      -- ^ The version of the health check. You can optionally pass this
      -- value in a call to UpdateHealthCheck to prevent overwriting
      -- another change to the health check.
    } deriving (Show, Generic)

-- | The ID of the specified health check.
hcId :: Lens' HealthCheck (Text)
hcId = lens _hcId (\s a -> s { _hcId = a })
{-# INLINE hcId #-}

-- | A unique string that identifies the request to create the health check.
hcCallerReference :: Lens' HealthCheck (Text)
hcCallerReference = lens _hcCallerReference (\s a -> s { _hcCallerReference = a })
{-# INLINE hcCallerReference #-}

-- | A complex type that contains the health check configuration.
hcHealthCheckConfig :: Lens' HealthCheck (HealthCheckConfig)
hcHealthCheckConfig = lens _hcHealthCheckConfig (\s a -> s { _hcHealthCheckConfig = a })
{-# INLINE hcHealthCheckConfig #-}

-- | The version of the health check. You can optionally pass this value in a
-- call to UpdateHealthCheck to prevent overwriting another change to the
-- health check.
hcHealthCheckVersion :: Lens' HealthCheck (Integer)
hcHealthCheckVersion = lens _hcHealthCheckVersion (\s a -> s { _hcHealthCheckVersion = a })
{-# INLINE hcHealthCheckVersion #-}

instance FromXML HealthCheck where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheck"

-- | A complex type that contains health check configuration.
data HealthCheckConfig = HealthCheckConfig
    { _hccIPAddress :: Maybe Text
      -- ^ IP Address of the instance being checked.
    , _hccPort :: Maybe Integer
      -- ^ Port on which connection will be opened to the instance to health
      -- check. For HTTP and HTTP_STR_MATCH this defaults to 80 if the
      -- port is not specified. For HTTPS and HTTPS_STR_MATCH this
      -- defaults to 443 if the port is not specified.
    , _hccType :: HealthCheckType
      -- ^ The type of health check to be performed. Currently supported
      -- types are TCP, HTTP, HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH.
    , _hccResourcePath :: Maybe Text
      -- ^ Path to ping on the instance to check the health. Required for
      -- HTTP, HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH health checks,
      -- HTTP request is issued to the instance on the given port and
      -- path.
    , _hccFullyQualifiedDomainName :: Maybe Text
      -- ^ Fully qualified domain name of the instance to be health checked.
    , _hccSearchString :: Maybe Text
      -- ^ A string to search for in the body of a health check response.
      -- Required for HTTP_STR_MATCH and HTTPS_STR_MATCH health checks.
    , _hccRequestInterval :: Maybe Integer
      -- ^ The number of seconds between the time that Route 53 gets a
      -- response from your endpoint and the time that it sends the next
      -- health-check request. Each Route 53 health checker makes requests
      -- at this interval. Valid values are 10 and 30. The default value
      -- is 30.
    , _hccFailureThreshold :: Maybe Integer
      -- ^ The number of consecutive health checks that an endpoint must
      -- pass or fail for Route 53 to change the current status of the
      -- endpoint from unhealthy to healthy or vice versa. Valid values
      -- are integers between 1 and 10. For more information, see "How
      -- Amazon Route 53 Determines Whether an Endpoint Is Healthy" in the
      -- Amazon Route 53 Developer Guide.
    } deriving (Show, Generic)

-- | IP Address of the instance being checked.
hccIPAddress :: Lens' HealthCheckConfig (Maybe Text)
hccIPAddress = lens _hccIPAddress (\s a -> s { _hccIPAddress = a })
{-# INLINE hccIPAddress #-}

-- | Port on which connection will be opened to the instance to health check.
-- For HTTP and HTTP_STR_MATCH this defaults to 80 if the port is not
-- specified. For HTTPS and HTTPS_STR_MATCH this defaults to 443 if the port
-- is not specified.
hccPort :: Lens' HealthCheckConfig (Maybe Integer)
hccPort = lens _hccPort (\s a -> s { _hccPort = a })
{-# INLINE hccPort #-}

-- | The type of health check to be performed. Currently supported types are
-- TCP, HTTP, HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH.
hccType :: Lens' HealthCheckConfig (HealthCheckType)
hccType = lens _hccType (\s a -> s { _hccType = a })
{-# INLINE hccType #-}

-- | Path to ping on the instance to check the health. Required for HTTP, HTTPS,
-- HTTP_STR_MATCH, and HTTPS_STR_MATCH health checks, HTTP request is issued
-- to the instance on the given port and path.
hccResourcePath :: Lens' HealthCheckConfig (Maybe Text)
hccResourcePath = lens _hccResourcePath (\s a -> s { _hccResourcePath = a })
{-# INLINE hccResourcePath #-}

-- | Fully qualified domain name of the instance to be health checked.
hccFullyQualifiedDomainName :: Lens' HealthCheckConfig (Maybe Text)
hccFullyQualifiedDomainName = lens _hccFullyQualifiedDomainName (\s a -> s { _hccFullyQualifiedDomainName = a })
{-# INLINE hccFullyQualifiedDomainName #-}

-- | A string to search for in the body of a health check response. Required for
-- HTTP_STR_MATCH and HTTPS_STR_MATCH health checks.
hccSearchString :: Lens' HealthCheckConfig (Maybe Text)
hccSearchString = lens _hccSearchString (\s a -> s { _hccSearchString = a })
{-# INLINE hccSearchString #-}

-- | The number of seconds between the time that Route 53 gets a response from
-- your endpoint and the time that it sends the next health-check request.
-- Each Route 53 health checker makes requests at this interval. Valid values
-- are 10 and 30. The default value is 30.
hccRequestInterval :: Lens' HealthCheckConfig (Maybe Integer)
hccRequestInterval = lens _hccRequestInterval (\s a -> s { _hccRequestInterval = a })
{-# INLINE hccRequestInterval #-}

-- | The number of consecutive health checks that an endpoint must pass or fail
-- for Route 53 to change the current status of the endpoint from unhealthy to
-- healthy or vice versa. Valid values are integers between 1 and 10. For more
-- information, see "How Amazon Route 53 Determines Whether an Endpoint Is
-- Healthy" in the Amazon Route 53 Developer Guide.
hccFailureThreshold :: Lens' HealthCheckConfig (Maybe Integer)
hccFailureThreshold = lens _hccFailureThreshold (\s a -> s { _hccFailureThreshold = a })
{-# INLINE hccFailureThreshold #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HealthCheckConfig' data type to populate a request.
mkHealthCheckConfig :: HealthCheckType -- ^ 'hccType'
                    -> HealthCheckConfig
mkHealthCheckConfig p1 = HealthCheckConfig
    { _hccIPAddress = Nothing
    , _hccPort = Nothing
    , _hccType = p3
    , _hccResourcePath = Nothing
    , _hccFullyQualifiedDomainName = Nothing
    , _hccSearchString = Nothing
    , _hccRequestInterval = Nothing
    , _hccFailureThreshold = Nothing
    }
{-# INLINE mkHealthCheckConfig #-}

instance FromXML HealthCheckConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheckConfig"

instance ToXML HealthCheckConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "HealthCheckConfig"

-- | A complex type that contains identifying information about the hosted zone.
data HostedZone = HostedZone
    { _hzId :: Text
      -- ^ The ID of the specified hosted zone.
    , _hzName :: Text
      -- ^ The name of the domain. This must be a fully-specified domain,
      -- for example, www.example.com. The trailing dot is optional; Route
      -- 53 assumes that the domain name is fully qualified. This means
      -- that Route 53 treats www.example.com (without a trailing dot) and
      -- www.example.com. (with a trailing dot) as identical. This is the
      -- name you have registered with your DNS registrar. You should ask
      -- your registrar to change the authoritative name servers for your
      -- domain to the set of NameServers elements returned in
      -- DelegationSet.
    , _hzCallerReference :: Text
      -- ^ A unique string that identifies the request to create the hosted
      -- zone.
    , _hzConfig :: Maybe HostedZoneConfig
      -- ^ A complex type that contains the Comment element.
    , _hzResourceRecordSetCount :: Maybe Integer
      -- ^ Total number of resource record sets in the hosted zone.
    } deriving (Show, Generic)

-- | The ID of the specified hosted zone.
hzId :: Lens' HostedZone (Text)
hzId = lens _hzId (\s a -> s { _hzId = a })
{-# INLINE hzId #-}

-- | The name of the domain. This must be a fully-specified domain, for example,
-- www.example.com. The trailing dot is optional; Route 53 assumes that the
-- domain name is fully qualified. This means that Route 53 treats
-- www.example.com (without a trailing dot) and www.example.com. (with a
-- trailing dot) as identical. This is the name you have registered with your
-- DNS registrar. You should ask your registrar to change the authoritative
-- name servers for your domain to the set of NameServers elements returned in
-- DelegationSet.
hzName :: Lens' HostedZone (Text)
hzName = lens _hzName (\s a -> s { _hzName = a })
{-# INLINE hzName #-}

-- | A unique string that identifies the request to create the hosted zone.
hzCallerReference :: Lens' HostedZone (Text)
hzCallerReference = lens _hzCallerReference (\s a -> s { _hzCallerReference = a })
{-# INLINE hzCallerReference #-}

-- | A complex type that contains the Comment element.
hzConfig :: Lens' HostedZone (Maybe HostedZoneConfig)
hzConfig = lens _hzConfig (\s a -> s { _hzConfig = a })
{-# INLINE hzConfig #-}

-- | Total number of resource record sets in the hosted zone.
hzResourceRecordSetCount :: Lens' HostedZone (Maybe Integer)
hzResourceRecordSetCount = lens _hzResourceRecordSetCount (\s a -> s { _hzResourceRecordSetCount = a })
{-# INLINE hzResourceRecordSetCount #-}

instance FromXML HostedZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HostedZone"

-- | Information about the resource record set to create or delete.
data ResourceRecordSet = ResourceRecordSet
    { _rrsName :: Text
      -- ^ The domain name of the current resource record set.
    , _rrsType :: RecordType
      -- ^ The type of the current resource record set.
    , _rrsSetIdentifier :: Maybe Text
      -- ^ Weighted, Latency, Geo, and Failover resource record sets only:
      -- An identifier that differentiates among multiple resource record
      -- sets that have the same combination of DNS name and type.
    , _rrsWeight :: Maybe Integer
      -- ^ Weighted resource record sets only: Among resource record sets
      -- that have the same combination of DNS name and type, a value that
      -- determines what portion of traffic for the current resource
      -- record set is routed to the associated location.
    , _rrsRegion :: Maybe Region
      -- ^ Latency-based resource record sets only: Among resource record
      -- sets that have the same combination of DNS name and type, a value
      -- that specifies the AWS region for the current resource record
      -- set.
    , _rrsGeoLocation :: Maybe GeoLocation
      -- ^ Geo location resource record sets only: Among resource record
      -- sets that have the same combination of DNS name and type, a value
      -- that specifies the geo location for the current resource record
      -- set.
    , _rrsFailover :: Maybe Failover
      -- ^ Failover resource record sets only: Among resource record sets
      -- that have the same combination of DNS name and type, a value that
      -- indicates whether the current resource record set is a primary or
      -- secondary resource record set. A failover set may contain at most
      -- one resource record set marked as primary and one resource record
      -- set marked as secondary. A resource record set marked as primary
      -- will be returned if any of the following are true: (1) an
      -- associated health check is passing, (2) if the resource record
      -- set is an alias with the evaluate target health and at least one
      -- target resource record set is healthy, (3) both the primary and
      -- secondary resource record set are failing health checks or (4)
      -- there is no secondary resource record set. A secondary resource
      -- record set will be returned if: (1) the primary is failing a
      -- health check and either the secondary is passing a health check
      -- or has no associated health check, or (2) there is no primary
      -- resource record set. Valid values: PRIMARY | SECONDARY.
    , _rrsTTL :: Maybe Integer
      -- ^ The cache time to live for the current resource record set.
    , _rrsResourceRecords :: [ResourceRecord]
      -- ^ A complex type that contains the resource records for the current
      -- resource record set.
    , _rrsAliasTarget :: Maybe AliasTarget
      -- ^ Alias resource record sets only: Information about the AWS
      -- resource to which you are redirecting traffic.
    , _rrsHealthCheckId :: Maybe Text
      -- ^ Health Check resource record sets only, not required for alias
      -- resource record sets: An identifier that is used to identify
      -- health check associated with the resource record set.
    } deriving (Show, Generic)

-- | The domain name of the current resource record set.
rrsName :: Lens' ResourceRecordSet (Text)
rrsName = lens _rrsName (\s a -> s { _rrsName = a })
{-# INLINE rrsName #-}

-- | The type of the current resource record set.
rrsType :: Lens' ResourceRecordSet (RecordType)
rrsType = lens _rrsType (\s a -> s { _rrsType = a })
{-# INLINE rrsType #-}

-- | Weighted, Latency, Geo, and Failover resource record sets only: An
-- identifier that differentiates among multiple resource record sets that
-- have the same combination of DNS name and type.
rrsSetIdentifier :: Lens' ResourceRecordSet (Maybe Text)
rrsSetIdentifier = lens _rrsSetIdentifier (\s a -> s { _rrsSetIdentifier = a })
{-# INLINE rrsSetIdentifier #-}

-- | Weighted resource record sets only: Among resource record sets that have
-- the same combination of DNS name and type, a value that determines what
-- portion of traffic for the current resource record set is routed to the
-- associated location.
rrsWeight :: Lens' ResourceRecordSet (Maybe Integer)
rrsWeight = lens _rrsWeight (\s a -> s { _rrsWeight = a })
{-# INLINE rrsWeight #-}

-- | Latency-based resource record sets only: Among resource record sets that
-- have the same combination of DNS name and type, a value that specifies the
-- AWS region for the current resource record set.
rrsRegion :: Lens' ResourceRecordSet (Maybe Region)
rrsRegion = lens _rrsRegion (\s a -> s { _rrsRegion = a })
{-# INLINE rrsRegion #-}

-- | Geo location resource record sets only: Among resource record sets that
-- have the same combination of DNS name and type, a value that specifies the
-- geo location for the current resource record set.
rrsGeoLocation :: Lens' ResourceRecordSet (Maybe GeoLocation)
rrsGeoLocation = lens _rrsGeoLocation (\s a -> s { _rrsGeoLocation = a })
{-# INLINE rrsGeoLocation #-}

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
{-# INLINE rrsFailover #-}

-- | The cache time to live for the current resource record set.
rrsTTL :: Lens' ResourceRecordSet (Maybe Integer)
rrsTTL = lens _rrsTTL (\s a -> s { _rrsTTL = a })
{-# INLINE rrsTTL #-}

-- | A complex type that contains the resource records for the current resource
-- record set.
rrsResourceRecords :: Lens' ResourceRecordSet ([ResourceRecord])
rrsResourceRecords = lens _rrsResourceRecords (\s a -> s { _rrsResourceRecords = a })
{-# INLINE rrsResourceRecords #-}

-- | Alias resource record sets only: Information about the AWS resource to
-- which you are redirecting traffic.
rrsAliasTarget :: Lens' ResourceRecordSet (Maybe AliasTarget)
rrsAliasTarget = lens _rrsAliasTarget (\s a -> s { _rrsAliasTarget = a })
{-# INLINE rrsAliasTarget #-}

-- | Health Check resource record sets only, not required for alias resource
-- record sets: An identifier that is used to identify health check associated
-- with the resource record set.
rrsHealthCheckId :: Lens' ResourceRecordSet (Maybe Text)
rrsHealthCheckId = lens _rrsHealthCheckId (\s a -> s { _rrsHealthCheckId = a })
{-# INLINE rrsHealthCheckId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ResourceRecordSet' data type to populate a request.
mkResourceRecordSet :: Text -- ^ 'rrsName'
                    -> RecordType -- ^ 'rrsType'
                    -> [ResourceRecord] -- ^ 'rrsResourceRecords'
                    -> ResourceRecordSet
mkResourceRecordSet p1 p2 p3 = ResourceRecordSet
    { _rrsName = p1
    , _rrsType = p2
    , _rrsSetIdentifier = Nothing
    , _rrsWeight = Nothing
    , _rrsRegion = Nothing
    , _rrsGeoLocation = Nothing
    , _rrsFailover = Nothing
    , _rrsTTL = Nothing
    , _rrsResourceRecords = p9
    , _rrsAliasTarget = Nothing
    , _rrsHealthCheckId = Nothing
    }
{-# INLINE mkResourceRecordSet #-}

instance FromXML ResourceRecordSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceRecordSet"

instance ToXML ResourceRecordSet where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ResourceRecordSet"

-- | A ResourceTagSet containing tags associated with the specified resource.
data ResourceTagSet = ResourceTagSet
    { _rtsResourceType :: Maybe TagResourceType
      -- ^ The type of the resource. The resource type for health checks is
      -- healthcheck.
    , _rtsResourceId :: Maybe Text
      -- ^ The ID for the specified resource.
    , _rtsTags :: Maybe [Tag]
      -- ^ The tags associated with the specified resource.
    } deriving (Show, Generic)

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
rtsResourceType :: Lens' ResourceTagSet (Maybe TagResourceType)
rtsResourceType = lens _rtsResourceType (\s a -> s { _rtsResourceType = a })
{-# INLINE rtsResourceType #-}

-- | The ID for the specified resource.
rtsResourceId :: Lens' ResourceTagSet (Maybe Text)
rtsResourceId = lens _rtsResourceId (\s a -> s { _rtsResourceId = a })
{-# INLINE rtsResourceId #-}

-- | The tags associated with the specified resource.
rtsTags :: Lens' ResourceTagSet (Maybe [Tag])
rtsTags = lens _rtsTags (\s a -> s { _rtsTags = a })
{-# INLINE rtsTags #-}

instance FromXML ResourceTagSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceTagSet"

-- | A single tag containing a key and value.
data Tag = Tag
    { _tKey :: Maybe Text
      -- ^ The key for a Tag.
    , _tValue :: Maybe Text
      -- ^ The value for a Tag.
    } deriving (Show, Generic)

-- | The key for a Tag.
tKey :: Lens' Tag (Maybe Text)
tKey = lens _tKey (\s a -> s { _tKey = a })
{-# INLINE tKey #-}

-- | The value for a Tag.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })
{-# INLINE tValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: Tag
mkTag = Tag
    { _tKey = Nothing
    , _tValue = Nothing
    }
{-# INLINE mkTag #-}

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToXML Tag where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tag"
