{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.Route53.V2013_04_01.Types where

import Control.Lens.TH (makeIso, makeLenses)
import Network.AWS.Prelude
import Network.AWS.Types (Region)
import qualified Network.AWS.Types.Map as Map
import Network.AWS.Signing.V3

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
data ResourceRecordSetFailover
    = ResourceRecordSetFailoverPrimary -- ^ PRIMARY
    | ResourceRecordSetFailoverSecondary -- ^ SECONDARY
      deriving (Eq, Show, Generic)

instance Hashable ResourceRecordSetFailover

instance FromText ResourceRecordSetFailover where
    parser = match "PRIMARY" ResourceRecordSetFailoverPrimary
         <|> match "SECONDARY" ResourceRecordSetFailoverSecondary

instance ToText ResourceRecordSetFailover where
    toText ResourceRecordSetFailoverPrimary = "PRIMARY"
    toText ResourceRecordSetFailoverSecondary = "SECONDARY"

instance ToByteString ResourceRecordSetFailover

instance FromXML ResourceRecordSetFailover where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceRecordSetFailover"

instance ToXML ResourceRecordSetFailover where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ResourceRecordSetFailover"

instance ToQuery ResourceRecordSetFailover where
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
    , _atEvaluateTargetHealth :: Bool
      -- ^ Alias resource record sets only: A boolean value that indicates
      -- whether this Resource Record Set should respect the health status
      -- of any health checks associated with the ALIAS target record
      -- which it is linked to. For more information and an example, see
      -- Creating Alias Resource Record Sets in the Amazon Route 53
      -- Developer Guide.
    , _atDNSName :: Text
      -- ^ Alias resource record sets only: The external DNS name associated
      -- with the AWS Resource. For more information and an example, see
      -- Creating Alias Resource Record Sets in the Amazon Route 53
      -- Developer Guide.
    } deriving (Show, Generic)

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

instance ToXML Change where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Change"

-- | A complex type that contains an optional comment and the Changes element.
data ChangeBatch = ChangeBatch
    { _cbChanges :: [Change]
      -- ^ A complex type that contains one Change element for each resource
      -- record set that you want to create or delete.
    , _cbComment :: Maybe Text
      -- ^ Optional: Any comments you want to include about a change batch
      -- request.
    } deriving (Show, Generic)

instance ToXML ChangeBatch where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ChangeBatch"

-- | A complex type that contains information about the specified change batch,
-- including the change batch ID, the status of the change, and the date and
-- time of the request.
data ChangeInfo = ChangeInfo
    { _ciStatus :: ChangeStatus
      -- ^ The current state of the request. PENDING indicates that this
      -- request has not yet been applied to all Amazon Route 53 DNS
      -- servers. Valid Values: PENDING | INSYNC.
    , _ciSubmittedAt :: ISO8601
      -- ^ The date and time the change was submitted, in the format
      -- YYYY-MM-DDThh:mm:ssZ, as specified in the ISO 8601 standard (for
      -- example, 2009-11-19T19:37:58Z). The Z after the time indicates
      -- that the time is listed in Coordinated Universal Time (UTC),
      -- which is synonymous with Greenwich Mean Time in this context.
    , _ciId :: Text
      -- ^ The ID of the request. Use this ID to track when the change has
      -- completed across all Amazon Route 53 DNS servers.
    , _ciComment :: Maybe Text
      -- ^ A complex type that describes change information about changes
      -- made to your hosted zone. This element contains an ID that you
      -- use when performing a GetChange action to get detailed
      -- information about the change.
    } deriving (Show, Generic)

instance FromXML ChangeInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeInfo"

-- | Geo location resource record sets only: Among resource record sets that
-- have the same combination of DNS name and type, a value that specifies the
-- geo location for the current resource record set.
data GeoLocation = GeoLocation
    { _glSubdivisionCode :: Maybe Text
      -- ^ The code for a country's subdivision (e.g., a province of
      -- Canada). A subdivision code is only valid with the appropriate
      -- country code. Constraint: Specifying SubdivisionCode without
      -- CountryCode returns an InvalidInput error.
    , _glCountryCode :: Maybe Text
      -- ^ The code for a country geo location. The default location uses
      -- '*' for the country code and will match all locations that are
      -- not matched by a geo location. The default geo location uses a *
      -- for the country code. All other country codes follow the ISO 3166
      -- two-character code.
    , _glContinentCode :: Maybe Text
      -- ^ The code for a continent geo location. Note: only continent
      -- locations have a continent code. Valid values: AF | AN | AS | EU
      -- | OC | NA | SA Constraint: Specifying ContinentCode with either
      -- CountryCode or SubdivisionCode returns an InvalidInput error.
    } deriving (Show, Generic)

instance FromXML GeoLocation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GeoLocation"

instance ToXML GeoLocation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GeoLocation"

-- | A complex type that contains information about a GeoLocation.
data GeoLocationDetails = GeoLocationDetails
    { _gldSubdivisionName :: Maybe Text
      -- ^ The name of the subdivision. This element is only present if
      -- SubdivisionCode is also present.
    , _gldSubdivisionCode :: Maybe Text
      -- ^ The code for a country's subdivision (e.g., a province of
      -- Canada). A subdivision code is only valid with the appropriate
      -- country code.
    , _gldCountryName :: Maybe Text
      -- ^ The name of the country. This element is only present if
      -- CountryCode is also present.
    , _gldCountryCode :: Maybe Text
      -- ^ The code for a country geo location. The default location uses
      -- '*' for the country code and will match all locations that are
      -- not matched by a geo location. The default geo location uses a *
      -- for the country code. All other country codes follow the ISO 3166
      -- two-character code.
    , _gldContinentCode :: Maybe Text
      -- ^ The code for a continent geo location. Note: only continent
      -- locations have a continent code.
    , _gldContinentName :: Maybe Text
      -- ^ The name of the continent. This element is only present if
      -- ContinentCode is also present.
    } deriving (Show, Generic)

instance FromXML GeoLocationDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GeoLocationDetails"

-- | A complex type that contains identifying information about the health
-- check.
data HealthCheck = HealthCheck
    { _hcHealthCheckConfig :: HealthCheckConfig
      -- ^ A complex type that contains the health check configuration.
    , _hcId :: Text
      -- ^ The ID of the specified health check.
    , _hcHealthCheckVersion :: Integer
      -- ^ The version of the health check. You can optionally pass this
      -- value in a call to UpdateHealthCheck to prevent overwriting
      -- another change to the health check.
    , _hcCallerReference :: Text
      -- ^ A unique string that identifies the request to create the health
      -- check.
    } deriving (Show, Generic)

instance FromXML HealthCheck where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheck"

-- | A complex type that contains the health check configuration.
data HealthCheckConfig = HealthCheckConfig
    { _hccFailureThreshold :: Maybe Integer
      -- ^ The number of consecutive health checks that an endpoint must
      -- pass or fail for Route 53 to change the current status of the
      -- endpoint from unhealthy to healthy or vice versa. Valid values
      -- are integers between 1 and 10. For more information, see "How
      -- Amazon Route 53 Determines Whether an Endpoint Is Healthy" in the
      -- Amazon Route 53 Developer Guide.
    , _hccIPAddress :: Maybe Text
      -- ^ IP Address of the instance being checked.
    , _hccSearchString :: Maybe Text
      -- ^ A string to search for in the body of a health check response.
      -- Required for HTTP_STR_MATCH and HTTPS_STR_MATCH health checks.
    , _hccResourcePath :: Maybe Text
      -- ^ Path to ping on the instance to check the health. Required for
      -- HTTP, HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH health checks,
      -- HTTP request is issued to the instance on the given port and
      -- path.
    , _hccType :: HealthCheckType
      -- ^ The type of health check to be performed. Currently supported
      -- types are TCP, HTTP, HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH.
    , _hccFullyQualifiedDomainName :: Maybe Text
      -- ^ Fully qualified domain name of the instance to be health checked.
    , _hccRequestInterval :: Maybe Integer
      -- ^ The number of seconds between the time that Route 53 gets a
      -- response from your endpoint and the time that it sends the next
      -- health-check request. Each Route 53 health checker makes requests
      -- at this interval. Valid values are 10 and 30. The default value
      -- is 30.
    , _hccPort :: Maybe Integer
      -- ^ Port on which connection will be opened to the instance to health
      -- check. For HTTP and HTTP_STR_MATCH this defaults to 80 if the
      -- port is not specified. For HTTPS and HTTPS_STR_MATCH this
      -- defaults to 443 if the port is not specified.
    } deriving (Show, Generic)

instance FromXML HealthCheckConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheckConfig"

instance ToXML HealthCheckConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "HealthCheckConfig"

-- | A complex type that contains identifying information about the hosted zone.
data HostedZone = HostedZone
    { _hzConfig :: Maybe HostedZoneConfig
      -- ^ A complex type that contains the Comment element.
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
    , _hzId :: Text
      -- ^ The ID of the specified hosted zone.
    , _hzResourceRecordSetCount :: Maybe Integer
      -- ^ Total number of resource record sets in the hosted zone.
    , _hzCallerReference :: Text
      -- ^ A unique string that identifies the request to create the hosted
      -- zone.
    } deriving (Show, Generic)

instance FromXML HostedZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HostedZone"

-- | Information about the resource record set to create or delete.
data ResourceRecordSet = ResourceRecordSet
    { _rrsTTL :: Maybe Integer
      -- ^ The cache time to live for the current resource record set.
    , _rrsResourceRecords :: Maybe [ResourceRecord]
      -- ^ A complex type that contains the resource records for the current
      -- resource record set.
    , _rrsAliasTarget :: Maybe AliasTarget
      -- ^ Alias resource record sets only: Information about the AWS
      -- resource to which you are redirecting traffic.
    , _rrsWeight :: Maybe Integer
      -- ^ Weighted resource record sets only: Among resource record sets
      -- that have the same combination of DNS name and type, a value that
      -- determines what portion of traffic for the current resource
      -- record set is routed to the associated location.
    , _rrsSetIdentifier :: Maybe Text
      -- ^ Weighted, Latency, Geo, and Failover resource record sets only:
      -- An identifier that differentiates among multiple resource record
      -- sets that have the same combination of DNS name and type.
    , _rrsFailover :: Maybe ResourceRecordSetFailover
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
    , _rrsName :: Text
      -- ^ The domain name of the current resource record set.
    , _rrsHealthCheckId :: Maybe Text
      -- ^ Health Check resource record sets only, not required for alias
      -- resource record sets: An identifier that is used to identify
      -- health check associated with the resource record set.
    , _rrsRegion :: Maybe Region
      -- ^ Latency-based resource record sets only: Among resource record
      -- sets that have the same combination of DNS name and type, a value
      -- that specifies the AWS region for the current resource record
      -- set.
    , _rrsType :: RecordType
      -- ^ The type of the current resource record set.
    , _rrsGeoLocation :: Maybe GeoLocation
      -- ^ Geo location resource record sets only: Among resource record
      -- sets that have the same combination of DNS name and type, a value
      -- that specifies the geo location for the current resource record
      -- set.
    } deriving (Show, Generic)

instance FromXML ResourceRecordSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceRecordSet"

instance ToXML ResourceRecordSet where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ResourceRecordSet"

-- | A ResourceTagSet containing tags associated with the specified resource.
data ResourceTagSet = ResourceTagSet
    { _rtsResourceId :: Maybe Text
      -- ^ The ID for the specified resource.
    , _rtsResourceType :: Maybe TagResourceType
      -- ^ The type of the resource. The resource type for health checks is
      -- healthcheck.
    , _rtsTags :: Maybe [Tag]
      -- ^ The tags associated with the specified resource.
    } deriving (Show, Generic)

instance FromXML ResourceTagSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResourceTagSet"

-- | A single tag containing a key and value.
data Tag = Tag
    { _tValue :: Maybe Text
      -- ^ The value for a Tag.
    , _tKey :: Maybe Text
      -- ^ The key for a Tag.
    } deriving (Show, Generic)

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToXML Tag where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Tag"

-- Newtypes
makeIso ''DelegationSet
makeIso ''HostedZoneConfig
makeIso ''ResourceRecord

-- Products
makeLenses ''AliasTarget
makeLenses ''Change
makeLenses ''ChangeBatch
makeLenses ''ChangeInfo
makeLenses ''GeoLocation
makeLenses ''GeoLocationDetails
makeLenses ''HealthCheck
makeLenses ''HealthCheckConfig
makeLenses ''HostedZone
makeLenses ''ResourceRecordSet
makeLenses ''ResourceTagSet
makeLenses ''Tag
