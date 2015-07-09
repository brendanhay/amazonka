{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types
    (
    -- * Service
      Route53

    -- * Errors
    , _HealthCheckVersionMismatch
    , _InvalidInput
    , _HostedZoneNotEmpty
    , _InvalidArgument
    , _DelegationSetAlreadyReusable
    , _PriorRequestNotComplete
    , _InvalidChangeBatch
    , _InvalidDomainName
    , _DelegationSetNotReusable
    , _HealthCheckAlreadyExists
    , _HostedZoneNotFound
    , _DelegationSetInUse
    , _NoSuchDelegationSet
    , _NoSuchGeoLocation
    , _DelegationSetNotAvailable
    , _VPCAssociationNotFound
    , _ThrottlingException
    , _NoSuchChange
    , _LimitsExceeded
    , _IncompatibleVersion
    , _NoSuchHostedZone
    , _TooManyHostedZones
    , _PublicZoneVPCAssociation
    , _ConflictingDomainExists
    , _LastVPCAssociation
    , _HealthCheckInUse
    , _DelegationSetAlreadyCreated
    , _TooManyHealthChecks
    , _NoSuchHealthCheck
    , _HostedZoneAlreadyExists
    , _InvalidVPCId

    -- * ChangeAction
    , ChangeAction (..)

    -- * ChangeStatus
    , ChangeStatus (..)

    -- * Failover
    , Failover (..)

    -- * HealthCheckType
    , HealthCheckType (..)

    -- * RecordType
    , RecordType (..)

    -- * TagResourceType
    , TagResourceType (..)

    -- * VPCRegion
    , VPCRegion (..)

    -- * AliasTarget
    , AliasTarget
    , aliasTarget
    , atHostedZoneId
    , atDNSName
    , atEvaluateTargetHealth

    -- * Change
    , Change
    , change
    , chaAction
    , chaResourceRecordSet

    -- * ChangeBatch
    , ChangeBatch
    , changeBatch
    , cbComment
    , cbChanges

    -- * ChangeInfo
    , ChangeInfo
    , changeInfo
    , ciComment
    , ciId
    , ciStatus
    , ciSubmittedAt

    -- * DelegationSet
    , DelegationSet
    , delegationSet
    , dsId
    , dsCallerReference
    , dsNameServers

    -- * GeoLocation
    , GeoLocation
    , geoLocation
    , glSubdivisionCode
    , glCountryCode
    , glContinentCode

    -- * GeoLocationDetails
    , GeoLocationDetails
    , geoLocationDetails
    , gldSubdivisionName
    , gldSubdivisionCode
    , gldCountryName
    , gldCountryCode
    , gldContinentCode
    , gldContinentName

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
    , hccFailureThreshold
    , hccSearchString
    , hccResourcePath
    , hccFullyQualifiedDomainName
    , hccRequestInterval
    , hccPort
    , hccType

    -- * HealthCheckObservation
    , HealthCheckObservation
    , healthCheckObservation
    , hcoIPAddress
    , hcoStatusReport

    -- * HostedZone
    , HostedZone
    , hostedZone
    , hzConfig
    , hzResourceRecordSetCount
    , hzId
    , hzName
    , hzCallerReference

    -- * HostedZoneConfig
    , HostedZoneConfig
    , hostedZoneConfig
    , hzcPrivateZone
    , hzcComment

    -- * ResourceRecord
    , ResourceRecord
    , resourceRecord
    , rrValue

    -- * ResourceRecordSet
    , ResourceRecordSet
    , resourceRecordSet
    , rrsTTL
    , rrsAliasTarget
    , rrsWeight
    , rrsSetIdentifier
    , rrsFailover
    , rrsHealthCheckId
    , rrsRegion
    , rrsGeoLocation
    , rrsName
    , rrsType
    , rrsResourceRecords

    -- * ResourceTagSet
    , ResourceTagSet
    , resourceTagSet
    , rtsResourceId
    , rtsResourceType
    , rtsTags

    -- * StatusReport
    , StatusReport
    , statusReport
    , srStatus
    , srCheckedTime

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * VPC
    , VPC
    , vpc
    , vpcVPCRegion
    , vpcVPCId

    , module Network.AWS.Route53.Internal
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Route53.Internal
import           Network.AWS.Sign.V4

-- | Version @2013-04-01@ of the Amazon Route 53 SDK.
data Route53

instance AWSService Route53 where
    type Sg Route53 = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Route53"
            , _svcPrefix = "route53"
            , _svcVersion = "2013-04-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
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

-- | Prism for HealthCheckVersionMismatch' errors.
_HealthCheckVersionMismatch :: AWSError a => Getting (First ServiceError) a ServiceError
_HealthCheckVersionMismatch =
    _ServiceError . hasStatus 409 . hasCode "HealthCheckVersionMismatch"

-- | Some value specified in the request is invalid or the XML document is
-- malformed.
_InvalidInput :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidInput = _ServiceError . hasStatus 400 . hasCode "InvalidInput"

-- | The hosted zone contains resource record sets in addition to the default
-- NS and SOA resource record sets. Before you can delete the hosted zone,
-- you must delete the additional resource record sets.
_HostedZoneNotEmpty :: AWSError a => Getting (First ServiceError) a ServiceError
_HostedZoneNotEmpty =
    _ServiceError . hasStatus 400 . hasCode "HostedZoneNotEmpty"

-- | At least one of the specified arguments is invalid.
_InvalidArgument :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidArgument = _ServiceError . hasCode "InvalidArgument"

-- | The specified delegation set has already been marked as reusable.
_DelegationSetAlreadyReusable :: AWSError a => Getting (First ServiceError) a ServiceError
_DelegationSetAlreadyReusable =
    _ServiceError . hasCode "DelegationSetAlreadyReusable"

-- | The request was rejected because Route 53 was still processing a prior
-- request.
_PriorRequestNotComplete :: AWSError a => Getting (First ServiceError) a ServiceError
_PriorRequestNotComplete =
    _ServiceError . hasStatus 400 . hasCode "PriorRequestNotComplete"

-- | This error contains a list of one or more error messages. Each error
-- message indicates one error in the change batch. For more information,
-- see
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html#example_Errors Example InvalidChangeBatch Errors>.
_InvalidChangeBatch :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidChangeBatch = _ServiceError . hasCode "InvalidChangeBatch"

-- | This error indicates that the specified domain name is not valid.
_InvalidDomainName :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDomainName =
    _ServiceError . hasStatus 400 . hasCode "InvalidDomainName"

-- | The specified delegation set has not been marked as reusable.
_DelegationSetNotReusable :: AWSError a => Getting (First ServiceError) a ServiceError
_DelegationSetNotReusable = _ServiceError . hasCode "DelegationSetNotReusable"

-- | The health check you are trying to create already exists. Route 53
-- returns this error when a health check has already been created with the
-- specified @CallerReference@.
_HealthCheckAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_HealthCheckAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "HealthCheckAlreadyExists"

-- | The specified HostedZone cannot be found.
_HostedZoneNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_HostedZoneNotFound = _ServiceError . hasCode "HostedZoneNotFound"

-- | The specified delegation contains associated hosted zones which must be
-- deleted before the reusable delegation set can be deleted.
_DelegationSetInUse :: AWSError a => Getting (First ServiceError) a ServiceError
_DelegationSetInUse = _ServiceError . hasCode "DelegationSetInUse"

-- | The specified delegation set does not exist.
_NoSuchDelegationSet :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchDelegationSet = _ServiceError . hasCode "NoSuchDelegationSet"

-- | The geo location you are trying to get does not exist.
_NoSuchGeoLocation :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchGeoLocation =
    _ServiceError . hasStatus 404 . hasCode "NoSuchGeoLocation"

-- | Route 53 allows some duplicate domain names, but there is a maximum
-- number of duplicate names. This error indicates that you have reached
-- that maximum. If you want to create another hosted zone with the same
-- name and Route 53 generates this error, you can request an increase to
-- the limit on the <http://aws.amazon.com/route53-request/ Contact Us>
-- page.
_DelegationSetNotAvailable :: AWSError a => Getting (First ServiceError) a ServiceError
_DelegationSetNotAvailable =
    _ServiceError . hasCode "DelegationSetNotAvailable"

-- | The VPC you specified is not currently associated with the hosted zone.
_VPCAssociationNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_VPCAssociationNotFound =
    _ServiceError . hasStatus 404 . hasCode "VPCAssociationNotFound"

-- | Prism for ThrottlingException' errors.
_ThrottlingException :: AWSError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
    _ServiceError . hasStatus 400 . hasCode "ThrottlingException"

-- | Prism for NoSuchChange' errors.
_NoSuchChange :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchChange = _ServiceError . hasStatus 404 . hasCode "NoSuchChange"

-- | The limits specified for a resource have been exceeded.
_LimitsExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitsExceeded = _ServiceError . hasCode "LimitsExceeded"

-- | The resource you are trying to access is unsupported on this Route 53
-- endpoint. Please consider using a newer endpoint or a tool that does so.
_IncompatibleVersion :: AWSError a => Getting (First ServiceError) a ServiceError
_IncompatibleVersion =
    _ServiceError . hasStatus 400 . hasCode "IncompatibleVersion"

-- | Prism for NoSuchHostedZone' errors.
_NoSuchHostedZone :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchHostedZone = _ServiceError . hasStatus 404 . hasCode "NoSuchHostedZone"

-- | This error indicates that you\'ve reached the maximum number of hosted
-- zones that can be created for the current AWS account. You can request
-- an increase to the limit on the
-- <http://aws.amazon.com/route53-request/ Contact Us> page.
_TooManyHostedZones :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyHostedZones =
    _ServiceError . hasStatus 400 . hasCode "TooManyHostedZones"

-- | The hosted zone you are trying to associate VPC with doesn\'t have any
-- VPC association. Route 53 currently doesn\'t support associate a VPC
-- with a public hosted zone.
_PublicZoneVPCAssociation :: AWSError a => Getting (First ServiceError) a ServiceError
_PublicZoneVPCAssociation =
    _ServiceError . hasStatus 400 . hasCode "PublicZoneVPCAssociation"

-- | Prism for ConflictingDomainExists' errors.
_ConflictingDomainExists :: AWSError a => Getting (First ServiceError) a ServiceError
_ConflictingDomainExists = _ServiceError . hasCode "ConflictingDomainExists"

-- | The VPC you are trying to disassociate from the hosted zone is the last
-- the VPC that is associated with the hosted zone. Route 53 currently
-- doesn\'t support disassociate the last VPC from the hosted zone.
_LastVPCAssociation :: AWSError a => Getting (First ServiceError) a ServiceError
_LastVPCAssociation =
    _ServiceError . hasStatus 400 . hasCode "LastVPCAssociation"

-- | There are resource records associated with this health check. Before you
-- can delete the health check, you must disassociate it from the resource
-- record sets.
_HealthCheckInUse :: AWSError a => Getting (First ServiceError) a ServiceError
_HealthCheckInUse = _ServiceError . hasStatus 400 . hasCode "HealthCheckInUse"

-- | A delegation set with the same owner and caller reference combination
-- has already been created.
_DelegationSetAlreadyCreated :: AWSError a => Getting (First ServiceError) a ServiceError
_DelegationSetAlreadyCreated =
    _ServiceError . hasCode "DelegationSetAlreadyCreated"

-- | Prism for TooManyHealthChecks' errors.
_TooManyHealthChecks :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyHealthChecks = _ServiceError . hasCode "TooManyHealthChecks"

-- | The health check you are trying to get or delete does not exist.
_NoSuchHealthCheck :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchHealthCheck =
    _ServiceError . hasStatus 404 . hasCode "NoSuchHealthCheck"

-- | The hosted zone you are trying to create already exists. Route 53
-- returns this error when a hosted zone has already been created with the
-- specified @CallerReference@.
_HostedZoneAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_HostedZoneAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "HostedZoneAlreadyExists"

-- | The hosted zone you are trying to create for your VPC_ID does not belong
-- to you. Route 53 returns this error when the VPC specified by @VPCId@
-- does not belong to you.
_InvalidVPCId :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidVPCId = _ServiceError . hasStatus 400 . hasCode "InvalidVPCId"

data ChangeAction
    = Create
    | Upsert
    | Delete
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "create" -> pure Create
        "delete" -> pure Delete
        "upsert" -> pure Upsert
        e -> fromTextError $ "Failure parsing ChangeAction from value: '" <> e
           <> "'. Accepted values: create, delete, upsert"

instance ToText ChangeAction where
    toText = \case
        Create -> "create"
        Delete -> "delete"
        Upsert -> "upsert"

instance Hashable ChangeAction
instance ToQuery ChangeAction
instance ToHeader ChangeAction

instance ToXML ChangeAction where
    toXML = toXMLText

data ChangeStatus
    = Pending
    | Insync
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ChangeStatus where
    parser = takeLowerText >>= \case
        "insync" -> pure Insync
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing ChangeStatus from value: '" <> e
           <> "'. Accepted values: insync, pending"

instance ToText ChangeStatus where
    toText = \case
        Insync -> "insync"
        Pending -> "pending"

instance Hashable ChangeStatus
instance ToQuery ChangeStatus
instance ToHeader ChangeStatus

instance FromXML ChangeStatus where
    parseXML = parseXMLText "ChangeStatus"

data Failover
    = Secondary
    | Primary
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Failover where
    parser = takeLowerText >>= \case
        "primary" -> pure Primary
        "secondary" -> pure Secondary
        e -> fromTextError $ "Failure parsing Failover from value: '" <> e
           <> "'. Accepted values: primary, secondary"

instance ToText Failover where
    toText = \case
        Primary -> "primary"
        Secondary -> "secondary"

instance Hashable Failover
instance ToQuery Failover
instance ToHeader Failover

instance FromXML Failover where
    parseXML = parseXMLText "Failover"

instance ToXML Failover where
    toXML = toXMLText

data HealthCheckType
    = HTTPS
    | TCP
    | HTTPSStrMatch
    | HTTP
    | HTTPStrMatch
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText HealthCheckType where
    parser = takeLowerText >>= \case
        "http" -> pure HTTP
        "https" -> pure HTTPS
        "https_str_match" -> pure HTTPSStrMatch
        "http_str_match" -> pure HTTPStrMatch
        "tcp" -> pure TCP
        e -> fromTextError $ "Failure parsing HealthCheckType from value: '" <> e
           <> "'. Accepted values: http, https, https_str_match, http_str_match, tcp"

instance ToText HealthCheckType where
    toText = \case
        HTTP -> "http"
        HTTPS -> "https"
        HTTPSStrMatch -> "https_str_match"
        HTTPStrMatch -> "http_str_match"
        TCP -> "tcp"

instance Hashable HealthCheckType
instance ToQuery HealthCheckType
instance ToHeader HealthCheckType

instance FromXML HealthCheckType where
    parseXML = parseXMLText "HealthCheckType"

instance ToXML HealthCheckType where
    toXML = toXMLText

data RecordType
    = Cname
    | Srv
    | MX
    | NS
    | Aaaa
    | A
    | Spf
    | Soa
    | Txt
    | Ptr
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RecordType where
    parser = takeLowerText >>= \case
        "a" -> pure A
        "aaaa" -> pure Aaaa
        "cname" -> pure Cname
        "mx" -> pure MX
        "ns" -> pure NS
        "ptr" -> pure Ptr
        "soa" -> pure Soa
        "spf" -> pure Spf
        "srv" -> pure Srv
        "txt" -> pure Txt
        e -> fromTextError $ "Failure parsing RecordType from value: '" <> e
           <> "'. Accepted values: a, aaaa, cname, mx, ns, ptr, soa, spf, srv, txt"

instance ToText RecordType where
    toText = \case
        A -> "a"
        Aaaa -> "aaaa"
        Cname -> "cname"
        MX -> "mx"
        NS -> "ns"
        Ptr -> "ptr"
        Soa -> "soa"
        Spf -> "spf"
        Srv -> "srv"
        Txt -> "txt"

instance Hashable RecordType
instance ToQuery RecordType
instance ToHeader RecordType

instance FromXML RecordType where
    parseXML = parseXMLText "RecordType"

instance ToXML RecordType where
    toXML = toXMLText

data TagResourceType
    = Healthcheck
    | Hostedzone
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TagResourceType where
    parser = takeLowerText >>= \case
        "healthcheck" -> pure Healthcheck
        "hostedzone" -> pure Hostedzone
        e -> fromTextError $ "Failure parsing TagResourceType from value: '" <> e
           <> "'. Accepted values: healthcheck, hostedzone"

instance ToText TagResourceType where
    toText = \case
        Healthcheck -> "healthcheck"
        Hostedzone -> "hostedzone"

instance Hashable TagResourceType
instance ToQuery TagResourceType
instance ToHeader TagResourceType

instance FromXML TagResourceType where
    parseXML = parseXMLText "TagResourceType"

instance ToXML TagResourceType where
    toXML = toXMLText

data VPCRegion
    = ApNortheast1
    | SaEast1
    | CnNorth1
    | UsWest2
    | UsEast1
    | EuWest1
    | EuCentral1
    | UsWest1
    | ApSoutheast2
    | ApSoutheast1
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText VPCRegion where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure ApNortheast1
        "ap-southeast-1" -> pure ApSoutheast1
        "ap-southeast-2" -> pure ApSoutheast2
        "cn-north-1" -> pure CnNorth1
        "eu-central-1" -> pure EuCentral1
        "eu-west-1" -> pure EuWest1
        "sa-east-1" -> pure SaEast1
        "us-east-1" -> pure UsEast1
        "us-west-1" -> pure UsWest1
        "us-west-2" -> pure UsWest2
        e -> fromTextError $ "Failure parsing VPCRegion from value: '" <> e
           <> "'. Accepted values: ap-northeast-1, ap-southeast-1, ap-southeast-2, cn-north-1, eu-central-1, eu-west-1, sa-east-1, us-east-1, us-west-1, us-west-2"

instance ToText VPCRegion where
    toText = \case
        ApNortheast1 -> "ap-northeast-1"
        ApSoutheast1 -> "ap-southeast-1"
        ApSoutheast2 -> "ap-southeast-2"
        CnNorth1 -> "cn-north-1"
        EuCentral1 -> "eu-central-1"
        EuWest1 -> "eu-west-1"
        SaEast1 -> "sa-east-1"
        UsEast1 -> "us-east-1"
        UsWest1 -> "us-west-1"
        UsWest2 -> "us-west-2"

instance Hashable VPCRegion
instance ToQuery VPCRegion
instance ToHeader VPCRegion

instance FromXML VPCRegion where
    parseXML = parseXMLText "VPCRegion"

instance ToXML VPCRegion where
    toXML = toXMLText

-- | /Alias resource record sets only:/ Information about the domain to which
-- you are redirecting traffic.
--
-- For more information and an example, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/CreatingAliasRRSets.html Creating Alias Resource Record Sets>
-- in the /Amazon Route 53 Developer Guide/
--
-- .
--
-- /See:/ 'aliasTarget' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atHostedZoneId'
--
-- * 'atDNSName'
--
-- * 'atEvaluateTargetHealth'
data AliasTarget = AliasTarget'
    { _atHostedZoneId         :: !Text
    , _atDNSName              :: !Text
    , _atEvaluateTargetHealth :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AliasTarget' smart constructor.
aliasTarget :: Text -> Text -> Bool -> AliasTarget
aliasTarget pHostedZoneId pDNSName pEvaluateTargetHealth =
    AliasTarget'
    { _atHostedZoneId = pHostedZoneId
    , _atDNSName = pDNSName
    , _atEvaluateTargetHealth = pEvaluateTargetHealth
    }

-- | /Alias resource record sets only:/ The value of the hosted zone ID for
-- the AWS resource.
--
-- For more information and an example, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/CreatingAliasRRSets.html Creating Alias Resource Record Sets>
-- in the /Amazon Route 53 Developer Guide/
--
-- .
atHostedZoneId :: Lens' AliasTarget Text
atHostedZoneId = lens _atHostedZoneId (\ s a -> s{_atHostedZoneId = a});

-- | /Alias resource record sets only:/ The external DNS name associated with
-- the AWS Resource.
--
-- For more information and an example, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/CreatingAliasRRSets.html Creating Alias Resource Record Sets>
-- in the /Amazon Route 53 Developer Guide/
--
-- .
atDNSName :: Lens' AliasTarget Text
atDNSName = lens _atDNSName (\ s a -> s{_atDNSName = a});

-- | /Alias resource record sets only:/ A boolean value that indicates
-- whether this Resource Record Set should respect the health status of any
-- health checks associated with the ALIAS target record which it is linked
-- to.
--
-- For more information and an example, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/CreatingAliasRRSets.html Creating Alias Resource Record Sets>
-- in the /Amazon Route 53 Developer Guide/
--
-- .
atEvaluateTargetHealth :: Lens' AliasTarget Bool
atEvaluateTargetHealth = lens _atEvaluateTargetHealth (\ s a -> s{_atEvaluateTargetHealth = a});

instance FromXML AliasTarget where
        parseXML x
          = AliasTarget' <$>
              (x .@ "HostedZoneId") <*> (x .@ "DNSName") <*>
                (x .@ "EvaluateTargetHealth")

instance ToXML AliasTarget where
        toXML AliasTarget'{..}
          = mconcat
              ["HostedZoneId" @= _atHostedZoneId,
               "DNSName" @= _atDNSName,
               "EvaluateTargetHealth" @= _atEvaluateTargetHealth]

-- | A complex type that contains the information for each change in a change
-- batch request.
--
-- /See:/ 'change' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chaAction'
--
-- * 'chaResourceRecordSet'
data Change = Change'
    { _chaAction            :: !ChangeAction
    , _chaResourceRecordSet :: !ResourceRecordSet
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'Change' smart constructor.
change :: ChangeAction -> ResourceRecordSet -> Change
change pAction pResourceRecordSet =
    Change'
    { _chaAction = pAction
    , _chaResourceRecordSet = pResourceRecordSet
    }

-- | The action to perform.
--
-- Valid values: @CREATE@ | @DELETE@ | @UPSERT@
chaAction :: Lens' Change ChangeAction
chaAction = lens _chaAction (\ s a -> s{_chaAction = a});

-- | Information about the resource record set to create or delete.
chaResourceRecordSet :: Lens' Change ResourceRecordSet
chaResourceRecordSet = lens _chaResourceRecordSet (\ s a -> s{_chaResourceRecordSet = a});

instance ToXML Change where
        toXML Change'{..}
          = mconcat
              ["Action" @= _chaAction,
               "ResourceRecordSet" @= _chaResourceRecordSet]

-- | A complex type that contains an optional comment and the changes that
-- you want to make with a change batch request.
--
-- /See:/ 'changeBatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbComment'
--
-- * 'cbChanges'
data ChangeBatch = ChangeBatch'
    { _cbComment :: !(Maybe Text)
    , _cbChanges :: !(List1 Change)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ChangeBatch' smart constructor.
changeBatch :: NonEmpty Change -> ChangeBatch
changeBatch pChanges =
    ChangeBatch'
    { _cbComment = Nothing
    , _cbChanges = _List1 # pChanges
    }

-- | /Optional:/ Any comments you want to include about a change batch
-- request.
cbComment :: Lens' ChangeBatch (Maybe Text)
cbComment = lens _cbComment (\ s a -> s{_cbComment = a});

-- | A complex type that contains one @Change@ element for each resource
-- record set that you want to create or delete.
cbChanges :: Lens' ChangeBatch (NonEmpty Change)
cbChanges = lens _cbChanges (\ s a -> s{_cbChanges = a}) . _List1;

instance ToXML ChangeBatch where
        toXML ChangeBatch'{..}
          = mconcat
              ["Comment" @= _cbComment,
               "Changes" @= toXMLList "Change" _cbChanges]

-- | A complex type that describes change information about changes made to
-- your hosted zone.
--
-- This element contains an ID that you use when performing a GetChange
-- action to get detailed information about the change.
--
-- /See:/ 'changeInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciComment'
--
-- * 'ciId'
--
-- * 'ciStatus'
--
-- * 'ciSubmittedAt'
data ChangeInfo = ChangeInfo'
    { _ciComment     :: !(Maybe Text)
    , _ciId          :: !Text
    , _ciStatus      :: !ChangeStatus
    , _ciSubmittedAt :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChangeInfo' smart constructor.
changeInfo :: Text -> ChangeStatus -> UTCTime -> ChangeInfo
changeInfo pId pStatus pSubmittedAt =
    ChangeInfo'
    { _ciComment = Nothing
    , _ciId = pId
    , _ciStatus = pStatus
    , _ciSubmittedAt = _Time # pSubmittedAt
    }

-- | A complex type that describes change information about changes made to
-- your hosted zone.
--
-- This element contains an ID that you use when performing a GetChange
-- action to get detailed information about the change.
ciComment :: Lens' ChangeInfo (Maybe Text)
ciComment = lens _ciComment (\ s a -> s{_ciComment = a});

-- | The ID of the request. Use this ID to track when the change has
-- completed across all Amazon Route 53 DNS servers.
ciId :: Lens' ChangeInfo Text
ciId = lens _ciId (\ s a -> s{_ciId = a});

-- | The current state of the request. @PENDING@ indicates that this request
-- has not yet been applied to all Amazon Route 53 DNS servers.
--
-- Valid Values: @PENDING@ | @INSYNC@
ciStatus :: Lens' ChangeInfo ChangeStatus
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a});

-- | The date and time the change was submitted, in the format
-- @YYYY-MM-DDThh:mm:ssZ@, as specified in the ISO 8601 standard (for
-- example, 2009-11-19T19:37:58Z). The @Z@ after the time indicates that
-- the time is listed in Coordinated Universal Time (UTC), which is
-- synonymous with Greenwich Mean Time in this context.
ciSubmittedAt :: Lens' ChangeInfo UTCTime
ciSubmittedAt = lens _ciSubmittedAt (\ s a -> s{_ciSubmittedAt = a}) . _Time;

instance FromXML ChangeInfo where
        parseXML x
          = ChangeInfo' <$>
              (x .@? "Comment") <*> (x .@ "Id") <*> (x .@ "Status")
                <*> (x .@ "SubmittedAt")

-- | A complex type that contains name server information.
--
-- /See:/ 'delegationSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsId'
--
-- * 'dsCallerReference'
--
-- * 'dsNameServers'
data DelegationSet = DelegationSet'
    { _dsId              :: !(Maybe Text)
    , _dsCallerReference :: !(Maybe Text)
    , _dsNameServers     :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DelegationSet' smart constructor.
delegationSet :: NonEmpty Text -> DelegationSet
delegationSet pNameServers =
    DelegationSet'
    { _dsId = Nothing
    , _dsCallerReference = Nothing
    , _dsNameServers = _List1 # pNameServers
    }

-- | FIXME: Undocumented member.
dsId :: Lens' DelegationSet (Maybe Text)
dsId = lens _dsId (\ s a -> s{_dsId = a});

-- | FIXME: Undocumented member.
dsCallerReference :: Lens' DelegationSet (Maybe Text)
dsCallerReference = lens _dsCallerReference (\ s a -> s{_dsCallerReference = a});

-- | A complex type that contains the authoritative name servers for the
-- hosted zone. Use the method provided by your domain registrar to add an
-- NS record to your domain for each @NameServer@ that is assigned to your
-- hosted zone.
dsNameServers :: Lens' DelegationSet (NonEmpty Text)
dsNameServers = lens _dsNameServers (\ s a -> s{_dsNameServers = a}) . _List1;

instance FromXML DelegationSet where
        parseXML x
          = DelegationSet' <$>
              (x .@? "Id") <*> (x .@? "CallerReference") <*>
                (x .@? "NameServers" .!@ mempty >>=
                   parseXMLList1 "NameServer")

-- | A complex type that contains information about a geo location.
--
-- /See:/ 'geoLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glSubdivisionCode'
--
-- * 'glCountryCode'
--
-- * 'glContinentCode'
data GeoLocation = GeoLocation'
    { _glSubdivisionCode :: !(Maybe Text)
    , _glCountryCode     :: !(Maybe Text)
    , _glContinentCode   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GeoLocation' smart constructor.
geoLocation :: GeoLocation
geoLocation =
    GeoLocation'
    { _glSubdivisionCode = Nothing
    , _glCountryCode = Nothing
    , _glContinentCode = Nothing
    }

-- | The code for a country\'s subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
--
-- Constraint: Specifying @SubdivisionCode@ without @CountryCode@ returns
-- an InvalidInput error.
glSubdivisionCode :: Lens' GeoLocation (Maybe Text)
glSubdivisionCode = lens _glSubdivisionCode (\ s a -> s{_glSubdivisionCode = a});

-- | The code for a country geo location. The default location uses \'*\' for
-- the country code and will match all locations that are not matched by a
-- geo location.
--
-- The default geo location uses a @*@ for the country code. All other
-- country codes follow the ISO 3166 two-character code.
glCountryCode :: Lens' GeoLocation (Maybe Text)
glCountryCode = lens _glCountryCode (\ s a -> s{_glCountryCode = a});

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code.
--
-- Valid values: @AF@ | @AN@ | @AS@ | @EU@ | @OC@ | @NA@ | @SA@
--
-- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or
-- @SubdivisionCode@ returns an InvalidInput error.
glContinentCode :: Lens' GeoLocation (Maybe Text)
glContinentCode = lens _glContinentCode (\ s a -> s{_glContinentCode = a});

instance FromXML GeoLocation where
        parseXML x
          = GeoLocation' <$>
              (x .@? "SubdivisionCode") <*> (x .@? "CountryCode")
                <*> (x .@? "ContinentCode")

instance ToXML GeoLocation where
        toXML GeoLocation'{..}
          = mconcat
              ["SubdivisionCode" @= _glSubdivisionCode,
               "CountryCode" @= _glCountryCode,
               "ContinentCode" @= _glContinentCode]

-- | A complex type that contains information about a @GeoLocation@.
--
-- /See:/ 'geoLocationDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gldSubdivisionName'
--
-- * 'gldSubdivisionCode'
--
-- * 'gldCountryName'
--
-- * 'gldCountryCode'
--
-- * 'gldContinentCode'
--
-- * 'gldContinentName'
data GeoLocationDetails = GeoLocationDetails'
    { _gldSubdivisionName :: !(Maybe Text)
    , _gldSubdivisionCode :: !(Maybe Text)
    , _gldCountryName     :: !(Maybe Text)
    , _gldCountryCode     :: !(Maybe Text)
    , _gldContinentCode   :: !(Maybe Text)
    , _gldContinentName   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GeoLocationDetails' smart constructor.
geoLocationDetails :: GeoLocationDetails
geoLocationDetails =
    GeoLocationDetails'
    { _gldSubdivisionName = Nothing
    , _gldSubdivisionCode = Nothing
    , _gldCountryName = Nothing
    , _gldCountryCode = Nothing
    , _gldContinentCode = Nothing
    , _gldContinentName = Nothing
    }

-- | The name of the subdivision. This element is only present if
-- @SubdivisionCode@ is also present.
gldSubdivisionName :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionName = lens _gldSubdivisionName (\ s a -> s{_gldSubdivisionName = a});

-- | The code for a country\'s subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
gldSubdivisionCode :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionCode = lens _gldSubdivisionCode (\ s a -> s{_gldSubdivisionCode = a});

-- | The name of the country. This element is only present if @CountryCode@
-- is also present.
gldCountryName :: Lens' GeoLocationDetails (Maybe Text)
gldCountryName = lens _gldCountryName (\ s a -> s{_gldCountryName = a});

-- | The code for a country geo location. The default location uses \'*\' for
-- the country code and will match all locations that are not matched by a
-- geo location.
--
-- The default geo location uses a @*@ for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gldCountryCode :: Lens' GeoLocationDetails (Maybe Text)
gldCountryCode = lens _gldCountryCode (\ s a -> s{_gldCountryCode = a});

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code.
gldContinentCode :: Lens' GeoLocationDetails (Maybe Text)
gldContinentCode = lens _gldContinentCode (\ s a -> s{_gldContinentCode = a});

-- | The name of the continent. This element is only present if
-- @ContinentCode@ is also present.
gldContinentName :: Lens' GeoLocationDetails (Maybe Text)
gldContinentName = lens _gldContinentName (\ s a -> s{_gldContinentName = a});

instance FromXML GeoLocationDetails where
        parseXML x
          = GeoLocationDetails' <$>
              (x .@? "SubdivisionName") <*>
                (x .@? "SubdivisionCode")
                <*> (x .@? "CountryName")
                <*> (x .@? "CountryCode")
                <*> (x .@? "ContinentCode")
                <*> (x .@? "ContinentName")

-- | A complex type that contains identifying information about the health
-- check.
--
-- /See:/ 'healthCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hcId'
--
-- * 'hcCallerReference'
--
-- * 'hcHealthCheckConfig'
--
-- * 'hcHealthCheckVersion'
data HealthCheck = HealthCheck'
    { _hcId                 :: !Text
    , _hcCallerReference    :: !Text
    , _hcHealthCheckConfig  :: !HealthCheckConfig
    , _hcHealthCheckVersion :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HealthCheck' smart constructor.
healthCheck :: Text -> Text -> HealthCheckConfig -> Natural -> HealthCheck
healthCheck pId pCallerReference pHealthCheckConfig pHealthCheckVersion =
    HealthCheck'
    { _hcId = pId
    , _hcCallerReference = pCallerReference
    , _hcHealthCheckConfig = pHealthCheckConfig
    , _hcHealthCheckVersion = _Nat # pHealthCheckVersion
    }

-- | The ID of the specified health check.
hcId :: Lens' HealthCheck Text
hcId = lens _hcId (\ s a -> s{_hcId = a});

-- | A unique string that identifies the request to create the health check.
hcCallerReference :: Lens' HealthCheck Text
hcCallerReference = lens _hcCallerReference (\ s a -> s{_hcCallerReference = a});

-- | A complex type that contains the health check configuration.
hcHealthCheckConfig :: Lens' HealthCheck HealthCheckConfig
hcHealthCheckConfig = lens _hcHealthCheckConfig (\ s a -> s{_hcHealthCheckConfig = a});

-- | The version of the health check. You can optionally pass this value in a
-- call to @UpdateHealthCheck@ to prevent overwriting another change to the
-- health check.
hcHealthCheckVersion :: Lens' HealthCheck Natural
hcHealthCheckVersion = lens _hcHealthCheckVersion (\ s a -> s{_hcHealthCheckVersion = a}) . _Nat;

instance FromXML HealthCheck where
        parseXML x
          = HealthCheck' <$>
              (x .@ "Id") <*> (x .@ "CallerReference") <*>
                (x .@ "HealthCheckConfig")
                <*> (x .@ "HealthCheckVersion")

-- | A complex type that contains the health check configuration.
--
-- /See:/ 'healthCheckConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hccIPAddress'
--
-- * 'hccFailureThreshold'
--
-- * 'hccSearchString'
--
-- * 'hccResourcePath'
--
-- * 'hccFullyQualifiedDomainName'
--
-- * 'hccRequestInterval'
--
-- * 'hccPort'
--
-- * 'hccType'
data HealthCheckConfig = HealthCheckConfig'
    { _hccIPAddress                :: !(Maybe Text)
    , _hccFailureThreshold         :: !(Maybe Nat)
    , _hccSearchString             :: !(Maybe Text)
    , _hccResourcePath             :: !(Maybe Text)
    , _hccFullyQualifiedDomainName :: !(Maybe Text)
    , _hccRequestInterval          :: !(Maybe Nat)
    , _hccPort                     :: !(Maybe Nat)
    , _hccType                     :: !HealthCheckType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HealthCheckConfig' smart constructor.
healthCheckConfig :: HealthCheckType -> HealthCheckConfig
healthCheckConfig pType =
    HealthCheckConfig'
    { _hccIPAddress = Nothing
    , _hccFailureThreshold = Nothing
    , _hccSearchString = Nothing
    , _hccResourcePath = Nothing
    , _hccFullyQualifiedDomainName = Nothing
    , _hccRequestInterval = Nothing
    , _hccPort = Nothing
    , _hccType = pType
    }

-- | IP Address of the instance being checked.
hccIPAddress :: Lens' HealthCheckConfig (Maybe Text)
hccIPAddress = lens _hccIPAddress (\ s a -> s{_hccIPAddress = a});

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Route 53 to change the current status of the endpoint from
-- unhealthy to healthy or vice versa.
--
-- Valid values are integers between 1 and 10. For more information, see
-- \"How Amazon Route 53 Determines Whether an Endpoint Is Healthy\" in the
-- Amazon Route 53 Developer Guide.
hccFailureThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccFailureThreshold = lens _hccFailureThreshold (\ s a -> s{_hccFailureThreshold = a}) . mapping _Nat;

-- | A string to search for in the body of a health check response. Required
-- for HTTP_STR_MATCH and HTTPS_STR_MATCH health checks.
hccSearchString :: Lens' HealthCheckConfig (Maybe Text)
hccSearchString = lens _hccSearchString (\ s a -> s{_hccSearchString = a});

-- | Path to ping on the instance to check the health. Required for HTTP,
-- HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH health checks, HTTP request
-- is issued to the instance on the given port and path.
hccResourcePath :: Lens' HealthCheckConfig (Maybe Text)
hccResourcePath = lens _hccResourcePath (\ s a -> s{_hccResourcePath = a});

-- | Fully qualified domain name of the instance to be health checked.
hccFullyQualifiedDomainName :: Lens' HealthCheckConfig (Maybe Text)
hccFullyQualifiedDomainName = lens _hccFullyQualifiedDomainName (\ s a -> s{_hccFullyQualifiedDomainName = a});

-- | The number of seconds between the time that Route 53 gets a response
-- from your endpoint and the time that it sends the next health-check
-- request.
--
-- Each Route 53 health checker makes requests at this interval. Valid
-- values are 10 and 30. The default value is 30.
hccRequestInterval :: Lens' HealthCheckConfig (Maybe Natural)
hccRequestInterval = lens _hccRequestInterval (\ s a -> s{_hccRequestInterval = a}) . mapping _Nat;

-- | Port on which connection will be opened to the instance to health check.
-- For HTTP and HTTP_STR_MATCH this defaults to 80 if the port is not
-- specified. For HTTPS and HTTPS_STR_MATCH this defaults to 443 if the
-- port is not specified.
hccPort :: Lens' HealthCheckConfig (Maybe Natural)
hccPort = lens _hccPort (\ s a -> s{_hccPort = a}) . mapping _Nat;

-- | The type of health check to be performed. Currently supported types are
-- TCP, HTTP, HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH.
hccType :: Lens' HealthCheckConfig HealthCheckType
hccType = lens _hccType (\ s a -> s{_hccType = a});

instance FromXML HealthCheckConfig where
        parseXML x
          = HealthCheckConfig' <$>
              (x .@? "IPAddress") <*> (x .@? "FailureThreshold")
                <*> (x .@? "SearchString")
                <*> (x .@? "ResourcePath")
                <*> (x .@? "FullyQualifiedDomainName")
                <*> (x .@? "RequestInterval")
                <*> (x .@? "Port")
                <*> (x .@ "Type")

instance ToXML HealthCheckConfig where
        toXML HealthCheckConfig'{..}
          = mconcat
              ["IPAddress" @= _hccIPAddress,
               "FailureThreshold" @= _hccFailureThreshold,
               "SearchString" @= _hccSearchString,
               "ResourcePath" @= _hccResourcePath,
               "FullyQualifiedDomainName" @=
                 _hccFullyQualifiedDomainName,
               "RequestInterval" @= _hccRequestInterval,
               "Port" @= _hccPort, "Type" @= _hccType]

-- | A complex type that contains the IP address of a Route 53 health checker
-- and the reason for the health check status.
--
-- /See:/ 'healthCheckObservation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hcoIPAddress'
--
-- * 'hcoStatusReport'
data HealthCheckObservation = HealthCheckObservation'
    { _hcoIPAddress    :: !(Maybe Text)
    , _hcoStatusReport :: !(Maybe StatusReport)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HealthCheckObservation' smart constructor.
healthCheckObservation :: HealthCheckObservation
healthCheckObservation =
    HealthCheckObservation'
    { _hcoIPAddress = Nothing
    , _hcoStatusReport = Nothing
    }

-- | The IP address of the Route 53 health checker that performed the health
-- check.
hcoIPAddress :: Lens' HealthCheckObservation (Maybe Text)
hcoIPAddress = lens _hcoIPAddress (\ s a -> s{_hcoIPAddress = a});

-- | A complex type that contains information about the health check status
-- for the current observation.
hcoStatusReport :: Lens' HealthCheckObservation (Maybe StatusReport)
hcoStatusReport = lens _hcoStatusReport (\ s a -> s{_hcoStatusReport = a});

instance FromXML HealthCheckObservation where
        parseXML x
          = HealthCheckObservation' <$>
              (x .@? "IPAddress") <*> (x .@? "StatusReport")

-- | A complex type that contain information about the specified hosted zone.
--
-- /See:/ 'hostedZone' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hzConfig'
--
-- * 'hzResourceRecordSetCount'
--
-- * 'hzId'
--
-- * 'hzName'
--
-- * 'hzCallerReference'
data HostedZone = HostedZone'
    { _hzConfig                 :: !(Maybe HostedZoneConfig)
    , _hzResourceRecordSetCount :: !(Maybe Integer)
    , _hzId                     :: !Text
    , _hzName                   :: !Text
    , _hzCallerReference        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HostedZone' smart constructor.
hostedZone :: Text -> Text -> Text -> HostedZone
hostedZone pId pName pCallerReference =
    HostedZone'
    { _hzConfig = Nothing
    , _hzResourceRecordSetCount = Nothing
    , _hzId = pId
    , _hzName = pName
    , _hzCallerReference = pCallerReference
    }

-- | A complex type that contains the @Comment@ element.
hzConfig :: Lens' HostedZone (Maybe HostedZoneConfig)
hzConfig = lens _hzConfig (\ s a -> s{_hzConfig = a});

-- | Total number of resource record sets in the hosted zone.
hzResourceRecordSetCount :: Lens' HostedZone (Maybe Integer)
hzResourceRecordSetCount = lens _hzResourceRecordSetCount (\ s a -> s{_hzResourceRecordSetCount = a});

-- | The ID of the specified hosted zone.
hzId :: Lens' HostedZone Text
hzId = lens _hzId (\ s a -> s{_hzId = a});

-- | The name of the domain. This must be a fully-specified domain, for
-- example, www.example.com. The trailing dot is optional; Route 53 assumes
-- that the domain name is fully qualified. This means that Route 53 treats
-- www.example.com (without a trailing dot) and www.example.com. (with a
-- trailing dot) as identical.
--
-- This is the name you have registered with your DNS registrar. You should
-- ask your registrar to change the authoritative name servers for your
-- domain to the set of @NameServers@ elements returned in @DelegationSet@.
hzName :: Lens' HostedZone Text
hzName = lens _hzName (\ s a -> s{_hzName = a});

-- | A unique string that identifies the request to create the hosted zone.
hzCallerReference :: Lens' HostedZone Text
hzCallerReference = lens _hzCallerReference (\ s a -> s{_hzCallerReference = a});

instance FromXML HostedZone where
        parseXML x
          = HostedZone' <$>
              (x .@? "Config") <*> (x .@? "ResourceRecordSetCount")
                <*> (x .@ "Id")
                <*> (x .@ "Name")
                <*> (x .@ "CallerReference")

-- | A complex type that contains an optional comment about your hosted zone.
-- If you don\'t want to specify a comment, you can omit the
-- @HostedZoneConfig@ and @Comment@ elements from the XML document.
--
-- /See:/ 'hostedZoneConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hzcPrivateZone'
--
-- * 'hzcComment'
data HostedZoneConfig = HostedZoneConfig'
    { _hzcPrivateZone :: !(Maybe Bool)
    , _hzcComment     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HostedZoneConfig' smart constructor.
hostedZoneConfig :: HostedZoneConfig
hostedZoneConfig =
    HostedZoneConfig'
    { _hzcPrivateZone = Nothing
    , _hzcComment = Nothing
    }

-- | A value that indicates whether this is a private hosted zone. The value
-- is returned in the response; do not specify it in the request.
hzcPrivateZone :: Lens' HostedZoneConfig (Maybe Bool)
hzcPrivateZone = lens _hzcPrivateZone (\ s a -> s{_hzcPrivateZone = a});

-- | An optional comment about your hosted zone. If you don\'t want to
-- specify a comment, you can omit the @HostedZoneConfig@ and @Comment@
-- elements from the XML document.
hzcComment :: Lens' HostedZoneConfig (Maybe Text)
hzcComment = lens _hzcComment (\ s a -> s{_hzcComment = a});

instance FromXML HostedZoneConfig where
        parseXML x
          = HostedZoneConfig' <$>
              (x .@? "PrivateZone") <*> (x .@? "Comment")

instance ToXML HostedZoneConfig where
        toXML HostedZoneConfig'{..}
          = mconcat
              ["PrivateZone" @= _hzcPrivateZone,
               "Comment" @= _hzcComment]

-- | A complex type that contains the value of the @Value@ element for the
-- current resource record set.
--
-- /See:/ 'resourceRecord' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrValue'
newtype ResourceRecord = ResourceRecord'
    { _rrValue :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResourceRecord' smart constructor.
resourceRecord :: Text -> ResourceRecord
resourceRecord pValue =
    ResourceRecord'
    { _rrValue = pValue
    }

-- | The value of the @Value@ element for the current resource record set.
rrValue :: Lens' ResourceRecord Text
rrValue = lens _rrValue (\ s a -> s{_rrValue = a});

instance FromXML ResourceRecord where
        parseXML x = ResourceRecord' <$> (x .@ "Value")

instance ToXML ResourceRecord where
        toXML ResourceRecord'{..}
          = mconcat ["Value" @= _rrValue]

-- | A complex type that contains information about the current resource
-- record set.
--
-- /See:/ 'resourceRecordSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrsTTL'
--
-- * 'rrsAliasTarget'
--
-- * 'rrsWeight'
--
-- * 'rrsSetIdentifier'
--
-- * 'rrsFailover'
--
-- * 'rrsHealthCheckId'
--
-- * 'rrsRegion'
--
-- * 'rrsGeoLocation'
--
-- * 'rrsName'
--
-- * 'rrsType'
--
-- * 'rrsResourceRecords'
data ResourceRecordSet = ResourceRecordSet'
    { _rrsTTL             :: !(Maybe Nat)
    , _rrsAliasTarget     :: !(Maybe AliasTarget)
    , _rrsWeight          :: !(Maybe Nat)
    , _rrsSetIdentifier   :: !(Maybe Text)
    , _rrsFailover        :: !(Maybe Failover)
    , _rrsHealthCheckId   :: !(Maybe Text)
    , _rrsRegion          :: !(Maybe Region)
    , _rrsGeoLocation     :: !(Maybe GeoLocation)
    , _rrsName            :: !Text
    , _rrsType            :: !RecordType
    , _rrsResourceRecords :: !(List1 ResourceRecord)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'ResourceRecordSet' smart constructor.
resourceRecordSet :: Text -> RecordType -> NonEmpty ResourceRecord -> ResourceRecordSet
resourceRecordSet pName pType pResourceRecords =
    ResourceRecordSet'
    { _rrsTTL = Nothing
    , _rrsAliasTarget = Nothing
    , _rrsWeight = Nothing
    , _rrsSetIdentifier = Nothing
    , _rrsFailover = Nothing
    , _rrsHealthCheckId = Nothing
    , _rrsRegion = Nothing
    , _rrsGeoLocation = Nothing
    , _rrsName = pName
    , _rrsType = pType
    , _rrsResourceRecords = _List1 # pResourceRecords
    }

-- | The cache time to live for the current resource record set.
rrsTTL :: Lens' ResourceRecordSet (Maybe Natural)
rrsTTL = lens _rrsTTL (\ s a -> s{_rrsTTL = a}) . mapping _Nat;

-- | /Alias resource record sets only:/ Information about the AWS resource to
-- which you are redirecting traffic.
rrsAliasTarget :: Lens' ResourceRecordSet (Maybe AliasTarget)
rrsAliasTarget = lens _rrsAliasTarget (\ s a -> s{_rrsAliasTarget = a});

-- | /Weighted resource record sets only:/ Among resource record sets that
-- have the same combination of DNS name and type, a value that determines
-- what portion of traffic for the current resource record set is routed to
-- the associated location.
rrsWeight :: Lens' ResourceRecordSet (Maybe Natural)
rrsWeight = lens _rrsWeight (\ s a -> s{_rrsWeight = a}) . mapping _Nat;

-- | /Weighted, Latency, Geo, and Failover resource record sets only:/ An
-- identifier that differentiates among multiple resource record sets that
-- have the same combination of DNS name and type.
rrsSetIdentifier :: Lens' ResourceRecordSet (Maybe Text)
rrsSetIdentifier = lens _rrsSetIdentifier (\ s a -> s{_rrsSetIdentifier = a});

-- | /Failover resource record sets only:/ Among resource record sets that
-- have the same combination of DNS name and type, a value that indicates
-- whether the current resource record set is a primary or secondary
-- resource record set. A failover set may contain at most one resource
-- record set marked as primary and one resource record set marked as
-- secondary. A resource record set marked as primary will be returned if
-- any of the following are true: (1) an associated health check is
-- passing, (2) if the resource record set is an alias with the evaluate
-- target health and at least one target resource record set is healthy,
-- (3) both the primary and secondary resource record set are failing
-- health checks or (4) there is no secondary resource record set. A
-- secondary resource record set will be returned if: (1) the primary is
-- failing a health check and either the secondary is passing a health
-- check or has no associated health check, or (2) there is no primary
-- resource record set.
--
-- Valid values: @PRIMARY@ | @SECONDARY@
rrsFailover :: Lens' ResourceRecordSet (Maybe Failover)
rrsFailover = lens _rrsFailover (\ s a -> s{_rrsFailover = a});

-- | /Health Check resource record sets only, not required for alias resource
-- record sets:/ An identifier that is used to identify health check
-- associated with the resource record set.
rrsHealthCheckId :: Lens' ResourceRecordSet (Maybe Text)
rrsHealthCheckId = lens _rrsHealthCheckId (\ s a -> s{_rrsHealthCheckId = a});

-- | /Latency-based resource record sets only:/ Among resource record sets
-- that have the same combination of DNS name and type, a value that
-- specifies the AWS region for the current resource record set.
rrsRegion :: Lens' ResourceRecordSet (Maybe Region)
rrsRegion = lens _rrsRegion (\ s a -> s{_rrsRegion = a});

-- | /Geo location resource record sets only:/ Among resource record sets
-- that have the same combination of DNS name and type, a value that
-- specifies the geo location for the current resource record set.
rrsGeoLocation :: Lens' ResourceRecordSet (Maybe GeoLocation)
rrsGeoLocation = lens _rrsGeoLocation (\ s a -> s{_rrsGeoLocation = a});

-- | The domain name of the current resource record set.
rrsName :: Lens' ResourceRecordSet Text
rrsName = lens _rrsName (\ s a -> s{_rrsName = a});

-- | The type of the current resource record set.
rrsType :: Lens' ResourceRecordSet RecordType
rrsType = lens _rrsType (\ s a -> s{_rrsType = a});

-- | A complex type that contains the resource records for the current
-- resource record set.
rrsResourceRecords :: Lens' ResourceRecordSet (NonEmpty ResourceRecord)
rrsResourceRecords = lens _rrsResourceRecords (\ s a -> s{_rrsResourceRecords = a}) . _List1;

instance FromXML ResourceRecordSet where
        parseXML x
          = ResourceRecordSet' <$>
              (x .@? "TTL") <*> (x .@? "AliasTarget") <*>
                (x .@? "Weight")
                <*> (x .@? "SetIdentifier")
                <*> (x .@? "Failover")
                <*> (x .@? "HealthCheckId")
                <*> (x .@? "Region")
                <*> (x .@? "GeoLocation")
                <*> (x .@ "Name")
                <*> (x .@ "Type")
                <*>
                (x .@? "ResourceRecords" .!@ mempty >>=
                   parseXMLList1 "ResourceRecord")

instance ToXML ResourceRecordSet where
        toXML ResourceRecordSet'{..}
          = mconcat
              ["TTL" @= _rrsTTL, "AliasTarget" @= _rrsAliasTarget,
               "Weight" @= _rrsWeight,
               "SetIdentifier" @= _rrsSetIdentifier,
               "Failover" @= _rrsFailover,
               "HealthCheckId" @= _rrsHealthCheckId,
               "Region" @= _rrsRegion,
               "GeoLocation" @= _rrsGeoLocation, "Name" @= _rrsName,
               "Type" @= _rrsType,
               "ResourceRecords" @=
                 toXMLList "ResourceRecord" _rrsResourceRecords]

-- | A complex type containing a resource and its associated tags.
--
-- /See:/ 'resourceTagSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtsResourceId'
--
-- * 'rtsResourceType'
--
-- * 'rtsTags'
data ResourceTagSet = ResourceTagSet'
    { _rtsResourceId   :: !(Maybe Text)
    , _rtsResourceType :: !(Maybe TagResourceType)
    , _rtsTags         :: !(Maybe (List1 Tag))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResourceTagSet' smart constructor.
resourceTagSet :: ResourceTagSet
resourceTagSet =
    ResourceTagSet'
    { _rtsResourceId = Nothing
    , _rtsResourceType = Nothing
    , _rtsTags = Nothing
    }

-- | The ID for the specified resource.
rtsResourceId :: Lens' ResourceTagSet (Maybe Text)
rtsResourceId = lens _rtsResourceId (\ s a -> s{_rtsResourceId = a});

-- | The type of the resource.
--
-- - The resource type for health checks is @healthcheck@.
--
-- - The resource type for hosted zones is @hostedzone@.
rtsResourceType :: Lens' ResourceTagSet (Maybe TagResourceType)
rtsResourceType = lens _rtsResourceType (\ s a -> s{_rtsResourceType = a});

-- | The tags associated with the specified resource.
rtsTags :: Lens' ResourceTagSet (Maybe (NonEmpty Tag))
rtsTags = lens _rtsTags (\ s a -> s{_rtsTags = a}) . mapping _List1;

instance FromXML ResourceTagSet where
        parseXML x
          = ResourceTagSet' <$>
              (x .@? "ResourceId") <*> (x .@? "ResourceType") <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList1 "Tag"))

-- | A complex type that contains information about the health check status
-- for the current observation.
--
-- /See:/ 'statusReport' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srStatus'
--
-- * 'srCheckedTime'
data StatusReport = StatusReport'
    { _srStatus      :: !(Maybe Text)
    , _srCheckedTime :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StatusReport' smart constructor.
statusReport :: StatusReport
statusReport =
    StatusReport'
    { _srStatus = Nothing
    , _srCheckedTime = Nothing
    }

-- | The observed health check status.
srStatus :: Lens' StatusReport (Maybe Text)
srStatus = lens _srStatus (\ s a -> s{_srStatus = a});

-- | The date and time the health check status was observed, in the format
-- @YYYY-MM-DDThh:mm:ssZ@, as specified in the ISO 8601 standard (for
-- example, 2009-11-19T19:37:58Z). The @Z@ after the time indicates that
-- the time is listed in Coordinated Universal Time (UTC), which is
-- synonymous with Greenwich Mean Time in this context.
srCheckedTime :: Lens' StatusReport (Maybe UTCTime)
srCheckedTime = lens _srCheckedTime (\ s a -> s{_srCheckedTime = a}) . mapping _Time;

instance FromXML StatusReport where
        parseXML x
          = StatusReport' <$>
              (x .@? "Status") <*> (x .@? "CheckedTime")

-- | A single tag containing a key and value.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Tag' smart constructor.
tag :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | The value for a @Tag@.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key for a @Tag@.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Value" @= _tagValue, "Key" @= _tagKey]

-- | /See:/ 'vpc' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcVPCRegion'
--
-- * 'vpcVPCId'
data VPC = VPC'
    { _vpcVPCRegion :: !(Maybe VPCRegion)
    , _vpcVPCId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VPC' smart constructor.
vpc :: VPC
vpc =
    VPC'
    { _vpcVPCRegion = Nothing
    , _vpcVPCId = Nothing
    }

-- | FIXME: Undocumented member.
vpcVPCRegion :: Lens' VPC (Maybe VPCRegion)
vpcVPCRegion = lens _vpcVPCRegion (\ s a -> s{_vpcVPCRegion = a});

-- | FIXME: Undocumented member.
vpcVPCId :: Lens' VPC (Maybe Text)
vpcVPCId = lens _vpcVPCId (\ s a -> s{_vpcVPCId = a});

instance FromXML VPC where
        parseXML x
          = VPC' <$> (x .@? "VPCRegion") <*> (x .@? "VPCId")

instance ToXML VPC where
        toXML VPC'{..}
          = mconcat
              ["VPCRegion" @= _vpcVPCRegion, "VPCId" @= _vpcVPCId]
