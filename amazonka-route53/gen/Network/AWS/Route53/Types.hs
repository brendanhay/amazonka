{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Route53.Types
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

module Network.AWS.Route53.Types
    (
    -- * Service
      Route53
    -- ** Errors
    , RESTError

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

    -- * ChangeAction
    , ChangeAction (..)

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

    -- * ChangeStatus
    , ChangeStatus (..)

    -- * DelegationSet
    , DelegationSet
    , delegationSet
    , dsId
    , dsNameServers
    , dsCallerReference

    -- * Failover
    , Failover (..)

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
    , hccSearchString
    , hccResourcePath
    , hccFullyQualifiedDomainName
    , hccType
    , hccFailureThreshold
    , hccRequestInterval
    , hccPort

    -- * HealthCheckObservation
    , HealthCheckObservation
    , healthCheckObservation
    , hcoIPAddress
    , hcoStatusReport

    -- * HealthCheckType
    , HealthCheckType (..)

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

    -- * RecordType
    , RecordType (..)

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
    , rrsFailover
    , rrsHealthCheckId
    , rrsGeoLocation
    , rrsName
    , rrsType
    , rrsResourceRecords
    , rrsSetIdentifier
    , rrsRegion

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

    -- * TagResourceType
    , TagResourceType (..)

    -- * VPC
    , VPC
    , vpc
    , vpcVPCId
    , vpcVPCRegion

    -- * VPCRegion
    , VPCRegion (..)

    , module Network.AWS.Route53.Internal
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Route53.Internal

-- | Version @2013-04-01@ of the Amazon Route 53 SDK.
data Route53

instance AWSService Route53 where
    type Sg Route53 = V4
    type Er Route53 = RESTError

    service = service'
      where
        service' :: Service Route53
        service' = Service
            { _svcAbbrev  = "Route53"
            , _svcPrefix  = "route53"
            , _svcVersion = "2013-04-01"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry Route53
        retry = undefined

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'aliasTarget' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atHostedZoneId'
--
-- * 'atDNSName'
--
-- * 'atEvaluateTargetHealth'
data AliasTarget = AliasTarget'{_atHostedZoneId :: Text, _atDNSName :: Text, _atEvaluateTargetHealth :: Bool} deriving (Eq, Read, Show)

-- | 'AliasTarget' smart constructor.
aliasTarget :: Text -> Text -> Bool -> AliasTarget
aliasTarget pHostedZoneId pDNSName pEvaluateTargetHealth = AliasTarget'{_atHostedZoneId = pHostedZoneId, _atDNSName = pDNSName, _atEvaluateTargetHealth = pEvaluateTargetHealth};

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
              x .@ "HostedZoneId" <*> x .@ "DNSName" <*>
                x .@ "EvaluateTargetHealth"

instance ToXML AliasTarget where
        toXML AliasTarget'{..}
          = mconcat
              ["HostedZoneId" @= _atHostedZoneId,
               "DNSName" @= _atDNSName,
               "EvaluateTargetHealth" @= _atEvaluateTargetHealth]

-- | /See:/ 'change' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chaAction'
--
-- * 'chaResourceRecordSet'
data Change = Change'{_chaAction :: ChangeAction, _chaResourceRecordSet :: ResourceRecordSet} deriving (Eq, Read, Show)

-- | 'Change' smart constructor.
change :: ChangeAction -> ResourceRecordSet -> Change
change pAction pResourceRecordSet = Change'{_chaAction = pAction, _chaResourceRecordSet = pResourceRecordSet};

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

data ChangeAction = Create | Upsert | Delete deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "CREATE" -> pure Create
        "DELETE" -> pure Delete
        "UPSERT" -> pure Upsert
        e -> fail ("Failure parsing ChangeAction from " ++ show e)

instance ToText ChangeAction where
    toText = \case
        Create -> "CREATE"
        Delete -> "DELETE"
        Upsert -> "UPSERT"

instance Hashable ChangeAction
instance ToQuery ChangeAction
instance ToHeader ChangeAction

instance ToXML ChangeAction where
    toXML = toXMLText

-- | /See:/ 'changeBatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbComment'
--
-- * 'cbChanges'
data ChangeBatch = ChangeBatch'{_cbComment :: Maybe Text, _cbChanges :: List1 Change} deriving (Eq, Read, Show)

-- | 'ChangeBatch' smart constructor.
changeBatch :: NonEmpty Change -> ChangeBatch
changeBatch pChanges = ChangeBatch'{_cbComment = Nothing, _cbChanges = _List1 # pChanges};

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
               "Changes" @= "Change" @@= _cbChanges]

-- | /See:/ 'changeInfo' smart constructor.
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
data ChangeInfo = ChangeInfo'{_ciComment :: Maybe Text, _ciId :: Text, _ciStatus :: ChangeStatus, _ciSubmittedAt :: ISO8601} deriving (Eq, Read, Show)

-- | 'ChangeInfo' smart constructor.
changeInfo :: Text -> ChangeStatus -> UTCTime -> ChangeInfo
changeInfo pId pStatus pSubmittedAt = ChangeInfo'{_ciComment = Nothing, _ciId = pId, _ciStatus = pStatus, _ciSubmittedAt = _Time # pSubmittedAt};

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
              x .@? "Comment" <*> x .@ "Id" <*> x .@ "Status" <*>
                x .@ "SubmittedAt"

data ChangeStatus = Pending | Insync deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ChangeStatus where
    parser = takeLowerText >>= \case
        "INSYNC" -> pure Insync
        "PENDING" -> pure Pending
        e -> fail ("Failure parsing ChangeStatus from " ++ show e)

instance ToText ChangeStatus where
    toText = \case
        Insync -> "INSYNC"
        Pending -> "PENDING"

instance Hashable ChangeStatus
instance ToQuery ChangeStatus
instance ToHeader ChangeStatus

instance FromXML ChangeStatus where
    parseXML = parseXMLText "ChangeStatus"

-- | /See:/ 'delegationSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsId'
--
-- * 'dsNameServers'
--
-- * 'dsCallerReference'
data DelegationSet = DelegationSet'{_dsId :: Maybe Text, _dsNameServers :: List1 Text, _dsCallerReference :: Text} deriving (Eq, Read, Show)

-- | 'DelegationSet' smart constructor.
delegationSet :: NonEmpty Text -> Text -> DelegationSet
delegationSet pNameServers pCallerReference = DelegationSet'{_dsId = Nothing, _dsNameServers = _List1 # pNameServers, _dsCallerReference = pCallerReference};

-- | FIXME: Undocumented member.
dsId :: Lens' DelegationSet (Maybe Text)
dsId = lens _dsId (\ s a -> s{_dsId = a});

-- | A complex type that contains the authoritative name servers for the
-- hosted zone. Use the method provided by your domain registrar to add an
-- NS record to your domain for each @NameServer@ that is assigned to your
-- hosted zone.
dsNameServers :: Lens' DelegationSet (NonEmpty Text)
dsNameServers = lens _dsNameServers (\ s a -> s{_dsNameServers = a}) . _List1;

-- | FIXME: Undocumented member.
dsCallerReference :: Lens' DelegationSet Text
dsCallerReference = lens _dsCallerReference (\ s a -> s{_dsCallerReference = a});

instance FromXML DelegationSet where
        parseXML x
          = DelegationSet' <$>
              x .@? "Id" <*>
                (x .@? "NameServers" .!@ mempty >>=
                   parseXMLList1 "NameServer")
                <*> x .@ "CallerReference"

data Failover = Secondary | Primary deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText Failover where
    parser = takeLowerText >>= \case
        "PRIMARY" -> pure Primary
        "SECONDARY" -> pure Secondary
        e -> fail ("Failure parsing Failover from " ++ show e)

instance ToText Failover where
    toText = \case
        Primary -> "PRIMARY"
        Secondary -> "SECONDARY"

instance Hashable Failover
instance ToQuery Failover
instance ToHeader Failover

instance FromXML Failover where
    parseXML = parseXMLText "Failover"

instance ToXML Failover where
    toXML = toXMLText

-- | /See:/ 'geoLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glSubdivisionCode'
--
-- * 'glCountryCode'
--
-- * 'glContinentCode'
data GeoLocation = GeoLocation'{_glSubdivisionCode :: Text, _glCountryCode :: Text, _glContinentCode :: Text} deriving (Eq, Read, Show)

-- | 'GeoLocation' smart constructor.
geoLocation :: Text -> Text -> Text -> GeoLocation
geoLocation pSubdivisionCode pCountryCode pContinentCode = GeoLocation'{_glSubdivisionCode = pSubdivisionCode, _glCountryCode = pCountryCode, _glContinentCode = pContinentCode};

-- | The code for a country\'s subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
--
-- Constraint: Specifying @SubdivisionCode@ without @CountryCode@ returns
-- an InvalidInput error.
glSubdivisionCode :: Lens' GeoLocation Text
glSubdivisionCode = lens _glSubdivisionCode (\ s a -> s{_glSubdivisionCode = a});

-- | The code for a country geo location. The default location uses \'*\' for
-- the country code and will match all locations that are not matched by a
-- geo location.
--
-- The default geo location uses a @*@ for the country code. All other
-- country codes follow the ISO 3166 two-character code.
glCountryCode :: Lens' GeoLocation Text
glCountryCode = lens _glCountryCode (\ s a -> s{_glCountryCode = a});

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code.
--
-- Valid values: @AF@ | @AN@ | @AS@ | @EU@ | @OC@ | @NA@ | @SA@
--
-- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or
-- @SubdivisionCode@ returns an InvalidInput error.
glContinentCode :: Lens' GeoLocation Text
glContinentCode = lens _glContinentCode (\ s a -> s{_glContinentCode = a});

instance FromXML GeoLocation where
        parseXML x
          = GeoLocation' <$>
              x .@ "SubdivisionCode" <*> x .@ "CountryCode" <*>
                x .@ "ContinentCode"

instance ToXML GeoLocation where
        toXML GeoLocation'{..}
          = mconcat
              ["SubdivisionCode" @= _glSubdivisionCode,
               "CountryCode" @= _glCountryCode,
               "ContinentCode" @= _glContinentCode]

-- | /See:/ 'geoLocationDetails' smart constructor.
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
data GeoLocationDetails = GeoLocationDetails'{_gldSubdivisionName :: Text, _gldSubdivisionCode :: Text, _gldCountryName :: Text, _gldCountryCode :: Text, _gldContinentCode :: Text, _gldContinentName :: Text} deriving (Eq, Read, Show)

-- | 'GeoLocationDetails' smart constructor.
geoLocationDetails :: Text -> Text -> Text -> Text -> Text -> Text -> GeoLocationDetails
geoLocationDetails pSubdivisionName pSubdivisionCode pCountryName pCountryCode pContinentCode pContinentName = GeoLocationDetails'{_gldSubdivisionName = pSubdivisionName, _gldSubdivisionCode = pSubdivisionCode, _gldCountryName = pCountryName, _gldCountryCode = pCountryCode, _gldContinentCode = pContinentCode, _gldContinentName = pContinentName};

-- | The name of the subdivision. This element is only present if
-- @SubdivisionCode@ is also present.
gldSubdivisionName :: Lens' GeoLocationDetails Text
gldSubdivisionName = lens _gldSubdivisionName (\ s a -> s{_gldSubdivisionName = a});

-- | The code for a country\'s subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
gldSubdivisionCode :: Lens' GeoLocationDetails Text
gldSubdivisionCode = lens _gldSubdivisionCode (\ s a -> s{_gldSubdivisionCode = a});

-- | The name of the country. This element is only present if @CountryCode@
-- is also present.
gldCountryName :: Lens' GeoLocationDetails Text
gldCountryName = lens _gldCountryName (\ s a -> s{_gldCountryName = a});

-- | The code for a country geo location. The default location uses \'*\' for
-- the country code and will match all locations that are not matched by a
-- geo location.
--
-- The default geo location uses a @*@ for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gldCountryCode :: Lens' GeoLocationDetails Text
gldCountryCode = lens _gldCountryCode (\ s a -> s{_gldCountryCode = a});

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code.
gldContinentCode :: Lens' GeoLocationDetails Text
gldContinentCode = lens _gldContinentCode (\ s a -> s{_gldContinentCode = a});

-- | The name of the continent. This element is only present if
-- @ContinentCode@ is also present.
gldContinentName :: Lens' GeoLocationDetails Text
gldContinentName = lens _gldContinentName (\ s a -> s{_gldContinentName = a});

instance FromXML GeoLocationDetails where
        parseXML x
          = GeoLocationDetails' <$>
              x .@ "SubdivisionName" <*> x .@ "SubdivisionCode" <*>
                x .@ "CountryName"
                <*> x .@ "CountryCode"
                <*> x .@ "ContinentCode"
                <*> x .@ "ContinentName"

-- | /See:/ 'healthCheck' smart constructor.
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
data HealthCheck = HealthCheck'{_hcId :: Text, _hcCallerReference :: Text, _hcHealthCheckConfig :: HealthCheckConfig, _hcHealthCheckVersion :: Nat} deriving (Eq, Read, Show)

-- | 'HealthCheck' smart constructor.
healthCheck :: Text -> Text -> HealthCheckConfig -> Natural -> HealthCheck
healthCheck pId pCallerReference pHealthCheckConfig pHealthCheckVersion = HealthCheck'{_hcId = pId, _hcCallerReference = pCallerReference, _hcHealthCheckConfig = pHealthCheckConfig, _hcHealthCheckVersion = _Nat # pHealthCheckVersion};

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
              x .@ "Id" <*> x .@ "CallerReference" <*>
                x .@ "HealthCheckConfig"
                <*> x .@ "HealthCheckVersion"

-- | /See:/ 'healthCheckConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hccIPAddress'
--
-- * 'hccSearchString'
--
-- * 'hccResourcePath'
--
-- * 'hccFullyQualifiedDomainName'
--
-- * 'hccType'
--
-- * 'hccFailureThreshold'
--
-- * 'hccRequestInterval'
--
-- * 'hccPort'
data HealthCheckConfig = HealthCheckConfig'{_hccIPAddress :: Maybe Text, _hccSearchString :: Maybe Text, _hccResourcePath :: Maybe Text, _hccFullyQualifiedDomainName :: Maybe Text, _hccType :: HealthCheckType, _hccFailureThreshold :: Nat, _hccRequestInterval :: Nat, _hccPort :: Nat} deriving (Eq, Read, Show)

-- | 'HealthCheckConfig' smart constructor.
healthCheckConfig :: HealthCheckType -> Natural -> Natural -> Natural -> HealthCheckConfig
healthCheckConfig pType' pFailureThreshold pRequestInterval pPort = HealthCheckConfig'{_hccIPAddress = Nothing, _hccSearchString = Nothing, _hccResourcePath = Nothing, _hccFullyQualifiedDomainName = Nothing, _hccType = pType', _hccFailureThreshold = _Nat # pFailureThreshold, _hccRequestInterval = _Nat # pRequestInterval, _hccPort = _Nat # pPort};

-- | IP Address of the instance being checked.
hccIPAddress :: Lens' HealthCheckConfig (Maybe Text)
hccIPAddress = lens _hccIPAddress (\ s a -> s{_hccIPAddress = a});

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

-- | The type of health check to be performed. Currently supported types are
-- TCP, HTTP, HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH.
hccType :: Lens' HealthCheckConfig HealthCheckType
hccType = lens _hccType (\ s a -> s{_hccType = a});

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Route 53 to change the current status of the endpoint from
-- unhealthy to healthy or vice versa.
--
-- Valid values are integers between 1 and 10. For more information, see
-- \"How Amazon Route 53 Determines Whether an Endpoint Is Healthy\" in the
-- Amazon Route 53 Developer Guide.
hccFailureThreshold :: Lens' HealthCheckConfig Natural
hccFailureThreshold = lens _hccFailureThreshold (\ s a -> s{_hccFailureThreshold = a}) . _Nat;

-- | The number of seconds between the time that Route 53 gets a response
-- from your endpoint and the time that it sends the next health-check
-- request.
--
-- Each Route 53 health checker makes requests at this interval. Valid
-- values are 10 and 30. The default value is 30.
hccRequestInterval :: Lens' HealthCheckConfig Natural
hccRequestInterval = lens _hccRequestInterval (\ s a -> s{_hccRequestInterval = a}) . _Nat;

-- | Port on which connection will be opened to the instance to health check.
-- For HTTP and HTTP_STR_MATCH this defaults to 80 if the port is not
-- specified. For HTTPS and HTTPS_STR_MATCH this defaults to 443 if the
-- port is not specified.
hccPort :: Lens' HealthCheckConfig Natural
hccPort = lens _hccPort (\ s a -> s{_hccPort = a}) . _Nat;

instance FromXML HealthCheckConfig where
        parseXML x
          = HealthCheckConfig' <$>
              x .@? "IPAddress" <*> x .@? "SearchString" <*>
                x .@? "ResourcePath"
                <*> x .@? "FullyQualifiedDomainName"
                <*> x .@ "Type"
                <*> x .@ "FailureThreshold"
                <*> x .@ "RequestInterval"
                <*> x .@ "Port"

instance ToXML HealthCheckConfig where
        toXML HealthCheckConfig'{..}
          = mconcat
              ["IPAddress" @= _hccIPAddress,
               "SearchString" @= _hccSearchString,
               "ResourcePath" @= _hccResourcePath,
               "FullyQualifiedDomainName" @=
                 _hccFullyQualifiedDomainName,
               "Type" @= _hccType,
               "FailureThreshold" @= _hccFailureThreshold,
               "RequestInterval" @= _hccRequestInterval,
               "Port" @= _hccPort]

-- | /See:/ 'healthCheckObservation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hcoIPAddress'
--
-- * 'hcoStatusReport'
data HealthCheckObservation = HealthCheckObservation'{_hcoIPAddress :: Maybe Text, _hcoStatusReport :: Maybe StatusReport} deriving (Eq, Read, Show)

-- | 'HealthCheckObservation' smart constructor.
healthCheckObservation :: HealthCheckObservation
healthCheckObservation = HealthCheckObservation'{_hcoIPAddress = Nothing, _hcoStatusReport = Nothing};

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
              x .@? "IPAddress" <*> x .@? "StatusReport"

data HealthCheckType = HTTPS | TCP | HTTPSStrMatch | HTTP | HTTPStrMatch deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText HealthCheckType where
    parser = takeLowerText >>= \case
        "HTTP" -> pure HTTP
        "HTTPS" -> pure HTTPS
        "HTTPS_STR_MATCH" -> pure HTTPSStrMatch
        "HTTP_STR_MATCH" -> pure HTTPStrMatch
        "TCP" -> pure TCP
        e -> fail ("Failure parsing HealthCheckType from " ++ show e)

instance ToText HealthCheckType where
    toText = \case
        HTTP -> "HTTP"
        HTTPS -> "HTTPS"
        HTTPSStrMatch -> "HTTPS_STR_MATCH"
        HTTPStrMatch -> "HTTP_STR_MATCH"
        TCP -> "TCP"

instance Hashable HealthCheckType
instance ToQuery HealthCheckType
instance ToHeader HealthCheckType

instance FromXML HealthCheckType where
    parseXML = parseXMLText "HealthCheckType"

instance ToXML HealthCheckType where
    toXML = toXMLText

-- | /See:/ 'hostedZone' smart constructor.
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
data HostedZone = HostedZone'{_hzConfig :: Maybe HostedZoneConfig, _hzResourceRecordSetCount :: Maybe Integer, _hzId :: Text, _hzName :: Text, _hzCallerReference :: Text} deriving (Eq, Read, Show)

-- | 'HostedZone' smart constructor.
hostedZone :: Text -> Text -> Text -> HostedZone
hostedZone pId pName pCallerReference = HostedZone'{_hzConfig = Nothing, _hzResourceRecordSetCount = Nothing, _hzId = pId, _hzName = pName, _hzCallerReference = pCallerReference};

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
              x .@? "Config" <*> x .@? "ResourceRecordSetCount" <*>
                x .@ "Id"
                <*> x .@ "Name"
                <*> x .@ "CallerReference"

-- | /See:/ 'hostedZoneConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hzcPrivateZone'
--
-- * 'hzcComment'
data HostedZoneConfig = HostedZoneConfig'{_hzcPrivateZone :: Maybe Bool, _hzcComment :: Maybe Text} deriving (Eq, Read, Show)

-- | 'HostedZoneConfig' smart constructor.
hostedZoneConfig :: HostedZoneConfig
hostedZoneConfig = HostedZoneConfig'{_hzcPrivateZone = Nothing, _hzcComment = Nothing};

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
              x .@? "PrivateZone" <*> x .@? "Comment"

instance ToXML HostedZoneConfig where
        toXML HostedZoneConfig'{..}
          = mconcat
              ["PrivateZone" @= _hzcPrivateZone,
               "Comment" @= _hzcComment]

data RecordType = Cname | Srv | MX | NS | Aaaa | A | Spf | Soa | Txt | Ptr deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RecordType where
    parser = takeLowerText >>= \case
        "A" -> pure A
        "AAAA" -> pure Aaaa
        "CNAME" -> pure Cname
        "MX" -> pure MX
        "NS" -> pure NS
        "PTR" -> pure Ptr
        "SOA" -> pure Soa
        "SPF" -> pure Spf
        "SRV" -> pure Srv
        "TXT" -> pure Txt
        e -> fail ("Failure parsing RecordType from " ++ show e)

instance ToText RecordType where
    toText = \case
        A -> "A"
        Aaaa -> "AAAA"
        Cname -> "CNAME"
        MX -> "MX"
        NS -> "NS"
        Ptr -> "PTR"
        Soa -> "SOA"
        Spf -> "SPF"
        Srv -> "SRV"
        Txt -> "TXT"

instance Hashable RecordType
instance ToQuery RecordType
instance ToHeader RecordType

instance FromXML RecordType where
    parseXML = parseXMLText "RecordType"

instance ToXML RecordType where
    toXML = toXMLText

-- | /See:/ 'resourceRecord' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrValue'
newtype ResourceRecord = ResourceRecord'{_rrValue :: Text} deriving (Eq, Read, Show)

-- | 'ResourceRecord' smart constructor.
resourceRecord :: Text -> ResourceRecord
resourceRecord pValue = ResourceRecord'{_rrValue = pValue};

-- | The value of the @Value@ element for the current resource record set.
rrValue :: Lens' ResourceRecord Text
rrValue = lens _rrValue (\ s a -> s{_rrValue = a});

instance FromXML ResourceRecord where
        parseXML x = ResourceRecord' <$> x .@ "Value"

instance ToXML ResourceRecord where
        toXML ResourceRecord'{..}
          = mconcat ["Value" @= _rrValue]

-- | /See:/ 'resourceRecordSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrsTTL'
--
-- * 'rrsAliasTarget'
--
-- * 'rrsWeight'
--
-- * 'rrsFailover'
--
-- * 'rrsHealthCheckId'
--
-- * 'rrsGeoLocation'
--
-- * 'rrsName'
--
-- * 'rrsType'
--
-- * 'rrsResourceRecords'
--
-- * 'rrsSetIdentifier'
--
-- * 'rrsRegion'
data ResourceRecordSet = ResourceRecordSet'{_rrsTTL :: Maybe Nat, _rrsAliasTarget :: Maybe AliasTarget, _rrsWeight :: Maybe Nat, _rrsFailover :: Maybe Failover, _rrsHealthCheckId :: Maybe Text, _rrsGeoLocation :: Maybe GeoLocation, _rrsName :: Text, _rrsType :: RecordType, _rrsResourceRecords :: List1 ResourceRecord, _rrsSetIdentifier :: Text, _rrsRegion :: Region} deriving (Eq, Read, Show)

-- | 'ResourceRecordSet' smart constructor.
resourceRecordSet :: Text -> RecordType -> NonEmpty ResourceRecord -> Text -> Region -> ResourceRecordSet
resourceRecordSet pName pType' pResourceRecords pSetIdentifier pRegion = ResourceRecordSet'{_rrsTTL = Nothing, _rrsAliasTarget = Nothing, _rrsWeight = Nothing, _rrsFailover = Nothing, _rrsHealthCheckId = Nothing, _rrsGeoLocation = Nothing, _rrsName = pName, _rrsType = pType', _rrsResourceRecords = _List1 # pResourceRecords, _rrsSetIdentifier = pSetIdentifier, _rrsRegion = pRegion};

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

-- | /Weighted, Latency, Geo, and Failover resource record sets only:/ An
-- identifier that differentiates among multiple resource record sets that
-- have the same combination of DNS name and type.
rrsSetIdentifier :: Lens' ResourceRecordSet Text
rrsSetIdentifier = lens _rrsSetIdentifier (\ s a -> s{_rrsSetIdentifier = a});

-- | /Latency-based resource record sets only:/ Among resource record sets
-- that have the same combination of DNS name and type, a value that
-- specifies the AWS region for the current resource record set.
rrsRegion :: Lens' ResourceRecordSet Region
rrsRegion = lens _rrsRegion (\ s a -> s{_rrsRegion = a});

instance FromXML ResourceRecordSet where
        parseXML x
          = ResourceRecordSet' <$>
              x .@? "TTL" <*> x .@? "AliasTarget" <*>
                x .@? "Weight"
                <*> x .@? "Failover"
                <*> x .@? "HealthCheckId"
                <*> x .@? "GeoLocation"
                <*> x .@ "Name"
                <*> x .@ "Type"
                <*>
                (x .@? "ResourceRecords" .!@ mempty >>=
                   parseXMLList1 "ResourceRecord")
                <*> x .@ "SetIdentifier"
                <*> x .@ "Region"

instance ToXML ResourceRecordSet where
        toXML ResourceRecordSet'{..}
          = mconcat
              ["TTL" @= _rrsTTL, "AliasTarget" @= _rrsAliasTarget,
               "Weight" @= _rrsWeight, "Failover" @= _rrsFailover,
               "HealthCheckId" @= _rrsHealthCheckId,
               "GeoLocation" @= _rrsGeoLocation, "Name" @= _rrsName,
               "Type" @= _rrsType,
               "ResourceRecords" @=
                 "ResourceRecord" @@= _rrsResourceRecords,
               "SetIdentifier" @= _rrsSetIdentifier,
               "Region" @= _rrsRegion]

-- | /See:/ 'resourceTagSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtsResourceId'
--
-- * 'rtsResourceType'
--
-- * 'rtsTags'
data ResourceTagSet = ResourceTagSet'{_rtsResourceId :: Maybe Text, _rtsResourceType :: Maybe TagResourceType, _rtsTags :: List1 Tag} deriving (Eq, Read, Show)

-- | 'ResourceTagSet' smart constructor.
resourceTagSet :: NonEmpty Tag -> ResourceTagSet
resourceTagSet pTags = ResourceTagSet'{_rtsResourceId = Nothing, _rtsResourceType = Nothing, _rtsTags = _List1 # pTags};

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
rtsTags :: Lens' ResourceTagSet (NonEmpty Tag)
rtsTags = lens _rtsTags (\ s a -> s{_rtsTags = a}) . _List1;

instance FromXML ResourceTagSet where
        parseXML x
          = ResourceTagSet' <$>
              x .@? "ResourceId" <*> x .@? "ResourceType" <*>
                (x .@? "Tags" .!@ mempty >>= parseXMLList1 "Tag")

-- | /See:/ 'statusReport' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srStatus'
--
-- * 'srCheckedTime'
data StatusReport = StatusReport'{_srStatus :: Maybe Text, _srCheckedTime :: Maybe ISO8601} deriving (Eq, Read, Show)

-- | 'StatusReport' smart constructor.
statusReport :: StatusReport
statusReport = StatusReport'{_srStatus = Nothing, _srCheckedTime = Nothing};

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
              x .@? "Status" <*> x .@? "CheckedTime"

-- | /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'{_tagValue :: Maybe Text, _tagKey :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Tag' smart constructor.
tag :: Tag
tag = Tag'{_tagValue = Nothing, _tagKey = Nothing};

-- | The value for a @Tag@.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key for a @Tag@.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x = Tag' <$> x .@? "Value" <*> x .@? "Key"

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Value" @= _tagValue, "Key" @= _tagKey]

data TagResourceType = Healthcheck | Hostedzone deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText TagResourceType where
    parser = takeLowerText >>= \case
        "healthcheck" -> pure Healthcheck
        "hostedzone" -> pure Hostedzone
        e -> fail ("Failure parsing TagResourceType from " ++ show e)

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

-- | /See:/ 'vpc' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpcVPCId'
--
-- * 'vpcVPCRegion'
data VPC = VPC'{_vpcVPCId :: Maybe Text, _vpcVPCRegion :: VPCRegion} deriving (Eq, Read, Show)

-- | 'VPC' smart constructor.
vpc :: VPCRegion -> VPC
vpc pVPCRegion = VPC'{_vpcVPCId = Nothing, _vpcVPCRegion = pVPCRegion};

-- | FIXME: Undocumented member.
vpcVPCId :: Lens' VPC (Maybe Text)
vpcVPCId = lens _vpcVPCId (\ s a -> s{_vpcVPCId = a});

-- | FIXME: Undocumented member.
vpcVPCRegion :: Lens' VPC VPCRegion
vpcVPCRegion = lens _vpcVPCRegion (\ s a -> s{_vpcVPCRegion = a});

instance FromXML VPC where
        parseXML x
          = VPC' <$> x .@? "VPCId" <*> x .@ "VPCRegion"

instance ToXML VPC where
        toXML VPC'{..}
          = mconcat
              ["VPCId" @= _vpcVPCId, "VPCRegion" @= _vpcVPCRegion]

data VPCRegion = APNortheast1 | SAEast1 | CNNorth1 | USWest2 | EUWest1 | USEast1 | USWest1 | EUCentral1 | APSoutheast2 | APSoutheast1 deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText VPCRegion where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure APNortheast1
        "ap-southeast-1" -> pure APSoutheast1
        "ap-southeast-2" -> pure APSoutheast2
        "cn-north-1" -> pure CNNorth1
        "eu-central-1" -> pure EUCentral1
        "eu-west-1" -> pure EUWest1
        "sa-east-1" -> pure SAEast1
        "us-east-1" -> pure USEast1
        "us-west-1" -> pure USWest1
        "us-west-2" -> pure USWest2
        e -> fail ("Failure parsing VPCRegion from " ++ show e)

instance ToText VPCRegion where
    toText = \case
        APNortheast1 -> "ap-northeast-1"
        APSoutheast1 -> "ap-southeast-1"
        APSoutheast2 -> "ap-southeast-2"
        CNNorth1 -> "cn-north-1"
        EUCentral1 -> "eu-central-1"
        EUWest1 -> "eu-west-1"
        SAEast1 -> "sa-east-1"
        USEast1 -> "us-east-1"
        USWest1 -> "us-west-1"
        USWest2 -> "us-west-2"

instance Hashable VPCRegion
instance ToQuery VPCRegion
instance ToHeader VPCRegion

instance FromXML VPCRegion where
    parseXML = parseXMLText "VPCRegion"

instance ToXML VPCRegion where
    toXML = toXMLText
