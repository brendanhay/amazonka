{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.Route53.Internal
import           Network.AWS.Route53.Types.Sum

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
data AliasTarget = AliasTarget'
    { _atHostedZoneId         :: !Text
    , _atDNSName              :: !Text
    , _atEvaluateTargetHealth :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AliasTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atHostedZoneId'
--
-- * 'atDNSName'
--
-- * 'atEvaluateTargetHealth'
aliasTarget
    :: Text -- ^ 'atHostedZoneId'
    -> Text -- ^ 'atDNSName'
    -> Bool -- ^ 'atEvaluateTargetHealth'
    -> AliasTarget
aliasTarget pHostedZoneId_ pDNSName_ pEvaluateTargetHealth_ =
    AliasTarget'
    { _atHostedZoneId = pHostedZoneId_
    , _atDNSName = pDNSName_
    , _atEvaluateTargetHealth = pEvaluateTargetHealth_
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
data Change = Change'
    { _cAction            :: !ChangeAction
    , _cResourceRecordSet :: !ResourceRecordSet
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Change' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAction'
--
-- * 'cResourceRecordSet'
change
    :: ChangeAction -- ^ 'cAction'
    -> ResourceRecordSet -- ^ 'cResourceRecordSet'
    -> Change
change pAction_ pResourceRecordSet_ =
    Change'
    { _cAction = pAction_
    , _cResourceRecordSet = pResourceRecordSet_
    }

-- | The action to perform.
--
-- Valid values: 'CREATE' | 'DELETE' | 'UPSERT'
cAction :: Lens' Change ChangeAction
cAction = lens _cAction (\ s a -> s{_cAction = a});

-- | Information about the resource record set to create or delete.
cResourceRecordSet :: Lens' Change ResourceRecordSet
cResourceRecordSet = lens _cResourceRecordSet (\ s a -> s{_cResourceRecordSet = a});

instance ToXML Change where
        toXML Change'{..}
          = mconcat
              ["Action" @= _cAction,
               "ResourceRecordSet" @= _cResourceRecordSet]

-- | A complex type that contains an optional comment and the changes that
-- you want to make with a change batch request.
--
-- /See:/ 'changeBatch' smart constructor.
data ChangeBatch = ChangeBatch'
    { _cbComment :: !(Maybe Text)
    , _cbChanges :: !(List1 Change)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbComment'
--
-- * 'cbChanges'
changeBatch
    :: NonEmpty Change -- ^ 'cbChanges'
    -> ChangeBatch
changeBatch pChanges_ =
    ChangeBatch'
    { _cbComment = Nothing
    , _cbChanges = _List1 # pChanges_
    }

-- | /Optional:/ Any comments you want to include about a change batch
-- request.
cbComment :: Lens' ChangeBatch (Maybe Text)
cbComment = lens _cbComment (\ s a -> s{_cbComment = a});

-- | A complex type that contains one 'Change' element for each resource
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
data ChangeInfo = ChangeInfo'
    { _ciComment     :: !(Maybe Text)
    , _ciId          :: !Text
    , _ciStatus      :: !ChangeStatus
    , _ciSubmittedAt :: !ISO8601
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciComment'
--
-- * 'ciId'
--
-- * 'ciStatus'
--
-- * 'ciSubmittedAt'
changeInfo
    :: Text -- ^ 'ciId'
    -> ChangeStatus -- ^ 'ciStatus'
    -> UTCTime -- ^ 'ciSubmittedAt'
    -> ChangeInfo
changeInfo pId_ pStatus_ pSubmittedAt_ =
    ChangeInfo'
    { _ciComment = Nothing
    , _ciId = pId_
    , _ciStatus = pStatus_
    , _ciSubmittedAt = _Time # pSubmittedAt_
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

-- | The current state of the request. 'PENDING' indicates that this request
-- has not yet been applied to all Amazon Route 53 DNS servers.
--
-- Valid Values: 'PENDING' | 'INSYNC'
ciStatus :: Lens' ChangeInfo ChangeStatus
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a});

-- | The date and time the change was submitted, in the format
-- 'YYYY-MM-DDThh:mm:ssZ', as specified in the ISO 8601 standard (for
-- example, 2009-11-19T19:37:58Z). The 'Z' after the time indicates that
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
data DelegationSet = DelegationSet'
    { _dsId              :: !(Maybe Text)
    , _dsCallerReference :: !(Maybe Text)
    , _dsNameServers     :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DelegationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsId'
--
-- * 'dsCallerReference'
--
-- * 'dsNameServers'
delegationSet
    :: NonEmpty Text -- ^ 'dsNameServers'
    -> DelegationSet
delegationSet pNameServers_ =
    DelegationSet'
    { _dsId = Nothing
    , _dsCallerReference = Nothing
    , _dsNameServers = _List1 # pNameServers_
    }

-- | Undocumented member.
dsId :: Lens' DelegationSet (Maybe Text)
dsId = lens _dsId (\ s a -> s{_dsId = a});

-- | Undocumented member.
dsCallerReference :: Lens' DelegationSet (Maybe Text)
dsCallerReference = lens _dsCallerReference (\ s a -> s{_dsCallerReference = a});

-- | A complex type that contains the authoritative name servers for the
-- hosted zone. Use the method provided by your domain registrar to add an
-- NS record to your domain for each 'NameServer' that is assigned to your
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
data GeoLocation = GeoLocation'
    { _glSubdivisionCode :: !(Maybe Text)
    , _glCountryCode     :: !(Maybe Text)
    , _glContinentCode   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GeoLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glSubdivisionCode'
--
-- * 'glCountryCode'
--
-- * 'glContinentCode'
geoLocation
    :: GeoLocation
geoLocation =
    GeoLocation'
    { _glSubdivisionCode = Nothing
    , _glCountryCode = Nothing
    , _glContinentCode = Nothing
    }

-- | The code for a country\'s subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
--
-- Constraint: Specifying 'SubdivisionCode' without 'CountryCode' returns
-- an InvalidInput error.
glSubdivisionCode :: Lens' GeoLocation (Maybe Text)
glSubdivisionCode = lens _glSubdivisionCode (\ s a -> s{_glSubdivisionCode = a});

-- | The code for a country geo location. The default location uses \'*\' for
-- the country code and will match all locations that are not matched by a
-- geo location.
--
-- The default geo location uses a '*' for the country code. All other
-- country codes follow the ISO 3166 two-character code.
glCountryCode :: Lens' GeoLocation (Maybe Text)
glCountryCode = lens _glCountryCode (\ s a -> s{_glCountryCode = a});

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code.
--
-- Valid values: 'AF' | 'AN' | 'AS' | 'EU' | 'OC' | 'NA' | 'SA'
--
-- Constraint: Specifying 'ContinentCode' with either 'CountryCode' or
-- 'SubdivisionCode' returns an InvalidInput error.
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

-- | A complex type that contains information about a 'GeoLocation'.
--
-- /See:/ 'geoLocationDetails' smart constructor.
data GeoLocationDetails = GeoLocationDetails'
    { _gldSubdivisionName :: !(Maybe Text)
    , _gldSubdivisionCode :: !(Maybe Text)
    , _gldCountryName     :: !(Maybe Text)
    , _gldCountryCode     :: !(Maybe Text)
    , _gldContinentCode   :: !(Maybe Text)
    , _gldContinentName   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GeoLocationDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
geoLocationDetails
    :: GeoLocationDetails
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
-- 'SubdivisionCode' is also present.
gldSubdivisionName :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionName = lens _gldSubdivisionName (\ s a -> s{_gldSubdivisionName = a});

-- | The code for a country\'s subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
gldSubdivisionCode :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionCode = lens _gldSubdivisionCode (\ s a -> s{_gldSubdivisionCode = a});

-- | The name of the country. This element is only present if 'CountryCode'
-- is also present.
gldCountryName :: Lens' GeoLocationDetails (Maybe Text)
gldCountryName = lens _gldCountryName (\ s a -> s{_gldCountryName = a});

-- | The code for a country geo location. The default location uses \'*\' for
-- the country code and will match all locations that are not matched by a
-- geo location.
--
-- The default geo location uses a '*' for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gldCountryCode :: Lens' GeoLocationDetails (Maybe Text)
gldCountryCode = lens _gldCountryCode (\ s a -> s{_gldCountryCode = a});

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code.
gldContinentCode :: Lens' GeoLocationDetails (Maybe Text)
gldContinentCode = lens _gldContinentCode (\ s a -> s{_gldContinentCode = a});

-- | The name of the continent. This element is only present if
-- 'ContinentCode' is also present.
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
data HealthCheck = HealthCheck'
    { _hcId                 :: !Text
    , _hcCallerReference    :: !Text
    , _hcHealthCheckConfig  :: !HealthCheckConfig
    , _hcHealthCheckVersion :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcId'
--
-- * 'hcCallerReference'
--
-- * 'hcHealthCheckConfig'
--
-- * 'hcHealthCheckVersion'
healthCheck
    :: Text -- ^ 'hcId'
    -> Text -- ^ 'hcCallerReference'
    -> HealthCheckConfig -- ^ 'hcHealthCheckConfig'
    -> Natural -- ^ 'hcHealthCheckVersion'
    -> HealthCheck
healthCheck pId_ pCallerReference_ pHealthCheckConfig_ pHealthCheckVersion_ =
    HealthCheck'
    { _hcId = pId_
    , _hcCallerReference = pCallerReference_
    , _hcHealthCheckConfig = pHealthCheckConfig_
    , _hcHealthCheckVersion = _Nat # pHealthCheckVersion_
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
-- call to 'UpdateHealthCheck' to prevent overwriting another change to the
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
data HealthCheckConfig = HealthCheckConfig'
    { _hccFailureThreshold         :: !(Maybe Nat)
    , _hccIPAddress                :: !(Maybe Text)
    , _hccSearchString             :: !(Maybe Text)
    , _hccHealthThreshold          :: !(Maybe Nat)
    , _hccResourcePath             :: !(Maybe Text)
    , _hccMeasureLatency           :: !(Maybe Bool)
    , _hccInverted                 :: !(Maybe Bool)
    , _hccFullyQualifiedDomainName :: !(Maybe Text)
    , _hccChildHealthChecks        :: !(Maybe [Text])
    , _hccRequestInterval          :: !(Maybe Nat)
    , _hccPort                     :: !(Maybe Nat)
    , _hccType                     :: !HealthCheckType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HealthCheckConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hccFailureThreshold'
--
-- * 'hccIPAddress'
--
-- * 'hccSearchString'
--
-- * 'hccHealthThreshold'
--
-- * 'hccResourcePath'
--
-- * 'hccMeasureLatency'
--
-- * 'hccInverted'
--
-- * 'hccFullyQualifiedDomainName'
--
-- * 'hccChildHealthChecks'
--
-- * 'hccRequestInterval'
--
-- * 'hccPort'
--
-- * 'hccType'
healthCheckConfig
    :: HealthCheckType -- ^ 'hccType'
    -> HealthCheckConfig
healthCheckConfig pType_ =
    HealthCheckConfig'
    { _hccFailureThreshold = Nothing
    , _hccIPAddress = Nothing
    , _hccSearchString = Nothing
    , _hccHealthThreshold = Nothing
    , _hccResourcePath = Nothing
    , _hccMeasureLatency = Nothing
    , _hccInverted = Nothing
    , _hccFullyQualifiedDomainName = Nothing
    , _hccChildHealthChecks = Nothing
    , _hccRequestInterval = Nothing
    , _hccPort = Nothing
    , _hccType = pType_
    }

-- | The number of consecutive health checks that an endpoint must pass or
-- fail for Route 53 to change the current status of the endpoint from
-- unhealthy to healthy or vice versa.
--
-- Valid values are integers between 1 and 10. For more information, see
-- \"How Amazon Route 53 Determines Whether an Endpoint Is Healthy\" in the
-- Amazon Route 53 Developer Guide.
hccFailureThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccFailureThreshold = lens _hccFailureThreshold (\ s a -> s{_hccFailureThreshold = a}) . mapping _Nat;

-- | IP Address of the instance being checked.
hccIPAddress :: Lens' HealthCheckConfig (Maybe Text)
hccIPAddress = lens _hccIPAddress (\ s a -> s{_hccIPAddress = a});

-- | A string to search for in the body of a health check response. Required
-- for HTTP_STR_MATCH and HTTPS_STR_MATCH health checks.
hccSearchString :: Lens' HealthCheckConfig (Maybe Text)
hccSearchString = lens _hccSearchString (\ s a -> s{_hccSearchString = a});

-- | The minimum number of child health checks that must be healthy for Route
-- 53 to consider the parent health check to be healthy. Valid values are
-- integers between 0 and 256, inclusive.
hccHealthThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccHealthThreshold = lens _hccHealthThreshold (\ s a -> s{_hccHealthThreshold = a}) . mapping _Nat;

-- | Path to ping on the instance to check the health. Required for HTTP,
-- HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH health checks, HTTP request
-- is issued to the instance on the given port and path.
hccResourcePath :: Lens' HealthCheckConfig (Maybe Text)
hccResourcePath = lens _hccResourcePath (\ s a -> s{_hccResourcePath = a});

-- | A Boolean value that indicates whether you want Route 53 to measure the
-- latency between health checkers in multiple AWS regions and your
-- endpoint and to display CloudWatch latency graphs in the Route 53
-- console.
hccMeasureLatency :: Lens' HealthCheckConfig (Maybe Bool)
hccMeasureLatency = lens _hccMeasureLatency (\ s a -> s{_hccMeasureLatency = a});

-- | A boolean value that indicates whether the status of health check should
-- be inverted. For example, if a health check is healthy but 'Inverted' is
-- 'True', then Route 53 considers the health check to be unhealthy.
hccInverted :: Lens' HealthCheckConfig (Maybe Bool)
hccInverted = lens _hccInverted (\ s a -> s{_hccInverted = a});

-- | Fully qualified domain name of the instance to be health checked.
hccFullyQualifiedDomainName :: Lens' HealthCheckConfig (Maybe Text)
hccFullyQualifiedDomainName = lens _hccFullyQualifiedDomainName (\ s a -> s{_hccFullyQualifiedDomainName = a});

-- | For a specified parent health check, a list of 'HealthCheckId' values
-- for the associated child health checks.
hccChildHealthChecks :: Lens' HealthCheckConfig [Text]
hccChildHealthChecks = lens _hccChildHealthChecks (\ s a -> s{_hccChildHealthChecks = a}) . _Default . _Coerce;

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
              (x .@? "FailureThreshold") <*> (x .@? "IPAddress")
                <*> (x .@? "SearchString")
                <*> (x .@? "HealthThreshold")
                <*> (x .@? "ResourcePath")
                <*> (x .@? "MeasureLatency")
                <*> (x .@? "Inverted")
                <*> (x .@? "FullyQualifiedDomainName")
                <*>
                (x .@? "ChildHealthChecks" .!@ mempty >>=
                   may (parseXMLList "ChildHealthCheck"))
                <*> (x .@? "RequestInterval")
                <*> (x .@? "Port")
                <*> (x .@ "Type")

instance ToXML HealthCheckConfig where
        toXML HealthCheckConfig'{..}
          = mconcat
              ["FailureThreshold" @= _hccFailureThreshold,
               "IPAddress" @= _hccIPAddress,
               "SearchString" @= _hccSearchString,
               "HealthThreshold" @= _hccHealthThreshold,
               "ResourcePath" @= _hccResourcePath,
               "MeasureLatency" @= _hccMeasureLatency,
               "Inverted" @= _hccInverted,
               "FullyQualifiedDomainName" @=
                 _hccFullyQualifiedDomainName,
               "ChildHealthChecks" @=
                 toXML
                   (toXMLList "ChildHealthCheck" <$>
                      _hccChildHealthChecks),
               "RequestInterval" @= _hccRequestInterval,
               "Port" @= _hccPort, "Type" @= _hccType]

-- | A complex type that contains the IP address of a Route 53 health checker
-- and the reason for the health check status.
--
-- /See:/ 'healthCheckObservation' smart constructor.
data HealthCheckObservation = HealthCheckObservation'
    { _hcoIPAddress    :: !(Maybe Text)
    , _hcoStatusReport :: !(Maybe StatusReport)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HealthCheckObservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcoIPAddress'
--
-- * 'hcoStatusReport'
healthCheckObservation
    :: HealthCheckObservation
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
data HostedZone = HostedZone'
    { _hzConfig                 :: !(Maybe HostedZoneConfig)
    , _hzResourceRecordSetCount :: !(Maybe Integer)
    , _hzId                     :: !Text
    , _hzName                   :: !Text
    , _hzCallerReference        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
hostedZone
    :: Text -- ^ 'hzId'
    -> Text -- ^ 'hzName'
    -> Text -- ^ 'hzCallerReference'
    -> HostedZone
hostedZone pId_ pName_ pCallerReference_ =
    HostedZone'
    { _hzConfig = Nothing
    , _hzResourceRecordSetCount = Nothing
    , _hzId = pId_
    , _hzName = pName_
    , _hzCallerReference = pCallerReference_
    }

-- | A complex type that contains the 'Comment' element.
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
-- domain to the set of 'NameServers' elements returned in 'DelegationSet'.
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
-- 'HostedZoneConfig' and 'Comment' elements from the XML document.
--
-- /See:/ 'hostedZoneConfig' smart constructor.
data HostedZoneConfig = HostedZoneConfig'
    { _hzcPrivateZone :: !(Maybe Bool)
    , _hzcComment     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HostedZoneConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hzcPrivateZone'
--
-- * 'hzcComment'
hostedZoneConfig
    :: HostedZoneConfig
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
-- specify a comment, you can omit the 'HostedZoneConfig' and 'Comment'
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

-- | A complex type that contains the value of the 'Value' element for the
-- current resource record set.
--
-- /See:/ 'resourceRecord' smart constructor.
newtype ResourceRecord = ResourceRecord'
    { _rrValue :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrValue'
resourceRecord
    :: Text -- ^ 'rrValue'
    -> ResourceRecord
resourceRecord pValue_ =
    ResourceRecord'
    { _rrValue = pValue_
    }

-- | The value of the 'Value' element for the current resource record set.
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
data ResourceRecordSet = ResourceRecordSet'
    { _rrsTTL             :: !(Maybe Nat)
    , _rrsResourceRecords :: !(Maybe (List1 ResourceRecord))
    , _rrsAliasTarget     :: !(Maybe AliasTarget)
    , _rrsWeight          :: !(Maybe Nat)
    , _rrsSetIdentifier   :: !(Maybe Text)
    , _rrsFailover        :: !(Maybe Failover)
    , _rrsHealthCheckId   :: !(Maybe Text)
    , _rrsRegion          :: !(Maybe Region)
    , _rrsGeoLocation     :: !(Maybe GeoLocation)
    , _rrsName            :: !Text
    , _rrsType            :: !RecordType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceRecordSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsTTL'
--
-- * 'rrsResourceRecords'
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
resourceRecordSet
    :: Text -- ^ 'rrsName'
    -> RecordType -- ^ 'rrsType'
    -> ResourceRecordSet
resourceRecordSet pName_ pType_ =
    ResourceRecordSet'
    { _rrsTTL = Nothing
    , _rrsResourceRecords = Nothing
    , _rrsAliasTarget = Nothing
    , _rrsWeight = Nothing
    , _rrsSetIdentifier = Nothing
    , _rrsFailover = Nothing
    , _rrsHealthCheckId = Nothing
    , _rrsRegion = Nothing
    , _rrsGeoLocation = Nothing
    , _rrsName = pName_
    , _rrsType = pType_
    }

-- | The cache time to live for the current resource record set.
rrsTTL :: Lens' ResourceRecordSet (Maybe Natural)
rrsTTL = lens _rrsTTL (\ s a -> s{_rrsTTL = a}) . mapping _Nat;

-- | A complex type that contains the resource records for the current
-- resource record set.
rrsResourceRecords :: Lens' ResourceRecordSet (Maybe (NonEmpty ResourceRecord))
rrsResourceRecords = lens _rrsResourceRecords (\ s a -> s{_rrsResourceRecords = a}) . mapping _List1;

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
-- Valid values: 'PRIMARY' | 'SECONDARY'
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

instance FromXML ResourceRecordSet where
        parseXML x
          = ResourceRecordSet' <$>
              (x .@? "TTL") <*>
                (x .@? "ResourceRecords" .!@ mempty >>=
                   may (parseXMLList1 "ResourceRecord"))
                <*> (x .@? "AliasTarget")
                <*> (x .@? "Weight")
                <*> (x .@? "SetIdentifier")
                <*> (x .@? "Failover")
                <*> (x .@? "HealthCheckId")
                <*> (x .@? "Region")
                <*> (x .@? "GeoLocation")
                <*> (x .@ "Name")
                <*> (x .@ "Type")

instance ToXML ResourceRecordSet where
        toXML ResourceRecordSet'{..}
          = mconcat
              ["TTL" @= _rrsTTL,
               "ResourceRecords" @=
                 toXML
                   (toXMLList "ResourceRecord" <$> _rrsResourceRecords),
               "AliasTarget" @= _rrsAliasTarget,
               "Weight" @= _rrsWeight,
               "SetIdentifier" @= _rrsSetIdentifier,
               "Failover" @= _rrsFailover,
               "HealthCheckId" @= _rrsHealthCheckId,
               "Region" @= _rrsRegion,
               "GeoLocation" @= _rrsGeoLocation, "Name" @= _rrsName,
               "Type" @= _rrsType]

-- | A complex type containing a resource and its associated tags.
--
-- /See:/ 'resourceTagSet' smart constructor.
data ResourceTagSet = ResourceTagSet'
    { _rtsResourceId   :: !(Maybe Text)
    , _rtsResourceType :: !(Maybe TagResourceType)
    , _rtsTags         :: !(Maybe (List1 Tag))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceTagSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtsResourceId'
--
-- * 'rtsResourceType'
--
-- * 'rtsTags'
resourceTagSet
    :: ResourceTagSet
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
-- - The resource type for health checks is 'healthcheck'.
--
-- - The resource type for hosted zones is 'hostedzone'.
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
data StatusReport = StatusReport'
    { _srStatus      :: !(Maybe Text)
    , _srCheckedTime :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatusReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srStatus'
--
-- * 'srCheckedTime'
statusReport
    :: StatusReport
statusReport =
    StatusReport'
    { _srStatus = Nothing
    , _srCheckedTime = Nothing
    }

-- | The observed health check status.
srStatus :: Lens' StatusReport (Maybe Text)
srStatus = lens _srStatus (\ s a -> s{_srStatus = a});

-- | The date and time the health check status was observed, in the format
-- 'YYYY-MM-DDThh:mm:ssZ', as specified in the ISO 8601 standard (for
-- example, 2009-11-19T19:37:58Z). The 'Z' after the time indicates that
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
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue'
--
-- * 'tagKey'
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | The value for a 'Tag'.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key for a 'Tag'.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Value" @= _tagValue, "Key" @= _tagKey]

-- | /See:/ 'vpc' smart constructor.
data VPC = VPC'
    { _vpcVPCRegion :: !(Maybe VPCRegion)
    , _vpcVPCId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcVPCRegion'
--
-- * 'vpcVPCId'
vpc
    :: VPC
vpc =
    VPC'
    { _vpcVPCRegion = Nothing
    , _vpcVPCId = Nothing
    }

-- | Undocumented member.
vpcVPCRegion :: Lens' VPC (Maybe VPCRegion)
vpcVPCRegion = lens _vpcVPCRegion (\ s a -> s{_vpcVPCRegion = a});

-- | Undocumented member.
vpcVPCId :: Lens' VPC (Maybe Text)
vpcVPCId = lens _vpcVPCId (\ s a -> s{_vpcVPCId = a});

instance FromXML VPC where
        parseXML x
          = VPC' <$> (x .@? "VPCRegion") <*> (x .@? "VPCId")

instance ToXML VPC where
        toXML VPC'{..}
          = mconcat
              ["VPCRegion" @= _vpcVPCRegion, "VPCId" @= _vpcVPCId]
