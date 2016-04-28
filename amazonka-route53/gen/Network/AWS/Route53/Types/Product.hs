{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Route53.Internal
import           Network.AWS.Route53.Types.Sum

-- | /Alias resource record sets only:/ Information about the CloudFront
-- distribution, ELB load balancer, Amazon S3 bucket, or Amazon Route 53
-- resource record set to which you are routing traffic.
--
-- If you\'re creating resource record sets for a private hosted zone, note
-- the following:
--
-- -   You can create alias resource record sets only for Amazon Route 53
--     resource record sets in the same private hosted zone. Creating alias
--     resource record sets for CloudFront distributions, ELB load
--     balancers, and Amazon S3 buckets is not supported.
-- -   You can\'t create alias resource record sets for failover,
--     geolocation, or latency resource record sets in a private hosted
--     zone.
--
-- For more information and an example, see
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/CreateAliasRRSAPI.html Example: Creating Alias Resource Record Sets>
-- in the /Amazon Route 53 API Reference/.
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

-- | /Alias resource record sets only:/ The value you use depends on where
-- you want to route queries:
--
-- -   __A CloudFront distribution:__ Specify 'Z2FDTNDATAQYW2'.
-- -   An ELB load balancer: Specify the value of the hosted zone ID for
--     the load balancer. You can get the hosted zone ID by using the AWS
--     Management Console, the ELB API, or the AWS CLI. Use the same method
--     to get values for 'HostedZoneId' and 'DNSName'. If you get one value
--     from the console and the other value from the API or the CLI,
--     creating the resource record set will fail.
-- -   __An Amazon S3 bucket that is configured as a static website:__
--     Specify the hosted zone ID for the Amazon S3 website endpoint in
--     which you created the bucket. For more information about valid
--     values, see the table
--     <http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Amazon Simple Storage Service (S3) Website Endpoints>
--     in the /Amazon Web Services General Reference/.
-- -   __Another Amazon Route 53 resource record set in your hosted zone:__
--     Specify the hosted zone ID of your hosted zone. (An alias resource
--     record set cannot reference a resource record set in a different
--     hosted zone.)
--
-- For more information and an example, see
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/CreateAliasRRSAPI.html Example: Creating Alias Resource Record Sets>
-- in the /Amazon Route 53 API Reference/.
atHostedZoneId :: Lens' AliasTarget Text
atHostedZoneId = lens _atHostedZoneId (\ s a -> s{_atHostedZoneId = a});

-- | /Alias resource record sets only:/ The external DNS name associated with
-- the AWS Resource. The value that you specify depends on where you want
-- to route queries:
--
-- -   __A CloudFront distribution:__ Specify the domain name that
--     CloudFront assigned when you created your distribution. Your
--     CloudFront distribution must include an alternate domain name that
--     matches the name of the resource record set. For example, if the
--     name of the resource record set is 'acme.example.com', your
--     CloudFront distribution must include 'acme.example.com' as one of
--     the alternate domain names. For more information, see
--     <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/CNAMEs.html Using Alternate Domain Names (CNAMEs)>
--     in the /Amazon CloudFront Developer Guide/.
-- -   __An ELB load balancer:__ Specify the DNS name associated with the
--     load balancer. You can get the DNS name by using the AWS Management
--     Console, the ELB API, or the AWS CLI. Use the same method to get
--     values for 'HostedZoneId' and 'DNSName'. If you get one value from
--     the console and the other value from the API or the CLI, creating
--     the resource record set will fail.
-- -   __An Elastic Beanstalk environment:__ Specify the CNAME attribute
--     for the environment. (The environment must have a regionalized
--     domain name.)
-- -   __An Amazon S3 bucket that is configured as a static website:__
--     Specify the domain name of the Amazon S3 website endpoint in which
--     you created the bucket; for example,
--     's3-website-us-east-1.amazonaws.com'. For more information about
--     valid values, see the table
--     <http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Amazon Simple Storage Service (S3) Website Endpoints>
--     in the /Amazon Web Services General Reference/. For more information
--     about using Amazon S3 buckets for websites, see
--     <http://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting a Static Website on Amazon S3>
--     in the /Amazon Simple Storage Service Developer Guide/.
-- -   __Another Amazon Route 53 resource record set:__ Specify the value
--     of the 'Name' element for a resource record set in the current
--     hosted zone.
--
-- For more information and an example, see
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/CreateAliasRRSAPI.html Example: Creating Alias Resource Record Sets>
-- in the /Amazon Route 53 API Reference/.
atDNSName :: Lens' AliasTarget Text
atDNSName = lens _atDNSName (\ s a -> s{_atDNSName = a});

-- | /Alias resource record sets only:/ If you set the value of
-- 'EvaluateTargetHealth' to 'true' for the resource record set or sets in
-- an alias, weighted alias, latency alias, or failover alias resource
-- record set, and if you specify a value for 'HealthCheckId' for every
-- resource record set that is referenced by these alias resource record
-- sets, the alias resource record sets inherit the health of the
-- referenced resource record sets.
--
-- In this configuration, when Amazon Route 53 receives a DNS query for an
-- alias resource record set:
--
-- 1.  Amazon Route 53 looks at the resource record sets that are
--     referenced by the alias resource record sets to determine which
--     health checks they\'re using.
-- 2.  Amazon Route 53 checks the current status of each health check.
--     (Amazon Route 53 periodically checks the health of the endpoint that
--     is specified in a health check; it doesn\'t perform the health check
--     when the DNS query arrives.)
-- 3.  Based on the status of the health checks, Amazon Route 53 determines
--     which resource record sets are healthy. Unhealthy resource record
--     sets are immediately removed from consideration. In addition, if all
--     of the resource record sets that are referenced by an alias resource
--     record set are unhealthy, that alias resource record set also is
--     immediately removed from consideration.
-- 4.  Based on the configuration of the alias resource record sets
--     (weighted alias or latency alias, for example) and the configuration
--     of the resource record sets that they reference, Amazon Route 53
--     chooses a resource record set from the healthy resource record sets,
--     and responds to the query.
--
-- Note the following:
--
-- -   You cannot set 'EvaluateTargetHealth' to true when the alias target
--     is a CloudFront distribution.
-- -   If the AWS resource that you specify in 'AliasTarget' is a resource
--     record set or a group of resource record sets (for example, a group
--     of weighted resource record sets), but it is not another alias
--     resource record set, we recommend that you associate a health check
--     with all of the resource record sets in the alias target. For more
--     information, see
--     <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-complex-configs.html#dns-failover-complex-configs-hc-omitting What Happens When You Omit Health Checks?>
--     in the /Amazon Route 53 Developer Guide/.
-- -   If you specify an ELB load balancer in 'AliasTarget', Elastic Load
--     Balancing routes queries only to the healthy Amazon EC2 instances
--     that are registered with the load balancer. If no Amazon EC2
--     instances are healthy or if the load balancer itself is unhealthy,
--     and if 'EvaluateTargetHealth' is 'true' for the corresponding alias
--     resource record set, Amazon Route 53 routes queries to other
--     resources.
-- -   When you create a load balancer, you configure settings for Elastic
--     Load Balancing health checks; they\'re not Amazon Route 53 health
--     checks, but they perform a similar function. Do not create Amazon
--     Route 53 health checks for the Amazon EC2 instances that you
--     register with an ELB load balancer. For more information, see
--     <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-complex-configs.html How Health Checks Work in More Complex Amazon Route 53 Configurations>
--     in the /Amazon Route 53 Developer Guide/.
--
-- We recommend that you set 'EvaluateTargetHealth' to 'true' only when you
-- have enough idle capacity to handle the failure of one or more
-- endpoints.
--
-- For more information and examples, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html Amazon Route 53 Health Checks and DNS Failover>
-- in the /Amazon Route 53 Developer Guide/.
atEvaluateTargetHealth :: Lens' AliasTarget Bool
atEvaluateTargetHealth = lens _atEvaluateTargetHealth (\ s a -> s{_atEvaluateTargetHealth = a});

instance FromXML AliasTarget where
        parseXML x
          = AliasTarget' <$>
              (x .@ "HostedZoneId") <*> (x .@ "DNSName") <*>
                (x .@ "EvaluateTargetHealth")

instance Hashable AliasTarget

instance NFData AliasTarget

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

-- | The action to perform:
--
-- -   'CREATE': Creates a resource record set that has the specified
--     values.
-- -   'DELETE': Deletes a existing resource record set that has the
--     specified values for 'Name', 'Type', 'SetIdentifier' (for latency,
--     weighted, geolocation, and failover resource record sets), and 'TTL'
--     (except alias resource record sets, for which the TTL is determined
--     by the AWS resource that you\'re routing DNS queries to).
-- -   'UPSERT': If a resource record set does not already exist, Amazon
--     Route 53 creates it. If a resource record set does exist, Amazon
--     Route 53 updates it with the values in the request. Amazon Route 53
--     can update an existing resource record set only when all of the
--     following values match: 'Name', 'Type', and 'SetIdentifier' (for
--     weighted, latency, geolocation, and failover resource record sets).
cAction :: Lens' Change ChangeAction
cAction = lens _cAction (\ s a -> s{_cAction = a});

-- | Information about the resource record set to create or delete.
cResourceRecordSet :: Lens' Change ResourceRecordSet
cResourceRecordSet = lens _cResourceRecordSet (\ s a -> s{_cResourceRecordSet = a});

instance Hashable Change

instance NFData Change

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

instance Hashable ChangeBatch

instance NFData ChangeBatch

instance ToXML ChangeBatch where
        toXML ChangeBatch'{..}
          = mconcat
              ["Comment" @= _cbComment,
               "Changes" @= toXMLList "Change" _cbChanges]

-- | A complex type that describes change information about changes made to
-- your hosted zone.
--
-- This element contains an ID that you use when performing a < GetChange>
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
-- This element contains an ID that you use when performing a < GetChange>
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
-- the time is listed in Coordinated Universal Time (UTC).
ciSubmittedAt :: Lens' ChangeInfo UTCTime
ciSubmittedAt = lens _ciSubmittedAt (\ s a -> s{_ciSubmittedAt = a}) . _Time;

instance FromXML ChangeInfo where
        parseXML x
          = ChangeInfo' <$>
              (x .@? "Comment") <*> (x .@ "Id") <*> (x .@ "Status")
                <*> (x .@ "SubmittedAt")

instance Hashable ChangeInfo

instance NFData ChangeInfo

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

instance Hashable DelegationSet

instance NFData DelegationSet

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
-- an < InvalidInput> error.
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
-- 'SubdivisionCode' returns an < InvalidInput> error.
glContinentCode :: Lens' GeoLocation (Maybe Text)
glContinentCode = lens _glContinentCode (\ s a -> s{_glContinentCode = a});

instance FromXML GeoLocation where
        parseXML x
          = GeoLocation' <$>
              (x .@? "SubdivisionCode") <*> (x .@? "CountryCode")
                <*> (x .@? "ContinentCode")

instance Hashable GeoLocation

instance NFData GeoLocation

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

instance Hashable GeoLocationDetails

instance NFData GeoLocationDetails

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

instance Hashable HealthCheck

instance NFData HealthCheck

-- | A complex type that contains the health check configuration.
--
-- /See:/ 'healthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
    { _hccFailureThreshold         :: !(Maybe Nat)
    , _hccIPAddress                :: !(Maybe Text)
    , _hccEnableSNI                :: !(Maybe Bool)
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
-- * 'hccEnableSNI'
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
    , _hccEnableSNI = Nothing
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
-- fail for Amazon Route 53 to change the current status of the endpoint
-- from unhealthy to healthy or vice versa.
--
-- Valid values are integers between 1 and 10. For more information, see
-- \"How Amazon Route 53 Determines Whether an Endpoint Is Healthy\" in the
-- Amazon Route 53 Developer Guide.
hccFailureThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccFailureThreshold = lens _hccFailureThreshold (\ s a -> s{_hccFailureThreshold = a}) . mapping _Nat;

-- | IP Address of the instance being checked.
hccIPAddress :: Lens' HealthCheckConfig (Maybe Text)
hccIPAddress = lens _hccIPAddress (\ s a -> s{_hccIPAddress = a});

-- | Specify whether you want Amazon Route 53 to send the value of
-- 'FullyQualifiedDomainName' to the endpoint in the 'client_hello' message
-- during TLS negotiation. If you don\'t specify a value for 'EnableSNI',
-- Amazon Route 53 defaults to 'true' when 'Type' is 'HTTPS' or
-- 'HTTPS_STR_MATCH' and defaults to 'false' when 'Type' is any other
-- value.
hccEnableSNI :: Lens' HealthCheckConfig (Maybe Bool)
hccEnableSNI = lens _hccEnableSNI (\ s a -> s{_hccEnableSNI = a});

-- | A string to search for in the body of a health check response. Required
-- for HTTP_STR_MATCH and HTTPS_STR_MATCH health checks. Amazon Route 53
-- considers case when searching for 'SearchString' in the response body.
hccSearchString :: Lens' HealthCheckConfig (Maybe Text)
hccSearchString = lens _hccSearchString (\ s a -> s{_hccSearchString = a});

-- | The minimum number of child health checks that must be healthy for
-- Amazon Route 53 to consider the parent health check to be healthy. Valid
-- values are integers between 0 and 256, inclusive.
hccHealthThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccHealthThreshold = lens _hccHealthThreshold (\ s a -> s{_hccHealthThreshold = a}) . mapping _Nat;

-- | Path to ping on the instance to check the health. Required for HTTP,
-- HTTPS, HTTP_STR_MATCH, and HTTPS_STR_MATCH health checks. The HTTP
-- request is issued to the instance on the given port and path.
hccResourcePath :: Lens' HealthCheckConfig (Maybe Text)
hccResourcePath = lens _hccResourcePath (\ s a -> s{_hccResourcePath = a});

-- | A Boolean value that indicates whether you want Amazon Route 53 to
-- measure the latency between health checkers in multiple AWS regions and
-- your endpoint and to display CloudWatch latency graphs in the Amazon
-- Route 53 console.
hccMeasureLatency :: Lens' HealthCheckConfig (Maybe Bool)
hccMeasureLatency = lens _hccMeasureLatency (\ s a -> s{_hccMeasureLatency = a});

-- | A boolean value that indicates whether the status of health check should
-- be inverted. For example, if a health check is healthy but 'Inverted' is
-- 'True', then Amazon Route 53 considers the health check to be unhealthy.
hccInverted :: Lens' HealthCheckConfig (Maybe Bool)
hccInverted = lens _hccInverted (\ s a -> s{_hccInverted = a});

-- | Fully qualified domain name of the instance to be health checked.
hccFullyQualifiedDomainName :: Lens' HealthCheckConfig (Maybe Text)
hccFullyQualifiedDomainName = lens _hccFullyQualifiedDomainName (\ s a -> s{_hccFullyQualifiedDomainName = a});

-- | For a specified parent health check, a list of 'HealthCheckId' values
-- for the associated child health checks.
hccChildHealthChecks :: Lens' HealthCheckConfig [Text]
hccChildHealthChecks = lens _hccChildHealthChecks (\ s a -> s{_hccChildHealthChecks = a}) . _Default . _Coerce;

-- | The number of seconds between the time that Amazon Route 53 gets a
-- response from your endpoint and the time that it sends the next
-- health-check request.
--
-- Each Amazon Route 53 health checker makes requests at this interval.
-- Valid values are 10 and 30. The default value is 30.
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
                <*> (x .@? "EnableSNI")
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

instance Hashable HealthCheckConfig

instance NFData HealthCheckConfig

instance ToXML HealthCheckConfig where
        toXML HealthCheckConfig'{..}
          = mconcat
              ["FailureThreshold" @= _hccFailureThreshold,
               "IPAddress" @= _hccIPAddress,
               "EnableSNI" @= _hccEnableSNI,
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

-- | A complex type that contains the IP address of a Amazon Route 53 health
-- checker and the reason for the health check status.
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

-- | The IP address of the Amazon Route 53 health checker that performed the
-- health check.
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

instance Hashable HealthCheckObservation

instance NFData HealthCheckObservation

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
-- example, www.example.com. The trailing dot is optional; Amazon Route 53
-- assumes that the domain name is fully qualified. This means that Amazon
-- Route 53 treats www.example.com (without a trailing dot) and
-- www.example.com. (with a trailing dot) as identical.
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

instance Hashable HostedZone

instance NFData HostedZone

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

-- | Undocumented member.
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

instance Hashable HostedZoneConfig

instance NFData HostedZoneConfig

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

-- | The current or new DNS record value, not to exceed 4,000 characters. In
-- the case of a 'DELETE' action, if the current value does not match the
-- actual value, an error is returned. For descriptions about how to format
-- 'Value' for different record types, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types>
-- in the /Amazon Route 53 Developer Guide/.
--
-- You can specify more than one value for all record types except 'CNAME'
-- and 'SOA'.
rrValue :: Lens' ResourceRecord Text
rrValue = lens _rrValue (\ s a -> s{_rrValue = a});

instance FromXML ResourceRecord where
        parseXML x = ResourceRecord' <$> (x .@ "Value")

instance Hashable ResourceRecord

instance NFData ResourceRecord

instance ToXML ResourceRecord where
        toXML ResourceRecord'{..}
          = mconcat ["Value" @= _rrValue]

-- | A complex type that contains information about the current resource
-- record set.
--
-- /See:/ 'resourceRecordSet' smart constructor.
data ResourceRecordSet = ResourceRecordSet'
    { _rrsTTL                     :: !(Maybe Nat)
    , _rrsResourceRecords         :: !(Maybe (List1 ResourceRecord))
    , _rrsAliasTarget             :: !(Maybe AliasTarget)
    , _rrsWeight                  :: !(Maybe Nat)
    , _rrsTrafficPolicyInstanceId :: !(Maybe Text)
    , _rrsSetIdentifier           :: !(Maybe Text)
    , _rrsFailover                :: !(Maybe Failover)
    , _rrsHealthCheckId           :: !(Maybe Text)
    , _rrsRegion                  :: !(Maybe Region)
    , _rrsGeoLocation             :: !(Maybe GeoLocation)
    , _rrsName                    :: !Text
    , _rrsType                    :: !RecordType
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
-- * 'rrsTrafficPolicyInstanceId'
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
    , _rrsTrafficPolicyInstanceId = Nothing
    , _rrsSetIdentifier = Nothing
    , _rrsFailover = Nothing
    , _rrsHealthCheckId = Nothing
    , _rrsRegion = Nothing
    , _rrsGeoLocation = Nothing
    , _rrsName = pName_
    , _rrsType = pType_
    }

-- | The cache time to live for the current resource record set. Note the
-- following:
--
-- -   If you\'re creating an alias resource record set, omit 'TTL'. Amazon
--     Route 53 uses the value of 'TTL' for the alias target.
-- -   If you\'re associating this resource record set with a health check
--     (if you\'re adding a 'HealthCheckId' element), we recommend that you
--     specify a 'TTL' of 60 seconds or less so clients respond quickly to
--     changes in health status.
-- -   All of the resource record sets in a group of weighted, latency,
--     geolocation, or failover resource record sets must have the same
--     value for 'TTL'.
-- -   If a group of weighted resource record sets includes one or more
--     weighted alias resource record sets for which the alias target is an
--     ELB load balancer, we recommend that you specify a 'TTL' of 60
--     seconds for all of the non-alias weighted resource record sets that
--     have the same name and type. Values other than 60 seconds (the TTL
--     for load balancers) will change the effect of the values that you
--     specify for 'Weight'.
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
-- the proportion of DNS queries that Amazon Route 53 responds to using the
-- current resource record set. Amazon Route 53 calculates the sum of the
-- weights for the resource record sets that have the same combination of
-- DNS name and type. Amazon Route 53 then responds to queries based on the
-- ratio of a resource\'s weight to the total. Note the following:
--
-- -   You must specify a value for the 'Weight' element for every weighted
--     resource record set.
-- -   You can only specify one 'ResourceRecord' per weighted resource
--     record set.
-- -   You cannot create latency, failover, or geolocation resource record
--     sets that have the same values for the 'Name' and 'Type' elements as
--     weighted resource record sets.
-- -   You can create a maximum of 100 weighted resource record sets that
--     have the same values for the 'Name' and 'Type' elements.
-- -   For weighted (but not weighted alias) resource record sets, if you
--     set 'Weight' to '0' for a resource record set, Amazon Route 53 never
--     responds to queries with the applicable value for that resource
--     record set. However, if you set 'Weight' to '0' for all resource
--     record sets that have the same combination of DNS name and type,
--     traffic is routed to all resources with equal probability.
--
--     The effect of setting 'Weight' to '0' is different when you
--     associate health checks with weighted resource record sets. For more
--     information, see
--     <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-configuring-options.html Options for Configuring Amazon Route 53 Active-Active and Active-Passive Failover>
--     in the /Amazon Route 53 Developer Guide/.
--
rrsWeight :: Lens' ResourceRecordSet (Maybe Natural)
rrsWeight = lens _rrsWeight (\ s a -> s{_rrsWeight = a}) . mapping _Nat;

-- | Undocumented member.
rrsTrafficPolicyInstanceId :: Lens' ResourceRecordSet (Maybe Text)
rrsTrafficPolicyInstanceId = lens _rrsTrafficPolicyInstanceId (\ s a -> s{_rrsTrafficPolicyInstanceId = a});

-- | /Weighted, Latency, Geo, and Failover resource record sets only:/ An
-- identifier that differentiates among multiple resource record sets that
-- have the same combination of DNS name and type. The value of
-- 'SetIdentifier' must be unique for each resource record set that has the
-- same combination of DNS name and type.
rrsSetIdentifier :: Lens' ResourceRecordSet (Maybe Text)
rrsSetIdentifier = lens _rrsSetIdentifier (\ s a -> s{_rrsSetIdentifier = a});

-- | /Failover resource record sets only:/ To configure failover, you add the
-- 'Failover' element to two resource record sets. For one resource record
-- set, you specify 'PRIMARY' as the value for 'Failover'; for the other
-- resource record set, you specify 'SECONDARY'. In addition, you include
-- the 'HealthCheckId' element and specify the health check that you want
-- Amazon Route 53 to perform for each resource record set.
--
-- You can create failover and failover alias resource record sets only in
-- public hosted zones.
--
-- Except where noted, the following failover behaviors assume that you
-- have included the 'HealthCheckId' element in both resource record sets:
--
-- -   When the primary resource record set is healthy, Amazon Route 53
--     responds to DNS queries with the applicable value from the primary
--     resource record set regardless of the health of the secondary
--     resource record set.
-- -   When the primary resource record set is unhealthy and the secondary
--     resource record set is healthy, Amazon Route 53 responds to DNS
--     queries with the applicable value from the secondary resource record
--     set.
-- -   When the secondary resource record set is unhealthy, Amazon Route 53
--     responds to DNS queries with the applicable value from the primary
--     resource record set regardless of the health of the primary resource
--     record set.
-- -   If you omit the 'HealthCheckId' element for the secondary resource
--     record set, and if the primary resource record set is unhealthy,
--     Amazon Route 53 always responds to DNS queries with the applicable
--     value from the secondary resource record set. This is true
--     regardless of the health of the associated endpoint.
--
-- You cannot create non-failover resource record sets that have the same
-- values for the 'Name' and 'Type' elements as failover resource record
-- sets.
--
-- For failover alias resource record sets, you must also include the
-- 'EvaluateTargetHealth' element and set the value to true.
--
-- For more information about configuring failover for Amazon Route 53, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html Amazon Route 53 Health Checks and DNS Failover>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Valid values: 'PRIMARY' | 'SECONDARY'
rrsFailover :: Lens' ResourceRecordSet (Maybe Failover)
rrsFailover = lens _rrsFailover (\ s a -> s{_rrsFailover = a});

-- | /Health Check resource record sets only, not required for alias resource
-- record sets:/ An identifier that is used to identify health check
-- associated with the resource record set.
rrsHealthCheckId :: Lens' ResourceRecordSet (Maybe Text)
rrsHealthCheckId = lens _rrsHealthCheckId (\ s a -> s{_rrsHealthCheckId = a});

-- | /Latency-based resource record sets only:/ The Amazon EC2 region where
-- the resource that is specified in this resource record set resides. The
-- resource typically is an AWS resource, such as an Amazon EC2 instance or
-- an ELB load balancer, and is referred to by an IP address or a DNS
-- domain name, depending on the record type.
--
-- You can create latency and latency alias resource record sets only in
-- public hosted zones.
--
-- When Amazon Route 53 receives a DNS query for a domain name and type for
-- which you have created latency resource record sets, Amazon Route 53
-- selects the latency resource record set that has the lowest latency
-- between the end user and the associated Amazon EC2 region. Amazon Route
-- 53 then returns the value that is associated with the selected resource
-- record set.
--
-- Note the following:
--
-- -   You can only specify one 'ResourceRecord' per latency resource
--     record set.
-- -   You can only create one latency resource record set for each Amazon
--     EC2 region.
-- -   You are not required to create latency resource record sets for all
--     Amazon EC2 regions. Amazon Route 53 will choose the region with the
--     best latency from among the regions for which you create latency
--     resource record sets.
-- -   You cannot create non-latency resource record sets that have the
--     same values for the 'Name' and 'Type' elements as latency resource
--     record sets.
rrsRegion :: Lens' ResourceRecordSet (Maybe Region)
rrsRegion = lens _rrsRegion (\ s a -> s{_rrsRegion = a});

-- | /Geo location resource record sets only:/ A complex type that lets you
-- control how Amazon Route 53 responds to DNS queries based on the
-- geographic origin of the query. For example, if you want all queries
-- from Africa to be routed to a web server with an IP address of
-- '192.0.2.111', create a resource record set with a 'Type' of 'A' and a
-- 'ContinentCode' of 'AF'.
--
-- You can create geolocation and geolocation alias resource record sets
-- only in public hosted zones.
--
-- If you create separate resource record sets for overlapping geographic
-- regions (for example, one resource record set for a continent and one
-- for a country on the same continent), priority goes to the smallest
-- geographic region. This allows you to route most queries for a continent
-- to one resource and to route queries for a country on that continent to
-- a different resource.
--
-- You cannot create two geolocation resource record sets that specify the
-- same geographic location.
--
-- The value '*' in the 'CountryCode' element matches all geographic
-- locations that aren\'t specified in other geolocation resource record
-- sets that have the same values for the 'Name' and 'Type' elements.
--
-- Geolocation works by mapping IP addresses to locations. However, some IP
-- addresses aren\'t mapped to geographic locations, so even if you create
-- geolocation resource record sets that cover all seven continents, Amazon
-- Route 53 will receive some DNS queries from locations that it can\'t
-- identify. We recommend that you create a resource record set for which
-- the value of 'CountryCode' is '*', which handles both queries that come
-- from locations for which you haven\'t created geolocation resource
-- record sets and queries from IP addresses that aren\'t mapped to a
-- location. If you don\'t create a '*' resource record set, Amazon Route
-- 53 returns a \"no answer\" response for queries from those locations.
--
-- You cannot create non-geolocation resource record sets that have the
-- same values for the 'Name' and 'Type' elements as geolocation resource
-- record sets.
rrsGeoLocation :: Lens' ResourceRecordSet (Maybe GeoLocation)
rrsGeoLocation = lens _rrsGeoLocation (\ s a -> s{_rrsGeoLocation = a});

-- | The name of the domain you want to perform the action on.
--
-- Enter a fully qualified domain name, for example, 'www.example.com'. You
-- can optionally include a trailing dot. If you omit the trailing dot,
-- Amazon Route 53 still assumes that the domain name that you specify is
-- fully qualified. This means that Amazon Route 53 treats
-- 'www.example.com' (without a trailing dot) and 'www.example.com.' (with
-- a trailing dot) as identical.
--
-- For information about how to specify characters other than a-z, 0-9, and
-- - (hyphen) and how to specify internationalized domain names, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format>
-- in the /Amazon Route 53 Developer Guide/.
--
-- You can use an asterisk (*) character in the name. DNS treats the *
-- character either as a wildcard or as the * character (ASCII 42),
-- depending on where it appears in the name. For more information, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-asterisk Using an Asterisk (*) in the Names of Hosted Zones and Resource Record Sets>
-- in the /Amazon Route 53 Developer Guide/
--
-- You can\'t use the * wildcard for resource records sets that have a type
-- of NS.
rrsName :: Lens' ResourceRecordSet Text
rrsName = lens _rrsName (\ s a -> s{_rrsName = a});

-- | The DNS record type. For information about different record types and
-- how data is encoded for them, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Valid values for basic resource record sets: 'A' | 'AAAA' | 'CNAME' |
-- 'MX' | 'NS' | 'PTR' | 'SOA' | 'SPF' | 'SRV' | 'TXT'
--
-- Values for weighted, latency, geolocation, and failover resource record
-- sets: 'A' | 'AAAA' | 'CNAME' | 'MX' | 'PTR' | 'SPF' | 'SRV' | 'TXT'.
-- When creating a group of weighted, latency, geolocation, or failover
-- resource record sets, specify the same value for all of the resource
-- record sets in the group.
--
-- SPF records were formerly used to verify the identity of the sender of
-- email messages. However, we no longer recommend that you create resource
-- record sets for which the value of 'Type' is 'SPF'. RFC 7208, /Sender
-- Policy Framework (SPF) for Authorizing Use of Domains in Email, Version
-- 1/, has been updated to say, \"...[I]ts existence and mechanism defined
-- in [RFC4408] have led to some interoperability issues. Accordingly, its
-- use is no longer appropriate for SPF version 1; implementations are not
-- to use it.\" In RFC 7208, see section 14.1,
-- <http://tools.ietf.org/html/rfc7208#section-14.1 The SPF DNS Record Type>.
--
-- Values for alias resource record sets:
--
-- -   __CloudFront distributions:__ 'A'
-- -   __ELB load balancers:__ 'A' | 'AAAA'
-- -   __Amazon S3 buckets:__ A
-- -   __Another resource record set in this hosted zone:__ Specify the
--     type of the resource record set for which you\'re creating the
--     alias. Specify any value except 'NS' or 'SOA'.
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
                <*> (x .@? "TrafficPolicyInstanceId")
                <*> (x .@? "SetIdentifier")
                <*> (x .@? "Failover")
                <*> (x .@? "HealthCheckId")
                <*> (x .@? "Region")
                <*> (x .@? "GeoLocation")
                <*> (x .@ "Name")
                <*> (x .@ "Type")

instance Hashable ResourceRecordSet

instance NFData ResourceRecordSet

instance ToXML ResourceRecordSet where
        toXML ResourceRecordSet'{..}
          = mconcat
              ["TTL" @= _rrsTTL,
               "ResourceRecords" @=
                 toXML
                   (toXMLList "ResourceRecord" <$> _rrsResourceRecords),
               "AliasTarget" @= _rrsAliasTarget,
               "Weight" @= _rrsWeight,
               "TrafficPolicyInstanceId" @=
                 _rrsTrafficPolicyInstanceId,
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

instance Hashable ResourceTagSet

instance NFData ResourceTagSet

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
-- the time is listed in Coordinated Universal Time (UTC).
srCheckedTime :: Lens' StatusReport (Maybe UTCTime)
srCheckedTime = lens _srCheckedTime (\ s a -> s{_srCheckedTime = a}) . mapping _Time;

instance FromXML StatusReport where
        parseXML x
          = StatusReport' <$>
              (x .@? "Status") <*> (x .@? "CheckedTime")

instance Hashable StatusReport

instance NFData StatusReport

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

instance Hashable Tag

instance NFData Tag

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Value" @= _tagValue, "Key" @= _tagKey]

-- | /See:/ 'trafficPolicy' smart constructor.
data TrafficPolicy = TrafficPolicy'
    { _tpComment  :: !(Maybe Text)
    , _tpId       :: !Text
    , _tpVersion  :: !Nat
    , _tpName     :: !Text
    , _tpType     :: !RecordType
    , _tpDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrafficPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpComment'
--
-- * 'tpId'
--
-- * 'tpVersion'
--
-- * 'tpName'
--
-- * 'tpType'
--
-- * 'tpDocument'
trafficPolicy
    :: Text -- ^ 'tpId'
    -> Natural -- ^ 'tpVersion'
    -> Text -- ^ 'tpName'
    -> RecordType -- ^ 'tpType'
    -> Text -- ^ 'tpDocument'
    -> TrafficPolicy
trafficPolicy pId_ pVersion_ pName_ pType_ pDocument_ =
    TrafficPolicy'
    { _tpComment = Nothing
    , _tpId = pId_
    , _tpVersion = _Nat # pVersion_
    , _tpName = pName_
    , _tpType = pType_
    , _tpDocument = pDocument_
    }

-- | Undocumented member.
tpComment :: Lens' TrafficPolicy (Maybe Text)
tpComment = lens _tpComment (\ s a -> s{_tpComment = a});

-- | Undocumented member.
tpId :: Lens' TrafficPolicy Text
tpId = lens _tpId (\ s a -> s{_tpId = a});

-- | Undocumented member.
tpVersion :: Lens' TrafficPolicy Natural
tpVersion = lens _tpVersion (\ s a -> s{_tpVersion = a}) . _Nat;

-- | Undocumented member.
tpName :: Lens' TrafficPolicy Text
tpName = lens _tpName (\ s a -> s{_tpName = a});

-- | Undocumented member.
tpType :: Lens' TrafficPolicy RecordType
tpType = lens _tpType (\ s a -> s{_tpType = a});

-- | Undocumented member.
tpDocument :: Lens' TrafficPolicy Text
tpDocument = lens _tpDocument (\ s a -> s{_tpDocument = a});

instance FromXML TrafficPolicy where
        parseXML x
          = TrafficPolicy' <$>
              (x .@? "Comment") <*> (x .@ "Id") <*>
                (x .@ "Version")
                <*> (x .@ "Name")
                <*> (x .@ "Type")
                <*> (x .@ "Document")

instance Hashable TrafficPolicy

instance NFData TrafficPolicy

-- | /See:/ 'trafficPolicyInstance' smart constructor.
data TrafficPolicyInstance = TrafficPolicyInstance'
    { _tpiId                   :: !Text
    , _tpiHostedZoneId         :: !Text
    , _tpiName                 :: !Text
    , _tpiTTL                  :: !Nat
    , _tpiState                :: !Text
    , _tpiMessage              :: !Text
    , _tpiTrafficPolicyId      :: !Text
    , _tpiTrafficPolicyVersion :: !Nat
    , _tpiTrafficPolicyType    :: !RecordType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrafficPolicyInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpiId'
--
-- * 'tpiHostedZoneId'
--
-- * 'tpiName'
--
-- * 'tpiTTL'
--
-- * 'tpiState'
--
-- * 'tpiMessage'
--
-- * 'tpiTrafficPolicyId'
--
-- * 'tpiTrafficPolicyVersion'
--
-- * 'tpiTrafficPolicyType'
trafficPolicyInstance
    :: Text -- ^ 'tpiId'
    -> Text -- ^ 'tpiHostedZoneId'
    -> Text -- ^ 'tpiName'
    -> Natural -- ^ 'tpiTTL'
    -> Text -- ^ 'tpiState'
    -> Text -- ^ 'tpiMessage'
    -> Text -- ^ 'tpiTrafficPolicyId'
    -> Natural -- ^ 'tpiTrafficPolicyVersion'
    -> RecordType -- ^ 'tpiTrafficPolicyType'
    -> TrafficPolicyInstance
trafficPolicyInstance pId_ pHostedZoneId_ pName_ pTTL_ pState_ pMessage_ pTrafficPolicyId_ pTrafficPolicyVersion_ pTrafficPolicyType_ =
    TrafficPolicyInstance'
    { _tpiId = pId_
    , _tpiHostedZoneId = pHostedZoneId_
    , _tpiName = pName_
    , _tpiTTL = _Nat # pTTL_
    , _tpiState = pState_
    , _tpiMessage = pMessage_
    , _tpiTrafficPolicyId = pTrafficPolicyId_
    , _tpiTrafficPolicyVersion = _Nat # pTrafficPolicyVersion_
    , _tpiTrafficPolicyType = pTrafficPolicyType_
    }

-- | Undocumented member.
tpiId :: Lens' TrafficPolicyInstance Text
tpiId = lens _tpiId (\ s a -> s{_tpiId = a});

-- | Undocumented member.
tpiHostedZoneId :: Lens' TrafficPolicyInstance Text
tpiHostedZoneId = lens _tpiHostedZoneId (\ s a -> s{_tpiHostedZoneId = a});

-- | Undocumented member.
tpiName :: Lens' TrafficPolicyInstance Text
tpiName = lens _tpiName (\ s a -> s{_tpiName = a});

-- | Undocumented member.
tpiTTL :: Lens' TrafficPolicyInstance Natural
tpiTTL = lens _tpiTTL (\ s a -> s{_tpiTTL = a}) . _Nat;

-- | Undocumented member.
tpiState :: Lens' TrafficPolicyInstance Text
tpiState = lens _tpiState (\ s a -> s{_tpiState = a});

-- | Undocumented member.
tpiMessage :: Lens' TrafficPolicyInstance Text
tpiMessage = lens _tpiMessage (\ s a -> s{_tpiMessage = a});

-- | Undocumented member.
tpiTrafficPolicyId :: Lens' TrafficPolicyInstance Text
tpiTrafficPolicyId = lens _tpiTrafficPolicyId (\ s a -> s{_tpiTrafficPolicyId = a});

-- | Undocumented member.
tpiTrafficPolicyVersion :: Lens' TrafficPolicyInstance Natural
tpiTrafficPolicyVersion = lens _tpiTrafficPolicyVersion (\ s a -> s{_tpiTrafficPolicyVersion = a}) . _Nat;

-- | Undocumented member.
tpiTrafficPolicyType :: Lens' TrafficPolicyInstance RecordType
tpiTrafficPolicyType = lens _tpiTrafficPolicyType (\ s a -> s{_tpiTrafficPolicyType = a});

instance FromXML TrafficPolicyInstance where
        parseXML x
          = TrafficPolicyInstance' <$>
              (x .@ "Id") <*> (x .@ "HostedZoneId") <*>
                (x .@ "Name")
                <*> (x .@ "TTL")
                <*> (x .@ "State")
                <*> (x .@ "Message")
                <*> (x .@ "TrafficPolicyId")
                <*> (x .@ "TrafficPolicyVersion")
                <*> (x .@ "TrafficPolicyType")

instance Hashable TrafficPolicyInstance

instance NFData TrafficPolicyInstance

-- | /See:/ 'trafficPolicySummary' smart constructor.
data TrafficPolicySummary = TrafficPolicySummary'
    { _tpsId                 :: !Text
    , _tpsName               :: !Text
    , _tpsType               :: !RecordType
    , _tpsLatestVersion      :: !Nat
    , _tpsTrafficPolicyCount :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrafficPolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpsId'
--
-- * 'tpsName'
--
-- * 'tpsType'
--
-- * 'tpsLatestVersion'
--
-- * 'tpsTrafficPolicyCount'
trafficPolicySummary
    :: Text -- ^ 'tpsId'
    -> Text -- ^ 'tpsName'
    -> RecordType -- ^ 'tpsType'
    -> Natural -- ^ 'tpsLatestVersion'
    -> Natural -- ^ 'tpsTrafficPolicyCount'
    -> TrafficPolicySummary
trafficPolicySummary pId_ pName_ pType_ pLatestVersion_ pTrafficPolicyCount_ =
    TrafficPolicySummary'
    { _tpsId = pId_
    , _tpsName = pName_
    , _tpsType = pType_
    , _tpsLatestVersion = _Nat # pLatestVersion_
    , _tpsTrafficPolicyCount = _Nat # pTrafficPolicyCount_
    }

-- | Undocumented member.
tpsId :: Lens' TrafficPolicySummary Text
tpsId = lens _tpsId (\ s a -> s{_tpsId = a});

-- | Undocumented member.
tpsName :: Lens' TrafficPolicySummary Text
tpsName = lens _tpsName (\ s a -> s{_tpsName = a});

-- | Undocumented member.
tpsType :: Lens' TrafficPolicySummary RecordType
tpsType = lens _tpsType (\ s a -> s{_tpsType = a});

-- | Undocumented member.
tpsLatestVersion :: Lens' TrafficPolicySummary Natural
tpsLatestVersion = lens _tpsLatestVersion (\ s a -> s{_tpsLatestVersion = a}) . _Nat;

-- | Undocumented member.
tpsTrafficPolicyCount :: Lens' TrafficPolicySummary Natural
tpsTrafficPolicyCount = lens _tpsTrafficPolicyCount (\ s a -> s{_tpsTrafficPolicyCount = a}) . _Nat;

instance FromXML TrafficPolicySummary where
        parseXML x
          = TrafficPolicySummary' <$>
              (x .@ "Id") <*> (x .@ "Name") <*> (x .@ "Type") <*>
                (x .@ "LatestVersion")
                <*> (x .@ "TrafficPolicyCount")

instance Hashable TrafficPolicySummary

instance NFData TrafficPolicySummary

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

instance Hashable VPC

instance NFData VPC

instance ToXML VPC where
        toXML VPC'{..}
          = mconcat
              ["VPCRegion" @= _vpcVPCRegion, "VPCId" @= _vpcVPCId]
