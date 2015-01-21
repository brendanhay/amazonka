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

-- Module      : Network.AWS.ELB.Types
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

module Network.AWS.ELB.Types
    (
    -- * Service
      ELB
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * SourceSecurityGroup
    , SourceSecurityGroup
    , sourceSecurityGroup
    , ssgGroupName
    , ssgOwnerAlias

    -- * TagDescription
    , TagDescription
    , tagDescription
    , tdLoadBalancerName
    , tdTags

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * PolicyAttributeTypeDescription
    , PolicyAttributeTypeDescription
    , policyAttributeTypeDescription
    , patdAttributeName
    , patdAttributeType
    , patdCardinality
    , patdDefaultValue
    , patdDescription

    -- * HealthCheck
    , HealthCheck
    , healthCheck
    , hcHealthyThreshold
    , hcInterval
    , hcTarget
    , hcTimeout
    , hcUnhealthyThreshold

    -- * CrossZoneLoadBalancing
    , CrossZoneLoadBalancing
    , crossZoneLoadBalancing
    , czlbEnabled

    -- * LoadBalancerAttributes
    , LoadBalancerAttributes
    , loadBalancerAttributes
    , lbaAccessLog
    , lbaAdditionalAttributes
    , lbaConnectionDraining
    , lbaConnectionSettings
    , lbaCrossZoneLoadBalancing

    -- * AccessLog
    , AccessLog
    , accessLog
    , alEmitInterval
    , alEnabled
    , alS3BucketName
    , alS3BucketPrefix

    -- * ListenerDescription
    , ListenerDescription
    , listenerDescription
    , ldListener
    , ldPolicyNames

    -- * LBCookieStickinessPolicy
    , LBCookieStickinessPolicy
    , lbcookieStickinessPolicy
    , lbcspCookieExpirationPeriod
    , lbcspPolicyName

    -- * PolicyDescription
    , PolicyDescription
    , policyDescription
    , pdPolicyAttributeDescriptions
    , pdPolicyName
    , pdPolicyTypeName

    -- * AppCookieStickinessPolicy
    , AppCookieStickinessPolicy
    , appCookieStickinessPolicy
    , acspCookieName
    , acspPolicyName

    -- * PolicyAttribute
    , PolicyAttribute
    , policyAttribute
    , paAttributeName
    , paAttributeValue

    -- * LoadBalancerDescription
    , LoadBalancerDescription
    , loadBalancerDescription
    , lbdAvailabilityZones
    , lbdBackendServerDescriptions
    , lbdCanonicalHostedZoneName
    , lbdCanonicalHostedZoneNameID
    , lbdCreatedTime
    , lbdDNSName
    , lbdHealthCheck
    , lbdInstances
    , lbdListenerDescriptions
    , lbdLoadBalancerName
    , lbdPolicies
    , lbdScheme
    , lbdSecurityGroups
    , lbdSourceSecurityGroup
    , lbdSubnets
    , lbdVPCId

    -- * BackendServerDescription
    , BackendServerDescription
    , backendServerDescription
    , bsdInstancePort
    , bsdPolicyNames

    -- * PolicyAttributeDescription
    , PolicyAttributeDescription
    , policyAttributeDescription
    , padAttributeName
    , padAttributeValue

    -- * AdditionalAttribute
    , AdditionalAttribute
    , additionalAttribute
    , aaKey
    , aaValue

    -- * ConnectionSettings
    , ConnectionSettings
    , connectionSettings
    , csIdleTimeout

    -- * PolicyTypeDescription
    , PolicyTypeDescription
    , policyTypeDescription
    , ptdDescription
    , ptdPolicyAttributeTypeDescriptions
    , ptdPolicyTypeName

    -- * Policies
    , Policies
    , policies
    , pAppCookieStickinessPolicies
    , pLBCookieStickinessPolicies
    , pOtherPolicies

    -- * Listener
    , Listener
    , listener
    , lInstancePort
    , lInstanceProtocol
    , lLoadBalancerPort
    , lProtocol
    , lSSLCertificateId

    -- * ConnectionDraining
    , ConnectionDraining
    , connectionDraining
    , cdEnabled
    , cdTimeout

    -- * InstanceState
    , InstanceState
    , instanceState
    , isDescription
    , isInstanceId
    , isReasonCode
    , isState

    -- * TagKeyOnly
    , TagKeyOnly
    , tagKeyOnly
    , tkoKey

    -- * Instance
    , Instance
    , instance'
    , iInstanceId
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2012-06-01@ of the Amazon Elastic Load Balancing service.
data ELB

instance AWSService ELB where
    type Sg ELB = V4
    type Er ELB = RESTError

    service = service'
      where
        service' :: Service ELB
        service' = Service
            { _svcAbbrev       = "ELB"
            , _svcPrefix       = "elasticloadbalancing"
            , _svcVersion      = "2012-06-01"
            , _svcTargetPrefix = Nothing
            , _svcJSONVersion  = Nothing
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry ELB
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 400 && "Throttling" == e = True -- Throttling
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://elasticloadbalancing.amazonaws.com/doc/2012-06-01/"
{-# INLINE ns #-}

data SourceSecurityGroup = SourceSecurityGroup
    { _ssgGroupName  :: Maybe Text
    , _ssgOwnerAlias :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'SourceSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssgGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ssgOwnerAlias' @::@ 'Maybe' 'Text'
--
sourceSecurityGroup :: SourceSecurityGroup
sourceSecurityGroup = SourceSecurityGroup
    { _ssgOwnerAlias = Nothing
    , _ssgGroupName  = Nothing
    }

-- | Name of the source security group. Use this value for the '--source-group'
-- parameter of the 'ec2-authorize' command in the Amazon EC2 command line tool.
ssgGroupName :: Lens' SourceSecurityGroup (Maybe Text)
ssgGroupName = lens _ssgGroupName (\s a -> s { _ssgGroupName = a })

-- | Owner of the source security group. Use this value for the '--source-group-user' parameter of the 'ec2-authorize' command in the Amazon EC2 command line tool.
ssgOwnerAlias :: Lens' SourceSecurityGroup (Maybe Text)
ssgOwnerAlias = lens _ssgOwnerAlias (\s a -> s { _ssgOwnerAlias = a })

instance FromXML SourceSecurityGroup where
    parseXML x = SourceSecurityGroup
        <$> x .@? "GroupName"
        <*> x .@? "OwnerAlias"

instance ToQuery SourceSecurityGroup where
    toQuery SourceSecurityGroup{..} = mconcat
        [ "GroupName"  =? _ssgGroupName
        , "OwnerAlias" =? _ssgOwnerAlias
        ]

data TagDescription = TagDescription
    { _tdLoadBalancerName :: Maybe Text
    , _tdTags             :: List1 "member" Tag
    } deriving (Eq, Read, Show)

-- | 'TagDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdLoadBalancerName' @::@ 'Maybe' 'Text'
--
-- * 'tdTags' @::@ 'NonEmpty' 'Tag'
--
tagDescription :: NonEmpty Tag -- ^ 'tdTags'
               -> TagDescription
tagDescription p1 = TagDescription
    { _tdTags             = withIso _List1 (const id) p1
    , _tdLoadBalancerName = Nothing
    }

-- | The name of the load balancer.
tdLoadBalancerName :: Lens' TagDescription (Maybe Text)
tdLoadBalancerName =
    lens _tdLoadBalancerName (\s a -> s { _tdLoadBalancerName = a })

-- | List of tags associated with the load balancer.
tdTags :: Lens' TagDescription (NonEmpty Tag)
tdTags = lens _tdTags (\s a -> s { _tdTags = a }) . _List1

instance FromXML TagDescription where
    parseXML x = TagDescription
        <$> x .@? "LoadBalancerName"
        <*> x .@  "Tags"

instance ToQuery TagDescription where
    toQuery TagDescription{..} = mconcat
        [ "LoadBalancerName" =? _tdLoadBalancerName
        , "Tags"             =? _tdTags
        ]

data Tag = Tag
    { _tagKey   :: Text
    , _tagValue :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey' @::@ 'Text'
--
-- * 'tagValue' @::@ 'Maybe' 'Text'
--
tag :: Text -- ^ 'tagKey'
    -> Tag
tag p1 = Tag
    { _tagKey   = p1
    , _tagValue = Nothing
    }

-- | The key of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | The value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromXML Tag where
    parseXML x = Tag
        <$> x .@  "Key"
        <*> x .@? "Value"

instance ToQuery Tag where
    toQuery Tag{..} = mconcat
        [ "Key"   =? _tagKey
        , "Value" =? _tagValue
        ]

data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription
    { _patdAttributeName :: Maybe Text
    , _patdAttributeType :: Maybe Text
    , _patdCardinality   :: Maybe Text
    , _patdDefaultValue  :: Maybe Text
    , _patdDescription   :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PolicyAttributeTypeDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'patdAttributeName' @::@ 'Maybe' 'Text'
--
-- * 'patdAttributeType' @::@ 'Maybe' 'Text'
--
-- * 'patdCardinality' @::@ 'Maybe' 'Text'
--
-- * 'patdDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'patdDescription' @::@ 'Maybe' 'Text'
--
policyAttributeTypeDescription :: PolicyAttributeTypeDescription
policyAttributeTypeDescription = PolicyAttributeTypeDescription
    { _patdAttributeName = Nothing
    , _patdAttributeType = Nothing
    , _patdDescription   = Nothing
    , _patdDefaultValue  = Nothing
    , _patdCardinality   = Nothing
    }

-- | The name of the attribute associated with the policy type.
patdAttributeName :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdAttributeName =
    lens _patdAttributeName (\s a -> s { _patdAttributeName = a })

-- | The type of attribute. For example, Boolean, Integer, etc.
patdAttributeType :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdAttributeType =
    lens _patdAttributeType (\s a -> s { _patdAttributeType = a })

-- | The cardinality of the attribute. Valid Values:  ONE(1) : Single value
-- required ZERO_OR_ONE(0..1) : Up to one value can be supplied ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
-- ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
patdCardinality :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdCardinality = lens _patdCardinality (\s a -> s { _patdCardinality = a })

-- | The default value of the attribute, if applicable.
patdDefaultValue :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdDefaultValue = lens _patdDefaultValue (\s a -> s { _patdDefaultValue = a })

-- | A human-readable description of the attribute.
patdDescription :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdDescription = lens _patdDescription (\s a -> s { _patdDescription = a })

instance FromXML PolicyAttributeTypeDescription where
    parseXML x = PolicyAttributeTypeDescription
        <$> x .@? "AttributeName"
        <*> x .@? "AttributeType"
        <*> x .@? "Cardinality"
        <*> x .@? "DefaultValue"
        <*> x .@? "Description"

instance ToQuery PolicyAttributeTypeDescription where
    toQuery PolicyAttributeTypeDescription{..} = mconcat
        [ "AttributeName" =? _patdAttributeName
        , "AttributeType" =? _patdAttributeType
        , "Cardinality"   =? _patdCardinality
        , "DefaultValue"  =? _patdDefaultValue
        , "Description"   =? _patdDescription
        ]

data HealthCheck = HealthCheck
    { _hcHealthyThreshold   :: Nat
    , _hcInterval           :: Nat
    , _hcTarget             :: Text
    , _hcTimeout            :: Nat
    , _hcUnhealthyThreshold :: Nat
    } deriving (Eq, Ord, Read, Show)

-- | 'HealthCheck' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hcHealthyThreshold' @::@ 'Natural'
--
-- * 'hcInterval' @::@ 'Natural'
--
-- * 'hcTarget' @::@ 'Text'
--
-- * 'hcTimeout' @::@ 'Natural'
--
-- * 'hcUnhealthyThreshold' @::@ 'Natural'
--
healthCheck :: Text -- ^ 'hcTarget'
            -> Natural -- ^ 'hcInterval'
            -> Natural -- ^ 'hcTimeout'
            -> Natural -- ^ 'hcUnhealthyThreshold'
            -> Natural -- ^ 'hcHealthyThreshold'
            -> HealthCheck
healthCheck p1 p2 p3 p4 p5 = HealthCheck
    { _hcTarget             = p1
    , _hcInterval           = withIso _Nat (const id) p2
    , _hcTimeout            = withIso _Nat (const id) p3
    , _hcUnhealthyThreshold = withIso _Nat (const id) p4
    , _hcHealthyThreshold   = withIso _Nat (const id) p5
    }

-- | Specifies the number of consecutive health probe successes required before
-- moving the instance to the /Healthy/ state.
hcHealthyThreshold :: Lens' HealthCheck Natural
hcHealthyThreshold =
    lens _hcHealthyThreshold (\s a -> s { _hcHealthyThreshold = a })
        . _Nat

-- | Specifies the approximate interval, in seconds, between health checks of an
-- individual instance.
hcInterval :: Lens' HealthCheck Natural
hcInterval = lens _hcInterval (\s a -> s { _hcInterval = a }) . _Nat

-- | Specifies the instance being checked. The protocol is either TCP, HTTP,
-- HTTPS, or SSL. The range of valid ports is one (1) through 65535.
--
-- TCP is the default, specified as a TCP: port pair, for example "TCP:5000".
-- In this case a healthcheck simply attempts to open a TCP connection to the
-- instance on the specified port. Failure to connect within the configured
-- timeout is considered unhealthy.
--
-- SSL is also specified as SSL: port pair, for example, SSL:5000.
--
-- For HTTP or HTTPS protocol, the situation is different. You have to include
-- a ping path in the string. HTTP is specified as a HTTP:port;/;PathToPing;
-- grouping, for example "HTTP:80/weather/us/wa/seattle". In this case, a HTTP
-- GET request is issued to the instance on the given port and path. Any answer
-- other than "200 OK" within the timeout period is considered unhealthy.
--
-- The total length of the HTTP ping target needs to be 1024 16-bit Unicode
-- characters or less.
--
--
hcTarget :: Lens' HealthCheck Text
hcTarget = lens _hcTarget (\s a -> s { _hcTarget = a })

-- | Specifies the amount of time, in seconds, during which no response means a
-- failed health probe.
--
-- This value must be less than the /Interval/ value.
hcTimeout :: Lens' HealthCheck Natural
hcTimeout = lens _hcTimeout (\s a -> s { _hcTimeout = a }) . _Nat

-- | Specifies the number of consecutive health probe failures required before
-- moving the instance to the /Unhealthy/ state.
hcUnhealthyThreshold :: Lens' HealthCheck Natural
hcUnhealthyThreshold =
    lens _hcUnhealthyThreshold (\s a -> s { _hcUnhealthyThreshold = a })
        . _Nat

instance FromXML HealthCheck where
    parseXML x = HealthCheck
        <$> x .@  "HealthyThreshold"
        <*> x .@  "Interval"
        <*> x .@  "Target"
        <*> x .@  "Timeout"
        <*> x .@  "UnhealthyThreshold"

instance ToQuery HealthCheck where
    toQuery HealthCheck{..} = mconcat
        [ "HealthyThreshold"   =? _hcHealthyThreshold
        , "Interval"           =? _hcInterval
        , "Target"             =? _hcTarget
        , "Timeout"            =? _hcTimeout
        , "UnhealthyThreshold" =? _hcUnhealthyThreshold
        ]

newtype CrossZoneLoadBalancing = CrossZoneLoadBalancing
    { _czlbEnabled :: Bool
    } deriving (Eq, Ord, Read, Show, Enum)

-- | 'CrossZoneLoadBalancing' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'czlbEnabled' @::@ 'Bool'
--
crossZoneLoadBalancing :: Bool -- ^ 'czlbEnabled'
                       -> CrossZoneLoadBalancing
crossZoneLoadBalancing p1 = CrossZoneLoadBalancing
    { _czlbEnabled = p1
    }

-- | Specifies whether cross-zone load balancing is enabled for the load balancer.
czlbEnabled :: Lens' CrossZoneLoadBalancing Bool
czlbEnabled = lens _czlbEnabled (\s a -> s { _czlbEnabled = a })

instance FromXML CrossZoneLoadBalancing where
    parseXML x = CrossZoneLoadBalancing
        <$> x .@  "Enabled"

instance ToQuery CrossZoneLoadBalancing where
    toQuery CrossZoneLoadBalancing{..} = mconcat
        [ "Enabled" =? _czlbEnabled
        ]

data LoadBalancerAttributes = LoadBalancerAttributes
    { _lbaAccessLog              :: Maybe AccessLog
    , _lbaAdditionalAttributes   :: List "member" AdditionalAttribute
    , _lbaConnectionDraining     :: Maybe ConnectionDraining
    , _lbaConnectionSettings     :: Maybe ConnectionSettings
    , _lbaCrossZoneLoadBalancing :: Maybe CrossZoneLoadBalancing
    } deriving (Eq, Read, Show)

-- | 'LoadBalancerAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbaAccessLog' @::@ 'Maybe' 'AccessLog'
--
-- * 'lbaAdditionalAttributes' @::@ ['AdditionalAttribute']
--
-- * 'lbaConnectionDraining' @::@ 'Maybe' 'ConnectionDraining'
--
-- * 'lbaConnectionSettings' @::@ 'Maybe' 'ConnectionSettings'
--
-- * 'lbaCrossZoneLoadBalancing' @::@ 'Maybe' 'CrossZoneLoadBalancing'
--
loadBalancerAttributes :: LoadBalancerAttributes
loadBalancerAttributes = LoadBalancerAttributes
    { _lbaCrossZoneLoadBalancing = Nothing
    , _lbaAccessLog              = Nothing
    , _lbaConnectionDraining     = Nothing
    , _lbaConnectionSettings     = Nothing
    , _lbaAdditionalAttributes   = mempty
    }

-- | The name of the load balancer attribute. If enabled, the load balancer
-- captures detailed information of all the requests and delivers the
-- information to the Amazon S3 bucket that you specify.
--
-- For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/enable-access-logs.html Enable Access Logs>.
lbaAccessLog :: Lens' LoadBalancerAttributes (Maybe AccessLog)
lbaAccessLog = lens _lbaAccessLog (\s a -> s { _lbaAccessLog = a })

lbaAdditionalAttributes :: Lens' LoadBalancerAttributes [AdditionalAttribute]
lbaAdditionalAttributes =
    lens _lbaAdditionalAttributes (\s a -> s { _lbaAdditionalAttributes = a })
        . _List

-- | The name of the load balancer attribute. If enabled, the load balancer allows
-- existing requests to complete before the load balancer shifts traffic away
-- from a deregistered or unhealthy back-end instance.
--
-- For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/config-conn-drain.html Enable Connection Draining>.
lbaConnectionDraining :: Lens' LoadBalancerAttributes (Maybe ConnectionDraining)
lbaConnectionDraining =
    lens _lbaConnectionDraining (\s a -> s { _lbaConnectionDraining = a })

-- | The name of the load balancer attribute.
--
-- By default, Elastic Load Balancing maintains a 60-second idle connection
-- timeout for both front-end and back-end connections of your load balancer. If
-- the 'ConnectionSettings' attribute is set, Elastic Load Balancing will allow
-- the connections to remain idle (no data is sent over the connection) for the
-- specified duration.
--
-- For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/config-idle-timeout.html Configure Idle Connection Timeout>.
lbaConnectionSettings :: Lens' LoadBalancerAttributes (Maybe ConnectionSettings)
lbaConnectionSettings =
    lens _lbaConnectionSettings (\s a -> s { _lbaConnectionSettings = a })

-- | The name of the load balancer attribute. If enabled, the load balancer routes
-- the request traffic evenly across all back-end instances regardless of the
-- Availability Zones.
--
-- For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/enable-disable-crosszone-lb.html Enable Cross-Zone Load Balancing>.
lbaCrossZoneLoadBalancing :: Lens' LoadBalancerAttributes (Maybe CrossZoneLoadBalancing)
lbaCrossZoneLoadBalancing =
    lens _lbaCrossZoneLoadBalancing
        (\s a -> s { _lbaCrossZoneLoadBalancing = a })

instance FromXML LoadBalancerAttributes where
    parseXML x = LoadBalancerAttributes
        <$> x .@? "AccessLog"
        <*> x .@? "AdditionalAttributes" .!@ mempty
        <*> x .@? "ConnectionDraining"
        <*> x .@? "ConnectionSettings"
        <*> x .@? "CrossZoneLoadBalancing"

instance ToQuery LoadBalancerAttributes where
    toQuery LoadBalancerAttributes{..} = mconcat
        [ "AccessLog"              =? _lbaAccessLog
        , "AdditionalAttributes"   =? _lbaAdditionalAttributes
        , "ConnectionDraining"     =? _lbaConnectionDraining
        , "ConnectionSettings"     =? _lbaConnectionSettings
        , "CrossZoneLoadBalancing" =? _lbaCrossZoneLoadBalancing
        ]

data AccessLog = AccessLog
    { _alEmitInterval   :: Maybe Int
    , _alEnabled        :: Bool
    , _alS3BucketName   :: Maybe Text
    , _alS3BucketPrefix :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AccessLog' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'alEmitInterval' @::@ 'Maybe' 'Int'
--
-- * 'alEnabled' @::@ 'Bool'
--
-- * 'alS3BucketName' @::@ 'Maybe' 'Text'
--
-- * 'alS3BucketPrefix' @::@ 'Maybe' 'Text'
--
accessLog :: Bool -- ^ 'alEnabled'
          -> AccessLog
accessLog p1 = AccessLog
    { _alEnabled        = p1
    , _alS3BucketName   = Nothing
    , _alEmitInterval   = Nothing
    , _alS3BucketPrefix = Nothing
    }

-- | The interval for publishing the access logs. You can specify an interval of
-- either 5 minutes or 60 minutes.
--
-- Default: 60 minutes
alEmitInterval :: Lens' AccessLog (Maybe Int)
alEmitInterval = lens _alEmitInterval (\s a -> s { _alEmitInterval = a })

-- | Specifies whether access log is enabled for the load balancer.
alEnabled :: Lens' AccessLog Bool
alEnabled = lens _alEnabled (\s a -> s { _alEnabled = a })

-- | The name of the Amazon S3 bucket where the access logs are stored.
alS3BucketName :: Lens' AccessLog (Maybe Text)
alS3BucketName = lens _alS3BucketName (\s a -> s { _alS3BucketName = a })

-- | The logical hierarchy you created for your Amazon S3 bucket, for example 'my-bucket-prefix/prod'. If the prefix is not provided, the log is placed at the root level of the
-- bucket.
alS3BucketPrefix :: Lens' AccessLog (Maybe Text)
alS3BucketPrefix = lens _alS3BucketPrefix (\s a -> s { _alS3BucketPrefix = a })

instance FromXML AccessLog where
    parseXML x = AccessLog
        <$> x .@? "EmitInterval"
        <*> x .@  "Enabled"
        <*> x .@? "S3BucketName"
        <*> x .@? "S3BucketPrefix"

instance ToQuery AccessLog where
    toQuery AccessLog{..} = mconcat
        [ "EmitInterval"   =? _alEmitInterval
        , "Enabled"        =? _alEnabled
        , "S3BucketName"   =? _alS3BucketName
        , "S3BucketPrefix" =? _alS3BucketPrefix
        ]

data ListenerDescription = ListenerDescription
    { _ldListener    :: Maybe Listener
    , _ldPolicyNames :: List "member" Text
    } deriving (Eq, Read, Show)

-- | 'ListenerDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldListener' @::@ 'Maybe' 'Listener'
--
-- * 'ldPolicyNames' @::@ ['Text']
--
listenerDescription :: ListenerDescription
listenerDescription = ListenerDescription
    { _ldListener    = Nothing
    , _ldPolicyNames = mempty
    }

ldListener :: Lens' ListenerDescription (Maybe Listener)
ldListener = lens _ldListener (\s a -> s { _ldListener = a })

-- | A list of policies enabled for this listener. An empty list indicates that
-- no policies are enabled.
ldPolicyNames :: Lens' ListenerDescription [Text]
ldPolicyNames = lens _ldPolicyNames (\s a -> s { _ldPolicyNames = a }) . _List

instance FromXML ListenerDescription where
    parseXML x = ListenerDescription
        <$> x .@? "Listener"
        <*> x .@? "PolicyNames" .!@ mempty

instance ToQuery ListenerDescription where
    toQuery ListenerDescription{..} = mconcat
        [ "Listener"    =? _ldListener
        , "PolicyNames" =? _ldPolicyNames
        ]

data LBCookieStickinessPolicy = LBCookieStickinessPolicy
    { _lbcspCookieExpirationPeriod :: Maybe Integer
    , _lbcspPolicyName             :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'LBCookieStickinessPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbcspCookieExpirationPeriod' @::@ 'Maybe' 'Integer'
--
-- * 'lbcspPolicyName' @::@ 'Maybe' 'Text'
--
lbcookieStickinessPolicy :: LBCookieStickinessPolicy
lbcookieStickinessPolicy = LBCookieStickinessPolicy
    { _lbcspPolicyName             = Nothing
    , _lbcspCookieExpirationPeriod = Nothing
    }

-- | The time period in seconds after which the cookie should be considered stale.
-- Not specifying this parameter indicates that the stickiness session will last
-- for the duration of the browser session.
lbcspCookieExpirationPeriod :: Lens' LBCookieStickinessPolicy (Maybe Integer)
lbcspCookieExpirationPeriod =
    lens _lbcspCookieExpirationPeriod
        (\s a -> s { _lbcspCookieExpirationPeriod = a })

-- | The name for the policy being created. The name must be unique within the set
-- of policies for this load balancer.
lbcspPolicyName :: Lens' LBCookieStickinessPolicy (Maybe Text)
lbcspPolicyName = lens _lbcspPolicyName (\s a -> s { _lbcspPolicyName = a })

instance FromXML LBCookieStickinessPolicy where
    parseXML x = LBCookieStickinessPolicy
        <$> x .@? "CookieExpirationPeriod"
        <*> x .@? "PolicyName"

instance ToQuery LBCookieStickinessPolicy where
    toQuery LBCookieStickinessPolicy{..} = mconcat
        [ "CookieExpirationPeriod" =? _lbcspCookieExpirationPeriod
        , "PolicyName"             =? _lbcspPolicyName
        ]

data PolicyDescription = PolicyDescription
    { _pdPolicyAttributeDescriptions :: List "member" PolicyAttributeDescription
    , _pdPolicyName                  :: Maybe Text
    , _pdPolicyTypeName              :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'PolicyDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdPolicyAttributeDescriptions' @::@ ['PolicyAttributeDescription']
--
-- * 'pdPolicyName' @::@ 'Maybe' 'Text'
--
-- * 'pdPolicyTypeName' @::@ 'Maybe' 'Text'
--
policyDescription :: PolicyDescription
policyDescription = PolicyDescription
    { _pdPolicyName                  = Nothing
    , _pdPolicyTypeName              = Nothing
    , _pdPolicyAttributeDescriptions = mempty
    }

-- | A list of policy attribute description structures.
pdPolicyAttributeDescriptions :: Lens' PolicyDescription [PolicyAttributeDescription]
pdPolicyAttributeDescriptions =
    lens _pdPolicyAttributeDescriptions
        (\s a -> s { _pdPolicyAttributeDescriptions = a })
            . _List

-- | The name of the policy associated with the load balancer.
pdPolicyName :: Lens' PolicyDescription (Maybe Text)
pdPolicyName = lens _pdPolicyName (\s a -> s { _pdPolicyName = a })

-- | The name of the policy type associated with the load balancer.
pdPolicyTypeName :: Lens' PolicyDescription (Maybe Text)
pdPolicyTypeName = lens _pdPolicyTypeName (\s a -> s { _pdPolicyTypeName = a })

instance FromXML PolicyDescription where
    parseXML x = PolicyDescription
        <$> x .@? "PolicyAttributeDescriptions" .!@ mempty
        <*> x .@? "PolicyName"
        <*> x .@? "PolicyTypeName"

instance ToQuery PolicyDescription where
    toQuery PolicyDescription{..} = mconcat
        [ "PolicyAttributeDescriptions" =? _pdPolicyAttributeDescriptions
        , "PolicyName"                  =? _pdPolicyName
        , "PolicyTypeName"              =? _pdPolicyTypeName
        ]

data AppCookieStickinessPolicy = AppCookieStickinessPolicy
    { _acspCookieName :: Maybe Text
    , _acspPolicyName :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AppCookieStickinessPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acspCookieName' @::@ 'Maybe' 'Text'
--
-- * 'acspPolicyName' @::@ 'Maybe' 'Text'
--
appCookieStickinessPolicy :: AppCookieStickinessPolicy
appCookieStickinessPolicy = AppCookieStickinessPolicy
    { _acspPolicyName = Nothing
    , _acspCookieName = Nothing
    }

-- | The name of the application cookie used for stickiness.
acspCookieName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acspCookieName = lens _acspCookieName (\s a -> s { _acspCookieName = a })

-- | The mnemonic name for the policy being created. The name must be unique
-- within a set of policies for this load balancer.
acspPolicyName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acspPolicyName = lens _acspPolicyName (\s a -> s { _acspPolicyName = a })

instance FromXML AppCookieStickinessPolicy where
    parseXML x = AppCookieStickinessPolicy
        <$> x .@? "CookieName"
        <*> x .@? "PolicyName"

instance ToQuery AppCookieStickinessPolicy where
    toQuery AppCookieStickinessPolicy{..} = mconcat
        [ "CookieName" =? _acspCookieName
        , "PolicyName" =? _acspPolicyName
        ]

data PolicyAttribute = PolicyAttribute
    { _paAttributeName  :: Maybe Text
    , _paAttributeValue :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PolicyAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'paAttributeName' @::@ 'Maybe' 'Text'
--
-- * 'paAttributeValue' @::@ 'Maybe' 'Text'
--
policyAttribute :: PolicyAttribute
policyAttribute = PolicyAttribute
    { _paAttributeName  = Nothing
    , _paAttributeValue = Nothing
    }

-- | The name of the attribute associated with the policy.
paAttributeName :: Lens' PolicyAttribute (Maybe Text)
paAttributeName = lens _paAttributeName (\s a -> s { _paAttributeName = a })

-- | The value of the attribute associated with the policy.
paAttributeValue :: Lens' PolicyAttribute (Maybe Text)
paAttributeValue = lens _paAttributeValue (\s a -> s { _paAttributeValue = a })

instance FromXML PolicyAttribute where
    parseXML x = PolicyAttribute
        <$> x .@? "AttributeName"
        <*> x .@? "AttributeValue"

instance ToQuery PolicyAttribute where
    toQuery PolicyAttribute{..} = mconcat
        [ "AttributeName"  =? _paAttributeName
        , "AttributeValue" =? _paAttributeValue
        ]

data LoadBalancerDescription = LoadBalancerDescription
    { _lbdAvailabilityZones         :: List "member" Text
    , _lbdBackendServerDescriptions :: List "member" BackendServerDescription
    , _lbdCanonicalHostedZoneName   :: Maybe Text
    , _lbdCanonicalHostedZoneNameID :: Maybe Text
    , _lbdCreatedTime               :: Maybe ISO8601
    , _lbdDNSName                   :: Maybe Text
    , _lbdHealthCheck               :: Maybe HealthCheck
    , _lbdInstances                 :: List "member" Instance
    , _lbdListenerDescriptions      :: List "member" ListenerDescription
    , _lbdLoadBalancerName          :: Maybe Text
    , _lbdPolicies                  :: Maybe Policies
    , _lbdScheme                    :: Maybe Text
    , _lbdSecurityGroups            :: List "member" Text
    , _lbdSourceSecurityGroup       :: Maybe SourceSecurityGroup
    , _lbdSubnets                   :: List "member" Text
    , _lbdVPCId                     :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'LoadBalancerDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbdAvailabilityZones' @::@ ['Text']
--
-- * 'lbdBackendServerDescriptions' @::@ ['BackendServerDescription']
--
-- * 'lbdCanonicalHostedZoneName' @::@ 'Maybe' 'Text'
--
-- * 'lbdCanonicalHostedZoneNameID' @::@ 'Maybe' 'Text'
--
-- * 'lbdCreatedTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'lbdDNSName' @::@ 'Maybe' 'Text'
--
-- * 'lbdHealthCheck' @::@ 'Maybe' 'HealthCheck'
--
-- * 'lbdInstances' @::@ ['Instance']
--
-- * 'lbdListenerDescriptions' @::@ ['ListenerDescription']
--
-- * 'lbdLoadBalancerName' @::@ 'Maybe' 'Text'
--
-- * 'lbdPolicies' @::@ 'Maybe' 'Policies'
--
-- * 'lbdScheme' @::@ 'Maybe' 'Text'
--
-- * 'lbdSecurityGroups' @::@ ['Text']
--
-- * 'lbdSourceSecurityGroup' @::@ 'Maybe' 'SourceSecurityGroup'
--
-- * 'lbdSubnets' @::@ ['Text']
--
-- * 'lbdVPCId' @::@ 'Maybe' 'Text'
--
loadBalancerDescription :: LoadBalancerDescription
loadBalancerDescription = LoadBalancerDescription
    { _lbdLoadBalancerName          = Nothing
    , _lbdDNSName                   = Nothing
    , _lbdCanonicalHostedZoneName   = Nothing
    , _lbdCanonicalHostedZoneNameID = Nothing
    , _lbdListenerDescriptions      = mempty
    , _lbdPolicies                  = Nothing
    , _lbdBackendServerDescriptions = mempty
    , _lbdAvailabilityZones         = mempty
    , _lbdSubnets                   = mempty
    , _lbdVPCId                     = Nothing
    , _lbdInstances                 = mempty
    , _lbdHealthCheck               = Nothing
    , _lbdSourceSecurityGroup       = Nothing
    , _lbdSecurityGroups            = mempty
    , _lbdCreatedTime               = Nothing
    , _lbdScheme                    = Nothing
    }

-- | Specifies a list of Availability Zones.
lbdAvailabilityZones :: Lens' LoadBalancerDescription [Text]
lbdAvailabilityZones =
    lens _lbdAvailabilityZones (\s a -> s { _lbdAvailabilityZones = a })
        . _List

-- | Contains a list of back-end server descriptions.
lbdBackendServerDescriptions :: Lens' LoadBalancerDescription [BackendServerDescription]
lbdBackendServerDescriptions =
    lens _lbdBackendServerDescriptions
        (\s a -> s { _lbdBackendServerDescriptions = a })
            . _List

-- | Provides the name of the Amazon Route 53 hosted zone that is associated with
-- the load balancer. For information on how to associate your load balancer
-- with a hosted zone, go to <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/using-domain-names-with-elb.html Using Domain Names With Elastic Load Balancing> in
-- the /Elastic Load Balancing Developer Guide/.
lbdCanonicalHostedZoneName :: Lens' LoadBalancerDescription (Maybe Text)
lbdCanonicalHostedZoneName =
    lens _lbdCanonicalHostedZoneName
        (\s a -> s { _lbdCanonicalHostedZoneName = a })

-- | Provides the ID of the Amazon Route 53 hosted zone name that is associated
-- with the load balancer. For information on how to associate or disassociate
-- your load balancer with a hosted zone, go to <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/using-domain-names-with-elb.html Using Domain Names With ElasticLoad Balancing> in the /Elastic Load Balancing Developer Guide/.
lbdCanonicalHostedZoneNameID :: Lens' LoadBalancerDescription (Maybe Text)
lbdCanonicalHostedZoneNameID =
    lens _lbdCanonicalHostedZoneNameID
        (\s a -> s { _lbdCanonicalHostedZoneNameID = a })

-- | Provides the date and time the load balancer was created.
lbdCreatedTime :: Lens' LoadBalancerDescription (Maybe UTCTime)
lbdCreatedTime = lens _lbdCreatedTime (\s a -> s { _lbdCreatedTime = a }) . mapping _Time

-- | Specifies the external DNS name associated with the load balancer.
lbdDNSName :: Lens' LoadBalancerDescription (Maybe Text)
lbdDNSName = lens _lbdDNSName (\s a -> s { _lbdDNSName = a })

-- | Specifies information regarding the various health probes conducted on the
-- load balancer.
lbdHealthCheck :: Lens' LoadBalancerDescription (Maybe HealthCheck)
lbdHealthCheck = lens _lbdHealthCheck (\s a -> s { _lbdHealthCheck = a })

-- | Provides a list of EC2 instance IDs for the load balancer.
lbdInstances :: Lens' LoadBalancerDescription [Instance]
lbdInstances = lens _lbdInstances (\s a -> s { _lbdInstances = a }) . _List

-- | LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and PolicyNames
-- are returned in a list of tuples in the ListenerDescriptions element.
lbdListenerDescriptions :: Lens' LoadBalancerDescription [ListenerDescription]
lbdListenerDescriptions =
    lens _lbdListenerDescriptions (\s a -> s { _lbdListenerDescriptions = a })
        . _List

-- | Specifies the name associated with the load balancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName =
    lens _lbdLoadBalancerName (\s a -> s { _lbdLoadBalancerName = a })

-- | Provides a list of policies defined for the load balancer.
lbdPolicies :: Lens' LoadBalancerDescription (Maybe Policies)
lbdPolicies = lens _lbdPolicies (\s a -> s { _lbdPolicies = a })

-- | Specifies the type of load balancer.
--
-- If the 'Scheme' is 'internet-facing', the load balancer has a publicly
-- resolvable DNS name that resolves to public IP addresses.
--
-- If the 'Scheme' is 'internal', the load balancer has a publicly resolvable DNS
-- name that resolves to private IP addresses.
--
-- This option is only available for load balancers attached to an Amazon VPC.
lbdScheme :: Lens' LoadBalancerDescription (Maybe Text)
lbdScheme = lens _lbdScheme (\s a -> s { _lbdScheme = a })

-- | The security groups the load balancer is a member of (VPC only).
lbdSecurityGroups :: Lens' LoadBalancerDescription [Text]
lbdSecurityGroups =
    lens _lbdSecurityGroups (\s a -> s { _lbdSecurityGroups = a })
        . _List

-- | The security group that you can use as part of your inbound rules for your
-- load balancer's back-end Amazon EC2 application instances. To only allow
-- traffic from load balancers, add a security group rule to your back end
-- instance that specifies this source security group as the inbound source.
lbdSourceSecurityGroup :: Lens' LoadBalancerDescription (Maybe SourceSecurityGroup)
lbdSourceSecurityGroup =
    lens _lbdSourceSecurityGroup (\s a -> s { _lbdSourceSecurityGroup = a })

-- | Provides a list of VPC subnet IDs for the load balancer.
lbdSubnets :: Lens' LoadBalancerDescription [Text]
lbdSubnets = lens _lbdSubnets (\s a -> s { _lbdSubnets = a }) . _List

-- | Provides the ID of the VPC attached to the load balancer.
lbdVPCId :: Lens' LoadBalancerDescription (Maybe Text)
lbdVPCId = lens _lbdVPCId (\s a -> s { _lbdVPCId = a })

instance FromXML LoadBalancerDescription where
    parseXML x = LoadBalancerDescription
        <$> x .@? "AvailabilityZones" .!@ mempty
        <*> x .@? "BackendServerDescriptions" .!@ mempty
        <*> x .@? "CanonicalHostedZoneName"
        <*> x .@? "CanonicalHostedZoneNameID"
        <*> x .@? "CreatedTime"
        <*> x .@? "DNSName"
        <*> x .@? "HealthCheck"
        <*> x .@? "Instances" .!@ mempty
        <*> x .@? "ListenerDescriptions" .!@ mempty
        <*> x .@? "LoadBalancerName"
        <*> x .@? "Policies"
        <*> x .@? "Scheme"
        <*> x .@? "SecurityGroups" .!@ mempty
        <*> x .@? "SourceSecurityGroup"
        <*> x .@? "Subnets" .!@ mempty
        <*> x .@? "VPCId"

instance ToQuery LoadBalancerDescription where
    toQuery LoadBalancerDescription{..} = mconcat
        [ "AvailabilityZones"         =? _lbdAvailabilityZones
        , "BackendServerDescriptions" =? _lbdBackendServerDescriptions
        , "CanonicalHostedZoneName"   =? _lbdCanonicalHostedZoneName
        , "CanonicalHostedZoneNameID" =? _lbdCanonicalHostedZoneNameID
        , "CreatedTime"               =? _lbdCreatedTime
        , "DNSName"                   =? _lbdDNSName
        , "HealthCheck"               =? _lbdHealthCheck
        , "Instances"                 =? _lbdInstances
        , "ListenerDescriptions"      =? _lbdListenerDescriptions
        , "LoadBalancerName"          =? _lbdLoadBalancerName
        , "Policies"                  =? _lbdPolicies
        , "Scheme"                    =? _lbdScheme
        , "SecurityGroups"            =? _lbdSecurityGroups
        , "SourceSecurityGroup"       =? _lbdSourceSecurityGroup
        , "Subnets"                   =? _lbdSubnets
        , "VPCId"                     =? _lbdVPCId
        ]

data BackendServerDescription = BackendServerDescription
    { _bsdInstancePort :: Maybe Nat
    , _bsdPolicyNames  :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'BackendServerDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bsdInstancePort' @::@ 'Maybe' 'Natural'
--
-- * 'bsdPolicyNames' @::@ ['Text']
--
backendServerDescription :: BackendServerDescription
backendServerDescription = BackendServerDescription
    { _bsdInstancePort = Nothing
    , _bsdPolicyNames  = mempty
    }

-- | Provides the port on which the back-end server is listening.
bsdInstancePort :: Lens' BackendServerDescription (Maybe Natural)
bsdInstancePort = lens _bsdInstancePort (\s a -> s { _bsdInstancePort = a }) . mapping _Nat

-- | Provides a list of policy names enabled for the back-end server.
bsdPolicyNames :: Lens' BackendServerDescription [Text]
bsdPolicyNames = lens _bsdPolicyNames (\s a -> s { _bsdPolicyNames = a }) . _List

instance FromXML BackendServerDescription where
    parseXML x = BackendServerDescription
        <$> x .@? "InstancePort"
        <*> x .@? "PolicyNames" .!@ mempty

instance ToQuery BackendServerDescription where
    toQuery BackendServerDescription{..} = mconcat
        [ "InstancePort" =? _bsdInstancePort
        , "PolicyNames"  =? _bsdPolicyNames
        ]

data PolicyAttributeDescription = PolicyAttributeDescription
    { _padAttributeName  :: Maybe Text
    , _padAttributeValue :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PolicyAttributeDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'padAttributeName' @::@ 'Maybe' 'Text'
--
-- * 'padAttributeValue' @::@ 'Maybe' 'Text'
--
policyAttributeDescription :: PolicyAttributeDescription
policyAttributeDescription = PolicyAttributeDescription
    { _padAttributeName  = Nothing
    , _padAttributeValue = Nothing
    }

-- | The name of the attribute associated with the policy.
padAttributeName :: Lens' PolicyAttributeDescription (Maybe Text)
padAttributeName = lens _padAttributeName (\s a -> s { _padAttributeName = a })

-- | The value of the attribute associated with the policy.
padAttributeValue :: Lens' PolicyAttributeDescription (Maybe Text)
padAttributeValue =
    lens _padAttributeValue (\s a -> s { _padAttributeValue = a })

instance FromXML PolicyAttributeDescription where
    parseXML x = PolicyAttributeDescription
        <$> x .@? "AttributeName"
        <*> x .@? "AttributeValue"

instance ToQuery PolicyAttributeDescription where
    toQuery PolicyAttributeDescription{..} = mconcat
        [ "AttributeName"  =? _padAttributeName
        , "AttributeValue" =? _padAttributeValue
        ]

data AdditionalAttribute = AdditionalAttribute
    { _aaKey   :: Maybe Text
    , _aaValue :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AdditionalAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaKey' @::@ 'Maybe' 'Text'
--
-- * 'aaValue' @::@ 'Maybe' 'Text'
--
additionalAttribute :: AdditionalAttribute
additionalAttribute = AdditionalAttribute
    { _aaKey   = Nothing
    , _aaValue = Nothing
    }

aaKey :: Lens' AdditionalAttribute (Maybe Text)
aaKey = lens _aaKey (\s a -> s { _aaKey = a })

aaValue :: Lens' AdditionalAttribute (Maybe Text)
aaValue = lens _aaValue (\s a -> s { _aaValue = a })

instance FromXML AdditionalAttribute where
    parseXML x = AdditionalAttribute
        <$> x .@? "Key"
        <*> x .@? "Value"

instance ToQuery AdditionalAttribute where
    toQuery AdditionalAttribute{..} = mconcat
        [ "Key"   =? _aaKey
        , "Value" =? _aaValue
        ]

newtype ConnectionSettings = ConnectionSettings
    { _csIdleTimeout :: Nat
    } deriving (Eq, Ord, Read, Show, Enum, Num, Integral, Real)

-- | 'ConnectionSettings' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csIdleTimeout' @::@ 'Natural'
--
connectionSettings :: Natural -- ^ 'csIdleTimeout'
                   -> ConnectionSettings
connectionSettings p1 = ConnectionSettings
    { _csIdleTimeout = withIso _Nat (const id) p1
    }

-- | Specifies the time (in seconds) the connection is allowed to be idle (no data
-- has been sent over the connection) before it is closed by the load balancer.
csIdleTimeout :: Lens' ConnectionSettings Natural
csIdleTimeout = lens _csIdleTimeout (\s a -> s { _csIdleTimeout = a }) . _Nat

instance FromXML ConnectionSettings where
    parseXML x = ConnectionSettings
        <$> x .@  "IdleTimeout"

instance ToQuery ConnectionSettings where
    toQuery ConnectionSettings{..} = mconcat
        [ "IdleTimeout" =? _csIdleTimeout
        ]

data PolicyTypeDescription = PolicyTypeDescription
    { _ptdDescription                     :: Maybe Text
    , _ptdPolicyAttributeTypeDescriptions :: List "member" PolicyAttributeTypeDescription
    , _ptdPolicyTypeName                  :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'PolicyTypeDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptdDescription' @::@ 'Maybe' 'Text'
--
-- * 'ptdPolicyAttributeTypeDescriptions' @::@ ['PolicyAttributeTypeDescription']
--
-- * 'ptdPolicyTypeName' @::@ 'Maybe' 'Text'
--
policyTypeDescription :: PolicyTypeDescription
policyTypeDescription = PolicyTypeDescription
    { _ptdPolicyTypeName                  = Nothing
    , _ptdDescription                     = Nothing
    , _ptdPolicyAttributeTypeDescriptions = mempty
    }

-- | A human-readable description of the policy type.
ptdDescription :: Lens' PolicyTypeDescription (Maybe Text)
ptdDescription = lens _ptdDescription (\s a -> s { _ptdDescription = a })

-- | The description of the policy attributes associated with the load balancer
-- policies defined by the Elastic Load Balancing service.
ptdPolicyAttributeTypeDescriptions :: Lens' PolicyTypeDescription [PolicyAttributeTypeDescription]
ptdPolicyAttributeTypeDescriptions =
    lens _ptdPolicyAttributeTypeDescriptions
        (\s a -> s { _ptdPolicyAttributeTypeDescriptions = a })
            . _List

-- | The name of the policy type.
ptdPolicyTypeName :: Lens' PolicyTypeDescription (Maybe Text)
ptdPolicyTypeName =
    lens _ptdPolicyTypeName (\s a -> s { _ptdPolicyTypeName = a })

instance FromXML PolicyTypeDescription where
    parseXML x = PolicyTypeDescription
        <$> x .@? "Description"
        <*> x .@? "PolicyAttributeTypeDescriptions" .!@ mempty
        <*> x .@? "PolicyTypeName"

instance ToQuery PolicyTypeDescription where
    toQuery PolicyTypeDescription{..} = mconcat
        [ "Description"                     =? _ptdDescription
        , "PolicyAttributeTypeDescriptions" =? _ptdPolicyAttributeTypeDescriptions
        , "PolicyTypeName"                  =? _ptdPolicyTypeName
        ]

data Policies = Policies
    { _pAppCookieStickinessPolicies :: List "member" AppCookieStickinessPolicy
    , _pLBCookieStickinessPolicies  :: List "member" LBCookieStickinessPolicy
    , _pOtherPolicies               :: List "member" Text
    } deriving (Eq, Read, Show)

-- | 'Policies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pAppCookieStickinessPolicies' @::@ ['AppCookieStickinessPolicy']
--
-- * 'pLBCookieStickinessPolicies' @::@ ['LBCookieStickinessPolicy']
--
-- * 'pOtherPolicies' @::@ ['Text']
--
policies :: Policies
policies = Policies
    { _pAppCookieStickinessPolicies = mempty
    , _pLBCookieStickinessPolicies  = mempty
    , _pOtherPolicies               = mempty
    }

-- | A list of the 'AppCookieStickinessPolicy' objects created with 'CreateAppCookieStickinessPolicy'.
pAppCookieStickinessPolicies :: Lens' Policies [AppCookieStickinessPolicy]
pAppCookieStickinessPolicies =
    lens _pAppCookieStickinessPolicies
        (\s a -> s { _pAppCookieStickinessPolicies = a })
            . _List

-- | A list of 'LBCookieStickinessPolicy' objects created with 'CreateAppCookieStickinessPolicy'.
pLBCookieStickinessPolicies :: Lens' Policies [LBCookieStickinessPolicy]
pLBCookieStickinessPolicies =
    lens _pLBCookieStickinessPolicies
        (\s a -> s { _pLBCookieStickinessPolicies = a })
            . _List

-- | A list of policy names other than the stickiness policies.
pOtherPolicies :: Lens' Policies [Text]
pOtherPolicies = lens _pOtherPolicies (\s a -> s { _pOtherPolicies = a }) . _List

instance FromXML Policies where
    parseXML x = Policies
        <$> x .@? "AppCookieStickinessPolicies" .!@ mempty
        <*> x .@? "LBCookieStickinessPolicies" .!@ mempty
        <*> x .@? "OtherPolicies" .!@ mempty

instance ToQuery Policies where
    toQuery Policies{..} = mconcat
        [ "AppCookieStickinessPolicies" =? _pAppCookieStickinessPolicies
        , "LBCookieStickinessPolicies"  =? _pLBCookieStickinessPolicies
        , "OtherPolicies"               =? _pOtherPolicies
        ]

data Listener = Listener
    { _lInstancePort     :: Nat
    , _lInstanceProtocol :: Maybe Text
    , _lLoadBalancerPort :: Int
    , _lProtocol         :: Text
    , _lSSLCertificateId :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Listener' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lInstancePort' @::@ 'Natural'
--
-- * 'lInstanceProtocol' @::@ 'Maybe' 'Text'
--
-- * 'lLoadBalancerPort' @::@ 'Int'
--
-- * 'lProtocol' @::@ 'Text'
--
-- * 'lSSLCertificateId' @::@ 'Maybe' 'Text'
--
listener :: Text -- ^ 'lProtocol'
         -> Int -- ^ 'lLoadBalancerPort'
         -> Natural -- ^ 'lInstancePort'
         -> Listener
listener p1 p2 p3 = Listener
    { _lProtocol         = p1
    , _lLoadBalancerPort = p2
    , _lInstancePort     = withIso _Nat (const id) p3
    , _lInstanceProtocol = Nothing
    , _lSSLCertificateId = Nothing
    }

-- | Specifies the TCP port on which the instance server is listening. This
-- property cannot be modified for the life of the load balancer.
lInstancePort :: Lens' Listener Natural
lInstancePort = lens _lInstancePort (\s a -> s { _lInstancePort = a }) . _Nat

-- | Specifies the protocol to use for routing traffic to back-end instances -
-- HTTP, HTTPS, TCP, or SSL. This property cannot be modified for the life of
-- the load balancer.
--
-- If the front-end protocol is HTTP or HTTPS, 'InstanceProtocol' has to be at
-- the same protocol layer, i.e., HTTP or HTTPS. Likewise, if the front-end
-- protocol is TCP or SSL, InstanceProtocol has to be TCP or SSL.   If there is
-- another listener with the same 'InstancePort' whose 'InstanceProtocol' is secure,
-- i.e., HTTPS or SSL, the listener's 'InstanceProtocol' has to be secure, i.e.,
-- HTTPS or SSL. If there is another listener with the same 'InstancePort' whose 'InstanceProtocol' is HTTP or TCP, the listener's 'InstanceProtocol' must be either HTTP or TCP.
lInstanceProtocol :: Lens' Listener (Maybe Text)
lInstanceProtocol =
    lens _lInstanceProtocol (\s a -> s { _lInstanceProtocol = a })

-- | Specifies the external load balancer port number. This property cannot be
-- modified for the life of the load balancer.
lLoadBalancerPort :: Lens' Listener Int
lLoadBalancerPort =
    lens _lLoadBalancerPort (\s a -> s { _lLoadBalancerPort = a })

-- | Specifies the load balancer transport protocol to use for routing - HTTP,
-- HTTPS, TCP or SSL. This property cannot be modified for the life of the load
-- balancer.
lProtocol :: Lens' Listener Text
lProtocol = lens _lProtocol (\s a -> s { _lProtocol = a })

-- | The ARN string of the server certificate. To get the ARN of the server
-- certificate, call the AWS Identity and Access Management <http://docs.aws.amazon.com/IAM/latest/APIReference/index.html?API_UploadServerCertificate.html UploadServerCertificate > API.
lSSLCertificateId :: Lens' Listener (Maybe Text)
lSSLCertificateId =
    lens _lSSLCertificateId (\s a -> s { _lSSLCertificateId = a })

instance FromXML Listener where
    parseXML x = Listener
        <$> x .@  "InstancePort"
        <*> x .@? "InstanceProtocol"
        <*> x .@  "LoadBalancerPort"
        <*> x .@  "Protocol"
        <*> x .@? "SSLCertificateId"

instance ToQuery Listener where
    toQuery Listener{..} = mconcat
        [ "InstancePort"     =? _lInstancePort
        , "InstanceProtocol" =? _lInstanceProtocol
        , "LoadBalancerPort" =? _lLoadBalancerPort
        , "Protocol"         =? _lProtocol
        , "SSLCertificateId" =? _lSSLCertificateId
        ]

data ConnectionDraining = ConnectionDraining
    { _cdEnabled :: Bool
    , _cdTimeout :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'ConnectionDraining' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdEnabled' @::@ 'Bool'
--
-- * 'cdTimeout' @::@ 'Maybe' 'Int'
--
connectionDraining :: Bool -- ^ 'cdEnabled'
                   -> ConnectionDraining
connectionDraining p1 = ConnectionDraining
    { _cdEnabled = p1
    , _cdTimeout = Nothing
    }

-- | Specifies whether connection draining is enabled for the load balancer.
cdEnabled :: Lens' ConnectionDraining Bool
cdEnabled = lens _cdEnabled (\s a -> s { _cdEnabled = a })

-- | Specifies the maximum time (in seconds) to keep the existing connections open
-- before deregistering the instances.
cdTimeout :: Lens' ConnectionDraining (Maybe Int)
cdTimeout = lens _cdTimeout (\s a -> s { _cdTimeout = a })

instance FromXML ConnectionDraining where
    parseXML x = ConnectionDraining
        <$> x .@  "Enabled"
        <*> x .@? "Timeout"

instance ToQuery ConnectionDraining where
    toQuery ConnectionDraining{..} = mconcat
        [ "Enabled" =? _cdEnabled
        , "Timeout" =? _cdTimeout
        ]

data InstanceState = InstanceState
    { _isDescription :: Maybe Text
    , _isInstanceId  :: Maybe Text
    , _isReasonCode  :: Maybe Text
    , _isState       :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'InstanceState' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isDescription' @::@ 'Maybe' 'Text'
--
-- * 'isInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'isReasonCode' @::@ 'Maybe' 'Text'
--
-- * 'isState' @::@ 'Maybe' 'Text'
--
instanceState :: InstanceState
instanceState = InstanceState
    { _isInstanceId  = Nothing
    , _isState       = Nothing
    , _isReasonCode  = Nothing
    , _isDescription = Nothing
    }

-- | Provides a description of the instance state.
isDescription :: Lens' InstanceState (Maybe Text)
isDescription = lens _isDescription (\s a -> s { _isDescription = a })

-- | Provides an EC2 instance ID.
isInstanceId :: Lens' InstanceState (Maybe Text)
isInstanceId = lens _isInstanceId (\s a -> s { _isInstanceId = a })

-- | Provides information about the cause of /OutOfService/ instances.
-- Specifically, it indicates whether the cause is Elastic Load Balancing or the
-- instance behind the load balancer.
--
-- Valid value: 'ELB'|'Instance'|'N/A'
isReasonCode :: Lens' InstanceState (Maybe Text)
isReasonCode = lens _isReasonCode (\s a -> s { _isReasonCode = a })

-- | Specifies the current state of the instance.
--
-- Valid value: 'InService'|'OutOfService'|'Unknown'
isState :: Lens' InstanceState (Maybe Text)
isState = lens _isState (\s a -> s { _isState = a })

instance FromXML InstanceState where
    parseXML x = InstanceState
        <$> x .@? "Description"
        <*> x .@? "InstanceId"
        <*> x .@? "ReasonCode"
        <*> x .@? "State"

instance ToQuery InstanceState where
    toQuery InstanceState{..} = mconcat
        [ "Description" =? _isDescription
        , "InstanceId"  =? _isInstanceId
        , "ReasonCode"  =? _isReasonCode
        , "State"       =? _isState
        ]

newtype TagKeyOnly = TagKeyOnly
    { _tkoKey :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'TagKeyOnly' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tkoKey' @::@ 'Maybe' 'Text'
--
tagKeyOnly :: TagKeyOnly
tagKeyOnly = TagKeyOnly
    { _tkoKey = Nothing
    }

-- | The name of the key.
tkoKey :: Lens' TagKeyOnly (Maybe Text)
tkoKey = lens _tkoKey (\s a -> s { _tkoKey = a })

instance FromXML TagKeyOnly where
    parseXML x = TagKeyOnly
        <$> x .@? "Key"

instance ToQuery TagKeyOnly where
    toQuery TagKeyOnly{..} = mconcat
        [ "Key" =? _tkoKey
        ]

newtype Instance = Instance
    { _iInstanceId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'Instance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iInstanceId' @::@ 'Maybe' 'Text'
--
instance' :: Instance
instance' = Instance
    { _iInstanceId = Nothing
    }

-- | Provides an EC2 instance ID.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\s a -> s { _iInstanceId = a })

instance FromXML Instance where
    parseXML x = Instance
        <$> x .@? "InstanceId"

instance ToQuery Instance where
    toQuery Instance{..} = mconcat
        [ "InstanceId" =? _iInstanceId
        ]
