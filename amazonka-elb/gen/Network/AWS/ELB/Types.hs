{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

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

-- | Elastic Load Balancing automatically distributes incoming application
-- traffic across multiple Amazon EC2 instances. It enables you to achieve
-- greater levels of fault tolerance in your applications, seamlessly
-- providing the required amount of load balancing capacity needed to
-- distribute application traffic.
module Network.AWS.ELB.Types
    (
    -- * Service
      ELB
    -- ** XML
    , xmlOptions

    -- * ConnectionSettings
    , ConnectionSettings
    , mkConnectionSettings
    , csIdleTimeout

    -- * CrossZoneLoadBalancing
    , CrossZoneLoadBalancing
    , mkCrossZoneLoadBalancing
    , czlbEnabled

    -- * Instance
    , Instance
    , mkInstance
    , iInstanceId

    -- * TagKeyOnly
    , TagKeyOnly
    , mkTagKeyOnly
    , tkoKey

    -- * AccessLog
    , AccessLog
    , mkAccessLog
    , alEnabled
    , alS3BucketName
    , alEmitInterval
    , alS3BucketPrefix

    -- * AppCookieStickinessPolicy
    , AppCookieStickinessPolicy
    , mkAppCookieStickinessPolicy
    , acspPolicyName
    , acspCookieName

    -- * BackendServerDescription
    , BackendServerDescription
    , mkBackendServerDescription
    , bsdInstancePort
    , bsdPolicyNames

    -- * ConnectionDraining
    , ConnectionDraining
    , mkConnectionDraining
    , cdEnabled
    , cdTimeout

    -- * HealthCheck
    , HealthCheck
    , mkHealthCheck
    , hcTarget
    , hcInterval
    , hcTimeout
    , hcUnhealthyThreshold
    , hcHealthyThreshold

    -- * InstanceState
    , InstanceState
    , mkInstanceState
    , isInstanceId
    , isState
    , isReasonCode
    , isDescription

    -- * LBCookieStickinessPolicy
    , LBCookieStickinessPolicy
    , mkLBCookieStickinessPolicy
    , lbcspPolicyName
    , lbcspCookieExpirationPeriod

    -- * Listener
    , Listener
    , mkListener
    , lProtocol
    , lLoadBalancerPort
    , lInstanceProtocol
    , lInstancePort
    , lSSLCertificateId

    -- * ListenerDescription
    , ListenerDescription
    , mkListenerDescription
    , ldListener
    , ldPolicyNames

    -- * LoadBalancerAttributes
    , LoadBalancerAttributes
    , mkLoadBalancerAttributes
    , lbaCrossZoneLoadBalancing
    , lbaAccessLog
    , lbaConnectionDraining
    , lbaConnectionSettings

    -- * LoadBalancerDescription
    , LoadBalancerDescription
    , mkLoadBalancerDescription
    , lbdLoadBalancerName
    , lbdDNSName
    , lbdCanonicalHostedZoneName
    , lbdCanonicalHostedZoneNameID
    , lbdListenerDescriptions
    , lbdPolicies
    , lbdBackendServerDescriptions
    , lbdAvailabilityZones
    , lbdSubnets
    , lbdVPCId
    , lbdInstances
    , lbdHealthCheck
    , lbdSourceSecurityGroup
    , lbdSecurityGroups
    , lbdCreatedTime
    , lbdScheme

    -- * Policies
    , Policies
    , mkPolicies
    , pAppCookieStickinessPolicies
    , pLBCookieStickinessPolicies
    , pOtherPolicies

    -- * PolicyAttribute
    , PolicyAttribute
    , mkPolicyAttribute
    , paAttributeName
    , paAttributeValue

    -- * PolicyAttributeDescription
    , PolicyAttributeDescription
    , mkPolicyAttributeDescription
    , padAttributeName
    , padAttributeValue

    -- * PolicyAttributeTypeDescription
    , PolicyAttributeTypeDescription
    , mkPolicyAttributeTypeDescription
    , patdAttributeName
    , patdAttributeType
    , patdDescription
    , patdDefaultValue
    , patdCardinality

    -- * PolicyDescription
    , PolicyDescription
    , mkPolicyDescription
    , pdPolicyName
    , pdPolicyTypeName
    , pdPolicyAttributeDescriptions

    -- * PolicyTypeDescription
    , PolicyTypeDescription
    , mkPolicyTypeDescription
    , ptdPolicyTypeName
    , ptdDescription
    , ptdPolicyAttributeTypeDescriptions

    -- * SourceSecurityGroup
    , SourceSecurityGroup
    , mkSourceSecurityGroup
    , ssgOwnerAlias
    , ssgGroupName

    -- * Tag
    , Tag
    , mkTag
    , tKey
    , tValue

    -- * TagDescription
    , TagDescription
    , mkTagDescription
    , tdLoadBalancerName
    , tdTags
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-06-01@) of the
-- @Elastic Load Balancing@ service.
data ELB deriving (Typeable)

instance AWSService ELB where
    type Sg ELB = V4
    data Er ELB
        = AccessPointNotFoundException
        | CertificateNotFoundException
        | DuplicateAccessPointNameException
        | DuplicateListenerException
        | DuplicatePolicyNameException
        | DuplicateTagKeysException
        | ELBClient HttpException
        | ELBSerializer String
        | ELBService String
        | InvalidConfigurationRequestException
        | InvalidEndPointException
        | InvalidSchemeException
        | InvalidSecurityGroupException
        | InvalidSubnetException
        | ListenerNotFoundException
        | LoadBalancerAttributeNotFoundException
        | PolicyNotFoundException
        | PolicyTypeNotFoundException
        | SubnetNotFoundException
        | TooManyAccessPointsException
        | TooManyPoliciesException
        | TooManyTagsException

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "elasticloadbalancing"
        , _svcVersion  = "2012-06-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er ELB)
deriving instance Generic (Er ELB)

instance AWSError (Er ELB) where
    awsError = const "ELBError"

instance AWSServiceError (Er ELB) where
    serviceError    = ELBService
    clientError     = ELBClient
    serializerError = ELBSerializer

instance Exception (Er ELB)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

-- | The name of the load balancer attribute. By default, Elastic Load Balancing
-- maintains a 60-second idle connection timeout for both front-end and
-- back-end connections of your load balancer. If the ConnectionSettings
-- attribute is set, Elastic Load Balancing will allow the connections to
-- remain idle (no data is sent over the connection) for the specified
-- duration. For more information, see Configure Idle Connection Timeout.
newtype ConnectionSettings = ConnectionSettings
    { _csIdleTimeout :: Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConnectionSettings' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdleTimeout ::@ @Integer@
--
mkConnectionSettings :: Integer -- ^ 'csIdleTimeout'
                     -> ConnectionSettings
mkConnectionSettings p1 = ConnectionSettings
    { _csIdleTimeout = p1
    }

-- | Specifies the time (in seconds) the connection is allowed to be idle (no
-- data has been sent over the connection) before it is closed by the load
-- balancer.
csIdleTimeout :: Lens' ConnectionSettings Integer
csIdleTimeout = lens _csIdleTimeout (\s a -> s { _csIdleTimeout = a })

instance FromXML ConnectionSettings where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConnectionSettings"

instance ToQuery ConnectionSettings where
    toQuery = genericQuery def

-- | The name of the load balancer attribute. If enabled, the load balancer
-- routes the request traffic evenly across all back-end instances regardless
-- of the Availability Zones. For more information, see Enable Cross-Zone Load
-- Balancing.
newtype CrossZoneLoadBalancing = CrossZoneLoadBalancing
    { _czlbEnabled :: Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CrossZoneLoadBalancing' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Enabled ::@ @Bool@
--
mkCrossZoneLoadBalancing :: Bool -- ^ 'czlbEnabled'
                         -> CrossZoneLoadBalancing
mkCrossZoneLoadBalancing p1 = CrossZoneLoadBalancing
    { _czlbEnabled = p1
    }

-- | Specifies whether cross-zone load balancing is enabled for the load
-- balancer.
czlbEnabled :: Lens' CrossZoneLoadBalancing Bool
czlbEnabled = lens _czlbEnabled (\s a -> s { _czlbEnabled = a })

instance FromXML CrossZoneLoadBalancing where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CrossZoneLoadBalancing"

instance ToQuery CrossZoneLoadBalancing where
    toQuery = genericQuery def

-- | The Instance data type.
newtype Instance = Instance
    { _iInstanceId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
mkInstance :: Instance
mkInstance = Instance
    { _iInstanceId = Nothing
    }

-- | Provides an EC2 instance ID.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\s a -> s { _iInstanceId = a })

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Instance"

instance ToQuery Instance where
    toQuery = genericQuery def

-- | The key of a tag to be removed.
newtype TagKeyOnly = TagKeyOnly
    { _tkoKey :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TagKeyOnly' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe Text@
--
mkTagKeyOnly :: TagKeyOnly
mkTagKeyOnly = TagKeyOnly
    { _tkoKey = Nothing
    }

-- | The name of the key.
tkoKey :: Lens' TagKeyOnly (Maybe Text)
tkoKey = lens _tkoKey (\s a -> s { _tkoKey = a })

instance ToQuery TagKeyOnly where
    toQuery = genericQuery def

-- | The name of the load balancer attribute. If enabled, the load balancer
-- captures detailed information of all the requests and delivers the
-- information to the Amazon S3 bucket that you specify. For more information,
-- see Enable Access Logs.
data AccessLog = AccessLog
    { _alEnabled :: Bool
    , _alS3BucketName :: Maybe Text
    , _alEmitInterval :: Maybe Integer
    , _alS3BucketPrefix :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccessLog' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Enabled ::@ @Bool@
--
-- * @S3BucketName ::@ @Maybe Text@
--
-- * @EmitInterval ::@ @Maybe Integer@
--
-- * @S3BucketPrefix ::@ @Maybe Text@
--
mkAccessLog :: Bool -- ^ 'alEnabled'
            -> AccessLog
mkAccessLog p1 = AccessLog
    { _alEnabled = p1
    , _alS3BucketName = Nothing
    , _alEmitInterval = Nothing
    , _alS3BucketPrefix = Nothing
    }

-- | Specifies whether access log is enabled for the load balancer.
alEnabled :: Lens' AccessLog Bool
alEnabled = lens _alEnabled (\s a -> s { _alEnabled = a })

-- | The name of the Amazon S3 bucket where the access logs are stored.
alS3BucketName :: Lens' AccessLog (Maybe Text)
alS3BucketName = lens _alS3BucketName (\s a -> s { _alS3BucketName = a })

-- | The interval for publishing the access logs. You can specify an interval of
-- either 5 minutes or 60 minutes. Default: 60 minutes.
alEmitInterval :: Lens' AccessLog (Maybe Integer)
alEmitInterval = lens _alEmitInterval (\s a -> s { _alEmitInterval = a })

-- | The logical hierarchy you created for your Amazon S3 bucket, for example
-- my-bucket-prefix/prod. If the prefix is not provided, the log is placed at
-- the root level of the bucket.
alS3BucketPrefix :: Lens' AccessLog (Maybe Text)
alS3BucketPrefix =
    lens _alS3BucketPrefix (\s a -> s { _alS3BucketPrefix = a })

instance FromXML AccessLog where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessLog"

instance ToQuery AccessLog where
    toQuery = genericQuery def

-- | The AppCookieStickinessPolicy data type.
data AppCookieStickinessPolicy = AppCookieStickinessPolicy
    { _acspPolicyName :: Maybe Text
    , _acspCookieName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AppCookieStickinessPolicy' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PolicyName ::@ @Maybe Text@
--
-- * @CookieName ::@ @Maybe Text@
--
mkAppCookieStickinessPolicy :: AppCookieStickinessPolicy
mkAppCookieStickinessPolicy = AppCookieStickinessPolicy
    { _acspPolicyName = Nothing
    , _acspCookieName = Nothing
    }

-- | The mnemonic name for the policy being created. The name must be unique
-- within a set of policies for this load balancer.
acspPolicyName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acspPolicyName = lens _acspPolicyName (\s a -> s { _acspPolicyName = a })

-- | The name of the application cookie used for stickiness.
acspCookieName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acspCookieName = lens _acspCookieName (\s a -> s { _acspCookieName = a })

instance FromXML AppCookieStickinessPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AppCookieStickinessPolicy"

instance ToQuery AppCookieStickinessPolicy where
    toQuery = genericQuery def

-- | This data type is used as a response element in the DescribeLoadBalancers
-- action to describe the configuration of the back-end server.
data BackendServerDescription = BackendServerDescription
    { _bsdInstancePort :: Maybe Integer
    , _bsdPolicyNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BackendServerDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstancePort ::@ @Maybe Integer@
--
-- * @PolicyNames ::@ @[Text]@
--
mkBackendServerDescription :: BackendServerDescription
mkBackendServerDescription = BackendServerDescription
    { _bsdInstancePort = Nothing
    , _bsdPolicyNames = mempty
    }

-- | Provides the port on which the back-end server is listening.
bsdInstancePort :: Lens' BackendServerDescription (Maybe Integer)
bsdInstancePort = lens _bsdInstancePort (\s a -> s { _bsdInstancePort = a })

-- | Provides a list of policy names enabled for the back-end server.
bsdPolicyNames :: Lens' BackendServerDescription [Text]
bsdPolicyNames = lens _bsdPolicyNames (\s a -> s { _bsdPolicyNames = a })

instance FromXML BackendServerDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BackendServerDescription"

instance ToQuery BackendServerDescription where
    toQuery = genericQuery def

-- | The name of the load balancer attribute. If enabled, the load balancer
-- allows existing requests to complete before the load balancer shifts
-- traffic away from a deregistered or unhealthy back-end instance. For more
-- information, see Enable Connection Draining.
data ConnectionDraining = ConnectionDraining
    { _cdEnabled :: Bool
    , _cdTimeout :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConnectionDraining' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Enabled ::@ @Bool@
--
-- * @Timeout ::@ @Maybe Integer@
--
mkConnectionDraining :: Bool -- ^ 'cdEnabled'
                     -> ConnectionDraining
mkConnectionDraining p1 = ConnectionDraining
    { _cdEnabled = p1
    , _cdTimeout = Nothing
    }

-- | Specifies whether connection draining is enabled for the load balancer.
cdEnabled :: Lens' ConnectionDraining Bool
cdEnabled = lens _cdEnabled (\s a -> s { _cdEnabled = a })

-- | Specifies the maximum time (in seconds) to keep the existing connections
-- open before deregistering the instances.
cdTimeout :: Lens' ConnectionDraining (Maybe Integer)
cdTimeout = lens _cdTimeout (\s a -> s { _cdTimeout = a })

instance FromXML ConnectionDraining where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConnectionDraining"

instance ToQuery ConnectionDraining where
    toQuery = genericQuery def

-- | A structure containing the configuration information for the new
-- healthcheck.
data HealthCheck = HealthCheck
    { _hcTarget :: Text
    , _hcInterval :: Integer
    , _hcTimeout :: Integer
    , _hcUnhealthyThreshold :: Integer
    , _hcHealthyThreshold :: Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HealthCheck' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Target ::@ @Text@
--
-- * @Interval ::@ @Integer@
--
-- * @Timeout ::@ @Integer@
--
-- * @UnhealthyThreshold ::@ @Integer@
--
-- * @HealthyThreshold ::@ @Integer@
--
mkHealthCheck :: Text -- ^ 'hcTarget'
              -> Integer -- ^ 'hcInterval'
              -> Integer -- ^ 'hcTimeout'
              -> Integer -- ^ 'hcUnhealthyThreshold'
              -> Integer -- ^ 'hcHealthyThreshold'
              -> HealthCheck
mkHealthCheck p1 p2 p3 p4 p5 = HealthCheck
    { _hcTarget = p1
    , _hcInterval = p2
    , _hcTimeout = p3
    , _hcUnhealthyThreshold = p4
    , _hcHealthyThreshold = p5
    }

-- | Specifies the instance being checked. The protocol is either TCP, HTTP,
-- HTTPS, or SSL. The range of valid ports is one (1) through 65535. TCP is
-- the default, specified as a TCP: port pair, for example "TCP:5000". In this
-- case a healthcheck simply attempts to open a TCP connection to the instance
-- on the specified port. Failure to connect within the configured timeout is
-- considered unhealthy. SSL is also specified as SSL: port pair, for example,
-- SSL:5000. For HTTP or HTTPS protocol, the situation is different. You have
-- to include a ping path in the string. HTTP is specified as a
-- HTTP:port;/;PathToPing; grouping, for example
-- "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request is issued
-- to the instance on the given port and path. Any answer other than "200 OK"
-- within the timeout period is considered unhealthy. The total length of the
-- HTTP ping target needs to be 1024 16-bit Unicode characters or less.
hcTarget :: Lens' HealthCheck Text
hcTarget = lens _hcTarget (\s a -> s { _hcTarget = a })

-- | Specifies the approximate interval, in seconds, between health checks of an
-- individual instance.
hcInterval :: Lens' HealthCheck Integer
hcInterval = lens _hcInterval (\s a -> s { _hcInterval = a })

-- | Specifies the amount of time, in seconds, during which no response means a
-- failed health probe. This value must be less than the Interval value.
hcTimeout :: Lens' HealthCheck Integer
hcTimeout = lens _hcTimeout (\s a -> s { _hcTimeout = a })

-- | Specifies the number of consecutive health probe failures required before
-- moving the instance to the Unhealthy state.
hcUnhealthyThreshold :: Lens' HealthCheck Integer
hcUnhealthyThreshold =
    lens _hcUnhealthyThreshold (\s a -> s { _hcUnhealthyThreshold = a })

-- | Specifies the number of consecutive health probe successes required before
-- moving the instance to the Healthy state.
hcHealthyThreshold :: Lens' HealthCheck Integer
hcHealthyThreshold =
    lens _hcHealthyThreshold (\s a -> s { _hcHealthyThreshold = a })

instance FromXML HealthCheck where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheck"

instance ToQuery HealthCheck where
    toQuery = genericQuery def

-- | The InstanceState data type.
data InstanceState = InstanceState
    { _isInstanceId :: Maybe Text
    , _isState :: Maybe Text
    , _isReasonCode :: Maybe Text
    , _isDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceState' data type.
--
-- 'InstanceState' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @State ::@ @Maybe Text@
--
-- * @ReasonCode ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
mkInstanceState :: InstanceState
mkInstanceState = InstanceState
    { _isInstanceId = Nothing
    , _isState = Nothing
    , _isReasonCode = Nothing
    , _isDescription = Nothing
    }

-- | Provides an EC2 instance ID.
isInstanceId :: Lens' InstanceState (Maybe Text)
isInstanceId = lens _isInstanceId (\s a -> s { _isInstanceId = a })

-- | Specifies the current state of the instance. Valid value:
-- InService|OutOfService|Unknown.
isState :: Lens' InstanceState (Maybe Text)
isState = lens _isState (\s a -> s { _isState = a })

-- | Provides information about the cause of OutOfService instances.
-- Specifically, it indicates whether the cause is Elastic Load Balancing or
-- the instance behind the load balancer. Valid value: ELB|Instance|N/A.
isReasonCode :: Lens' InstanceState (Maybe Text)
isReasonCode = lens _isReasonCode (\s a -> s { _isReasonCode = a })

-- | Provides a description of the instance state.
isDescription :: Lens' InstanceState (Maybe Text)
isDescription = lens _isDescription (\s a -> s { _isDescription = a })

instance FromXML InstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceState"

-- | The LBCookieStickinessPolicy data type.
data LBCookieStickinessPolicy = LBCookieStickinessPolicy
    { _lbcspPolicyName :: Maybe Text
    , _lbcspCookieExpirationPeriod :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LBCookieStickinessPolicy' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PolicyName ::@ @Maybe Text@
--
-- * @CookieExpirationPeriod ::@ @Maybe Integer@
--
mkLBCookieStickinessPolicy :: LBCookieStickinessPolicy
mkLBCookieStickinessPolicy = LBCookieStickinessPolicy
    { _lbcspPolicyName = Nothing
    , _lbcspCookieExpirationPeriod = Nothing
    }

-- | The name for the policy being created. The name must be unique within the
-- set of policies for this load balancer.
lbcspPolicyName :: Lens' LBCookieStickinessPolicy (Maybe Text)
lbcspPolicyName = lens _lbcspPolicyName (\s a -> s { _lbcspPolicyName = a })

-- | The time period in seconds after which the cookie should be considered
-- stale. Not specifying this parameter indicates that the stickiness session
-- will last for the duration of the browser session.
lbcspCookieExpirationPeriod :: Lens' LBCookieStickinessPolicy (Maybe Integer)
lbcspCookieExpirationPeriod =
    lens _lbcspCookieExpirationPeriod
         (\s a -> s { _lbcspCookieExpirationPeriod = a })

instance FromXML LBCookieStickinessPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LBCookieStickinessPolicy"

instance ToQuery LBCookieStickinessPolicy where
    toQuery = genericQuery def

-- | The Listener data type.
data Listener = Listener
    { _lProtocol :: Text
    , _lLoadBalancerPort :: Integer
    , _lInstanceProtocol :: Maybe Text
    , _lInstancePort :: Integer
    , _lSSLCertificateId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Listener' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Protocol ::@ @Text@
--
-- * @LoadBalancerPort ::@ @Integer@
--
-- * @InstanceProtocol ::@ @Maybe Text@
--
-- * @InstancePort ::@ @Integer@
--
-- * @SSLCertificateId ::@ @Maybe Text@
--
mkListener :: Text -- ^ 'lProtocol'
           -> Integer -- ^ 'lLoadBalancerPort'
           -> Integer -- ^ 'lInstancePort'
           -> Listener
mkListener p1 p2 p4 = Listener
    { _lProtocol = p1
    , _lLoadBalancerPort = p2
    , _lInstanceProtocol = Nothing
    , _lInstancePort = p4
    , _lSSLCertificateId = Nothing
    }

-- | Specifies the load balancer transport protocol to use for routing - HTTP,
-- HTTPS, TCP or SSL. This property cannot be modified for the life of the
-- load balancer.
lProtocol :: Lens' Listener Text
lProtocol = lens _lProtocol (\s a -> s { _lProtocol = a })

-- | Specifies the external load balancer port number. This property cannot be
-- modified for the life of the load balancer.
lLoadBalancerPort :: Lens' Listener Integer
lLoadBalancerPort =
    lens _lLoadBalancerPort (\s a -> s { _lLoadBalancerPort = a })

-- | Specifies the protocol to use for routing traffic to back-end instances -
-- HTTP, HTTPS, TCP, or SSL. This property cannot be modified for the life of
-- the load balancer. If the front-end protocol is HTTP or HTTPS,
-- InstanceProtocol has to be at the same protocol layer, i.e., HTTP or HTTPS.
-- Likewise, if the front-end protocol is TCP or SSL, InstanceProtocol has to
-- be TCP or SSL. If there is another listener with the same InstancePort
-- whose InstanceProtocol is secure, i.e., HTTPS or SSL, the listener's
-- InstanceProtocol has to be secure, i.e., HTTPS or SSL. If there is another
-- listener with the same InstancePort whose InstanceProtocol is HTTP or TCP,
-- the listener's InstanceProtocol must be either HTTP or TCP.
lInstanceProtocol :: Lens' Listener (Maybe Text)
lInstanceProtocol =
    lens _lInstanceProtocol (\s a -> s { _lInstanceProtocol = a })

-- | Specifies the TCP port on which the instance server is listening. This
-- property cannot be modified for the life of the load balancer.
lInstancePort :: Lens' Listener Integer
lInstancePort = lens _lInstancePort (\s a -> s { _lInstancePort = a })

-- | The ARN string of the server certificate. To get the ARN of the server
-- certificate, call the AWS Identity and Access Management
-- UploadServerCertificate API.
lSSLCertificateId :: Lens' Listener (Maybe Text)
lSSLCertificateId =
    lens _lSSLCertificateId (\s a -> s { _lSSLCertificateId = a })

instance FromXML Listener where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Listener"

instance ToQuery Listener where
    toQuery = genericQuery def

-- | The ListenerDescription data type.
data ListenerDescription = ListenerDescription
    { _ldListener :: Maybe Listener
    , _ldPolicyNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ListenerDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Listener ::@ @Maybe Listener@
--
-- * @PolicyNames ::@ @[Text]@
--
mkListenerDescription :: ListenerDescription
mkListenerDescription = ListenerDescription
    { _ldListener = Nothing
    , _ldPolicyNames = mempty
    }

-- | The Listener data type.
ldListener :: Lens' ListenerDescription (Maybe Listener)
ldListener = lens _ldListener (\s a -> s { _ldListener = a })

-- | A list of policies enabled for this listener. An empty list indicates that
-- no policies are enabled.
ldPolicyNames :: Lens' ListenerDescription [Text]
ldPolicyNames = lens _ldPolicyNames (\s a -> s { _ldPolicyNames = a })

instance FromXML ListenerDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListenerDescription"

instance ToQuery ListenerDescription where
    toQuery = genericQuery def

-- | The load balancer attributes structure.
data LoadBalancerAttributes = LoadBalancerAttributes
    { _lbaCrossZoneLoadBalancing :: Maybe CrossZoneLoadBalancing
    , _lbaAccessLog :: Maybe AccessLog
    , _lbaConnectionDraining :: Maybe ConnectionDraining
    , _lbaConnectionSettings :: Maybe ConnectionSettings
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoadBalancerAttributes' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CrossZoneLoadBalancing ::@ @Maybe CrossZoneLoadBalancing@
--
-- * @AccessLog ::@ @Maybe AccessLog@
--
-- * @ConnectionDraining ::@ @Maybe ConnectionDraining@
--
-- * @ConnectionSettings ::@ @Maybe ConnectionSettings@
--
mkLoadBalancerAttributes :: LoadBalancerAttributes
mkLoadBalancerAttributes = LoadBalancerAttributes
    { _lbaCrossZoneLoadBalancing = Nothing
    , _lbaAccessLog = Nothing
    , _lbaConnectionDraining = Nothing
    , _lbaConnectionSettings = Nothing
    }

-- | The name of the load balancer attribute. If enabled, the load balancer
-- routes the request traffic evenly across all back-end instances regardless
-- of the Availability Zones. For more information, see Enable Cross-Zone Load
-- Balancing.
lbaCrossZoneLoadBalancing :: Lens' LoadBalancerAttributes (Maybe CrossZoneLoadBalancing)
lbaCrossZoneLoadBalancing =
    lens _lbaCrossZoneLoadBalancing
         (\s a -> s { _lbaCrossZoneLoadBalancing = a })

-- | The name of the load balancer attribute. If enabled, the load balancer
-- captures detailed information of all the requests and delivers the
-- information to the Amazon S3 bucket that you specify. For more information,
-- see Enable Access Logs.
lbaAccessLog :: Lens' LoadBalancerAttributes (Maybe AccessLog)
lbaAccessLog = lens _lbaAccessLog (\s a -> s { _lbaAccessLog = a })

-- | The name of the load balancer attribute. If enabled, the load balancer
-- allows existing requests to complete before the load balancer shifts
-- traffic away from a deregistered or unhealthy back-end instance. For more
-- information, see Enable Connection Draining.
lbaConnectionDraining :: Lens' LoadBalancerAttributes (Maybe ConnectionDraining)
lbaConnectionDraining =
    lens _lbaConnectionDraining (\s a -> s { _lbaConnectionDraining = a })

-- | The name of the load balancer attribute. By default, Elastic Load Balancing
-- maintains a 60-second idle connection timeout for both front-end and
-- back-end connections of your load balancer. If the ConnectionSettings
-- attribute is set, Elastic Load Balancing will allow the connections to
-- remain idle (no data is sent over the connection) for the specified
-- duration. For more information, see Configure Idle Connection Timeout.
lbaConnectionSettings :: Lens' LoadBalancerAttributes (Maybe ConnectionSettings)
lbaConnectionSettings =
    lens _lbaConnectionSettings (\s a -> s { _lbaConnectionSettings = a })

instance FromXML LoadBalancerAttributes where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancerAttributes"

instance ToQuery LoadBalancerAttributes where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of DescribeLoadBalancers.
data LoadBalancerDescription = LoadBalancerDescription
    { _lbdLoadBalancerName :: Maybe Text
    , _lbdDNSName :: Maybe Text
    , _lbdCanonicalHostedZoneName :: Maybe Text
    , _lbdCanonicalHostedZoneNameID :: Maybe Text
    , _lbdListenerDescriptions :: [ListenerDescription]
    , _lbdPolicies :: Maybe Policies
    , _lbdBackendServerDescriptions :: [BackendServerDescription]
    , _lbdAvailabilityZones :: [Text]
    , _lbdSubnets :: [Text]
    , _lbdVPCId :: Maybe Text
    , _lbdInstances :: [Instance]
    , _lbdHealthCheck :: Maybe HealthCheck
    , _lbdSourceSecurityGroup :: Maybe SourceSecurityGroup
    , _lbdSecurityGroups :: [Text]
    , _lbdCreatedTime :: Maybe ISO8601
    , _lbdScheme :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoadBalancerDescription' data type.
--
-- 'LoadBalancerDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Maybe Text@
--
-- * @DNSName ::@ @Maybe Text@
--
-- * @CanonicalHostedZoneName ::@ @Maybe Text@
--
-- * @CanonicalHostedZoneNameID ::@ @Maybe Text@
--
-- * @ListenerDescriptions ::@ @[ListenerDescription]@
--
-- * @Policies ::@ @Maybe Policies@
--
-- * @BackendServerDescriptions ::@ @[BackendServerDescription]@
--
-- * @AvailabilityZones ::@ @[Text]@
--
-- * @Subnets ::@ @[Text]@
--
-- * @VPCId ::@ @Maybe Text@
--
-- * @Instances ::@ @[Instance]@
--
-- * @HealthCheck ::@ @Maybe HealthCheck@
--
-- * @SourceSecurityGroup ::@ @Maybe SourceSecurityGroup@
--
-- * @SecurityGroups ::@ @[Text]@
--
-- * @CreatedTime ::@ @Maybe ISO8601@
--
-- * @Scheme ::@ @Maybe Text@
--
mkLoadBalancerDescription :: LoadBalancerDescription
mkLoadBalancerDescription = LoadBalancerDescription
    { _lbdLoadBalancerName = Nothing
    , _lbdDNSName = Nothing
    , _lbdCanonicalHostedZoneName = Nothing
    , _lbdCanonicalHostedZoneNameID = Nothing
    , _lbdListenerDescriptions = mempty
    , _lbdPolicies = Nothing
    , _lbdBackendServerDescriptions = mempty
    , _lbdAvailabilityZones = mempty
    , _lbdSubnets = mempty
    , _lbdVPCId = Nothing
    , _lbdInstances = mempty
    , _lbdHealthCheck = Nothing
    , _lbdSourceSecurityGroup = Nothing
    , _lbdSecurityGroups = mempty
    , _lbdCreatedTime = Nothing
    , _lbdScheme = Nothing
    }

-- | Specifies the name associated with the load balancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName =
    lens _lbdLoadBalancerName (\s a -> s { _lbdLoadBalancerName = a })

-- | Specifies the external DNS name associated with the load balancer.
lbdDNSName :: Lens' LoadBalancerDescription (Maybe Text)
lbdDNSName = lens _lbdDNSName (\s a -> s { _lbdDNSName = a })

-- | Provides the name of the Amazon Route 53 hosted zone that is associated
-- with the load balancer. For information on how to associate your load
-- balancer with a hosted zone, go to Using Domain Names With Elastic Load
-- Balancing in the Elastic Load Balancing Developer Guide.
lbdCanonicalHostedZoneName :: Lens' LoadBalancerDescription (Maybe Text)
lbdCanonicalHostedZoneName =
    lens _lbdCanonicalHostedZoneName
         (\s a -> s { _lbdCanonicalHostedZoneName = a })

-- | Provides the ID of the Amazon Route 53 hosted zone name that is associated
-- with the load balancer. For information on how to associate or disassociate
-- your load balancer with a hosted zone, go to Using Domain Names With
-- Elastic Load Balancing in the Elastic Load Balancing Developer Guide.
lbdCanonicalHostedZoneNameID :: Lens' LoadBalancerDescription (Maybe Text)
lbdCanonicalHostedZoneNameID =
    lens _lbdCanonicalHostedZoneNameID
         (\s a -> s { _lbdCanonicalHostedZoneNameID = a })

-- | LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and PolicyNames
-- are returned in a list of tuples in the ListenerDescriptions element.
lbdListenerDescriptions :: Lens' LoadBalancerDescription [ListenerDescription]
lbdListenerDescriptions =
    lens _lbdListenerDescriptions
         (\s a -> s { _lbdListenerDescriptions = a })

-- | Provides a list of policies defined for the load balancer.
lbdPolicies :: Lens' LoadBalancerDescription (Maybe Policies)
lbdPolicies = lens _lbdPolicies (\s a -> s { _lbdPolicies = a })

-- | Contains a list of back-end server descriptions.
lbdBackendServerDescriptions :: Lens' LoadBalancerDescription [BackendServerDescription]
lbdBackendServerDescriptions =
    lens _lbdBackendServerDescriptions
         (\s a -> s { _lbdBackendServerDescriptions = a })

-- | Specifies a list of Availability Zones.
lbdAvailabilityZones :: Lens' LoadBalancerDescription [Text]
lbdAvailabilityZones =
    lens _lbdAvailabilityZones (\s a -> s { _lbdAvailabilityZones = a })

-- | Provides a list of VPC subnet IDs for the load balancer.
lbdSubnets :: Lens' LoadBalancerDescription [Text]
lbdSubnets = lens _lbdSubnets (\s a -> s { _lbdSubnets = a })

-- | Provides the ID of the VPC attached to the load balancer.
lbdVPCId :: Lens' LoadBalancerDescription (Maybe Text)
lbdVPCId = lens _lbdVPCId (\s a -> s { _lbdVPCId = a })

-- | Provides a list of EC2 instance IDs for the load balancer.
lbdInstances :: Lens' LoadBalancerDescription [Instance]
lbdInstances = lens _lbdInstances (\s a -> s { _lbdInstances = a })

-- | Specifies information regarding the various health probes conducted on the
-- load balancer.
lbdHealthCheck :: Lens' LoadBalancerDescription (Maybe HealthCheck)
lbdHealthCheck = lens _lbdHealthCheck (\s a -> s { _lbdHealthCheck = a })

-- | The security group that you can use as part of your inbound rules for your
-- load balancer's back-end Amazon EC2 application instances. To only allow
-- traffic from load balancers, add a security group rule to your back end
-- instance that specifies this source security group as the inbound source.
lbdSourceSecurityGroup :: Lens' LoadBalancerDescription (Maybe SourceSecurityGroup)
lbdSourceSecurityGroup =
    lens _lbdSourceSecurityGroup (\s a -> s { _lbdSourceSecurityGroup = a })

-- | The security groups the load balancer is a member of (VPC only).
lbdSecurityGroups :: Lens' LoadBalancerDescription [Text]
lbdSecurityGroups =
    lens _lbdSecurityGroups (\s a -> s { _lbdSecurityGroups = a })

-- | Provides the date and time the load balancer was created.
lbdCreatedTime :: Lens' LoadBalancerDescription (Maybe ISO8601)
lbdCreatedTime = lens _lbdCreatedTime (\s a -> s { _lbdCreatedTime = a })

-- | Specifies the type of load balancer. If the Scheme is internet-facing, the
-- load balancer has a publicly resolvable DNS name that resolves to public IP
-- addresses. If the Scheme is internal, the load balancer has a publicly
-- resolvable DNS name that resolves to private IP addresses. This option is
-- only available for load balancers attached to an Amazon VPC.
lbdScheme :: Lens' LoadBalancerDescription (Maybe Text)
lbdScheme = lens _lbdScheme (\s a -> s { _lbdScheme = a })

instance FromXML LoadBalancerDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancerDescription"

-- | Provides a list of policies defined for the load balancer.
data Policies = Policies
    { _pAppCookieStickinessPolicies :: [AppCookieStickinessPolicy]
    , _pLBCookieStickinessPolicies :: [LBCookieStickinessPolicy]
    , _pOtherPolicies :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Policies' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AppCookieStickinessPolicies ::@ @[AppCookieStickinessPolicy]@
--
-- * @LBCookieStickinessPolicies ::@ @[LBCookieStickinessPolicy]@
--
-- * @OtherPolicies ::@ @[Text]@
--
mkPolicies :: Policies
mkPolicies = Policies
    { _pAppCookieStickinessPolicies = mempty
    , _pLBCookieStickinessPolicies = mempty
    , _pOtherPolicies = mempty
    }

-- | A list of the AppCookieStickinessPolicy objects created with
-- CreateAppCookieStickinessPolicy.
pAppCookieStickinessPolicies :: Lens' Policies [AppCookieStickinessPolicy]
pAppCookieStickinessPolicies =
    lens _pAppCookieStickinessPolicies
         (\s a -> s { _pAppCookieStickinessPolicies = a })

-- | A list of LBCookieStickinessPolicy objects created with
-- CreateAppCookieStickinessPolicy.
pLBCookieStickinessPolicies :: Lens' Policies [LBCookieStickinessPolicy]
pLBCookieStickinessPolicies =
    lens _pLBCookieStickinessPolicies
         (\s a -> s { _pLBCookieStickinessPolicies = a })

-- | A list of policy names other than the stickiness policies.
pOtherPolicies :: Lens' Policies [Text]
pOtherPolicies = lens _pOtherPolicies (\s a -> s { _pOtherPolicies = a })

instance FromXML Policies where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Policies"

instance ToQuery Policies where
    toQuery = genericQuery def

-- | The PolicyAttribute data type. This data type contains a key/value pair
-- that defines properties of a specific policy.
data PolicyAttribute = PolicyAttribute
    { _paAttributeName :: Maybe Text
    , _paAttributeValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PolicyAttribute' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeName ::@ @Maybe Text@
--
-- * @AttributeValue ::@ @Maybe Text@
--
mkPolicyAttribute :: PolicyAttribute
mkPolicyAttribute = PolicyAttribute
    { _paAttributeName = Nothing
    , _paAttributeValue = Nothing
    }

-- | The name of the attribute associated with the policy.
paAttributeName :: Lens' PolicyAttribute (Maybe Text)
paAttributeName = lens _paAttributeName (\s a -> s { _paAttributeName = a })

-- | The value of the attribute associated with the policy.
paAttributeValue :: Lens' PolicyAttribute (Maybe Text)
paAttributeValue =
    lens _paAttributeValue (\s a -> s { _paAttributeValue = a })

instance ToQuery PolicyAttribute where
    toQuery = genericQuery def

-- | The PolicyAttributeDescription data type. This data type is used to
-- describe the attributes and values associated with a policy.
data PolicyAttributeDescription = PolicyAttributeDescription
    { _padAttributeName :: Maybe Text
    , _padAttributeValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PolicyAttributeDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeName ::@ @Maybe Text@
--
-- * @AttributeValue ::@ @Maybe Text@
--
mkPolicyAttributeDescription :: PolicyAttributeDescription
mkPolicyAttributeDescription = PolicyAttributeDescription
    { _padAttributeName = Nothing
    , _padAttributeValue = Nothing
    }

-- | The name of the attribute associated with the policy.
padAttributeName :: Lens' PolicyAttributeDescription (Maybe Text)
padAttributeName =
    lens _padAttributeName (\s a -> s { _padAttributeName = a })

-- | The value of the attribute associated with the policy.
padAttributeValue :: Lens' PolicyAttributeDescription (Maybe Text)
padAttributeValue =
    lens _padAttributeValue (\s a -> s { _padAttributeValue = a })

instance FromXML PolicyAttributeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PolicyAttributeDescription"

instance ToQuery PolicyAttributeDescription where
    toQuery = genericQuery def

-- | The PolicyAttributeTypeDescription data type. This data type is used to
-- describe values that are acceptable for the policy attribute.
data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription
    { _patdAttributeName :: Maybe Text
    , _patdAttributeType :: Maybe Text
    , _patdDescription :: Maybe Text
    , _patdDefaultValue :: Maybe Text
    , _patdCardinality :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PolicyAttributeTypeDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeName ::@ @Maybe Text@
--
-- * @AttributeType ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @Cardinality ::@ @Maybe Text@
--
mkPolicyAttributeTypeDescription :: PolicyAttributeTypeDescription
mkPolicyAttributeTypeDescription = PolicyAttributeTypeDescription
    { _patdAttributeName = Nothing
    , _patdAttributeType = Nothing
    , _patdDescription = Nothing
    , _patdDefaultValue = Nothing
    , _patdCardinality = Nothing
    }

-- | The name of the attribute associated with the policy type.
patdAttributeName :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdAttributeName =
    lens _patdAttributeName (\s a -> s { _patdAttributeName = a })

-- | The type of attribute. For example, Boolean, Integer, etc.
patdAttributeType :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdAttributeType =
    lens _patdAttributeType (\s a -> s { _patdAttributeType = a })

-- | A human-readable description of the attribute.
patdDescription :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdDescription = lens _patdDescription (\s a -> s { _patdDescription = a })

-- | The default value of the attribute, if applicable.
patdDefaultValue :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdDefaultValue =
    lens _patdDefaultValue (\s a -> s { _patdDefaultValue = a })

-- | The cardinality of the attribute. Valid Values: ONE(1) : Single value
-- required ZERO_OR_ONE(0..1) : Up to one value can be supplied
-- ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
-- ONE_OR_MORE(1..*0) : Required. Multiple values are allowed.
patdCardinality :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdCardinality = lens _patdCardinality (\s a -> s { _patdCardinality = a })

instance FromXML PolicyAttributeTypeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PolicyAttributeTypeDescription"

instance ToQuery PolicyAttributeTypeDescription where
    toQuery = genericQuery def

-- | The PolicyDescription data type.
data PolicyDescription = PolicyDescription
    { _pdPolicyName :: Maybe Text
    , _pdPolicyTypeName :: Maybe Text
    , _pdPolicyAttributeDescriptions :: [PolicyAttributeDescription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PolicyDescription' data type.
--
-- 'PolicyDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PolicyName ::@ @Maybe Text@
--
-- * @PolicyTypeName ::@ @Maybe Text@
--
-- * @PolicyAttributeDescriptions ::@ @[PolicyAttributeDescription]@
--
mkPolicyDescription :: PolicyDescription
mkPolicyDescription = PolicyDescription
    { _pdPolicyName = Nothing
    , _pdPolicyTypeName = Nothing
    , _pdPolicyAttributeDescriptions = mempty
    }

-- | The name of the policy associated with the load balancer.
pdPolicyName :: Lens' PolicyDescription (Maybe Text)
pdPolicyName = lens _pdPolicyName (\s a -> s { _pdPolicyName = a })

-- | The name of the policy type associated with the load balancer.
pdPolicyTypeName :: Lens' PolicyDescription (Maybe Text)
pdPolicyTypeName =
    lens _pdPolicyTypeName (\s a -> s { _pdPolicyTypeName = a })

-- | A list of policy attribute description structures.
pdPolicyAttributeDescriptions :: Lens' PolicyDescription [PolicyAttributeDescription]
pdPolicyAttributeDescriptions =
    lens _pdPolicyAttributeDescriptions
         (\s a -> s { _pdPolicyAttributeDescriptions = a })

instance FromXML PolicyDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PolicyDescription"

-- | The PolicyTypeDescription data type.
data PolicyTypeDescription = PolicyTypeDescription
    { _ptdPolicyTypeName :: Maybe Text
    , _ptdDescription :: Maybe Text
    , _ptdPolicyAttributeTypeDescriptions :: [PolicyAttributeTypeDescription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PolicyTypeDescription' data type.
--
-- 'PolicyTypeDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PolicyTypeName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @PolicyAttributeTypeDescriptions ::@ @[PolicyAttributeTypeDescription]@
--
mkPolicyTypeDescription :: PolicyTypeDescription
mkPolicyTypeDescription = PolicyTypeDescription
    { _ptdPolicyTypeName = Nothing
    , _ptdDescription = Nothing
    , _ptdPolicyAttributeTypeDescriptions = mempty
    }

-- | The name of the policy type.
ptdPolicyTypeName :: Lens' PolicyTypeDescription (Maybe Text)
ptdPolicyTypeName =
    lens _ptdPolicyTypeName (\s a -> s { _ptdPolicyTypeName = a })

-- | A human-readable description of the policy type.
ptdDescription :: Lens' PolicyTypeDescription (Maybe Text)
ptdDescription = lens _ptdDescription (\s a -> s { _ptdDescription = a })

-- | The description of the policy attributes associated with the load balancer
-- policies defined by the Elastic Load Balancing service.
ptdPolicyAttributeTypeDescriptions :: Lens' PolicyTypeDescription [PolicyAttributeTypeDescription]
ptdPolicyAttributeTypeDescriptions =
    lens _ptdPolicyAttributeTypeDescriptions
         (\s a -> s { _ptdPolicyAttributeTypeDescriptions = a })

instance FromXML PolicyTypeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PolicyTypeDescription"

-- | The security group that you can use as part of your inbound rules for your
-- load balancer's back-end Amazon EC2 application instances. To only allow
-- traffic from load balancers, add a security group rule to your back end
-- instance that specifies this source security group as the inbound source.
data SourceSecurityGroup = SourceSecurityGroup
    { _ssgOwnerAlias :: Maybe Text
    , _ssgGroupName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SourceSecurityGroup' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerAlias ::@ @Maybe Text@
--
-- * @GroupName ::@ @Maybe Text@
--
mkSourceSecurityGroup :: SourceSecurityGroup
mkSourceSecurityGroup = SourceSecurityGroup
    { _ssgOwnerAlias = Nothing
    , _ssgGroupName = Nothing
    }

-- | Owner of the source security group. Use this value for the
-- --source-group-user parameter of the ec2-authorize command in the Amazon
-- EC2 command line tool.
ssgOwnerAlias :: Lens' SourceSecurityGroup (Maybe Text)
ssgOwnerAlias = lens _ssgOwnerAlias (\s a -> s { _ssgOwnerAlias = a })

-- | Name of the source security group. Use this value for the --source-group
-- parameter of the ec2-authorize command in the Amazon EC2 command line tool.
ssgGroupName :: Lens' SourceSecurityGroup (Maybe Text)
ssgGroupName = lens _ssgGroupName (\s a -> s { _ssgGroupName = a })

instance FromXML SourceSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SourceSecurityGroup"

instance ToQuery SourceSecurityGroup where
    toQuery = genericQuery def

-- | Metadata assigned to a load balancer consisting of key-value pair. For more
-- information, see Tagging in the Elastic Load Balancing Developer Guide.
data Tag = Tag
    { _tKey :: Text
    , _tValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Text@
--
-- * @Value ::@ @Maybe Text@
--
mkTag :: Text -- ^ 'tKey'
      -> Tag
mkTag p1 = Tag
    { _tKey = p1
    , _tValue = Nothing
    }

-- | The key of the tag.
tKey :: Lens' Tag Text
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | The value of the tag.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | The descriptions of all the tags associated with load balancer.
data TagDescription = TagDescription
    { _tdLoadBalancerName :: Maybe Text
    , _tdTags :: Maybe (List1 Tag)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TagDescription' data type.
--
-- 'TagDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Maybe Text@
--
-- * @Tags ::@ @Maybe (List1 Tag)@
--
mkTagDescription :: TagDescription
mkTagDescription = TagDescription
    { _tdLoadBalancerName = Nothing
    , _tdTags = Nothing
    }

-- | The name of the load balancer.
tdLoadBalancerName :: Lens' TagDescription (Maybe Text)
tdLoadBalancerName =
    lens _tdLoadBalancerName (\s a -> s { _tdLoadBalancerName = a })

-- | List of tags associated with the load balancer.
tdTags :: Lens' TagDescription (Maybe (List1 Tag))
tdTags = lens _tdTags (\s a -> s { _tdTags = a })

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagDescription"
