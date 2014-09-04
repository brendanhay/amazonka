{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.Types
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
module Network.AWS.ELB.V2012_06_01.Types
    (
    -- * Service
      ELB
    -- ** Errors
    , Er (..)
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
    , jInstanceId

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
    , acsqPolicyName
    , acsqCookieName

    -- * BackendServerDescription
    , BackendServerDescription
    , mkBackendServerDescription
    , bseInstancePort
    , bsePolicyNames

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
    , iuInstanceId
    , iuState
    , iuReasonCode
    , iuDescription

    -- * LBCookieStickinessPolicy
    , LBCookieStickinessPolicy
    , mkLBCookieStickinessPolicy
    , lbcsqPolicyName
    , lbcsqCookieExpirationPeriod

    -- * Listener
    , Listener
    , mkListener
    , mProtocol
    , mLoadBalancerPort
    , mInstanceProtocol
    , mInstancePort
    , mSSLCertificateId

    -- * ListenerDescription
    , ListenerDescription
    , mkListenerDescription
    , leListener
    , lePolicyNames

    -- * LoadBalancerAttributes
    , LoadBalancerAttributes
    , mkLoadBalancerAttributes
    , lbaCrossZoneLoadBalancing
    , lbaAccessLog
    , lbaConnectionDraining
    , lbaConnectionSettings

    -- * LoadBalancerDescription
    , LoadBalancerDescription
    , lbeLoadBalancerName
    , lbeDNSName
    , lbeCanonicalHostedZoneName
    , lbeCanonicalHostedZoneNameID
    , lbeListenerDescriptions
    , lbePolicies
    , lbeBackendServerDescriptions
    , lbeAvailabilityZones
    , lbeSubnets
    , lbeVPCId
    , lbeInstances
    , lbeHealthCheck
    , lbeSourceSecurityGroup
    , lbeSecurityGroups
    , lbeCreatedTime
    , lbeScheme

    -- * Policies
    , Policies
    , mkPolicies
    , pxAppCookieStickinessPolicies
    , pxLBCookieStickinessPolicies
    , pxOtherPolicies

    -- * PolicyAttribute
    , PolicyAttribute
    , mkPolicyAttribute
    , pbAttributeName
    , pbAttributeValue

    -- * PolicyAttributeDescription
    , PolicyAttributeDescription
    , mkPolicyAttributeDescription
    , paeAttributeName
    , paeAttributeValue

    -- * PolicyAttributeTypeDescription
    , PolicyAttributeTypeDescription
    , mkPolicyAttributeTypeDescription
    , pateAttributeName
    , pateAttributeType
    , pateDescription
    , pateDefaultValue
    , pateCardinality

    -- * PolicyDescription
    , PolicyDescription
    , pePolicyName
    , pePolicyTypeName
    , pePolicyAttributeDescriptions

    -- * PolicyTypeDescription
    , PolicyTypeDescription
    , ptePolicyTypeName
    , pteDescription
    , ptePolicyAttributeTypeDescriptions

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
    , teLoadBalancerName
    , teTags
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
    { xmlNamespace = Just "http://elasticloadbalancing.amazonaws.com/doc/2012-06-01/"
    }

-- | The name of the load balancer attribute. By default, Elastic Load Balancing
-- maintains a 60-second idle connection timeout for both front-end and
-- back-end connections of your load balancer. If the ConnectionSettings
-- attribute is set, Elastic Load Balancing will allow the connections to
-- remain idle (no data is sent over the connection) for the specified
-- duration. For more information, see Configure Idle Connection Timeout.
newtype ConnectionSettings = ConnectionSettings
    { _csIdleTimeout :: Integer
      -- ^ Specifies the time (in seconds) the connection is allowed to be
      -- idle (no data has been sent over the connection) before it is
      -- closed by the load balancer.
    } deriving (Show, Generic)

-- | Specifies the time (in seconds) the connection is allowed to be idle (no
-- data has been sent over the connection) before it is closed by the load
-- balancer.
csIdleTimeout :: Lens' ConnectionSettings (Integer)
csIdleTimeout = lens _csIdleTimeout (\s a -> s { _csIdleTimeout = a })
{-# INLINE csIdleTimeout #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConnectionSettings' data type to populate a request.
mkConnectionSettings :: Integer -- ^ 'csIdleTimeout'
                     -> ConnectionSettings
mkConnectionSettings p1 = ConnectionSettings
    { _csIdleTimeout = p1
    }
{-# INLINE mkConnectionSettings #-}

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
      -- ^ Specifies whether cross-zone load balancing is enabled for the
      -- load balancer.
    } deriving (Show, Generic)

-- | Specifies whether cross-zone load balancing is enabled for the load
-- balancer.
czlbEnabled :: Lens' CrossZoneLoadBalancing (Bool)
czlbEnabled = lens _czlbEnabled (\s a -> s { _czlbEnabled = a })
{-# INLINE czlbEnabled #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CrossZoneLoadBalancing' data type to populate a request.
mkCrossZoneLoadBalancing :: Bool -- ^ 'czlbEnabled'
                         -> CrossZoneLoadBalancing
mkCrossZoneLoadBalancing p1 = CrossZoneLoadBalancing
    { _czlbEnabled = p1
    }
{-# INLINE mkCrossZoneLoadBalancing #-}

instance FromXML CrossZoneLoadBalancing where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CrossZoneLoadBalancing"

instance ToQuery CrossZoneLoadBalancing where
    toQuery = genericQuery def

-- | The Instance data type.
newtype Instance = Instance
    { _jInstanceId :: Maybe Text
      -- ^ Provides an EC2 instance ID.
    } deriving (Show, Generic)

-- | Provides an EC2 instance ID.
jInstanceId :: Lens' Instance (Maybe Text)
jInstanceId = lens _jInstanceId (\s a -> s { _jInstanceId = a })
{-# INLINE jInstanceId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type to populate a request.
mkInstance :: Instance
mkInstance = Instance
    { _jInstanceId = Nothing
    }
{-# INLINE mkInstance #-}

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Instance"

instance ToQuery Instance where
    toQuery = genericQuery def

-- | The key of a tag to be removed.
newtype TagKeyOnly = TagKeyOnly
    { _tkoKey :: Maybe Text
      -- ^ The name of the key.
    } deriving (Show, Generic)

-- | The name of the key.
tkoKey :: Lens' TagKeyOnly (Maybe Text)
tkoKey = lens _tkoKey (\s a -> s { _tkoKey = a })
{-# INLINE tkoKey #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TagKeyOnly' data type to populate a request.
mkTagKeyOnly :: TagKeyOnly
mkTagKeyOnly = TagKeyOnly
    { _tkoKey = Nothing
    }
{-# INLINE mkTagKeyOnly #-}

instance ToQuery TagKeyOnly where
    toQuery = genericQuery def

-- | The name of the load balancer attribute. If enabled, the load balancer
-- captures detailed information of all the requests and delivers the
-- information to the Amazon S3 bucket that you specify. For more information,
-- see Enable Access Logs.
data AccessLog = AccessLog
    { _alEnabled :: Bool
      -- ^ Specifies whether access log is enabled for the load balancer.
    , _alS3BucketName :: Maybe Text
      -- ^ The name of the Amazon S3 bucket where the access logs are
      -- stored.
    , _alEmitInterval :: Maybe Integer
      -- ^ The interval for publishing the access logs. You can specify an
      -- interval of either 5 minutes or 60 minutes. Default: 60 minutes.
    , _alS3BucketPrefix :: Maybe Text
      -- ^ The logical hierarchy you created for your Amazon S3 bucket, for
      -- example my-bucket-prefix/prod. If the prefix is not provided, the
      -- log is placed at the root level of the bucket.
    } deriving (Show, Generic)

-- | Specifies whether access log is enabled for the load balancer.
alEnabled :: Lens' AccessLog (Bool)
alEnabled = lens _alEnabled (\s a -> s { _alEnabled = a })
{-# INLINE alEnabled #-}

-- | The name of the Amazon S3 bucket where the access logs are stored.
alS3BucketName :: Lens' AccessLog (Maybe Text)
alS3BucketName = lens _alS3BucketName (\s a -> s { _alS3BucketName = a })
{-# INLINE alS3BucketName #-}

-- | The interval for publishing the access logs. You can specify an interval of
-- either 5 minutes or 60 minutes. Default: 60 minutes.
alEmitInterval :: Lens' AccessLog (Maybe Integer)
alEmitInterval = lens _alEmitInterval (\s a -> s { _alEmitInterval = a })
{-# INLINE alEmitInterval #-}

-- | The logical hierarchy you created for your Amazon S3 bucket, for example
-- my-bucket-prefix/prod. If the prefix is not provided, the log is placed at
-- the root level of the bucket.
alS3BucketPrefix :: Lens' AccessLog (Maybe Text)
alS3BucketPrefix = lens _alS3BucketPrefix (\s a -> s { _alS3BucketPrefix = a })
{-# INLINE alS3BucketPrefix #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AccessLog' data type to populate a request.
mkAccessLog :: Bool -- ^ 'alEnabled'
            -> AccessLog
mkAccessLog p1 = AccessLog
    { _alEnabled = p1
    , _alS3BucketName = Nothing
    , _alEmitInterval = Nothing
    , _alS3BucketPrefix = Nothing
    }
{-# INLINE mkAccessLog #-}

instance FromXML AccessLog where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AccessLog"

instance ToQuery AccessLog where
    toQuery = genericQuery def

-- | The AppCookieStickinessPolicy data type.
data AppCookieStickinessPolicy = AppCookieStickinessPolicy
    { _acsqPolicyName :: Maybe Text
      -- ^ The mnemonic name for the policy being created. The name must be
      -- unique within a set of policies for this load balancer.
    , _acsqCookieName :: Maybe Text
      -- ^ The name of the application cookie used for stickiness.
    } deriving (Show, Generic)

-- | The mnemonic name for the policy being created. The name must be unique
-- within a set of policies for this load balancer.
acsqPolicyName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acsqPolicyName = lens _acsqPolicyName (\s a -> s { _acsqPolicyName = a })
{-# INLINE acsqPolicyName #-}

-- | The name of the application cookie used for stickiness.
acsqCookieName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acsqCookieName = lens _acsqCookieName (\s a -> s { _acsqCookieName = a })
{-# INLINE acsqCookieName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AppCookieStickinessPolicy' data type to populate a request.
mkAppCookieStickinessPolicy :: AppCookieStickinessPolicy
mkAppCookieStickinessPolicy = AppCookieStickinessPolicy
    { _acsqPolicyName = Nothing
    , _acsqCookieName = Nothing
    }
{-# INLINE mkAppCookieStickinessPolicy #-}

instance FromXML AppCookieStickinessPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AppCookieStickinessPolicy"

instance ToQuery AppCookieStickinessPolicy where
    toQuery = genericQuery def

-- | This data type is used as a response element in the DescribeLoadBalancers
-- action to describe the configuration of the back-end server.
data BackendServerDescription = BackendServerDescription
    { _bseInstancePort :: Maybe Integer
      -- ^ Provides the port on which the back-end server is listening.
    , _bsePolicyNames :: [Text]
      -- ^ Provides a list of policy names enabled for the back-end server.
    } deriving (Show, Generic)

-- | Provides the port on which the back-end server is listening.
bseInstancePort :: Lens' BackendServerDescription (Maybe Integer)
bseInstancePort = lens _bseInstancePort (\s a -> s { _bseInstancePort = a })
{-# INLINE bseInstancePort #-}

-- | Provides a list of policy names enabled for the back-end server.
bsePolicyNames :: Lens' BackendServerDescription ([Text])
bsePolicyNames = lens _bsePolicyNames (\s a -> s { _bsePolicyNames = a })
{-# INLINE bsePolicyNames #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BackendServerDescription' data type to populate a request.
mkBackendServerDescription :: BackendServerDescription
mkBackendServerDescription = BackendServerDescription
    { _bseInstancePort = Nothing
    , _bsePolicyNames = mempty
    }
{-# INLINE mkBackendServerDescription #-}

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
      -- ^ Specifies whether connection draining is enabled for the load
      -- balancer.
    , _cdTimeout :: Maybe Integer
      -- ^ Specifies the maximum time (in seconds) to keep the existing
      -- connections open before deregistering the instances.
    } deriving (Show, Generic)

-- | Specifies whether connection draining is enabled for the load balancer.
cdEnabled :: Lens' ConnectionDraining (Bool)
cdEnabled = lens _cdEnabled (\s a -> s { _cdEnabled = a })
{-# INLINE cdEnabled #-}

-- | Specifies the maximum time (in seconds) to keep the existing connections
-- open before deregistering the instances.
cdTimeout :: Lens' ConnectionDraining (Maybe Integer)
cdTimeout = lens _cdTimeout (\s a -> s { _cdTimeout = a })
{-# INLINE cdTimeout #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConnectionDraining' data type to populate a request.
mkConnectionDraining :: Bool -- ^ 'cdEnabled'
                     -> ConnectionDraining
mkConnectionDraining p1 = ConnectionDraining
    { _cdEnabled = p1
    , _cdTimeout = Nothing
    }
{-# INLINE mkConnectionDraining #-}

instance FromXML ConnectionDraining where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConnectionDraining"

instance ToQuery ConnectionDraining where
    toQuery = genericQuery def

-- | A structure containing the configuration information for the new
-- healthcheck.
data HealthCheck = HealthCheck
    { _hcTarget :: Text
      -- ^ Specifies the instance being checked. The protocol is either TCP,
      -- HTTP, HTTPS, or SSL. The range of valid ports is one (1) through
      -- 65535. TCP is the default, specified as a TCP: port pair, for
      -- example "TCP:5000". In this case a healthcheck simply attempts to
      -- open a TCP connection to the instance on the specified port.
      -- Failure to connect within the configured timeout is considered
      -- unhealthy. SSL is also specified as SSL: port pair, for example,
      -- SSL:5000. For HTTP or HTTPS protocol, the situation is different.
      -- You have to include a ping path in the string. HTTP is specified
      -- as a HTTP:port;/;PathToPing; grouping, for example
      -- "HTTP:80/weather/us/wa/seattle". In this case, a HTTP GET request
      -- is issued to the instance on the given port and path. Any answer
      -- other than "200 OK" within the timeout period is considered
      -- unhealthy. The total length of the HTTP ping target needs to be
      -- 1024 16-bit Unicode characters or less.
    , _hcInterval :: Integer
      -- ^ Specifies the approximate interval, in seconds, between health
      -- checks of an individual instance.
    , _hcTimeout :: Integer
      -- ^ Specifies the amount of time, in seconds, during which no
      -- response means a failed health probe. This value must be less
      -- than the Interval value.
    , _hcUnhealthyThreshold :: Integer
      -- ^ Specifies the number of consecutive health probe failures
      -- required before moving the instance to the Unhealthy state.
    , _hcHealthyThreshold :: Integer
      -- ^ Specifies the number of consecutive health probe successes
      -- required before moving the instance to the Healthy state.
    } deriving (Show, Generic)

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
hcTarget :: Lens' HealthCheck (Text)
hcTarget = lens _hcTarget (\s a -> s { _hcTarget = a })
{-# INLINE hcTarget #-}

-- | Specifies the approximate interval, in seconds, between health checks of an
-- individual instance.
hcInterval :: Lens' HealthCheck (Integer)
hcInterval = lens _hcInterval (\s a -> s { _hcInterval = a })
{-# INLINE hcInterval #-}

-- | Specifies the amount of time, in seconds, during which no response means a
-- failed health probe. This value must be less than the Interval value.
hcTimeout :: Lens' HealthCheck (Integer)
hcTimeout = lens _hcTimeout (\s a -> s { _hcTimeout = a })
{-# INLINE hcTimeout #-}

-- | Specifies the number of consecutive health probe failures required before
-- moving the instance to the Unhealthy state.
hcUnhealthyThreshold :: Lens' HealthCheck (Integer)
hcUnhealthyThreshold = lens _hcUnhealthyThreshold (\s a -> s { _hcUnhealthyThreshold = a })
{-# INLINE hcUnhealthyThreshold #-}

-- | Specifies the number of consecutive health probe successes required before
-- moving the instance to the Healthy state.
hcHealthyThreshold :: Lens' HealthCheck (Integer)
hcHealthyThreshold = lens _hcHealthyThreshold (\s a -> s { _hcHealthyThreshold = a })
{-# INLINE hcHealthyThreshold #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HealthCheck' data type to populate a request.
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
{-# INLINE mkHealthCheck #-}

instance FromXML HealthCheck where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "HealthCheck"

instance ToQuery HealthCheck where
    toQuery = genericQuery def

-- | The InstanceState data type.
data InstanceState = InstanceState
    { _iuInstanceId :: Maybe Text
      -- ^ Provides an EC2 instance ID.
    , _iuState :: Maybe Text
      -- ^ Specifies the current state of the instance. Valid value:
      -- InService|OutOfService|Unknown.
    , _iuReasonCode :: Maybe Text
      -- ^ Provides information about the cause of OutOfService instances.
      -- Specifically, it indicates whether the cause is Elastic Load
      -- Balancing or the instance behind the load balancer. Valid value:
      -- ELB|Instance|N/A.
    , _iuDescription :: Maybe Text
      -- ^ Provides a description of the instance state.
    } deriving (Show, Generic)

-- | Provides an EC2 instance ID.
iuInstanceId :: Lens' InstanceState (Maybe Text)
iuInstanceId = lens _iuInstanceId (\s a -> s { _iuInstanceId = a })
{-# INLINE iuInstanceId #-}

-- | Specifies the current state of the instance. Valid value:
-- InService|OutOfService|Unknown.
iuState :: Lens' InstanceState (Maybe Text)
iuState = lens _iuState (\s a -> s { _iuState = a })
{-# INLINE iuState #-}

-- | Provides information about the cause of OutOfService instances.
-- Specifically, it indicates whether the cause is Elastic Load Balancing or
-- the instance behind the load balancer. Valid value: ELB|Instance|N/A.
iuReasonCode :: Lens' InstanceState (Maybe Text)
iuReasonCode = lens _iuReasonCode (\s a -> s { _iuReasonCode = a })
{-# INLINE iuReasonCode #-}

-- | Provides a description of the instance state.
iuDescription :: Lens' InstanceState (Maybe Text)
iuDescription = lens _iuDescription (\s a -> s { _iuDescription = a })
{-# INLINE iuDescription #-}

instance FromXML InstanceState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceState"

-- | The LBCookieStickinessPolicy data type.
data LBCookieStickinessPolicy = LBCookieStickinessPolicy
    { _lbcsqPolicyName :: Maybe Text
      -- ^ The name for the policy being created. The name must be unique
      -- within the set of policies for this load balancer.
    , _lbcsqCookieExpirationPeriod :: Maybe Integer
      -- ^ The time period in seconds after which the cookie should be
      -- considered stale. Not specifying this parameter indicates that
      -- the stickiness session will last for the duration of the browser
      -- session.
    } deriving (Show, Generic)

-- | The name for the policy being created. The name must be unique within the
-- set of policies for this load balancer.
lbcsqPolicyName :: Lens' LBCookieStickinessPolicy (Maybe Text)
lbcsqPolicyName = lens _lbcsqPolicyName (\s a -> s { _lbcsqPolicyName = a })
{-# INLINE lbcsqPolicyName #-}

-- | The time period in seconds after which the cookie should be considered
-- stale. Not specifying this parameter indicates that the stickiness session
-- will last for the duration of the browser session.
lbcsqCookieExpirationPeriod :: Lens' LBCookieStickinessPolicy (Maybe Integer)
lbcsqCookieExpirationPeriod = lens _lbcsqCookieExpirationPeriod (\s a -> s { _lbcsqCookieExpirationPeriod = a })
{-# INLINE lbcsqCookieExpirationPeriod #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LBCookieStickinessPolicy' data type to populate a request.
mkLBCookieStickinessPolicy :: LBCookieStickinessPolicy
mkLBCookieStickinessPolicy = LBCookieStickinessPolicy
    { _lbcsqPolicyName = Nothing
    , _lbcsqCookieExpirationPeriod = Nothing
    }
{-# INLINE mkLBCookieStickinessPolicy #-}

instance FromXML LBCookieStickinessPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LBCookieStickinessPolicy"

instance ToQuery LBCookieStickinessPolicy where
    toQuery = genericQuery def

-- | The Listener data type.
data Listener = Listener
    { _mProtocol :: Text
      -- ^ Specifies the load balancer transport protocol to use for routing
      -- - HTTP, HTTPS, TCP or SSL. This property cannot be modified for
      -- the life of the load balancer.
    , _mLoadBalancerPort :: Integer
      -- ^ Specifies the external load balancer port number. This property
      -- cannot be modified for the life of the load balancer.
    , _mInstanceProtocol :: Maybe Text
      -- ^ Specifies the protocol to use for routing traffic to back-end
      -- instances - HTTP, HTTPS, TCP, or SSL. This property cannot be
      -- modified for the life of the load balancer. If the front-end
      -- protocol is HTTP or HTTPS, InstanceProtocol has to be at the same
      -- protocol layer, i.e., HTTP or HTTPS. Likewise, if the front-end
      -- protocol is TCP or SSL, InstanceProtocol has to be TCP or SSL. If
      -- there is another listener with the same InstancePort whose
      -- InstanceProtocol is secure, i.e., HTTPS or SSL, the listener's
      -- InstanceProtocol has to be secure, i.e., HTTPS or SSL. If there
      -- is another listener with the same InstancePort whose
      -- InstanceProtocol is HTTP or TCP, the listener's InstanceProtocol
      -- must be either HTTP or TCP.
    , _mInstancePort :: Integer
      -- ^ Specifies the TCP port on which the instance server is listening.
      -- This property cannot be modified for the life of the load
      -- balancer.
    , _mSSLCertificateId :: Maybe Text
      -- ^ The ARN string of the server certificate. To get the ARN of the
      -- server certificate, call the AWS Identity and Access Management
      -- UploadServerCertificate API.
    } deriving (Show, Generic)

-- | Specifies the load balancer transport protocol to use for routing - HTTP,
-- HTTPS, TCP or SSL. This property cannot be modified for the life of the
-- load balancer.
mProtocol :: Lens' Listener (Text)
mProtocol = lens _mProtocol (\s a -> s { _mProtocol = a })
{-# INLINE mProtocol #-}

-- | Specifies the external load balancer port number. This property cannot be
-- modified for the life of the load balancer.
mLoadBalancerPort :: Lens' Listener (Integer)
mLoadBalancerPort = lens _mLoadBalancerPort (\s a -> s { _mLoadBalancerPort = a })
{-# INLINE mLoadBalancerPort #-}

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
mInstanceProtocol :: Lens' Listener (Maybe Text)
mInstanceProtocol = lens _mInstanceProtocol (\s a -> s { _mInstanceProtocol = a })
{-# INLINE mInstanceProtocol #-}

-- | Specifies the TCP port on which the instance server is listening. This
-- property cannot be modified for the life of the load balancer.
mInstancePort :: Lens' Listener (Integer)
mInstancePort = lens _mInstancePort (\s a -> s { _mInstancePort = a })
{-# INLINE mInstancePort #-}

-- | The ARN string of the server certificate. To get the ARN of the server
-- certificate, call the AWS Identity and Access Management
-- UploadServerCertificate API.
mSSLCertificateId :: Lens' Listener (Maybe Text)
mSSLCertificateId = lens _mSSLCertificateId (\s a -> s { _mSSLCertificateId = a })
{-# INLINE mSSLCertificateId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Listener' data type to populate a request.
mkListener :: Text -- ^ 'mProtocol'
           -> Integer -- ^ 'mLoadBalancerPort'
           -> Integer -- ^ 'mInstancePort'
           -> Listener
mkListener p1 p2 p3 = Listener
    { _mProtocol = p1
    , _mLoadBalancerPort = p2
    , _mInstanceProtocol = Nothing
    , _mInstancePort = p4
    , _mSSLCertificateId = Nothing
    }
{-# INLINE mkListener #-}

instance FromXML Listener where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Listener"

instance ToQuery Listener where
    toQuery = genericQuery def

-- | The ListenerDescription data type.
data ListenerDescription = ListenerDescription
    { _leListener :: Maybe Listener
      -- ^ The Listener data type.
    , _lePolicyNames :: [Text]
      -- ^ A list of policies enabled for this listener. An empty list
      -- indicates that no policies are enabled.
    } deriving (Show, Generic)

-- | The Listener data type.
leListener :: Lens' ListenerDescription (Maybe Listener)
leListener = lens _leListener (\s a -> s { _leListener = a })
{-# INLINE leListener #-}

-- | A list of policies enabled for this listener. An empty list indicates that
-- no policies are enabled.
lePolicyNames :: Lens' ListenerDescription ([Text])
lePolicyNames = lens _lePolicyNames (\s a -> s { _lePolicyNames = a })
{-# INLINE lePolicyNames #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ListenerDescription' data type to populate a request.
mkListenerDescription :: ListenerDescription
mkListenerDescription = ListenerDescription
    { _leListener = Nothing
    , _lePolicyNames = mempty
    }
{-# INLINE mkListenerDescription #-}

instance FromXML ListenerDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListenerDescription"

instance ToQuery ListenerDescription where
    toQuery = genericQuery def

-- | The load balancer attributes structure.
data LoadBalancerAttributes = LoadBalancerAttributes
    { _lbaCrossZoneLoadBalancing :: Maybe CrossZoneLoadBalancing
      -- ^ The name of the load balancer attribute. If enabled, the load
      -- balancer routes the request traffic evenly across all back-end
      -- instances regardless of the Availability Zones. For more
      -- information, see Enable Cross-Zone Load Balancing.
    , _lbaAccessLog :: Maybe AccessLog
      -- ^ The name of the load balancer attribute. If enabled, the load
      -- balancer captures detailed information of all the requests and
      -- delivers the information to the Amazon S3 bucket that you
      -- specify. For more information, see Enable Access Logs.
    , _lbaConnectionDraining :: Maybe ConnectionDraining
      -- ^ The name of the load balancer attribute. If enabled, the load
      -- balancer allows existing requests to complete before the load
      -- balancer shifts traffic away from a deregistered or unhealthy
      -- back-end instance. For more information, see Enable Connection
      -- Draining.
    , _lbaConnectionSettings :: Maybe ConnectionSettings
      -- ^ The name of the load balancer attribute. By default, Elastic Load
      -- Balancing maintains a 60-second idle connection timeout for both
      -- front-end and back-end connections of your load balancer. If the
      -- ConnectionSettings attribute is set, Elastic Load Balancing will
      -- allow the connections to remain idle (no data is sent over the
      -- connection) for the specified duration. For more information, see
      -- Configure Idle Connection Timeout.
    } deriving (Show, Generic)

-- | The name of the load balancer attribute. If enabled, the load balancer
-- routes the request traffic evenly across all back-end instances regardless
-- of the Availability Zones. For more information, see Enable Cross-Zone Load
-- Balancing.
lbaCrossZoneLoadBalancing :: Lens' LoadBalancerAttributes (Maybe CrossZoneLoadBalancing)
lbaCrossZoneLoadBalancing = lens _lbaCrossZoneLoadBalancing (\s a -> s { _lbaCrossZoneLoadBalancing = a })
{-# INLINE lbaCrossZoneLoadBalancing #-}

-- | The name of the load balancer attribute. If enabled, the load balancer
-- captures detailed information of all the requests and delivers the
-- information to the Amazon S3 bucket that you specify. For more information,
-- see Enable Access Logs.
lbaAccessLog :: Lens' LoadBalancerAttributes (Maybe AccessLog)
lbaAccessLog = lens _lbaAccessLog (\s a -> s { _lbaAccessLog = a })
{-# INLINE lbaAccessLog #-}

-- | The name of the load balancer attribute. If enabled, the load balancer
-- allows existing requests to complete before the load balancer shifts
-- traffic away from a deregistered or unhealthy back-end instance. For more
-- information, see Enable Connection Draining.
lbaConnectionDraining :: Lens' LoadBalancerAttributes (Maybe ConnectionDraining)
lbaConnectionDraining = lens _lbaConnectionDraining (\s a -> s { _lbaConnectionDraining = a })
{-# INLINE lbaConnectionDraining #-}

-- | The name of the load balancer attribute. By default, Elastic Load Balancing
-- maintains a 60-second idle connection timeout for both front-end and
-- back-end connections of your load balancer. If the ConnectionSettings
-- attribute is set, Elastic Load Balancing will allow the connections to
-- remain idle (no data is sent over the connection) for the specified
-- duration. For more information, see Configure Idle Connection Timeout.
lbaConnectionSettings :: Lens' LoadBalancerAttributes (Maybe ConnectionSettings)
lbaConnectionSettings = lens _lbaConnectionSettings (\s a -> s { _lbaConnectionSettings = a })
{-# INLINE lbaConnectionSettings #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoadBalancerAttributes' data type to populate a request.
mkLoadBalancerAttributes :: LoadBalancerAttributes
mkLoadBalancerAttributes = LoadBalancerAttributes
    { _lbaCrossZoneLoadBalancing = Nothing
    , _lbaAccessLog = Nothing
    , _lbaConnectionDraining = Nothing
    , _lbaConnectionSettings = Nothing
    }
{-# INLINE mkLoadBalancerAttributes #-}

instance FromXML LoadBalancerAttributes where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancerAttributes"

instance ToQuery LoadBalancerAttributes where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of DescribeLoadBalancers.
data LoadBalancerDescription = LoadBalancerDescription
    { _lbeLoadBalancerName :: Maybe Text
      -- ^ Specifies the name associated with the load balancer.
    , _lbeDNSName :: Maybe Text
      -- ^ Specifies the external DNS name associated with the load
      -- balancer.
    , _lbeCanonicalHostedZoneName :: Maybe Text
      -- ^ Provides the name of the Amazon Route 53 hosted zone that is
      -- associated with the load balancer. For information on how to
      -- associate your load balancer with a hosted zone, go to Using
      -- Domain Names With Elastic Load Balancing in the Elastic Load
      -- Balancing Developer Guide.
    , _lbeCanonicalHostedZoneNameID :: Maybe Text
      -- ^ Provides the ID of the Amazon Route 53 hosted zone name that is
      -- associated with the load balancer. For information on how to
      -- associate or disassociate your load balancer with a hosted zone,
      -- go to Using Domain Names With Elastic Load Balancing in the
      -- Elastic Load Balancing Developer Guide.
    , _lbeListenerDescriptions :: [ListenerDescription]
      -- ^ LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and
      -- PolicyNames are returned in a list of tuples in the
      -- ListenerDescriptions element.
    , _lbePolicies :: Maybe Policies
      -- ^ Provides a list of policies defined for the load balancer.
    , _lbeBackendServerDescriptions :: [BackendServerDescription]
      -- ^ Contains a list of back-end server descriptions.
    , _lbeAvailabilityZones :: [Text]
      -- ^ Specifies a list of Availability Zones.
    , _lbeSubnets :: [Text]
      -- ^ Provides a list of VPC subnet IDs for the load balancer.
    , _lbeVPCId :: Maybe Text
      -- ^ Provides the ID of the VPC attached to the load balancer.
    , _lbeInstances :: [Instance]
      -- ^ Provides a list of EC2 instance IDs for the load balancer.
    , _lbeHealthCheck :: Maybe HealthCheck
      -- ^ Specifies information regarding the various health probes
      -- conducted on the load balancer.
    , _lbeSourceSecurityGroup :: Maybe SourceSecurityGroup
      -- ^ The security group that you can use as part of your inbound rules
      -- for your load balancer's back-end Amazon EC2 application
      -- instances. To only allow traffic from load balancers, add a
      -- security group rule to your back end instance that specifies this
      -- source security group as the inbound source.
    , _lbeSecurityGroups :: [Text]
      -- ^ The security groups the load balancer is a member of (VPC only).
    , _lbeCreatedTime :: Maybe ISO8601
      -- ^ Provides the date and time the load balancer was created.
    , _lbeScheme :: Maybe Text
      -- ^ Specifies the type of load balancer. If the Scheme is
      -- internet-facing, the load balancer has a publicly resolvable DNS
      -- name that resolves to public IP addresses. If the Scheme is
      -- internal, the load balancer has a publicly resolvable DNS name
      -- that resolves to private IP addresses. This option is only
      -- available for load balancers attached to an Amazon VPC.
    } deriving (Show, Generic)

-- | Specifies the name associated with the load balancer.
lbeLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbeLoadBalancerName = lens _lbeLoadBalancerName (\s a -> s { _lbeLoadBalancerName = a })
{-# INLINE lbeLoadBalancerName #-}

-- | Specifies the external DNS name associated with the load balancer.
lbeDNSName :: Lens' LoadBalancerDescription (Maybe Text)
lbeDNSName = lens _lbeDNSName (\s a -> s { _lbeDNSName = a })
{-# INLINE lbeDNSName #-}

-- | Provides the name of the Amazon Route 53 hosted zone that is associated
-- with the load balancer. For information on how to associate your load
-- balancer with a hosted zone, go to Using Domain Names With Elastic Load
-- Balancing in the Elastic Load Balancing Developer Guide.
lbeCanonicalHostedZoneName :: Lens' LoadBalancerDescription (Maybe Text)
lbeCanonicalHostedZoneName = lens _lbeCanonicalHostedZoneName (\s a -> s { _lbeCanonicalHostedZoneName = a })
{-# INLINE lbeCanonicalHostedZoneName #-}

-- | Provides the ID of the Amazon Route 53 hosted zone name that is associated
-- with the load balancer. For information on how to associate or disassociate
-- your load balancer with a hosted zone, go to Using Domain Names With
-- Elastic Load Balancing in the Elastic Load Balancing Developer Guide.
lbeCanonicalHostedZoneNameID :: Lens' LoadBalancerDescription (Maybe Text)
lbeCanonicalHostedZoneNameID = lens _lbeCanonicalHostedZoneNameID (\s a -> s { _lbeCanonicalHostedZoneNameID = a })
{-# INLINE lbeCanonicalHostedZoneNameID #-}

-- | LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and PolicyNames
-- are returned in a list of tuples in the ListenerDescriptions element.
lbeListenerDescriptions :: Lens' LoadBalancerDescription ([ListenerDescription])
lbeListenerDescriptions = lens _lbeListenerDescriptions (\s a -> s { _lbeListenerDescriptions = a })
{-# INLINE lbeListenerDescriptions #-}

-- | Provides a list of policies defined for the load balancer.
lbePolicies :: Lens' LoadBalancerDescription (Maybe Policies)
lbePolicies = lens _lbePolicies (\s a -> s { _lbePolicies = a })
{-# INLINE lbePolicies #-}

-- | Contains a list of back-end server descriptions.
lbeBackendServerDescriptions :: Lens' LoadBalancerDescription ([BackendServerDescription])
lbeBackendServerDescriptions = lens _lbeBackendServerDescriptions (\s a -> s { _lbeBackendServerDescriptions = a })
{-# INLINE lbeBackendServerDescriptions #-}

-- | Specifies a list of Availability Zones.
lbeAvailabilityZones :: Lens' LoadBalancerDescription ([Text])
lbeAvailabilityZones = lens _lbeAvailabilityZones (\s a -> s { _lbeAvailabilityZones = a })
{-# INLINE lbeAvailabilityZones #-}

-- | Provides a list of VPC subnet IDs for the load balancer.
lbeSubnets :: Lens' LoadBalancerDescription ([Text])
lbeSubnets = lens _lbeSubnets (\s a -> s { _lbeSubnets = a })
{-# INLINE lbeSubnets #-}

-- | Provides the ID of the VPC attached to the load balancer.
lbeVPCId :: Lens' LoadBalancerDescription (Maybe Text)
lbeVPCId = lens _lbeVPCId (\s a -> s { _lbeVPCId = a })
{-# INLINE lbeVPCId #-}

-- | Provides a list of EC2 instance IDs for the load balancer.
lbeInstances :: Lens' LoadBalancerDescription ([Instance])
lbeInstances = lens _lbeInstances (\s a -> s { _lbeInstances = a })
{-# INLINE lbeInstances #-}

-- | Specifies information regarding the various health probes conducted on the
-- load balancer.
lbeHealthCheck :: Lens' LoadBalancerDescription (Maybe HealthCheck)
lbeHealthCheck = lens _lbeHealthCheck (\s a -> s { _lbeHealthCheck = a })
{-# INLINE lbeHealthCheck #-}

-- | The security group that you can use as part of your inbound rules for your
-- load balancer's back-end Amazon EC2 application instances. To only allow
-- traffic from load balancers, add a security group rule to your back end
-- instance that specifies this source security group as the inbound source.
lbeSourceSecurityGroup :: Lens' LoadBalancerDescription (Maybe SourceSecurityGroup)
lbeSourceSecurityGroup = lens _lbeSourceSecurityGroup (\s a -> s { _lbeSourceSecurityGroup = a })
{-# INLINE lbeSourceSecurityGroup #-}

-- | The security groups the load balancer is a member of (VPC only).
lbeSecurityGroups :: Lens' LoadBalancerDescription ([Text])
lbeSecurityGroups = lens _lbeSecurityGroups (\s a -> s { _lbeSecurityGroups = a })
{-# INLINE lbeSecurityGroups #-}

-- | Provides the date and time the load balancer was created.
lbeCreatedTime :: Lens' LoadBalancerDescription (Maybe ISO8601)
lbeCreatedTime = lens _lbeCreatedTime (\s a -> s { _lbeCreatedTime = a })
{-# INLINE lbeCreatedTime #-}

-- | Specifies the type of load balancer. If the Scheme is internet-facing, the
-- load balancer has a publicly resolvable DNS name that resolves to public IP
-- addresses. If the Scheme is internal, the load balancer has a publicly
-- resolvable DNS name that resolves to private IP addresses. This option is
-- only available for load balancers attached to an Amazon VPC.
lbeScheme :: Lens' LoadBalancerDescription (Maybe Text)
lbeScheme = lens _lbeScheme (\s a -> s { _lbeScheme = a })
{-# INLINE lbeScheme #-}

instance FromXML LoadBalancerDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancerDescription"

-- | Provides a list of policies defined for the load balancer.
data Policies = Policies
    { _pxAppCookieStickinessPolicies :: [AppCookieStickinessPolicy]
      -- ^ A list of the AppCookieStickinessPolicy objects created with
      -- CreateAppCookieStickinessPolicy.
    , _pxLBCookieStickinessPolicies :: [LBCookieStickinessPolicy]
      -- ^ A list of LBCookieStickinessPolicy objects created with
      -- CreateAppCookieStickinessPolicy.
    , _pxOtherPolicies :: [Text]
      -- ^ A list of policy names other than the stickiness policies.
    } deriving (Show, Generic)

-- | A list of the AppCookieStickinessPolicy objects created with
-- CreateAppCookieStickinessPolicy.
pxAppCookieStickinessPolicies :: Lens' Policies ([AppCookieStickinessPolicy])
pxAppCookieStickinessPolicies = lens _pxAppCookieStickinessPolicies (\s a -> s { _pxAppCookieStickinessPolicies = a })
{-# INLINE pxAppCookieStickinessPolicies #-}

-- | A list of LBCookieStickinessPolicy objects created with
-- CreateAppCookieStickinessPolicy.
pxLBCookieStickinessPolicies :: Lens' Policies ([LBCookieStickinessPolicy])
pxLBCookieStickinessPolicies = lens _pxLBCookieStickinessPolicies (\s a -> s { _pxLBCookieStickinessPolicies = a })
{-# INLINE pxLBCookieStickinessPolicies #-}

-- | A list of policy names other than the stickiness policies.
pxOtherPolicies :: Lens' Policies ([Text])
pxOtherPolicies = lens _pxOtherPolicies (\s a -> s { _pxOtherPolicies = a })
{-# INLINE pxOtherPolicies #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Policies' data type to populate a request.
mkPolicies :: Policies
mkPolicies = Policies
    { _pxAppCookieStickinessPolicies = mempty
    , _pxLBCookieStickinessPolicies = mempty
    , _pxOtherPolicies = mempty
    }
{-# INLINE mkPolicies #-}

instance FromXML Policies where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Policies"

instance ToQuery Policies where
    toQuery = genericQuery def

-- | The PolicyAttribute data type. This data type contains a key/value pair
-- that defines properties of a specific policy.
data PolicyAttribute = PolicyAttribute
    { _pbAttributeName :: Maybe Text
      -- ^ The name of the attribute associated with the policy.
    , _pbAttributeValue :: Maybe Text
      -- ^ The value of the attribute associated with the policy.
    } deriving (Show, Generic)

-- | The name of the attribute associated with the policy.
pbAttributeName :: Lens' PolicyAttribute (Maybe Text)
pbAttributeName = lens _pbAttributeName (\s a -> s { _pbAttributeName = a })
{-# INLINE pbAttributeName #-}

-- | The value of the attribute associated with the policy.
pbAttributeValue :: Lens' PolicyAttribute (Maybe Text)
pbAttributeValue = lens _pbAttributeValue (\s a -> s { _pbAttributeValue = a })
{-# INLINE pbAttributeValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PolicyAttribute' data type to populate a request.
mkPolicyAttribute :: PolicyAttribute
mkPolicyAttribute = PolicyAttribute
    { _pbAttributeName = Nothing
    , _pbAttributeValue = Nothing
    }
{-# INLINE mkPolicyAttribute #-}

instance ToQuery PolicyAttribute where
    toQuery = genericQuery def

-- | The PolicyAttributeDescription data type. This data type is used to
-- describe the attributes and values associated with a policy.
data PolicyAttributeDescription = PolicyAttributeDescription
    { _paeAttributeName :: Maybe Text
      -- ^ The name of the attribute associated with the policy.
    , _paeAttributeValue :: Maybe Text
      -- ^ The value of the attribute associated with the policy.
    } deriving (Show, Generic)

-- | The name of the attribute associated with the policy.
paeAttributeName :: Lens' PolicyAttributeDescription (Maybe Text)
paeAttributeName = lens _paeAttributeName (\s a -> s { _paeAttributeName = a })
{-# INLINE paeAttributeName #-}

-- | The value of the attribute associated with the policy.
paeAttributeValue :: Lens' PolicyAttributeDescription (Maybe Text)
paeAttributeValue = lens _paeAttributeValue (\s a -> s { _paeAttributeValue = a })
{-# INLINE paeAttributeValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PolicyAttributeDescription' data type to populate a request.
mkPolicyAttributeDescription :: PolicyAttributeDescription
mkPolicyAttributeDescription = PolicyAttributeDescription
    { _paeAttributeName = Nothing
    , _paeAttributeValue = Nothing
    }
{-# INLINE mkPolicyAttributeDescription #-}

instance FromXML PolicyAttributeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PolicyAttributeDescription"

instance ToQuery PolicyAttributeDescription where
    toQuery = genericQuery def

-- | The PolicyAttributeTypeDescription data type. This data type is used to
-- describe values that are acceptable for the policy attribute.
data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription
    { _pateAttributeName :: Maybe Text
      -- ^ The name of the attribute associated with the policy type.
    , _pateAttributeType :: Maybe Text
      -- ^ The type of attribute. For example, Boolean, Integer, etc.
    , _pateDescription :: Maybe Text
      -- ^ A human-readable description of the attribute.
    , _pateDefaultValue :: Maybe Text
      -- ^ The default value of the attribute, if applicable.
    , _pateCardinality :: Maybe Text
      -- ^ The cardinality of the attribute. Valid Values: ONE(1) : Single
      -- value required ZERO_OR_ONE(0..1) : Up to one value can be
      -- supplied ZERO_OR_MORE(0..*) : Optional. Multiple values are
      -- allowed ONE_OR_MORE(1..*0) : Required. Multiple values are
      -- allowed.
    } deriving (Show, Generic)

-- | The name of the attribute associated with the policy type.
pateAttributeName :: Lens' PolicyAttributeTypeDescription (Maybe Text)
pateAttributeName = lens _pateAttributeName (\s a -> s { _pateAttributeName = a })
{-# INLINE pateAttributeName #-}

-- | The type of attribute. For example, Boolean, Integer, etc.
pateAttributeType :: Lens' PolicyAttributeTypeDescription (Maybe Text)
pateAttributeType = lens _pateAttributeType (\s a -> s { _pateAttributeType = a })
{-# INLINE pateAttributeType #-}

-- | A human-readable description of the attribute.
pateDescription :: Lens' PolicyAttributeTypeDescription (Maybe Text)
pateDescription = lens _pateDescription (\s a -> s { _pateDescription = a })
{-# INLINE pateDescription #-}

-- | The default value of the attribute, if applicable.
pateDefaultValue :: Lens' PolicyAttributeTypeDescription (Maybe Text)
pateDefaultValue = lens _pateDefaultValue (\s a -> s { _pateDefaultValue = a })
{-# INLINE pateDefaultValue #-}

-- | The cardinality of the attribute. Valid Values: ONE(1) : Single value
-- required ZERO_OR_ONE(0..1) : Up to one value can be supplied
-- ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
-- ONE_OR_MORE(1..*0) : Required. Multiple values are allowed.
pateCardinality :: Lens' PolicyAttributeTypeDescription (Maybe Text)
pateCardinality = lens _pateCardinality (\s a -> s { _pateCardinality = a })
{-# INLINE pateCardinality #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PolicyAttributeTypeDescription' data type to populate a request.
mkPolicyAttributeTypeDescription :: PolicyAttributeTypeDescription
mkPolicyAttributeTypeDescription = PolicyAttributeTypeDescription
    { _pateAttributeName = Nothing
    , _pateAttributeType = Nothing
    , _pateDescription = Nothing
    , _pateDefaultValue = Nothing
    , _pateCardinality = Nothing
    }
{-# INLINE mkPolicyAttributeTypeDescription #-}

instance FromXML PolicyAttributeTypeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PolicyAttributeTypeDescription"

instance ToQuery PolicyAttributeTypeDescription where
    toQuery = genericQuery def

-- | The PolicyDescription data type.
data PolicyDescription = PolicyDescription
    { _pePolicyName :: Maybe Text
      -- ^ The name of the policy associated with the load balancer.
    , _pePolicyTypeName :: Maybe Text
      -- ^ The name of the policy type associated with the load balancer.
    , _pePolicyAttributeDescriptions :: [PolicyAttributeDescription]
      -- ^ A list of policy attribute description structures.
    } deriving (Show, Generic)

-- | The name of the policy associated with the load balancer.
pePolicyName :: Lens' PolicyDescription (Maybe Text)
pePolicyName = lens _pePolicyName (\s a -> s { _pePolicyName = a })
{-# INLINE pePolicyName #-}

-- | The name of the policy type associated with the load balancer.
pePolicyTypeName :: Lens' PolicyDescription (Maybe Text)
pePolicyTypeName = lens _pePolicyTypeName (\s a -> s { _pePolicyTypeName = a })
{-# INLINE pePolicyTypeName #-}

-- | A list of policy attribute description structures.
pePolicyAttributeDescriptions :: Lens' PolicyDescription ([PolicyAttributeDescription])
pePolicyAttributeDescriptions = lens _pePolicyAttributeDescriptions (\s a -> s { _pePolicyAttributeDescriptions = a })
{-# INLINE pePolicyAttributeDescriptions #-}

instance FromXML PolicyDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PolicyDescription"

-- | The PolicyTypeDescription data type.
data PolicyTypeDescription = PolicyTypeDescription
    { _ptePolicyTypeName :: Maybe Text
      -- ^ The name of the policy type.
    , _pteDescription :: Maybe Text
      -- ^ A human-readable description of the policy type.
    , _ptePolicyAttributeTypeDescriptions :: [PolicyAttributeTypeDescription]
      -- ^ The description of the policy attributes associated with the load
      -- balancer policies defined by the Elastic Load Balancing service.
    } deriving (Show, Generic)

-- | The name of the policy type.
ptePolicyTypeName :: Lens' PolicyTypeDescription (Maybe Text)
ptePolicyTypeName = lens _ptePolicyTypeName (\s a -> s { _ptePolicyTypeName = a })
{-# INLINE ptePolicyTypeName #-}

-- | A human-readable description of the policy type.
pteDescription :: Lens' PolicyTypeDescription (Maybe Text)
pteDescription = lens _pteDescription (\s a -> s { _pteDescription = a })
{-# INLINE pteDescription #-}

-- | The description of the policy attributes associated with the load balancer
-- policies defined by the Elastic Load Balancing service.
ptePolicyAttributeTypeDescriptions :: Lens' PolicyTypeDescription ([PolicyAttributeTypeDescription])
ptePolicyAttributeTypeDescriptions = lens _ptePolicyAttributeTypeDescriptions (\s a -> s { _ptePolicyAttributeTypeDescriptions = a })
{-# INLINE ptePolicyAttributeTypeDescriptions #-}

instance FromXML PolicyTypeDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PolicyTypeDescription"

-- | The security group that you can use as part of your inbound rules for your
-- load balancer's back-end Amazon EC2 application instances. To only allow
-- traffic from load balancers, add a security group rule to your back end
-- instance that specifies this source security group as the inbound source.
data SourceSecurityGroup = SourceSecurityGroup
    { _ssgOwnerAlias :: Maybe Text
      -- ^ Owner of the source security group. Use this value for the
      -- --source-group-user parameter of the ec2-authorize command in the
      -- Amazon EC2 command line tool.
    , _ssgGroupName :: Maybe Text
      -- ^ Name of the source security group. Use this value for the
      -- --source-group parameter of the ec2-authorize command in the
      -- Amazon EC2 command line tool.
    } deriving (Show, Generic)

-- | Owner of the source security group. Use this value for the
-- --source-group-user parameter of the ec2-authorize command in the Amazon
-- EC2 command line tool.
ssgOwnerAlias :: Lens' SourceSecurityGroup (Maybe Text)
ssgOwnerAlias = lens _ssgOwnerAlias (\s a -> s { _ssgOwnerAlias = a })
{-# INLINE ssgOwnerAlias #-}

-- | Name of the source security group. Use this value for the --source-group
-- parameter of the ec2-authorize command in the Amazon EC2 command line tool.
ssgGroupName :: Lens' SourceSecurityGroup (Maybe Text)
ssgGroupName = lens _ssgGroupName (\s a -> s { _ssgGroupName = a })
{-# INLINE ssgGroupName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SourceSecurityGroup' data type to populate a request.
mkSourceSecurityGroup :: SourceSecurityGroup
mkSourceSecurityGroup = SourceSecurityGroup
    { _ssgOwnerAlias = Nothing
    , _ssgGroupName = Nothing
    }
{-# INLINE mkSourceSecurityGroup #-}

instance FromXML SourceSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SourceSecurityGroup"

instance ToQuery SourceSecurityGroup where
    toQuery = genericQuery def

-- | Metadata assigned to a load balancer consisting of key-value pair. For more
-- information, see Tagging in the Elastic Load Balancing Developer Guide.
data Tag = Tag
    { _tKey :: Text
      -- ^ The key of the tag.
    , _tValue :: Maybe Text
      -- ^ The value of the tag.
    } deriving (Show, Generic)

-- | The key of the tag.
tKey :: Lens' Tag (Text)
tKey = lens _tKey (\s a -> s { _tKey = a })
{-# INLINE tKey #-}

-- | The value of the tag.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })
{-# INLINE tValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: Text -- ^ 'tKey'
      -> Tag
mkTag p1 = Tag
    { _tKey = p1
    , _tValue = Nothing
    }
{-# INLINE mkTag #-}

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag where
    toQuery = genericQuery def

-- | The descriptions of all the tags associated with load balancer.
data TagDescription = TagDescription
    { _teLoadBalancerName :: Maybe Text
      -- ^ The name of the load balancer.
    , _teTags :: Maybe [Tag]
      -- ^ List of tags associated with the load balancer.
    } deriving (Show, Generic)

-- | The name of the load balancer.
teLoadBalancerName :: Lens' TagDescription (Maybe Text)
teLoadBalancerName = lens _teLoadBalancerName (\s a -> s { _teLoadBalancerName = a })
{-# INLINE teLoadBalancerName #-}

-- | List of tags associated with the load balancer.
teTags :: Lens' TagDescription (Maybe [Tag])
teTags = lens _teTags (\s a -> s { _teTags = a })
{-# INLINE teTags #-}

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagDescription"
