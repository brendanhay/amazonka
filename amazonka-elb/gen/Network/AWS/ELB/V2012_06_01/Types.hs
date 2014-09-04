{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , ConnectionSettings (..)
    , csIdleTimeout

    -- * CrossZoneLoadBalancing
    , CrossZoneLoadBalancing (..)
    , czlbEnabled

    -- * Instance
    , Instance (..)
    , jInstanceId

    -- * TagKeyOnly
    , TagKeyOnly (..)
    , tkoKey

    -- * AccessLog
    , AccessLog (..)
    , alEnabled
    , alS3BucketName
    , alEmitInterval
    , alS3BucketPrefix

    -- * AppCookieStickinessPolicy
    , AppCookieStickinessPolicy (..)
    , acsqPolicyName
    , acsqCookieName

    -- * BackendServerDescription
    , BackendServerDescription (..)
    , bseInstancePort
    , bsePolicyNames

    -- * ConnectionDraining
    , ConnectionDraining (..)
    , cdEnabled
    , cdTimeout

    -- * HealthCheck
    , HealthCheck (..)
    , hcTarget
    , hcInterval
    , hcTimeout
    , hcUnhealthyThreshold
    , hcHealthyThreshold

    -- * InstanceState
    , InstanceState (..)
    , iuInstanceId
    , iuState
    , iuReasonCode
    , iuDescription

    -- * LBCookieStickinessPolicy
    , LBCookieStickinessPolicy (..)
    , lbcsqPolicyName
    , lbcsqCookieExpirationPeriod

    -- * Listener
    , Listener (..)
    , mProtocol
    , mLoadBalancerPort
    , mInstanceProtocol
    , mInstancePort
    , mSSLCertificateId

    -- * ListenerDescription
    , ListenerDescription (..)
    , leListener
    , lePolicyNames

    -- * LoadBalancerAttributes
    , LoadBalancerAttributes (..)
    , lbaCrossZoneLoadBalancing
    , lbaAccessLog
    , lbaConnectionDraining
    , lbaConnectionSettings

    -- * LoadBalancerDescription
    , LoadBalancerDescription (..)
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
    , Policies (..)
    , pxAppCookieStickinessPolicies
    , pxLBCookieStickinessPolicies
    , pxOtherPolicies

    -- * PolicyAttribute
    , PolicyAttribute (..)
    , pbAttributeName
    , pbAttributeValue

    -- * PolicyAttributeDescription
    , PolicyAttributeDescription (..)
    , paeAttributeName
    , paeAttributeValue

    -- * PolicyAttributeTypeDescription
    , PolicyAttributeTypeDescription (..)
    , pateAttributeName
    , pateAttributeType
    , pateDescription
    , pateDefaultValue
    , pateCardinality

    -- * PolicyDescription
    , PolicyDescription (..)
    , pePolicyName
    , pePolicyTypeName
    , pePolicyAttributeDescriptions

    -- * PolicyTypeDescription
    , PolicyTypeDescription (..)
    , ptePolicyTypeName
    , pteDescription
    , ptePolicyAttributeTypeDescriptions

    -- * SourceSecurityGroup
    , SourceSecurityGroup (..)
    , ssgOwnerAlias
    , ssgGroupName

    -- * Tag
    , Tag (..)
    , tKey
    , tValue

    -- * TagDescription
    , TagDescription (..)
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
csIdleTimeout f x =
    f (_csIdleTimeout x)
        <&> \y -> x { _csIdleTimeout = y }
{-# INLINE csIdleTimeout #-}

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
czlbEnabled f x =
    f (_czlbEnabled x)
        <&> \y -> x { _czlbEnabled = y }
{-# INLINE czlbEnabled #-}

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
jInstanceId f x =
    f (_jInstanceId x)
        <&> \y -> x { _jInstanceId = y }
{-# INLINE jInstanceId #-}

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
tkoKey f x =
    f (_tkoKey x)
        <&> \y -> x { _tkoKey = y }
{-# INLINE tkoKey #-}

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
alEnabled f x =
    f (_alEnabled x)
        <&> \y -> x { _alEnabled = y }
{-# INLINE alEnabled #-}

-- | The name of the Amazon S3 bucket where the access logs are stored.
alS3BucketName :: Lens' AccessLog (Maybe Text)
alS3BucketName f x =
    f (_alS3BucketName x)
        <&> \y -> x { _alS3BucketName = y }
{-# INLINE alS3BucketName #-}

-- | The interval for publishing the access logs. You can specify an interval of
-- either 5 minutes or 60 minutes. Default: 60 minutes.
alEmitInterval :: Lens' AccessLog (Maybe Integer)
alEmitInterval f x =
    f (_alEmitInterval x)
        <&> \y -> x { _alEmitInterval = y }
{-# INLINE alEmitInterval #-}

-- | The logical hierarchy you created for your Amazon S3 bucket, for example
-- my-bucket-prefix/prod. If the prefix is not provided, the log is placed at
-- the root level of the bucket.
alS3BucketPrefix :: Lens' AccessLog (Maybe Text)
alS3BucketPrefix f x =
    f (_alS3BucketPrefix x)
        <&> \y -> x { _alS3BucketPrefix = y }
{-# INLINE alS3BucketPrefix #-}

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
acsqPolicyName f x =
    f (_acsqPolicyName x)
        <&> \y -> x { _acsqPolicyName = y }
{-# INLINE acsqPolicyName #-}

-- | The name of the application cookie used for stickiness.
acsqCookieName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acsqCookieName f x =
    f (_acsqCookieName x)
        <&> \y -> x { _acsqCookieName = y }
{-# INLINE acsqCookieName #-}

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
bseInstancePort f x =
    f (_bseInstancePort x)
        <&> \y -> x { _bseInstancePort = y }
{-# INLINE bseInstancePort #-}

-- | Provides a list of policy names enabled for the back-end server.
bsePolicyNames :: Lens' BackendServerDescription ([Text])
bsePolicyNames f x =
    f (_bsePolicyNames x)
        <&> \y -> x { _bsePolicyNames = y }
{-# INLINE bsePolicyNames #-}

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
cdEnabled f x =
    f (_cdEnabled x)
        <&> \y -> x { _cdEnabled = y }
{-# INLINE cdEnabled #-}

-- | Specifies the maximum time (in seconds) to keep the existing connections
-- open before deregistering the instances.
cdTimeout :: Lens' ConnectionDraining (Maybe Integer)
cdTimeout f x =
    f (_cdTimeout x)
        <&> \y -> x { _cdTimeout = y }
{-# INLINE cdTimeout #-}

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
hcTarget f x =
    f (_hcTarget x)
        <&> \y -> x { _hcTarget = y }
{-# INLINE hcTarget #-}

-- | Specifies the approximate interval, in seconds, between health checks of an
-- individual instance.
hcInterval :: Lens' HealthCheck (Integer)
hcInterval f x =
    f (_hcInterval x)
        <&> \y -> x { _hcInterval = y }
{-# INLINE hcInterval #-}

-- | Specifies the amount of time, in seconds, during which no response means a
-- failed health probe. This value must be less than the Interval value.
hcTimeout :: Lens' HealthCheck (Integer)
hcTimeout f x =
    f (_hcTimeout x)
        <&> \y -> x { _hcTimeout = y }
{-# INLINE hcTimeout #-}

-- | Specifies the number of consecutive health probe failures required before
-- moving the instance to the Unhealthy state.
hcUnhealthyThreshold :: Lens' HealthCheck (Integer)
hcUnhealthyThreshold f x =
    f (_hcUnhealthyThreshold x)
        <&> \y -> x { _hcUnhealthyThreshold = y }
{-# INLINE hcUnhealthyThreshold #-}

-- | Specifies the number of consecutive health probe successes required before
-- moving the instance to the Healthy state.
hcHealthyThreshold :: Lens' HealthCheck (Integer)
hcHealthyThreshold f x =
    f (_hcHealthyThreshold x)
        <&> \y -> x { _hcHealthyThreshold = y }
{-# INLINE hcHealthyThreshold #-}

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
iuInstanceId f x =
    f (_iuInstanceId x)
        <&> \y -> x { _iuInstanceId = y }
{-# INLINE iuInstanceId #-}

-- | Specifies the current state of the instance. Valid value:
-- InService|OutOfService|Unknown.
iuState :: Lens' InstanceState (Maybe Text)
iuState f x =
    f (_iuState x)
        <&> \y -> x { _iuState = y }
{-# INLINE iuState #-}

-- | Provides information about the cause of OutOfService instances.
-- Specifically, it indicates whether the cause is Elastic Load Balancing or
-- the instance behind the load balancer. Valid value: ELB|Instance|N/A.
iuReasonCode :: Lens' InstanceState (Maybe Text)
iuReasonCode f x =
    f (_iuReasonCode x)
        <&> \y -> x { _iuReasonCode = y }
{-# INLINE iuReasonCode #-}

-- | Provides a description of the instance state.
iuDescription :: Lens' InstanceState (Maybe Text)
iuDescription f x =
    f (_iuDescription x)
        <&> \y -> x { _iuDescription = y }
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
lbcsqPolicyName f x =
    f (_lbcsqPolicyName x)
        <&> \y -> x { _lbcsqPolicyName = y }
{-# INLINE lbcsqPolicyName #-}

-- | The time period in seconds after which the cookie should be considered
-- stale. Not specifying this parameter indicates that the stickiness session
-- will last for the duration of the browser session.
lbcsqCookieExpirationPeriod :: Lens' LBCookieStickinessPolicy (Maybe Integer)
lbcsqCookieExpirationPeriod f x =
    f (_lbcsqCookieExpirationPeriod x)
        <&> \y -> x { _lbcsqCookieExpirationPeriod = y }
{-# INLINE lbcsqCookieExpirationPeriod #-}

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
mProtocol f x =
    f (_mProtocol x)
        <&> \y -> x { _mProtocol = y }
{-# INLINE mProtocol #-}

-- | Specifies the external load balancer port number. This property cannot be
-- modified for the life of the load balancer.
mLoadBalancerPort :: Lens' Listener (Integer)
mLoadBalancerPort f x =
    f (_mLoadBalancerPort x)
        <&> \y -> x { _mLoadBalancerPort = y }
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
mInstanceProtocol f x =
    f (_mInstanceProtocol x)
        <&> \y -> x { _mInstanceProtocol = y }
{-# INLINE mInstanceProtocol #-}

-- | Specifies the TCP port on which the instance server is listening. This
-- property cannot be modified for the life of the load balancer.
mInstancePort :: Lens' Listener (Integer)
mInstancePort f x =
    f (_mInstancePort x)
        <&> \y -> x { _mInstancePort = y }
{-# INLINE mInstancePort #-}

-- | The ARN string of the server certificate. To get the ARN of the server
-- certificate, call the AWS Identity and Access Management
-- UploadServerCertificate API.
mSSLCertificateId :: Lens' Listener (Maybe Text)
mSSLCertificateId f x =
    f (_mSSLCertificateId x)
        <&> \y -> x { _mSSLCertificateId = y }
{-# INLINE mSSLCertificateId #-}

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
leListener f x =
    f (_leListener x)
        <&> \y -> x { _leListener = y }
{-# INLINE leListener #-}

-- | A list of policies enabled for this listener. An empty list indicates that
-- no policies are enabled.
lePolicyNames :: Lens' ListenerDescription ([Text])
lePolicyNames f x =
    f (_lePolicyNames x)
        <&> \y -> x { _lePolicyNames = y }
{-# INLINE lePolicyNames #-}

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
lbaCrossZoneLoadBalancing f x =
    f (_lbaCrossZoneLoadBalancing x)
        <&> \y -> x { _lbaCrossZoneLoadBalancing = y }
{-# INLINE lbaCrossZoneLoadBalancing #-}

-- | The name of the load balancer attribute. If enabled, the load balancer
-- captures detailed information of all the requests and delivers the
-- information to the Amazon S3 bucket that you specify. For more information,
-- see Enable Access Logs.
lbaAccessLog :: Lens' LoadBalancerAttributes (Maybe AccessLog)
lbaAccessLog f x =
    f (_lbaAccessLog x)
        <&> \y -> x { _lbaAccessLog = y }
{-# INLINE lbaAccessLog #-}

-- | The name of the load balancer attribute. If enabled, the load balancer
-- allows existing requests to complete before the load balancer shifts
-- traffic away from a deregistered or unhealthy back-end instance. For more
-- information, see Enable Connection Draining.
lbaConnectionDraining :: Lens' LoadBalancerAttributes (Maybe ConnectionDraining)
lbaConnectionDraining f x =
    f (_lbaConnectionDraining x)
        <&> \y -> x { _lbaConnectionDraining = y }
{-# INLINE lbaConnectionDraining #-}

-- | The name of the load balancer attribute. By default, Elastic Load Balancing
-- maintains a 60-second idle connection timeout for both front-end and
-- back-end connections of your load balancer. If the ConnectionSettings
-- attribute is set, Elastic Load Balancing will allow the connections to
-- remain idle (no data is sent over the connection) for the specified
-- duration. For more information, see Configure Idle Connection Timeout.
lbaConnectionSettings :: Lens' LoadBalancerAttributes (Maybe ConnectionSettings)
lbaConnectionSettings f x =
    f (_lbaConnectionSettings x)
        <&> \y -> x { _lbaConnectionSettings = y }
{-# INLINE lbaConnectionSettings #-}

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
lbeLoadBalancerName f x =
    f (_lbeLoadBalancerName x)
        <&> \y -> x { _lbeLoadBalancerName = y }
{-# INLINE lbeLoadBalancerName #-}

-- | Specifies the external DNS name associated with the load balancer.
lbeDNSName :: Lens' LoadBalancerDescription (Maybe Text)
lbeDNSName f x =
    f (_lbeDNSName x)
        <&> \y -> x { _lbeDNSName = y }
{-# INLINE lbeDNSName #-}

-- | Provides the name of the Amazon Route 53 hosted zone that is associated
-- with the load balancer. For information on how to associate your load
-- balancer with a hosted zone, go to Using Domain Names With Elastic Load
-- Balancing in the Elastic Load Balancing Developer Guide.
lbeCanonicalHostedZoneName :: Lens' LoadBalancerDescription (Maybe Text)
lbeCanonicalHostedZoneName f x =
    f (_lbeCanonicalHostedZoneName x)
        <&> \y -> x { _lbeCanonicalHostedZoneName = y }
{-# INLINE lbeCanonicalHostedZoneName #-}

-- | Provides the ID of the Amazon Route 53 hosted zone name that is associated
-- with the load balancer. For information on how to associate or disassociate
-- your load balancer with a hosted zone, go to Using Domain Names With
-- Elastic Load Balancing in the Elastic Load Balancing Developer Guide.
lbeCanonicalHostedZoneNameID :: Lens' LoadBalancerDescription (Maybe Text)
lbeCanonicalHostedZoneNameID f x =
    f (_lbeCanonicalHostedZoneNameID x)
        <&> \y -> x { _lbeCanonicalHostedZoneNameID = y }
{-# INLINE lbeCanonicalHostedZoneNameID #-}

-- | LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and PolicyNames
-- are returned in a list of tuples in the ListenerDescriptions element.
lbeListenerDescriptions :: Lens' LoadBalancerDescription ([ListenerDescription])
lbeListenerDescriptions f x =
    f (_lbeListenerDescriptions x)
        <&> \y -> x { _lbeListenerDescriptions = y }
{-# INLINE lbeListenerDescriptions #-}

-- | Provides a list of policies defined for the load balancer.
lbePolicies :: Lens' LoadBalancerDescription (Maybe Policies)
lbePolicies f x =
    f (_lbePolicies x)
        <&> \y -> x { _lbePolicies = y }
{-# INLINE lbePolicies #-}

-- | Contains a list of back-end server descriptions.
lbeBackendServerDescriptions :: Lens' LoadBalancerDescription ([BackendServerDescription])
lbeBackendServerDescriptions f x =
    f (_lbeBackendServerDescriptions x)
        <&> \y -> x { _lbeBackendServerDescriptions = y }
{-# INLINE lbeBackendServerDescriptions #-}

-- | Specifies a list of Availability Zones.
lbeAvailabilityZones :: Lens' LoadBalancerDescription ([Text])
lbeAvailabilityZones f x =
    f (_lbeAvailabilityZones x)
        <&> \y -> x { _lbeAvailabilityZones = y }
{-# INLINE lbeAvailabilityZones #-}

-- | Provides a list of VPC subnet IDs for the load balancer.
lbeSubnets :: Lens' LoadBalancerDescription ([Text])
lbeSubnets f x =
    f (_lbeSubnets x)
        <&> \y -> x { _lbeSubnets = y }
{-# INLINE lbeSubnets #-}

-- | Provides the ID of the VPC attached to the load balancer.
lbeVPCId :: Lens' LoadBalancerDescription (Maybe Text)
lbeVPCId f x =
    f (_lbeVPCId x)
        <&> \y -> x { _lbeVPCId = y }
{-# INLINE lbeVPCId #-}

-- | Provides a list of EC2 instance IDs for the load balancer.
lbeInstances :: Lens' LoadBalancerDescription ([Instance])
lbeInstances f x =
    f (_lbeInstances x)
        <&> \y -> x { _lbeInstances = y }
{-# INLINE lbeInstances #-}

-- | Specifies information regarding the various health probes conducted on the
-- load balancer.
lbeHealthCheck :: Lens' LoadBalancerDescription (Maybe HealthCheck)
lbeHealthCheck f x =
    f (_lbeHealthCheck x)
        <&> \y -> x { _lbeHealthCheck = y }
{-# INLINE lbeHealthCheck #-}

-- | The security group that you can use as part of your inbound rules for your
-- load balancer's back-end Amazon EC2 application instances. To only allow
-- traffic from load balancers, add a security group rule to your back end
-- instance that specifies this source security group as the inbound source.
lbeSourceSecurityGroup :: Lens' LoadBalancerDescription (Maybe SourceSecurityGroup)
lbeSourceSecurityGroup f x =
    f (_lbeSourceSecurityGroup x)
        <&> \y -> x { _lbeSourceSecurityGroup = y }
{-# INLINE lbeSourceSecurityGroup #-}

-- | The security groups the load balancer is a member of (VPC only).
lbeSecurityGroups :: Lens' LoadBalancerDescription ([Text])
lbeSecurityGroups f x =
    f (_lbeSecurityGroups x)
        <&> \y -> x { _lbeSecurityGroups = y }
{-# INLINE lbeSecurityGroups #-}

-- | Provides the date and time the load balancer was created.
lbeCreatedTime :: Lens' LoadBalancerDescription (Maybe ISO8601)
lbeCreatedTime f x =
    f (_lbeCreatedTime x)
        <&> \y -> x { _lbeCreatedTime = y }
{-# INLINE lbeCreatedTime #-}

-- | Specifies the type of load balancer. If the Scheme is internet-facing, the
-- load balancer has a publicly resolvable DNS name that resolves to public IP
-- addresses. If the Scheme is internal, the load balancer has a publicly
-- resolvable DNS name that resolves to private IP addresses. This option is
-- only available for load balancers attached to an Amazon VPC.
lbeScheme :: Lens' LoadBalancerDescription (Maybe Text)
lbeScheme f x =
    f (_lbeScheme x)
        <&> \y -> x { _lbeScheme = y }
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
pxAppCookieStickinessPolicies f x =
    f (_pxAppCookieStickinessPolicies x)
        <&> \y -> x { _pxAppCookieStickinessPolicies = y }
{-# INLINE pxAppCookieStickinessPolicies #-}

-- | A list of LBCookieStickinessPolicy objects created with
-- CreateAppCookieStickinessPolicy.
pxLBCookieStickinessPolicies :: Lens' Policies ([LBCookieStickinessPolicy])
pxLBCookieStickinessPolicies f x =
    f (_pxLBCookieStickinessPolicies x)
        <&> \y -> x { _pxLBCookieStickinessPolicies = y }
{-# INLINE pxLBCookieStickinessPolicies #-}

-- | A list of policy names other than the stickiness policies.
pxOtherPolicies :: Lens' Policies ([Text])
pxOtherPolicies f x =
    f (_pxOtherPolicies x)
        <&> \y -> x { _pxOtherPolicies = y }
{-# INLINE pxOtherPolicies #-}

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
pbAttributeName f x =
    f (_pbAttributeName x)
        <&> \y -> x { _pbAttributeName = y }
{-# INLINE pbAttributeName #-}

-- | The value of the attribute associated with the policy.
pbAttributeValue :: Lens' PolicyAttribute (Maybe Text)
pbAttributeValue f x =
    f (_pbAttributeValue x)
        <&> \y -> x { _pbAttributeValue = y }
{-# INLINE pbAttributeValue #-}

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
paeAttributeName f x =
    f (_paeAttributeName x)
        <&> \y -> x { _paeAttributeName = y }
{-# INLINE paeAttributeName #-}

-- | The value of the attribute associated with the policy.
paeAttributeValue :: Lens' PolicyAttributeDescription (Maybe Text)
paeAttributeValue f x =
    f (_paeAttributeValue x)
        <&> \y -> x { _paeAttributeValue = y }
{-# INLINE paeAttributeValue #-}

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
pateAttributeName f x =
    f (_pateAttributeName x)
        <&> \y -> x { _pateAttributeName = y }
{-# INLINE pateAttributeName #-}

-- | The type of attribute. For example, Boolean, Integer, etc.
pateAttributeType :: Lens' PolicyAttributeTypeDescription (Maybe Text)
pateAttributeType f x =
    f (_pateAttributeType x)
        <&> \y -> x { _pateAttributeType = y }
{-# INLINE pateAttributeType #-}

-- | A human-readable description of the attribute.
pateDescription :: Lens' PolicyAttributeTypeDescription (Maybe Text)
pateDescription f x =
    f (_pateDescription x)
        <&> \y -> x { _pateDescription = y }
{-# INLINE pateDescription #-}

-- | The default value of the attribute, if applicable.
pateDefaultValue :: Lens' PolicyAttributeTypeDescription (Maybe Text)
pateDefaultValue f x =
    f (_pateDefaultValue x)
        <&> \y -> x { _pateDefaultValue = y }
{-# INLINE pateDefaultValue #-}

-- | The cardinality of the attribute. Valid Values: ONE(1) : Single value
-- required ZERO_OR_ONE(0..1) : Up to one value can be supplied
-- ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
-- ONE_OR_MORE(1..*0) : Required. Multiple values are allowed.
pateCardinality :: Lens' PolicyAttributeTypeDescription (Maybe Text)
pateCardinality f x =
    f (_pateCardinality x)
        <&> \y -> x { _pateCardinality = y }
{-# INLINE pateCardinality #-}

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
pePolicyName f x =
    f (_pePolicyName x)
        <&> \y -> x { _pePolicyName = y }
{-# INLINE pePolicyName #-}

-- | The name of the policy type associated with the load balancer.
pePolicyTypeName :: Lens' PolicyDescription (Maybe Text)
pePolicyTypeName f x =
    f (_pePolicyTypeName x)
        <&> \y -> x { _pePolicyTypeName = y }
{-# INLINE pePolicyTypeName #-}

-- | A list of policy attribute description structures.
pePolicyAttributeDescriptions :: Lens' PolicyDescription ([PolicyAttributeDescription])
pePolicyAttributeDescriptions f x =
    f (_pePolicyAttributeDescriptions x)
        <&> \y -> x { _pePolicyAttributeDescriptions = y }
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
ptePolicyTypeName f x =
    f (_ptePolicyTypeName x)
        <&> \y -> x { _ptePolicyTypeName = y }
{-# INLINE ptePolicyTypeName #-}

-- | A human-readable description of the policy type.
pteDescription :: Lens' PolicyTypeDescription (Maybe Text)
pteDescription f x =
    f (_pteDescription x)
        <&> \y -> x { _pteDescription = y }
{-# INLINE pteDescription #-}

-- | The description of the policy attributes associated with the load balancer
-- policies defined by the Elastic Load Balancing service.
ptePolicyAttributeTypeDescriptions :: Lens' PolicyTypeDescription ([PolicyAttributeTypeDescription])
ptePolicyAttributeTypeDescriptions f x =
    f (_ptePolicyAttributeTypeDescriptions x)
        <&> \y -> x { _ptePolicyAttributeTypeDescriptions = y }
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
ssgOwnerAlias f x =
    f (_ssgOwnerAlias x)
        <&> \y -> x { _ssgOwnerAlias = y }
{-# INLINE ssgOwnerAlias #-}

-- | Name of the source security group. Use this value for the --source-group
-- parameter of the ec2-authorize command in the Amazon EC2 command line tool.
ssgGroupName :: Lens' SourceSecurityGroup (Maybe Text)
ssgGroupName f x =
    f (_ssgGroupName x)
        <&> \y -> x { _ssgGroupName = y }
{-# INLINE ssgGroupName #-}

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
tKey f x =
    f (_tKey x)
        <&> \y -> x { _tKey = y }
{-# INLINE tKey #-}

-- | The value of the tag.
tValue :: Lens' Tag (Maybe Text)
tValue f x =
    f (_tValue x)
        <&> \y -> x { _tValue = y }
{-# INLINE tValue #-}

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
teLoadBalancerName f x =
    f (_teLoadBalancerName x)
        <&> \y -> x { _teLoadBalancerName = y }
{-# INLINE teLoadBalancerName #-}

-- | List of tags associated with the load balancer.
teTags :: Lens' TagDescription (Maybe [Tag])
teTags f x =
    f (_teTags x)
        <&> \y -> x { _teTags = y }
{-# INLINE teTags #-}

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagDescription"
