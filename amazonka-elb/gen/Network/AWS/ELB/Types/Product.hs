{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.Product where

import           Network.AWS.ELB.Types.Sum
import           Network.AWS.Prelude

-- | Information about the @AccessLog@ attribute.
--
-- /See:/ 'accessLog' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'alEmitInterval'
--
-- * 'alS3BucketPrefix'
--
-- * 'alS3BucketName'
--
-- * 'alEnabled'
data AccessLog = AccessLog'
    { _alEmitInterval   :: !(Maybe Int)
    , _alS3BucketPrefix :: !(Maybe Text)
    , _alS3BucketName   :: !(Maybe Text)
    , _alEnabled        :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AccessLog' smart constructor.
accessLog :: Bool -> AccessLog
accessLog pEnabled =
    AccessLog'
    { _alEmitInterval = Nothing
    , _alS3BucketPrefix = Nothing
    , _alS3BucketName = Nothing
    , _alEnabled = pEnabled
    }

-- | The interval for publishing the access logs. You can specify an interval
-- of either 5 minutes or 60 minutes.
--
-- Default: 60 minutes
alEmitInterval :: Lens' AccessLog (Maybe Int)
alEmitInterval = lens _alEmitInterval (\ s a -> s{_alEmitInterval = a});

-- | The logical hierarchy you created for your Amazon S3 bucket, for example
-- @my-bucket-prefix\/prod@. If the prefix is not provided, the log is
-- placed at the root level of the bucket.
alS3BucketPrefix :: Lens' AccessLog (Maybe Text)
alS3BucketPrefix = lens _alS3BucketPrefix (\ s a -> s{_alS3BucketPrefix = a});

-- | The name of the Amazon S3 bucket where the access logs are stored.
alS3BucketName :: Lens' AccessLog (Maybe Text)
alS3BucketName = lens _alS3BucketName (\ s a -> s{_alS3BucketName = a});

-- | Specifies whether access log is enabled for the load balancer.
alEnabled :: Lens' AccessLog Bool
alEnabled = lens _alEnabled (\ s a -> s{_alEnabled = a});

instance FromXML AccessLog where
        parseXML x
          = AccessLog' <$>
              (x .@? "EmitInterval") <*> (x .@? "S3BucketPrefix")
                <*> (x .@? "S3BucketName")
                <*> (x .@ "Enabled")

instance ToQuery AccessLog where
        toQuery AccessLog'{..}
          = mconcat
              ["EmitInterval" =: _alEmitInterval,
               "S3BucketPrefix" =: _alS3BucketPrefix,
               "S3BucketName" =: _alS3BucketName,
               "Enabled" =: _alEnabled]

-- | This data type is reserved.
--
-- /See:/ 'additionalAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaValue'
--
-- * 'aaKey'
data AdditionalAttribute = AdditionalAttribute'
    { _aaValue :: !(Maybe Text)
    , _aaKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AdditionalAttribute' smart constructor.
additionalAttribute :: AdditionalAttribute
additionalAttribute =
    AdditionalAttribute'
    { _aaValue = Nothing
    , _aaKey = Nothing
    }

-- | This parameter is reserved.
aaValue :: Lens' AdditionalAttribute (Maybe Text)
aaValue = lens _aaValue (\ s a -> s{_aaValue = a});

-- | This parameter is reserved.
aaKey :: Lens' AdditionalAttribute (Maybe Text)
aaKey = lens _aaKey (\ s a -> s{_aaKey = a});

instance FromXML AdditionalAttribute where
        parseXML x
          = AdditionalAttribute' <$>
              (x .@? "Value") <*> (x .@? "Key")

instance ToQuery AdditionalAttribute where
        toQuery AdditionalAttribute'{..}
          = mconcat ["Value" =: _aaValue, "Key" =: _aaKey]

-- | Information about a policy for application-controlled session
-- stickiness.
--
-- /See:/ 'appCookieStickinessPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acspPolicyName'
--
-- * 'acspCookieName'
data AppCookieStickinessPolicy = AppCookieStickinessPolicy'
    { _acspPolicyName :: !(Maybe Text)
    , _acspCookieName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AppCookieStickinessPolicy' smart constructor.
appCookieStickinessPolicy :: AppCookieStickinessPolicy
appCookieStickinessPolicy =
    AppCookieStickinessPolicy'
    { _acspPolicyName = Nothing
    , _acspCookieName = Nothing
    }

-- | The mnemonic name for the policy being created. The name must be unique
-- within a set of policies for this load balancer.
acspPolicyName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acspPolicyName = lens _acspPolicyName (\ s a -> s{_acspPolicyName = a});

-- | The name of the application cookie used for stickiness.
acspCookieName :: Lens' AppCookieStickinessPolicy (Maybe Text)
acspCookieName = lens _acspCookieName (\ s a -> s{_acspCookieName = a});

instance FromXML AppCookieStickinessPolicy where
        parseXML x
          = AppCookieStickinessPolicy' <$>
              (x .@? "PolicyName") <*> (x .@? "CookieName")

-- | Information about the configuration of a back-end server.
--
-- /See:/ 'backendServerDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bsdPolicyNames'
--
-- * 'bsdInstancePort'
data BackendServerDescription = BackendServerDescription'
    { _bsdPolicyNames  :: !(Maybe [Text])
    , _bsdInstancePort :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BackendServerDescription' smart constructor.
backendServerDescription :: BackendServerDescription
backendServerDescription =
    BackendServerDescription'
    { _bsdPolicyNames = Nothing
    , _bsdInstancePort = Nothing
    }

-- | The names of the policies enabled for the back-end server.
bsdPolicyNames :: Lens' BackendServerDescription [Text]
bsdPolicyNames = lens _bsdPolicyNames (\ s a -> s{_bsdPolicyNames = a}) . _Default;

-- | The port on which the back-end server is listening.
bsdInstancePort :: Lens' BackendServerDescription (Maybe Natural)
bsdInstancePort = lens _bsdInstancePort (\ s a -> s{_bsdInstancePort = a}) . mapping _Nat;

instance FromXML BackendServerDescription where
        parseXML x
          = BackendServerDescription' <$>
              (x .@? "PolicyNames" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "InstancePort")

-- | Information about the @ConnectionDraining@ attribute.
--
-- /See:/ 'connectionDraining' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdTimeout'
--
-- * 'cdEnabled'
data ConnectionDraining = ConnectionDraining'
    { _cdTimeout :: !(Maybe Int)
    , _cdEnabled :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConnectionDraining' smart constructor.
connectionDraining :: Bool -> ConnectionDraining
connectionDraining pEnabled =
    ConnectionDraining'
    { _cdTimeout = Nothing
    , _cdEnabled = pEnabled
    }

-- | The maximum time, in seconds, to keep the existing connections open
-- before deregistering the instances.
cdTimeout :: Lens' ConnectionDraining (Maybe Int)
cdTimeout = lens _cdTimeout (\ s a -> s{_cdTimeout = a});

-- | Specifies whether connection draining is enabled for the load balancer.
cdEnabled :: Lens' ConnectionDraining Bool
cdEnabled = lens _cdEnabled (\ s a -> s{_cdEnabled = a});

instance FromXML ConnectionDraining where
        parseXML x
          = ConnectionDraining' <$>
              (x .@? "Timeout") <*> (x .@ "Enabled")

instance ToQuery ConnectionDraining where
        toQuery ConnectionDraining'{..}
          = mconcat
              ["Timeout" =: _cdTimeout, "Enabled" =: _cdEnabled]

-- | Information about the @ConnectionSettings@ attribute.
--
-- /See:/ 'connectionSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csIdleTimeout'
newtype ConnectionSettings = ConnectionSettings'
    { _csIdleTimeout :: Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConnectionSettings' smart constructor.
connectionSettings :: Natural -> ConnectionSettings
connectionSettings pIdleTimeout =
    ConnectionSettings'
    { _csIdleTimeout = _Nat # pIdleTimeout
    }

-- | The time, in seconds, that the connection is allowed to be idle (no data
-- has been sent over the connection) before it is closed by the load
-- balancer.
csIdleTimeout :: Lens' ConnectionSettings Natural
csIdleTimeout = lens _csIdleTimeout (\ s a -> s{_csIdleTimeout = a}) . _Nat;

instance FromXML ConnectionSettings where
        parseXML x
          = ConnectionSettings' <$> (x .@ "IdleTimeout")

instance ToQuery ConnectionSettings where
        toQuery ConnectionSettings'{..}
          = mconcat ["IdleTimeout" =: _csIdleTimeout]

-- | Information about the @CrossZoneLoadBalancing@ attribute.
--
-- /See:/ 'crossZoneLoadBalancing' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'czlbEnabled'
newtype CrossZoneLoadBalancing = CrossZoneLoadBalancing'
    { _czlbEnabled :: Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CrossZoneLoadBalancing' smart constructor.
crossZoneLoadBalancing :: Bool -> CrossZoneLoadBalancing
crossZoneLoadBalancing pEnabled =
    CrossZoneLoadBalancing'
    { _czlbEnabled = pEnabled
    }

-- | Specifies whether cross-zone load balancing is enabled for the load
-- balancer.
czlbEnabled :: Lens' CrossZoneLoadBalancing Bool
czlbEnabled = lens _czlbEnabled (\ s a -> s{_czlbEnabled = a});

instance FromXML CrossZoneLoadBalancing where
        parseXML x
          = CrossZoneLoadBalancing' <$> (x .@ "Enabled")

instance ToQuery CrossZoneLoadBalancing where
        toQuery CrossZoneLoadBalancing'{..}
          = mconcat ["Enabled" =: _czlbEnabled]

-- | Information about a health check.
--
-- /See:/ 'healthCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hcTarget'
--
-- * 'hcInterval'
--
-- * 'hcTimeout'
--
-- * 'hcUnhealthyThreshold'
--
-- * 'hcHealthyThreshold'
data HealthCheck = HealthCheck'
    { _hcTarget             :: !Text
    , _hcInterval           :: !Nat
    , _hcTimeout            :: !Nat
    , _hcUnhealthyThreshold :: !Nat
    , _hcHealthyThreshold   :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HealthCheck' smart constructor.
healthCheck :: Text -> Natural -> Natural -> Natural -> Natural -> HealthCheck
healthCheck pTarget pInterval pTimeout pUnhealthyThreshold pHealthyThreshold =
    HealthCheck'
    { _hcTarget = pTarget
    , _hcInterval = _Nat # pInterval
    , _hcTimeout = _Nat # pTimeout
    , _hcUnhealthyThreshold = _Nat # pUnhealthyThreshold
    , _hcHealthyThreshold = _Nat # pHealthyThreshold
    }

-- | The instance being checked. The protocol is either TCP, HTTP, HTTPS, or
-- SSL. The range of valid ports is one (1) through 65535.
--
-- TCP is the default, specified as a TCP: port pair, for example
-- \"TCP:5000\". In this case, a health check simply attempts to open a TCP
-- connection to the instance on the specified port. Failure to connect
-- within the configured timeout is considered unhealthy.
--
-- SSL is also specified as SSL: port pair, for example, SSL:5000.
--
-- For HTTP\/HTTPS, you must include a ping path in the string. HTTP is
-- specified as a HTTP:port;\/;PathToPing; grouping, for example
-- \"HTTP:80\/weather\/us\/wa\/seattle\". In this case, a HTTP GET request
-- is issued to the instance on the given port and path. Any answer other
-- than \"200 OK\" within the timeout period is considered unhealthy.
--
-- The total length of the HTTP ping target must be 1024 16-bit Unicode
-- characters or less.
hcTarget :: Lens' HealthCheck Text
hcTarget = lens _hcTarget (\ s a -> s{_hcTarget = a});

-- | The approximate interval, in seconds, between health checks of an
-- individual instance.
hcInterval :: Lens' HealthCheck Natural
hcInterval = lens _hcInterval (\ s a -> s{_hcInterval = a}) . _Nat;

-- | The amount of time, in seconds, during which no response means a failed
-- health check.
--
-- This value must be less than the @Interval@ value.
hcTimeout :: Lens' HealthCheck Natural
hcTimeout = lens _hcTimeout (\ s a -> s{_hcTimeout = a}) . _Nat;

-- | The number of consecutive health check failures required before moving
-- the instance to the @Unhealthy@ state.
hcUnhealthyThreshold :: Lens' HealthCheck Natural
hcUnhealthyThreshold = lens _hcUnhealthyThreshold (\ s a -> s{_hcUnhealthyThreshold = a}) . _Nat;

-- | The number of consecutive health checks successes required before moving
-- the instance to the @Healthy@ state.
hcHealthyThreshold :: Lens' HealthCheck Natural
hcHealthyThreshold = lens _hcHealthyThreshold (\ s a -> s{_hcHealthyThreshold = a}) . _Nat;

instance FromXML HealthCheck where
        parseXML x
          = HealthCheck' <$>
              (x .@ "Target") <*> (x .@ "Interval") <*>
                (x .@ "Timeout")
                <*> (x .@ "UnhealthyThreshold")
                <*> (x .@ "HealthyThreshold")

instance ToQuery HealthCheck where
        toQuery HealthCheck'{..}
          = mconcat
              ["Target" =: _hcTarget, "Interval" =: _hcInterval,
               "Timeout" =: _hcTimeout,
               "UnhealthyThreshold" =: _hcUnhealthyThreshold,
               "HealthyThreshold" =: _hcHealthyThreshold]

-- | The ID of a back-end instance.
--
-- /See:/ 'instance'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'insInstanceId'
newtype Instance = Instance'
    { _insInstanceId :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Instance' smart constructor.
instance' :: Instance
instance' =
    Instance'
    { _insInstanceId = Nothing
    }

-- | The ID of the instance.
insInstanceId :: Lens' Instance (Maybe Text)
insInstanceId = lens _insInstanceId (\ s a -> s{_insInstanceId = a});

instance FromXML Instance where
        parseXML x = Instance' <$> (x .@? "InstanceId")

instance ToQuery Instance where
        toQuery Instance'{..}
          = mconcat ["InstanceId" =: _insInstanceId]

-- | Information about the state of a back-end instance.
--
-- /See:/ 'instanceState' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isInstanceId'
--
-- * 'isState'
--
-- * 'isReasonCode'
--
-- * 'isDescription'
data InstanceState = InstanceState'
    { _isInstanceId  :: !(Maybe Text)
    , _isState       :: !(Maybe Text)
    , _isReasonCode  :: !(Maybe Text)
    , _isDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceState' smart constructor.
instanceState :: InstanceState
instanceState =
    InstanceState'
    { _isInstanceId = Nothing
    , _isState = Nothing
    , _isReasonCode = Nothing
    , _isDescription = Nothing
    }

-- | The ID of the instance.
isInstanceId :: Lens' InstanceState (Maybe Text)
isInstanceId = lens _isInstanceId (\ s a -> s{_isInstanceId = a});

-- | The current state of the instance.
--
-- Valid values: @InService@ | @OutOfService@ | @Unknown@
isState :: Lens' InstanceState (Maybe Text)
isState = lens _isState (\ s a -> s{_isState = a});

-- | Information about the cause of @OutOfService@ instances. Specifically,
-- whether the cause is Elastic Load Balancing or the instance.
--
-- Valid values: @ELB@ | @Instance@ | @N\/A@
isReasonCode :: Lens' InstanceState (Maybe Text)
isReasonCode = lens _isReasonCode (\ s a -> s{_isReasonCode = a});

-- | A description of the instance state. This string can contain one or more
-- of the following messages.
--
-- -   @N\/A@
--
-- -   @A transient error occurred. Please try again later.@
--
-- -   @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@
--
-- -   @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@
--
-- -   @Instance registration is still in progress.@
--
-- -   @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@
--
-- -   @Instance is not currently registered with the LoadBalancer.@
--
-- -   @Instance deregistration currently in progress.@
--
-- -   @Disable Availability Zone is currently in progress.@
--
-- -   @Instance is in pending state.@
--
-- -   @Instance is in stopped state.@
--
-- -   @Instance is in terminated state.@
--
isDescription :: Lens' InstanceState (Maybe Text)
isDescription = lens _isDescription (\ s a -> s{_isDescription = a});

instance FromXML InstanceState where
        parseXML x
          = InstanceState' <$>
              (x .@? "InstanceId") <*> (x .@? "State") <*>
                (x .@? "ReasonCode")
                <*> (x .@? "Description")

-- | Information about a policy for duration-based session stickiness.
--
-- /See:/ 'lBCookieStickinessPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbcspPolicyName'
--
-- * 'lbcspCookieExpirationPeriod'
data LBCookieStickinessPolicy = LBCookieStickinessPolicy'
    { _lbcspPolicyName             :: !(Maybe Text)
    , _lbcspCookieExpirationPeriod :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LBCookieStickinessPolicy' smart constructor.
lBCookieStickinessPolicy :: LBCookieStickinessPolicy
lBCookieStickinessPolicy =
    LBCookieStickinessPolicy'
    { _lbcspPolicyName = Nothing
    , _lbcspCookieExpirationPeriod = Nothing
    }

-- | The name for the policy being created. The name must be unique within
-- the set of policies for this load balancer.
lbcspPolicyName :: Lens' LBCookieStickinessPolicy (Maybe Text)
lbcspPolicyName = lens _lbcspPolicyName (\ s a -> s{_lbcspPolicyName = a});

-- | The time period, in seconds, after which the cookie should be considered
-- stale. If this parameter is not specified, the stickiness session lasts
-- for the duration of the browser session.
lbcspCookieExpirationPeriod :: Lens' LBCookieStickinessPolicy (Maybe Integer)
lbcspCookieExpirationPeriod = lens _lbcspCookieExpirationPeriod (\ s a -> s{_lbcspCookieExpirationPeriod = a});

instance FromXML LBCookieStickinessPolicy where
        parseXML x
          = LBCookieStickinessPolicy' <$>
              (x .@? "PolicyName") <*>
                (x .@? "CookieExpirationPeriod")

-- | Information about a listener.
--
-- For information about the protocols and the ports supported by Elastic
-- Load Balancing, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/elb-listener-config.html Listener Configurations for Elastic Load Balancing>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- /See:/ 'listener' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisInstanceProtocol'
--
-- * 'lisSSLCertificateId'
--
-- * 'lisProtocol'
--
-- * 'lisLoadBalancerPort'
--
-- * 'lisInstancePort'
data Listener = Listener'
    { _lisInstanceProtocol :: !(Maybe Text)
    , _lisSSLCertificateId :: !(Maybe Text)
    , _lisProtocol         :: !Text
    , _lisLoadBalancerPort :: !Int
    , _lisInstancePort     :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Listener' smart constructor.
listener :: Text -> Int -> Natural -> Listener
listener pProtocol pLoadBalancerPort pInstancePort =
    Listener'
    { _lisInstanceProtocol = Nothing
    , _lisSSLCertificateId = Nothing
    , _lisProtocol = pProtocol
    , _lisLoadBalancerPort = pLoadBalancerPort
    , _lisInstancePort = _Nat # pInstancePort
    }

-- | The protocol to use for routing traffic to back-end instances: HTTP,
-- HTTPS, TCP, or SSL.
--
-- If the front-end protocol is HTTP, HTTPS, TCP, or SSL,
-- @InstanceProtocol@ must be at the same protocol.
--
-- If there is another listener with the same @InstancePort@ whose
-- @InstanceProtocol@ is secure, (HTTPS or SSL), the listener\'s
-- @InstanceProtocol@ must also be secure.
--
-- If there is another listener with the same @InstancePort@ whose
-- @InstanceProtocol@ is HTTP or TCP, the listener\'s @InstanceProtocol@
-- must be HTTP or TCP.
lisInstanceProtocol :: Lens' Listener (Maybe Text)
lisInstanceProtocol = lens _lisInstanceProtocol (\ s a -> s{_lisInstanceProtocol = a});

-- | The Amazon Resource Name (ARN) of the server certificate.
lisSSLCertificateId :: Lens' Listener (Maybe Text)
lisSSLCertificateId = lens _lisSSLCertificateId (\ s a -> s{_lisSSLCertificateId = a});

-- | The load balancer transport protocol to use for routing: HTTP, HTTPS,
-- TCP, or SSL.
lisProtocol :: Lens' Listener Text
lisProtocol = lens _lisProtocol (\ s a -> s{_lisProtocol = a});

-- | The port on which the load balancer is listening. The supported ports
-- are: 25, 80, 443, 465, 587, and 1024-65535.
lisLoadBalancerPort :: Lens' Listener Int
lisLoadBalancerPort = lens _lisLoadBalancerPort (\ s a -> s{_lisLoadBalancerPort = a});

-- | The port on which the instance is listening.
lisInstancePort :: Lens' Listener Natural
lisInstancePort = lens _lisInstancePort (\ s a -> s{_lisInstancePort = a}) . _Nat;

instance FromXML Listener where
        parseXML x
          = Listener' <$>
              (x .@? "InstanceProtocol") <*>
                (x .@? "SSLCertificateId")
                <*> (x .@ "Protocol")
                <*> (x .@ "LoadBalancerPort")
                <*> (x .@ "InstancePort")

instance ToQuery Listener where
        toQuery Listener'{..}
          = mconcat
              ["InstanceProtocol" =: _lisInstanceProtocol,
               "SSLCertificateId" =: _lisSSLCertificateId,
               "Protocol" =: _lisProtocol,
               "LoadBalancerPort" =: _lisLoadBalancerPort,
               "InstancePort" =: _lisInstancePort]

-- | The policies enabled for a listener.
--
-- /See:/ 'listenerDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldPolicyNames'
--
-- * 'ldListener'
data ListenerDescription = ListenerDescription'
    { _ldPolicyNames :: !(Maybe [Text])
    , _ldListener    :: !(Maybe Listener)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListenerDescription' smart constructor.
listenerDescription :: ListenerDescription
listenerDescription =
    ListenerDescription'
    { _ldPolicyNames = Nothing
    , _ldListener = Nothing
    }

-- | The policies. If there are no policies enabled, the list is empty.
ldPolicyNames :: Lens' ListenerDescription [Text]
ldPolicyNames = lens _ldPolicyNames (\ s a -> s{_ldPolicyNames = a}) . _Default;

-- | FIXME: Undocumented member.
ldListener :: Lens' ListenerDescription (Maybe Listener)
ldListener = lens _ldListener (\ s a -> s{_ldListener = a});

instance FromXML ListenerDescription where
        parseXML x
          = ListenerDescription' <$>
              (x .@? "PolicyNames" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "Listener")

-- | The attributes for a load balancer.
--
-- /See:/ 'loadBalancerAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbaCrossZoneLoadBalancing'
--
-- * 'lbaAccessLog'
--
-- * 'lbaAdditionalAttributes'
--
-- * 'lbaConnectionSettings'
--
-- * 'lbaConnectionDraining'
data LoadBalancerAttributes = LoadBalancerAttributes'
    { _lbaCrossZoneLoadBalancing :: !(Maybe CrossZoneLoadBalancing)
    , _lbaAccessLog              :: !(Maybe AccessLog)
    , _lbaAdditionalAttributes   :: !(Maybe [AdditionalAttribute])
    , _lbaConnectionSettings     :: !(Maybe ConnectionSettings)
    , _lbaConnectionDraining     :: !(Maybe ConnectionDraining)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoadBalancerAttributes' smart constructor.
loadBalancerAttributes :: LoadBalancerAttributes
loadBalancerAttributes =
    LoadBalancerAttributes'
    { _lbaCrossZoneLoadBalancing = Nothing
    , _lbaAccessLog = Nothing
    , _lbaAdditionalAttributes = Nothing
    , _lbaConnectionSettings = Nothing
    , _lbaConnectionDraining = Nothing
    }

-- | If enabled, the load balancer routes the request traffic evenly across
-- all back-end instances regardless of the Availability Zones.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/enable-disable-crosszone-lb.html Enable Cross-Zone Load Balancing>
-- in the /Elastic Load Balancing Developer Guide/.
lbaCrossZoneLoadBalancing :: Lens' LoadBalancerAttributes (Maybe CrossZoneLoadBalancing)
lbaCrossZoneLoadBalancing = lens _lbaCrossZoneLoadBalancing (\ s a -> s{_lbaCrossZoneLoadBalancing = a});

-- | If enabled, the load balancer captures detailed information of all
-- requests and delivers the information to the Amazon S3 bucket that you
-- specify.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/enable-access-logs.html Enable Access Logs>
-- in the /Elastic Load Balancing Developer Guide/.
lbaAccessLog :: Lens' LoadBalancerAttributes (Maybe AccessLog)
lbaAccessLog = lens _lbaAccessLog (\ s a -> s{_lbaAccessLog = a});

-- | This parameter is reserved.
lbaAdditionalAttributes :: Lens' LoadBalancerAttributes [AdditionalAttribute]
lbaAdditionalAttributes = lens _lbaAdditionalAttributes (\ s a -> s{_lbaAdditionalAttributes = a}) . _Default;

-- | If enabled, the load balancer allows the connections to remain idle (no
-- data is sent over the connection) for the specified duration.
--
-- By default, Elastic Load Balancing maintains a 60-second idle connection
-- timeout for both front-end and back-end connections of your load
-- balancer. For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/config-idle-timeout.html Configure Idle Connection Timeout>
-- in the /Elastic Load Balancing Developer Guide/.
lbaConnectionSettings :: Lens' LoadBalancerAttributes (Maybe ConnectionSettings)
lbaConnectionSettings = lens _lbaConnectionSettings (\ s a -> s{_lbaConnectionSettings = a});

-- | If enabled, the load balancer allows existing requests to complete
-- before the load balancer shifts traffic away from a deregistered or
-- unhealthy back-end instance.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/config-conn-drain.html Enable Connection Draining>
-- in the /Elastic Load Balancing Developer Guide/.
lbaConnectionDraining :: Lens' LoadBalancerAttributes (Maybe ConnectionDraining)
lbaConnectionDraining = lens _lbaConnectionDraining (\ s a -> s{_lbaConnectionDraining = a});

instance FromXML LoadBalancerAttributes where
        parseXML x
          = LoadBalancerAttributes' <$>
              (x .@? "CrossZoneLoadBalancing") <*>
                (x .@? "AccessLog")
                <*>
                (x .@? "AdditionalAttributes" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "ConnectionSettings")
                <*> (x .@? "ConnectionDraining")

instance ToQuery LoadBalancerAttributes where
        toQuery LoadBalancerAttributes'{..}
          = mconcat
              ["CrossZoneLoadBalancing" =:
                 _lbaCrossZoneLoadBalancing,
               "AccessLog" =: _lbaAccessLog,
               "AdditionalAttributes" =:
                 toQuery
                   (toQueryList "member" <$> _lbaAdditionalAttributes),
               "ConnectionSettings" =: _lbaConnectionSettings,
               "ConnectionDraining" =: _lbaConnectionDraining]

-- | Information about a load balancer.
--
-- /See:/ 'loadBalancerDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbdSourceSecurityGroup'
--
-- * 'lbdHealthCheck'
--
-- * 'lbdCanonicalHostedZoneName'
--
-- * 'lbdSecurityGroups'
--
-- * 'lbdLoadBalancerName'
--
-- * 'lbdCreatedTime'
--
-- * 'lbdVPCId'
--
-- * 'lbdSubnets'
--
-- * 'lbdAvailabilityZones'
--
-- * 'lbdBackendServerDescriptions'
--
-- * 'lbdCanonicalHostedZoneNameId'
--
-- * 'lbdInstances'
--
-- * 'lbdScheme'
--
-- * 'lbdListenerDescriptions'
--
-- * 'lbdDNSName'
--
-- * 'lbdPolicies'
data LoadBalancerDescription = LoadBalancerDescription'
    { _lbdSourceSecurityGroup       :: !(Maybe SourceSecurityGroup)
    , _lbdHealthCheck               :: !(Maybe HealthCheck)
    , _lbdCanonicalHostedZoneName   :: !(Maybe Text)
    , _lbdSecurityGroups            :: !(Maybe [Text])
    , _lbdLoadBalancerName          :: !(Maybe Text)
    , _lbdCreatedTime               :: !(Maybe ISO8601)
    , _lbdVPCId                     :: !(Maybe Text)
    , _lbdSubnets                   :: !(Maybe [Text])
    , _lbdAvailabilityZones         :: !(Maybe [Text])
    , _lbdBackendServerDescriptions :: !(Maybe [BackendServerDescription])
    , _lbdCanonicalHostedZoneNameId :: !(Maybe Text)
    , _lbdInstances                 :: !(Maybe [Instance])
    , _lbdScheme                    :: !(Maybe Text)
    , _lbdListenerDescriptions      :: !(Maybe [ListenerDescription])
    , _lbdDNSName                   :: !(Maybe Text)
    , _lbdPolicies                  :: !(Maybe Policies)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoadBalancerDescription' smart constructor.
loadBalancerDescription :: LoadBalancerDescription
loadBalancerDescription =
    LoadBalancerDescription'
    { _lbdSourceSecurityGroup = Nothing
    , _lbdHealthCheck = Nothing
    , _lbdCanonicalHostedZoneName = Nothing
    , _lbdSecurityGroups = Nothing
    , _lbdLoadBalancerName = Nothing
    , _lbdCreatedTime = Nothing
    , _lbdVPCId = Nothing
    , _lbdSubnets = Nothing
    , _lbdAvailabilityZones = Nothing
    , _lbdBackendServerDescriptions = Nothing
    , _lbdCanonicalHostedZoneNameId = Nothing
    , _lbdInstances = Nothing
    , _lbdScheme = Nothing
    , _lbdListenerDescriptions = Nothing
    , _lbdDNSName = Nothing
    , _lbdPolicies = Nothing
    }

-- | The security group that you can use as part of your inbound rules for
-- your load balancer\'s back-end application instances. To only allow
-- traffic from load balancers, add a security group rule to your back end
-- instance that specifies this source security group as the inbound
-- source.
lbdSourceSecurityGroup :: Lens' LoadBalancerDescription (Maybe SourceSecurityGroup)
lbdSourceSecurityGroup = lens _lbdSourceSecurityGroup (\ s a -> s{_lbdSourceSecurityGroup = a});

-- | Information about the health checks conducted on the load balancer.
lbdHealthCheck :: Lens' LoadBalancerDescription (Maybe HealthCheck)
lbdHealthCheck = lens _lbdHealthCheck (\ s a -> s{_lbdHealthCheck = a});

-- | The Amazon Route 53 hosted zone associated with the load balancer.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/using-domain-names-with-elb.html Using Domain Names With Elastic Load Balancing>
-- in the /Elastic Load Balancing Developer Guide/.
lbdCanonicalHostedZoneName :: Lens' LoadBalancerDescription (Maybe Text)
lbdCanonicalHostedZoneName = lens _lbdCanonicalHostedZoneName (\ s a -> s{_lbdCanonicalHostedZoneName = a});

-- | The security groups for the load balancer. Valid only for load balancers
-- in a VPC.
lbdSecurityGroups :: Lens' LoadBalancerDescription [Text]
lbdSecurityGroups = lens _lbdSecurityGroups (\ s a -> s{_lbdSecurityGroups = a}) . _Default;

-- | The name of the load balancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName = lens _lbdLoadBalancerName (\ s a -> s{_lbdLoadBalancerName = a});

-- | The date and time the load balancer was created.
lbdCreatedTime :: Lens' LoadBalancerDescription (Maybe UTCTime)
lbdCreatedTime = lens _lbdCreatedTime (\ s a -> s{_lbdCreatedTime = a}) . mapping _Time;

-- | The ID of the VPC for the load balancer.
lbdVPCId :: Lens' LoadBalancerDescription (Maybe Text)
lbdVPCId = lens _lbdVPCId (\ s a -> s{_lbdVPCId = a});

-- | The IDs of the subnets for the load balancer.
lbdSubnets :: Lens' LoadBalancerDescription [Text]
lbdSubnets = lens _lbdSubnets (\ s a -> s{_lbdSubnets = a}) . _Default;

-- | The Availability Zones for the load balancer.
lbdAvailabilityZones :: Lens' LoadBalancerDescription [Text]
lbdAvailabilityZones = lens _lbdAvailabilityZones (\ s a -> s{_lbdAvailabilityZones = a}) . _Default;

-- | Information about the back-end servers.
lbdBackendServerDescriptions :: Lens' LoadBalancerDescription [BackendServerDescription]
lbdBackendServerDescriptions = lens _lbdBackendServerDescriptions (\ s a -> s{_lbdBackendServerDescriptions = a}) . _Default;

-- | The ID of the Amazon Route 53 hosted zone name associated with the load
-- balancer.
lbdCanonicalHostedZoneNameId :: Lens' LoadBalancerDescription (Maybe Text)
lbdCanonicalHostedZoneNameId = lens _lbdCanonicalHostedZoneNameId (\ s a -> s{_lbdCanonicalHostedZoneNameId = a});

-- | The IDs of the instances for the load balancer.
lbdInstances :: Lens' LoadBalancerDescription [Instance]
lbdInstances = lens _lbdInstances (\ s a -> s{_lbdInstances = a}) . _Default;

-- | The type of load balancer. Valid only for load balancers in a VPC.
--
-- If @Scheme@ is @internet-facing@, the load balancer has a public DNS
-- name that resolves to a public IP address.
--
-- If @Scheme@ is @internal@, the load balancer has a public DNS name that
-- resolves to a private IP address.
lbdScheme :: Lens' LoadBalancerDescription (Maybe Text)
lbdScheme = lens _lbdScheme (\ s a -> s{_lbdScheme = a});

-- | The listeners for the load balancer.
lbdListenerDescriptions :: Lens' LoadBalancerDescription [ListenerDescription]
lbdListenerDescriptions = lens _lbdListenerDescriptions (\ s a -> s{_lbdListenerDescriptions = a}) . _Default;

-- | The external DNS name of the load balancer.
lbdDNSName :: Lens' LoadBalancerDescription (Maybe Text)
lbdDNSName = lens _lbdDNSName (\ s a -> s{_lbdDNSName = a});

-- | The policies defined for the load balancer.
lbdPolicies :: Lens' LoadBalancerDescription (Maybe Policies)
lbdPolicies = lens _lbdPolicies (\ s a -> s{_lbdPolicies = a});

instance FromXML LoadBalancerDescription where
        parseXML x
          = LoadBalancerDescription' <$>
              (x .@? "SourceSecurityGroup") <*>
                (x .@? "HealthCheck")
                <*> (x .@? "CanonicalHostedZoneName")
                <*>
                (x .@? "SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerName")
                <*> (x .@? "CreatedTime")
                <*> (x .@? "VPCId")
                <*>
                (x .@? "Subnets" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "BackendServerDescriptions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "CanonicalHostedZoneNameID")
                <*>
                (x .@? "Instances" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Scheme")
                <*>
                (x .@? "ListenerDescriptions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "DNSName")
                <*> (x .@? "Policies")

-- | The policies for a load balancer.
--
-- /See:/ 'policies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'polOtherPolicies'
--
-- * 'polLBCookieStickinessPolicies'
--
-- * 'polAppCookieStickinessPolicies'
data Policies = Policies'
    { _polOtherPolicies               :: !(Maybe [Text])
    , _polLBCookieStickinessPolicies  :: !(Maybe [LBCookieStickinessPolicy])
    , _polAppCookieStickinessPolicies :: !(Maybe [AppCookieStickinessPolicy])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Policies' smart constructor.
policies :: Policies
policies =
    Policies'
    { _polOtherPolicies = Nothing
    , _polLBCookieStickinessPolicies = Nothing
    , _polAppCookieStickinessPolicies = Nothing
    }

-- | The policies other than the stickiness policies.
polOtherPolicies :: Lens' Policies [Text]
polOtherPolicies = lens _polOtherPolicies (\ s a -> s{_polOtherPolicies = a}) . _Default;

-- | The stickiness policies created using CreateLBCookieStickinessPolicy.
polLBCookieStickinessPolicies :: Lens' Policies [LBCookieStickinessPolicy]
polLBCookieStickinessPolicies = lens _polLBCookieStickinessPolicies (\ s a -> s{_polLBCookieStickinessPolicies = a}) . _Default;

-- | The stickiness policies created using CreateAppCookieStickinessPolicy.
polAppCookieStickinessPolicies :: Lens' Policies [AppCookieStickinessPolicy]
polAppCookieStickinessPolicies = lens _polAppCookieStickinessPolicies (\ s a -> s{_polAppCookieStickinessPolicies = a}) . _Default;

instance FromXML Policies where
        parseXML x
          = Policies' <$>
              (x .@? "OtherPolicies" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*>
                (x .@? "LBCookieStickinessPolicies" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "AppCookieStickinessPolicies" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | Information about a policy attribute.
--
-- /See:/ 'policyAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'paAttributeValue'
--
-- * 'paAttributeName'
data PolicyAttribute = PolicyAttribute'
    { _paAttributeValue :: !(Maybe Text)
    , _paAttributeName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyAttribute' smart constructor.
policyAttribute :: PolicyAttribute
policyAttribute =
    PolicyAttribute'
    { _paAttributeValue = Nothing
    , _paAttributeName = Nothing
    }

-- | The value of the attribute.
paAttributeValue :: Lens' PolicyAttribute (Maybe Text)
paAttributeValue = lens _paAttributeValue (\ s a -> s{_paAttributeValue = a});

-- | The name of the attribute.
paAttributeName :: Lens' PolicyAttribute (Maybe Text)
paAttributeName = lens _paAttributeName (\ s a -> s{_paAttributeName = a});

instance ToQuery PolicyAttribute where
        toQuery PolicyAttribute'{..}
          = mconcat
              ["AttributeValue" =: _paAttributeValue,
               "AttributeName" =: _paAttributeName]

-- | Information about a policy attribute.
--
-- /See:/ 'policyAttributeDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'padAttributeValue'
--
-- * 'padAttributeName'
data PolicyAttributeDescription = PolicyAttributeDescription'
    { _padAttributeValue :: !(Maybe Text)
    , _padAttributeName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyAttributeDescription' smart constructor.
policyAttributeDescription :: PolicyAttributeDescription
policyAttributeDescription =
    PolicyAttributeDescription'
    { _padAttributeValue = Nothing
    , _padAttributeName = Nothing
    }

-- | The value of the attribute.
padAttributeValue :: Lens' PolicyAttributeDescription (Maybe Text)
padAttributeValue = lens _padAttributeValue (\ s a -> s{_padAttributeValue = a});

-- | The name of the attribute.
padAttributeName :: Lens' PolicyAttributeDescription (Maybe Text)
padAttributeName = lens _padAttributeName (\ s a -> s{_padAttributeName = a});

instance FromXML PolicyAttributeDescription where
        parseXML x
          = PolicyAttributeDescription' <$>
              (x .@? "AttributeValue") <*> (x .@? "AttributeName")

-- | Information about a policy attribute type.
--
-- /See:/ 'policyAttributeTypeDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'patdAttributeType'
--
-- * 'patdCardinality'
--
-- * 'patdDefaultValue'
--
-- * 'patdAttributeName'
--
-- * 'patdDescription'
data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription'
    { _patdAttributeType :: !(Maybe Text)
    , _patdCardinality   :: !(Maybe Text)
    , _patdDefaultValue  :: !(Maybe Text)
    , _patdAttributeName :: !(Maybe Text)
    , _patdDescription   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyAttributeTypeDescription' smart constructor.
policyAttributeTypeDescription :: PolicyAttributeTypeDescription
policyAttributeTypeDescription =
    PolicyAttributeTypeDescription'
    { _patdAttributeType = Nothing
    , _patdCardinality = Nothing
    , _patdDefaultValue = Nothing
    , _patdAttributeName = Nothing
    , _patdDescription = Nothing
    }

-- | The type of the attribute. For example, @Boolean@ or @Integer@.
patdAttributeType :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdAttributeType = lens _patdAttributeType (\ s a -> s{_patdAttributeType = a});

-- | The cardinality of the attribute.
--
-- Valid values:
--
-- -   ONE(1) : Single value required
-- -   ZERO_OR_ONE(0..1) : Up to one value can be supplied
-- -   ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
-- -   ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
patdCardinality :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdCardinality = lens _patdCardinality (\ s a -> s{_patdCardinality = a});

-- | The default value of the attribute, if applicable.
patdDefaultValue :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdDefaultValue = lens _patdDefaultValue (\ s a -> s{_patdDefaultValue = a});

-- | The name of the attribute.
patdAttributeName :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdAttributeName = lens _patdAttributeName (\ s a -> s{_patdAttributeName = a});

-- | A description of the attribute.
patdDescription :: Lens' PolicyAttributeTypeDescription (Maybe Text)
patdDescription = lens _patdDescription (\ s a -> s{_patdDescription = a});

instance FromXML PolicyAttributeTypeDescription where
        parseXML x
          = PolicyAttributeTypeDescription' <$>
              (x .@? "AttributeType") <*> (x .@? "Cardinality") <*>
                (x .@? "DefaultValue")
                <*> (x .@? "AttributeName")
                <*> (x .@? "Description")

-- | Information about a policy.
--
-- /See:/ 'policyDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdPolicyName'
--
-- * 'pdPolicyAttributeDescriptions'
--
-- * 'pdPolicyTypeName'
data PolicyDescription = PolicyDescription'
    { _pdPolicyName                  :: !(Maybe Text)
    , _pdPolicyAttributeDescriptions :: !(Maybe [PolicyAttributeDescription])
    , _pdPolicyTypeName              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyDescription' smart constructor.
policyDescription :: PolicyDescription
policyDescription =
    PolicyDescription'
    { _pdPolicyName = Nothing
    , _pdPolicyAttributeDescriptions = Nothing
    , _pdPolicyTypeName = Nothing
    }

-- | The name of the policy.
pdPolicyName :: Lens' PolicyDescription (Maybe Text)
pdPolicyName = lens _pdPolicyName (\ s a -> s{_pdPolicyName = a});

-- | The policy attributes.
pdPolicyAttributeDescriptions :: Lens' PolicyDescription [PolicyAttributeDescription]
pdPolicyAttributeDescriptions = lens _pdPolicyAttributeDescriptions (\ s a -> s{_pdPolicyAttributeDescriptions = a}) . _Default;

-- | The name of the policy type.
pdPolicyTypeName :: Lens' PolicyDescription (Maybe Text)
pdPolicyTypeName = lens _pdPolicyTypeName (\ s a -> s{_pdPolicyTypeName = a});

instance FromXML PolicyDescription where
        parseXML x
          = PolicyDescription' <$>
              (x .@? "PolicyName") <*>
                (x .@? "PolicyAttributeDescriptions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "PolicyTypeName")

-- | Information about a policy type.
--
-- /See:/ 'policyTypeDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptdPolicyTypeName'
--
-- * 'ptdDescription'
--
-- * 'ptdPolicyAttributeTypeDescriptions'
data PolicyTypeDescription = PolicyTypeDescription'
    { _ptdPolicyTypeName                  :: !(Maybe Text)
    , _ptdDescription                     :: !(Maybe Text)
    , _ptdPolicyAttributeTypeDescriptions :: !(Maybe [PolicyAttributeTypeDescription])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PolicyTypeDescription' smart constructor.
policyTypeDescription :: PolicyTypeDescription
policyTypeDescription =
    PolicyTypeDescription'
    { _ptdPolicyTypeName = Nothing
    , _ptdDescription = Nothing
    , _ptdPolicyAttributeTypeDescriptions = Nothing
    }

-- | The name of the policy type.
ptdPolicyTypeName :: Lens' PolicyTypeDescription (Maybe Text)
ptdPolicyTypeName = lens _ptdPolicyTypeName (\ s a -> s{_ptdPolicyTypeName = a});

-- | A description of the policy type.
ptdDescription :: Lens' PolicyTypeDescription (Maybe Text)
ptdDescription = lens _ptdDescription (\ s a -> s{_ptdDescription = a});

-- | The description of the policy attributes associated with the policies
-- defined by Elastic Load Balancing.
ptdPolicyAttributeTypeDescriptions :: Lens' PolicyTypeDescription [PolicyAttributeTypeDescription]
ptdPolicyAttributeTypeDescriptions = lens _ptdPolicyAttributeTypeDescriptions (\ s a -> s{_ptdPolicyAttributeTypeDescriptions = a}) . _Default;

instance FromXML PolicyTypeDescription where
        parseXML x
          = PolicyTypeDescription' <$>
              (x .@? "PolicyTypeName") <*> (x .@? "Description")
                <*>
                (x .@? "PolicyAttributeTypeDescriptions" .!@ mempty
                   >>= may (parseXMLList "member"))

-- | Information about a source security group.
--
-- /See:/ 'sourceSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssgOwnerAlias'
--
-- * 'ssgGroupName'
data SourceSecurityGroup = SourceSecurityGroup'
    { _ssgOwnerAlias :: !(Maybe Text)
    , _ssgGroupName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SourceSecurityGroup' smart constructor.
sourceSecurityGroup :: SourceSecurityGroup
sourceSecurityGroup =
    SourceSecurityGroup'
    { _ssgOwnerAlias = Nothing
    , _ssgGroupName = Nothing
    }

-- | The owner of the security group.
ssgOwnerAlias :: Lens' SourceSecurityGroup (Maybe Text)
ssgOwnerAlias = lens _ssgOwnerAlias (\ s a -> s{_ssgOwnerAlias = a});

-- | The name of the security group.
ssgGroupName :: Lens' SourceSecurityGroup (Maybe Text)
ssgGroupName = lens _ssgGroupName (\ s a -> s{_ssgGroupName = a});

instance FromXML SourceSecurityGroup where
        parseXML x
          = SourceSecurityGroup' <$>
              (x .@? "OwnerAlias") <*> (x .@? "GroupName")

-- | Information about a tag.
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
    , _tagKey   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Tag' smart constructor.
tag :: Text -> Tag
tag pKey =
    Tag'
    { _tagValue = Nothing
    , _tagKey = pKey
    }

-- | The value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@ "Key")

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | The tags associated with a load balancer.
--
-- /See:/ 'tagDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdLoadBalancerName'
--
-- * 'tdTags'
data TagDescription = TagDescription'
    { _tdLoadBalancerName :: !(Maybe Text)
    , _tdTags             :: !(Maybe (List1 Tag))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TagDescription' smart constructor.
tagDescription :: TagDescription
tagDescription =
    TagDescription'
    { _tdLoadBalancerName = Nothing
    , _tdTags = Nothing
    }

-- | The name of the load balancer.
tdLoadBalancerName :: Lens' TagDescription (Maybe Text)
tdLoadBalancerName = lens _tdLoadBalancerName (\ s a -> s{_tdLoadBalancerName = a});

-- | The tags.
tdTags :: Lens' TagDescription (Maybe (NonEmpty Tag))
tdTags = lens _tdTags (\ s a -> s{_tdTags = a}) . mapping _List1;

instance FromXML TagDescription where
        parseXML x
          = TagDescription' <$>
              (x .@? "LoadBalancerName") <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList1 "member"))

-- | The key of a tag.
--
-- /See:/ 'tagKeyOnly' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tkoKey'
newtype TagKeyOnly = TagKeyOnly'
    { _tkoKey :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TagKeyOnly' smart constructor.
tagKeyOnly :: TagKeyOnly
tagKeyOnly =
    TagKeyOnly'
    { _tkoKey = Nothing
    }

-- | The name of the key.
tkoKey :: Lens' TagKeyOnly (Maybe Text)
tkoKey = lens _tkoKey (\ s a -> s{_tkoKey = a});

instance ToQuery TagKeyOnly where
        toQuery TagKeyOnly'{..} = mconcat ["Key" =: _tkoKey]
