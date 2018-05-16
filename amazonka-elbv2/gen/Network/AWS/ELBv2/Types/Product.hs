{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.Product where

import Network.AWS.ELBv2.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an action.
--
--
--
-- /See:/ 'action' smart constructor.
data Action = Action'
  { _aType           :: !ActionTypeEnum
  , _aTargetGroupARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aType' - The type of action.
--
-- * 'aTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
action
    :: ActionTypeEnum -- ^ 'aType'
    -> Text -- ^ 'aTargetGroupARN'
    -> Action
action pType_ pTargetGroupARN_ =
  Action' {_aType = pType_, _aTargetGroupARN = pTargetGroupARN_}


-- | The type of action.
aType :: Lens' Action ActionTypeEnum
aType = lens _aType (\ s a -> s{_aType = a})

-- | The Amazon Resource Name (ARN) of the target group.
aTargetGroupARN :: Lens' Action Text
aTargetGroupARN = lens _aTargetGroupARN (\ s a -> s{_aTargetGroupARN = a})

instance FromXML Action where
        parseXML x
          = Action' <$>
              (x .@ "Type") <*> (x .@ "TargetGroupArn")

instance Hashable Action where

instance NFData Action where

instance ToQuery Action where
        toQuery Action'{..}
          = mconcat
              ["Type" =: _aType,
               "TargetGroupArn" =: _aTargetGroupARN]

-- | Information about an Availability Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { _azSubnetId              :: !(Maybe Text)
  , _azZoneName              :: !(Maybe Text)
  , _azLoadBalancerAddresses :: !(Maybe [LoadBalancerAddress])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azSubnetId' - The ID of the subnet.
--
-- * 'azZoneName' - The name of the Availability Zone.
--
-- * 'azLoadBalancerAddresses' - [Network Load Balancers] The static IP address.
availabilityZone
    :: AvailabilityZone
availabilityZone =
  AvailabilityZone'
    { _azSubnetId = Nothing
    , _azZoneName = Nothing
    , _azLoadBalancerAddresses = Nothing
    }


-- | The ID of the subnet.
azSubnetId :: Lens' AvailabilityZone (Maybe Text)
azSubnetId = lens _azSubnetId (\ s a -> s{_azSubnetId = a})

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\ s a -> s{_azZoneName = a})

-- | [Network Load Balancers] The static IP address.
azLoadBalancerAddresses :: Lens' AvailabilityZone [LoadBalancerAddress]
azLoadBalancerAddresses = lens _azLoadBalancerAddresses (\ s a -> s{_azLoadBalancerAddresses = a}) . _Default . _Coerce

instance FromXML AvailabilityZone where
        parseXML x
          = AvailabilityZone' <$>
              (x .@? "SubnetId") <*> (x .@? "ZoneName") <*>
                (x .@? "LoadBalancerAddresses" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where

-- | Information about an SSL server certificate.
--
--
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
  { _cCertificateARN :: !(Maybe Text)
  , _cIsDefault      :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateARN' - The Amazon Resource Name (ARN) of the certificate.
--
-- * 'cIsDefault' - Indicates whether the certificate is the default certificate.
certificate
    :: Certificate
certificate = Certificate' {_cCertificateARN = Nothing, _cIsDefault = Nothing}


-- | The Amazon Resource Name (ARN) of the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a})

-- | Indicates whether the certificate is the default certificate.
cIsDefault :: Lens' Certificate (Maybe Bool)
cIsDefault = lens _cIsDefault (\ s a -> s{_cIsDefault = a})

instance FromXML Certificate where
        parseXML x
          = Certificate' <$>
              (x .@? "CertificateArn") <*> (x .@? "IsDefault")

instance Hashable Certificate where

instance NFData Certificate where

instance ToQuery Certificate where
        toQuery Certificate'{..}
          = mconcat
              ["CertificateArn" =: _cCertificateARN,
               "IsDefault" =: _cIsDefault]

-- | Information about a cipher used in a policy.
--
--
--
-- /See:/ 'cipher' smart constructor.
data Cipher = Cipher'
  { _cPriority :: !(Maybe Int)
  , _cName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Cipher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cPriority' - The priority of the cipher.
--
-- * 'cName' - The name of the cipher.
cipher
    :: Cipher
cipher = Cipher' {_cPriority = Nothing, _cName = Nothing}


-- | The priority of the cipher.
cPriority :: Lens' Cipher (Maybe Int)
cPriority = lens _cPriority (\ s a -> s{_cPriority = a})

-- | The name of the cipher.
cName :: Lens' Cipher (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

instance FromXML Cipher where
        parseXML x
          = Cipher' <$> (x .@? "Priority") <*> (x .@? "Name")

instance Hashable Cipher where

instance NFData Cipher where

-- | Information about an Elastic Load Balancing resource limit for your AWS account.
--
--
--
-- /See:/ 'limit' smart constructor.
data Limit = Limit'
  { _lMax  :: !(Maybe Text)
  , _lName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Limit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMax' - The maximum value of the limit.
--
-- * 'lName' - The name of the limit. The possible values are:     * application-load-balancers     * listeners-per-application-load-balancer     * listeners-per-network-load-balancer     * network-load-balancers     * rules-per-application-load-balancer     * target-groups     * targets-per-application-load-balancer     * targets-per-availability-zone-per-network-load-balancer     * targets-per-network-load-balancer
limit
    :: Limit
limit = Limit' {_lMax = Nothing, _lName = Nothing}


-- | The maximum value of the limit.
lMax :: Lens' Limit (Maybe Text)
lMax = lens _lMax (\ s a -> s{_lMax = a})

-- | The name of the limit. The possible values are:     * application-load-balancers     * listeners-per-application-load-balancer     * listeners-per-network-load-balancer     * network-load-balancers     * rules-per-application-load-balancer     * target-groups     * targets-per-application-load-balancer     * targets-per-availability-zone-per-network-load-balancer     * targets-per-network-load-balancer
lName :: Lens' Limit (Maybe Text)
lName = lens _lName (\ s a -> s{_lName = a})

instance FromXML Limit where
        parseXML x
          = Limit' <$> (x .@? "Max") <*> (x .@? "Name")

instance Hashable Limit where

instance NFData Limit where

-- | Information about a listener.
--
--
--
-- /See:/ 'listener' smart constructor.
data Listener = Listener'
  { _lSSLPolicy       :: !(Maybe Text)
  , _lListenerARN     :: !(Maybe Text)
  , _lProtocol        :: !(Maybe ProtocolEnum)
  , _lDefaultActions  :: !(Maybe [Action])
  , _lCertificates    :: !(Maybe [Certificate])
  , _lLoadBalancerARN :: !(Maybe Text)
  , _lPort            :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lSSLPolicy' - The security policy that defines which ciphers and protocols are supported. The default is the current predefined security policy.
--
-- * 'lListenerARN' - The Amazon Resource Name (ARN) of the listener.
--
-- * 'lProtocol' - The protocol for connections from clients to the load balancer.
--
-- * 'lDefaultActions' - The default actions for the listener.
--
-- * 'lCertificates' - The SSL server certificate. You must provide a certificate if the protocol is HTTPS.
--
-- * 'lLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'lPort' - The port on which the load balancer is listening.
listener
    :: Listener
listener =
  Listener'
    { _lSSLPolicy = Nothing
    , _lListenerARN = Nothing
    , _lProtocol = Nothing
    , _lDefaultActions = Nothing
    , _lCertificates = Nothing
    , _lLoadBalancerARN = Nothing
    , _lPort = Nothing
    }


-- | The security policy that defines which ciphers and protocols are supported. The default is the current predefined security policy.
lSSLPolicy :: Lens' Listener (Maybe Text)
lSSLPolicy = lens _lSSLPolicy (\ s a -> s{_lSSLPolicy = a})

-- | The Amazon Resource Name (ARN) of the listener.
lListenerARN :: Lens' Listener (Maybe Text)
lListenerARN = lens _lListenerARN (\ s a -> s{_lListenerARN = a})

-- | The protocol for connections from clients to the load balancer.
lProtocol :: Lens' Listener (Maybe ProtocolEnum)
lProtocol = lens _lProtocol (\ s a -> s{_lProtocol = a})

-- | The default actions for the listener.
lDefaultActions :: Lens' Listener [Action]
lDefaultActions = lens _lDefaultActions (\ s a -> s{_lDefaultActions = a}) . _Default . _Coerce

-- | The SSL server certificate. You must provide a certificate if the protocol is HTTPS.
lCertificates :: Lens' Listener [Certificate]
lCertificates = lens _lCertificates (\ s a -> s{_lCertificates = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
lLoadBalancerARN :: Lens' Listener (Maybe Text)
lLoadBalancerARN = lens _lLoadBalancerARN (\ s a -> s{_lLoadBalancerARN = a})

-- | The port on which the load balancer is listening.
lPort :: Lens' Listener (Maybe Natural)
lPort = lens _lPort (\ s a -> s{_lPort = a}) . mapping _Nat

instance FromXML Listener where
        parseXML x
          = Listener' <$>
              (x .@? "SslPolicy") <*> (x .@? "ListenerArn") <*>
                (x .@? "Protocol")
                <*>
                (x .@? "DefaultActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Certificates" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerArn")
                <*> (x .@? "Port")

instance Hashable Listener where

instance NFData Listener where

-- | Information about a load balancer.
--
--
--
-- /See:/ 'loadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { _lbState                 :: !(Maybe LoadBalancerState)
  , _lbSecurityGroups        :: !(Maybe [Text])
  , _lbLoadBalancerName      :: !(Maybe Text)
  , _lbCreatedTime           :: !(Maybe ISO8601)
  , _lbVPCId                 :: !(Maybe Text)
  , _lbCanonicalHostedZoneId :: !(Maybe Text)
  , _lbAvailabilityZones     :: !(Maybe [AvailabilityZone])
  , _lbLoadBalancerARN       :: !(Maybe Text)
  , _lbIPAddressType         :: !(Maybe IPAddressType)
  , _lbScheme                :: !(Maybe LoadBalancerSchemeEnum)
  , _lbType                  :: !(Maybe LoadBalancerTypeEnum)
  , _lbDNSName               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbState' - The state of the load balancer.
--
-- * 'lbSecurityGroups' - The IDs of the security groups for the load balancer.
--
-- * 'lbLoadBalancerName' - The name of the load balancer.
--
-- * 'lbCreatedTime' - The date and time the load balancer was created.
--
-- * 'lbVPCId' - The ID of the VPC for the load balancer.
--
-- * 'lbCanonicalHostedZoneId' - The ID of the Amazon Route 53 hosted zone associated with the load balancer.
--
-- * 'lbAvailabilityZones' - The Availability Zones for the load balancer.
--
-- * 'lbLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'lbIPAddressType' - The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
--
-- * 'lbScheme' - The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the Internet. The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can only route requests from clients with access to the VPC for the load balancer.
--
-- * 'lbType' - The type of load balancer.
--
-- * 'lbDNSName' - The public DNS name of the load balancer.
loadBalancer
    :: LoadBalancer
loadBalancer =
  LoadBalancer'
    { _lbState = Nothing
    , _lbSecurityGroups = Nothing
    , _lbLoadBalancerName = Nothing
    , _lbCreatedTime = Nothing
    , _lbVPCId = Nothing
    , _lbCanonicalHostedZoneId = Nothing
    , _lbAvailabilityZones = Nothing
    , _lbLoadBalancerARN = Nothing
    , _lbIPAddressType = Nothing
    , _lbScheme = Nothing
    , _lbType = Nothing
    , _lbDNSName = Nothing
    }


-- | The state of the load balancer.
lbState :: Lens' LoadBalancer (Maybe LoadBalancerState)
lbState = lens _lbState (\ s a -> s{_lbState = a})

-- | The IDs of the security groups for the load balancer.
lbSecurityGroups :: Lens' LoadBalancer [Text]
lbSecurityGroups = lens _lbSecurityGroups (\ s a -> s{_lbSecurityGroups = a}) . _Default . _Coerce

-- | The name of the load balancer.
lbLoadBalancerName :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerName = lens _lbLoadBalancerName (\ s a -> s{_lbLoadBalancerName = a})

-- | The date and time the load balancer was created.
lbCreatedTime :: Lens' LoadBalancer (Maybe UTCTime)
lbCreatedTime = lens _lbCreatedTime (\ s a -> s{_lbCreatedTime = a}) . mapping _Time

-- | The ID of the VPC for the load balancer.
lbVPCId :: Lens' LoadBalancer (Maybe Text)
lbVPCId = lens _lbVPCId (\ s a -> s{_lbVPCId = a})

-- | The ID of the Amazon Route 53 hosted zone associated with the load balancer.
lbCanonicalHostedZoneId :: Lens' LoadBalancer (Maybe Text)
lbCanonicalHostedZoneId = lens _lbCanonicalHostedZoneId (\ s a -> s{_lbCanonicalHostedZoneId = a})

-- | The Availability Zones for the load balancer.
lbAvailabilityZones :: Lens' LoadBalancer [AvailabilityZone]
lbAvailabilityZones = lens _lbAvailabilityZones (\ s a -> s{_lbAvailabilityZones = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
lbLoadBalancerARN :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerARN = lens _lbLoadBalancerARN (\ s a -> s{_lbLoadBalancerARN = a})

-- | The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
lbIPAddressType :: Lens' LoadBalancer (Maybe IPAddressType)
lbIPAddressType = lens _lbIPAddressType (\ s a -> s{_lbIPAddressType = a})

-- | The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the Internet. The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can only route requests from clients with access to the VPC for the load balancer.
lbScheme :: Lens' LoadBalancer (Maybe LoadBalancerSchemeEnum)
lbScheme = lens _lbScheme (\ s a -> s{_lbScheme = a})

-- | The type of load balancer.
lbType :: Lens' LoadBalancer (Maybe LoadBalancerTypeEnum)
lbType = lens _lbType (\ s a -> s{_lbType = a})

-- | The public DNS name of the load balancer.
lbDNSName :: Lens' LoadBalancer (Maybe Text)
lbDNSName = lens _lbDNSName (\ s a -> s{_lbDNSName = a})

instance FromXML LoadBalancer where
        parseXML x
          = LoadBalancer' <$>
              (x .@? "State") <*>
                (x .@? "SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerName")
                <*> (x .@? "CreatedTime")
                <*> (x .@? "VpcId")
                <*> (x .@? "CanonicalHostedZoneId")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerArn")
                <*> (x .@? "IpAddressType")
                <*> (x .@? "Scheme")
                <*> (x .@? "Type")
                <*> (x .@? "DNSName")

instance Hashable LoadBalancer where

instance NFData LoadBalancer where

-- | Information about a static IP address for a load balancer.
--
--
--
-- /See:/ 'loadBalancerAddress' smart constructor.
data LoadBalancerAddress = LoadBalancerAddress'
  { _lbaIPAddress    :: !(Maybe Text)
  , _lbaAllocationId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbaIPAddress' - The static IP address.
--
-- * 'lbaAllocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address.
loadBalancerAddress
    :: LoadBalancerAddress
loadBalancerAddress =
  LoadBalancerAddress' {_lbaIPAddress = Nothing, _lbaAllocationId = Nothing}


-- | The static IP address.
lbaIPAddress :: Lens' LoadBalancerAddress (Maybe Text)
lbaIPAddress = lens _lbaIPAddress (\ s a -> s{_lbaIPAddress = a})

-- | [Network Load Balancers] The allocation ID of the Elastic IP address.
lbaAllocationId :: Lens' LoadBalancerAddress (Maybe Text)
lbaAllocationId = lens _lbaAllocationId (\ s a -> s{_lbaAllocationId = a})

instance FromXML LoadBalancerAddress where
        parseXML x
          = LoadBalancerAddress' <$>
              (x .@? "IpAddress") <*> (x .@? "AllocationId")

instance Hashable LoadBalancerAddress where

instance NFData LoadBalancerAddress where

-- | Information about a load balancer attribute.
--
--
--
-- /See:/ 'loadBalancerAttribute' smart constructor.
data LoadBalancerAttribute = LoadBalancerAttribute'
  { _lbaValue :: !(Maybe Text)
  , _lbaKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbaValue' - The value of the attribute.
--
-- * 'lbaKey' - The name of the attribute.     * @access_logs.s3.enabled@ - [Application Load Balancers] Indicates whether access logs stored in Amazon S3 are enabled. The value is @true@ or @false@ .     * @access_logs.s3.bucket@ - [Application Load Balancers] The name of the S3 bucket for the access logs. This attribute is required if access logs in Amazon S3 are enabled. The bucket must exist in the same region as the load balancer and have a bucket policy that grants Elastic Load Balancing permission to write to the bucket.     * @access_logs.s3.prefix@ - [Application Load Balancers] The prefix for the location in the S3 bucket. If you don't specify a prefix, the access logs are stored in the root of the bucket.     * @deletion_protection.enabled@ - Indicates whether deletion protection is enabled. The value is @true@ or @false@ .     * @idle_timeout.timeout_seconds@ - [Application Load Balancers] The idle timeout value, in seconds. The valid range is 1-4000. The default is 60 seconds.     * @load_balancing.cross_zone.enabled@ - [Network Load Balancers] Indicates whether cross-zone load balancing is enabled. The value is @true@ or @false@ . The default is @false@ .     * @routing.http2.enabled@ - [Application Load Balancers] Indicates whether HTTP/2 is enabled. The value is @true@ or @false@ . The default is @true@ .
loadBalancerAttribute
    :: LoadBalancerAttribute
loadBalancerAttribute =
  LoadBalancerAttribute' {_lbaValue = Nothing, _lbaKey = Nothing}


-- | The value of the attribute.
lbaValue :: Lens' LoadBalancerAttribute (Maybe Text)
lbaValue = lens _lbaValue (\ s a -> s{_lbaValue = a})

-- | The name of the attribute.     * @access_logs.s3.enabled@ - [Application Load Balancers] Indicates whether access logs stored in Amazon S3 are enabled. The value is @true@ or @false@ .     * @access_logs.s3.bucket@ - [Application Load Balancers] The name of the S3 bucket for the access logs. This attribute is required if access logs in Amazon S3 are enabled. The bucket must exist in the same region as the load balancer and have a bucket policy that grants Elastic Load Balancing permission to write to the bucket.     * @access_logs.s3.prefix@ - [Application Load Balancers] The prefix for the location in the S3 bucket. If you don't specify a prefix, the access logs are stored in the root of the bucket.     * @deletion_protection.enabled@ - Indicates whether deletion protection is enabled. The value is @true@ or @false@ .     * @idle_timeout.timeout_seconds@ - [Application Load Balancers] The idle timeout value, in seconds. The valid range is 1-4000. The default is 60 seconds.     * @load_balancing.cross_zone.enabled@ - [Network Load Balancers] Indicates whether cross-zone load balancing is enabled. The value is @true@ or @false@ . The default is @false@ .     * @routing.http2.enabled@ - [Application Load Balancers] Indicates whether HTTP/2 is enabled. The value is @true@ or @false@ . The default is @true@ .
lbaKey :: Lens' LoadBalancerAttribute (Maybe Text)
lbaKey = lens _lbaKey (\ s a -> s{_lbaKey = a})

instance FromXML LoadBalancerAttribute where
        parseXML x
          = LoadBalancerAttribute' <$>
              (x .@? "Value") <*> (x .@? "Key")

instance Hashable LoadBalancerAttribute where

instance NFData LoadBalancerAttribute where

instance ToQuery LoadBalancerAttribute where
        toQuery LoadBalancerAttribute'{..}
          = mconcat ["Value" =: _lbaValue, "Key" =: _lbaKey]

-- | Information about the state of the load balancer.
--
--
--
-- /See:/ 'loadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { _lbsReason :: !(Maybe Text)
  , _lbsCode   :: !(Maybe LoadBalancerStateEnum)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbsReason' - A description of the state.
--
-- * 'lbsCode' - The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
loadBalancerState
    :: LoadBalancerState
loadBalancerState =
  LoadBalancerState' {_lbsReason = Nothing, _lbsCode = Nothing}


-- | A description of the state.
lbsReason :: Lens' LoadBalancerState (Maybe Text)
lbsReason = lens _lbsReason (\ s a -> s{_lbsReason = a})

-- | The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
lbsCode :: Lens' LoadBalancerState (Maybe LoadBalancerStateEnum)
lbsCode = lens _lbsCode (\ s a -> s{_lbsCode = a})

instance FromXML LoadBalancerState where
        parseXML x
          = LoadBalancerState' <$>
              (x .@? "Reason") <*> (x .@? "Code")

instance Hashable LoadBalancerState where

instance NFData LoadBalancerState where

-- | Information to use when checking for a successful response from a target.
--
--
--
-- /See:/ 'matcher' smart constructor.
newtype Matcher = Matcher'
  { _mHTTPCode :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Matcher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mHTTPCode' - The HTTP codes. For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299"). For Network Load Balancers, this is 200 to 399.
matcher
    :: Text -- ^ 'mHTTPCode'
    -> Matcher
matcher pHTTPCode_ = Matcher' {_mHTTPCode = pHTTPCode_}


-- | The HTTP codes. For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299"). For Network Load Balancers, this is 200 to 399.
mHTTPCode :: Lens' Matcher Text
mHTTPCode = lens _mHTTPCode (\ s a -> s{_mHTTPCode = a})

instance FromXML Matcher where
        parseXML x = Matcher' <$> (x .@ "HttpCode")

instance Hashable Matcher where

instance NFData Matcher where

instance ToQuery Matcher where
        toQuery Matcher'{..}
          = mconcat ["HttpCode" =: _mHTTPCode]

-- | Information about a rule.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rPriority   :: !(Maybe Text)
  , _rActions    :: !(Maybe [Action])
  , _rConditions :: !(Maybe [RuleCondition])
  , _rRuleARN    :: !(Maybe Text)
  , _rIsDefault  :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rPriority' - The priority.
--
-- * 'rActions' - The actions.
--
-- * 'rConditions' - The conditions.
--
-- * 'rRuleARN' - The Amazon Resource Name (ARN) of the rule.
--
-- * 'rIsDefault' - Indicates whether this is the default rule.
rule
    :: Rule
rule =
  Rule'
    { _rPriority = Nothing
    , _rActions = Nothing
    , _rConditions = Nothing
    , _rRuleARN = Nothing
    , _rIsDefault = Nothing
    }


-- | The priority.
rPriority :: Lens' Rule (Maybe Text)
rPriority = lens _rPriority (\ s a -> s{_rPriority = a})

-- | The actions.
rActions :: Lens' Rule [Action]
rActions = lens _rActions (\ s a -> s{_rActions = a}) . _Default . _Coerce

-- | The conditions.
rConditions :: Lens' Rule [RuleCondition]
rConditions = lens _rConditions (\ s a -> s{_rConditions = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the rule.
rRuleARN :: Lens' Rule (Maybe Text)
rRuleARN = lens _rRuleARN (\ s a -> s{_rRuleARN = a})

-- | Indicates whether this is the default rule.
rIsDefault :: Lens' Rule (Maybe Bool)
rIsDefault = lens _rIsDefault (\ s a -> s{_rIsDefault = a})

instance FromXML Rule where
        parseXML x
          = Rule' <$>
              (x .@? "Priority") <*>
                (x .@? "Actions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Conditions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "RuleArn")
                <*> (x .@? "IsDefault")

instance Hashable Rule where

instance NFData Rule where

-- | Information about a condition for a rule.
--
--
--
-- /See:/ 'ruleCondition' smart constructor.
data RuleCondition = RuleCondition'
  { _rcField  :: !(Maybe Text)
  , _rcValues :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RuleCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcField' - The name of the field. The possible values are @host-header@ and @path-pattern@ .
--
-- * 'rcValues' - The condition value. If the field name is @host-header@ , you can specify a single host name (for example, my.example.com). A host name is case insensitive, can be up to 128 characters in length, and can contain any of the following characters. Note that you can include up to three wildcard characters.     * A-Z, a-z, 0-9     * - .     * * (matches 0 or more characters)     * ? (matches exactly 1 character) If the field name is @path-pattern@ , you can specify a single path pattern (for example, /img/*). A path pattern is case sensitive, can be up to 128 characters in length, and can contain any of the following characters. Note that you can include up to three wildcard characters.     * A-Z, a-z, 0-9     * _ - . $ / ~ " ' @ : +     * & (using &amp;)     * * (matches 0 or more characters)     * ? (matches exactly 1 character)
ruleCondition
    :: RuleCondition
ruleCondition = RuleCondition' {_rcField = Nothing, _rcValues = Nothing}


-- | The name of the field. The possible values are @host-header@ and @path-pattern@ .
rcField :: Lens' RuleCondition (Maybe Text)
rcField = lens _rcField (\ s a -> s{_rcField = a})

-- | The condition value. If the field name is @host-header@ , you can specify a single host name (for example, my.example.com). A host name is case insensitive, can be up to 128 characters in length, and can contain any of the following characters. Note that you can include up to three wildcard characters.     * A-Z, a-z, 0-9     * - .     * * (matches 0 or more characters)     * ? (matches exactly 1 character) If the field name is @path-pattern@ , you can specify a single path pattern (for example, /img/*). A path pattern is case sensitive, can be up to 128 characters in length, and can contain any of the following characters. Note that you can include up to three wildcard characters.     * A-Z, a-z, 0-9     * _ - . $ / ~ " ' @ : +     * & (using &amp;)     * * (matches 0 or more characters)     * ? (matches exactly 1 character)
rcValues :: Lens' RuleCondition [Text]
rcValues = lens _rcValues (\ s a -> s{_rcValues = a}) . _Default . _Coerce

instance FromXML RuleCondition where
        parseXML x
          = RuleCondition' <$>
              (x .@? "Field") <*>
                (x .@? "Values" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable RuleCondition where

instance NFData RuleCondition where

instance ToQuery RuleCondition where
        toQuery RuleCondition'{..}
          = mconcat
              ["Field" =: _rcField,
               "Values" =:
                 toQuery (toQueryList "member" <$> _rcValues)]

-- | Information about the priorities for the rules for a listener.
--
--
--
-- /See:/ 'rulePriorityPair' smart constructor.
data RulePriorityPair = RulePriorityPair'
  { _rppPriority :: !(Maybe Nat)
  , _rppRuleARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RulePriorityPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rppPriority' - The rule priority.
--
-- * 'rppRuleARN' - The Amazon Resource Name (ARN) of the rule.
rulePriorityPair
    :: RulePriorityPair
rulePriorityPair =
  RulePriorityPair' {_rppPriority = Nothing, _rppRuleARN = Nothing}


-- | The rule priority.
rppPriority :: Lens' RulePriorityPair (Maybe Natural)
rppPriority = lens _rppPriority (\ s a -> s{_rppPriority = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the rule.
rppRuleARN :: Lens' RulePriorityPair (Maybe Text)
rppRuleARN = lens _rppRuleARN (\ s a -> s{_rppRuleARN = a})

instance Hashable RulePriorityPair where

instance NFData RulePriorityPair where

instance ToQuery RulePriorityPair where
        toQuery RulePriorityPair'{..}
          = mconcat
              ["Priority" =: _rppPriority,
               "RuleArn" =: _rppRuleARN]

-- | Information about a policy used for SSL negotiation.
--
--
--
-- /See:/ 'sslPolicy' smart constructor.
data SSLPolicy = SSLPolicy'
  { _spCiphers      :: !(Maybe [Cipher])
  , _spName         :: !(Maybe Text)
  , _spSSLProtocols :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSLPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spCiphers' - The ciphers.
--
-- * 'spName' - The name of the policy.
--
-- * 'spSSLProtocols' - The protocols.
sslPolicy
    :: SSLPolicy
sslPolicy =
  SSLPolicy'
    {_spCiphers = Nothing, _spName = Nothing, _spSSLProtocols = Nothing}


-- | The ciphers.
spCiphers :: Lens' SSLPolicy [Cipher]
spCiphers = lens _spCiphers (\ s a -> s{_spCiphers = a}) . _Default . _Coerce

-- | The name of the policy.
spName :: Lens' SSLPolicy (Maybe Text)
spName = lens _spName (\ s a -> s{_spName = a})

-- | The protocols.
spSSLProtocols :: Lens' SSLPolicy [Text]
spSSLProtocols = lens _spSSLProtocols (\ s a -> s{_spSSLProtocols = a}) . _Default . _Coerce

instance FromXML SSLPolicy where
        parseXML x
          = SSLPolicy' <$>
              (x .@? "Ciphers" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "Name")
                <*>
                (x .@? "SslProtocols" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable SSLPolicy where

instance NFData SSLPolicy where

-- | Information about a subnet mapping.
--
--
--
-- /See:/ 'subnetMapping' smart constructor.
data SubnetMapping = SubnetMapping'
  { _smAllocationId :: !(Maybe Text)
  , _smSubnetId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubnetMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smAllocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address.
--
-- * 'smSubnetId' - The ID of the subnet.
subnetMapping
    :: SubnetMapping
subnetMapping =
  SubnetMapping' {_smAllocationId = Nothing, _smSubnetId = Nothing}


-- | [Network Load Balancers] The allocation ID of the Elastic IP address.
smAllocationId :: Lens' SubnetMapping (Maybe Text)
smAllocationId = lens _smAllocationId (\ s a -> s{_smAllocationId = a})

-- | The ID of the subnet.
smSubnetId :: Lens' SubnetMapping (Maybe Text)
smSubnetId = lens _smSubnetId (\ s a -> s{_smSubnetId = a})

instance Hashable SubnetMapping where

instance NFData SubnetMapping where

instance ToQuery SubnetMapping where
        toQuery SubnetMapping'{..}
          = mconcat
              ["AllocationId" =: _smAllocationId,
               "SubnetId" =: _smSubnetId]

-- | Information about a tag.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value of the tag.
--
-- * 'tagKey' - The key of the tag.
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | The value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@ "Key")

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | The tags associated with a resource.
--
--
--
-- /See:/ 'tagDescription' smart constructor.
data TagDescription = TagDescription'
  { _tdResourceARN :: !(Maybe Text)
  , _tdTags        :: !(Maybe (List1 Tag))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdResourceARN' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'tdTags' - Information about the tags.
tagDescription
    :: TagDescription
tagDescription = TagDescription' {_tdResourceARN = Nothing, _tdTags = Nothing}


-- | The Amazon Resource Name (ARN) of the resource.
tdResourceARN :: Lens' TagDescription (Maybe Text)
tdResourceARN = lens _tdResourceARN (\ s a -> s{_tdResourceARN = a})

-- | Information about the tags.
tdTags :: Lens' TagDescription (Maybe (NonEmpty Tag))
tdTags = lens _tdTags (\ s a -> s{_tdTags = a}) . mapping _List1

instance FromXML TagDescription where
        parseXML x
          = TagDescription' <$>
              (x .@? "ResourceArn") <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList1 "member"))

instance Hashable TagDescription where

instance NFData TagDescription where

-- | Information about a target.
--
--
--
-- /See:/ 'targetDescription' smart constructor.
data TargetDescription = TargetDescription'
  { _tdAvailabilityZone :: !(Maybe Text)
  , _tdPort             :: !(Maybe Nat)
  , _tdId               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdAvailabilityZone' - An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer. This parameter is not supported if the target type of the target group is @instance@ . If the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required. With an Application Load Balancer, if the IP address is outside the VPC for the target group, the only supported value is @all@ .
--
-- * 'tdPort' - The port on which the target is listening.
--
-- * 'tdId' - The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address.
targetDescription
    :: Text -- ^ 'tdId'
    -> TargetDescription
targetDescription pId_ =
  TargetDescription'
    {_tdAvailabilityZone = Nothing, _tdPort = Nothing, _tdId = pId_}


-- | An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer. This parameter is not supported if the target type of the target group is @instance@ . If the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required. With an Application Load Balancer, if the IP address is outside the VPC for the target group, the only supported value is @all@ .
tdAvailabilityZone :: Lens' TargetDescription (Maybe Text)
tdAvailabilityZone = lens _tdAvailabilityZone (\ s a -> s{_tdAvailabilityZone = a})

-- | The port on which the target is listening.
tdPort :: Lens' TargetDescription (Maybe Natural)
tdPort = lens _tdPort (\ s a -> s{_tdPort = a}) . mapping _Nat

-- | The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address.
tdId :: Lens' TargetDescription Text
tdId = lens _tdId (\ s a -> s{_tdId = a})

instance FromXML TargetDescription where
        parseXML x
          = TargetDescription' <$>
              (x .@? "AvailabilityZone") <*> (x .@? "Port") <*>
                (x .@ "Id")

instance Hashable TargetDescription where

instance NFData TargetDescription where

instance ToQuery TargetDescription where
        toQuery TargetDescription'{..}
          = mconcat
              ["AvailabilityZone" =: _tdAvailabilityZone,
               "Port" =: _tdPort, "Id" =: _tdId]

-- | Information about a target group.
--
--
--
-- /See:/ 'targetGroup' smart constructor.
data TargetGroup = TargetGroup'
  { _tgMatcher                    :: !(Maybe Matcher)
  , _tgHealthCheckPath            :: !(Maybe Text)
  , _tgUnhealthyThresholdCount    :: !(Maybe Nat)
  , _tgVPCId                      :: !(Maybe Text)
  , _tgTargetGroupARN             :: !(Maybe Text)
  , _tgProtocol                   :: !(Maybe ProtocolEnum)
  , _tgHealthCheckIntervalSeconds :: !(Maybe Nat)
  , _tgTargetType                 :: !(Maybe TargetTypeEnum)
  , _tgHealthyThresholdCount      :: !(Maybe Nat)
  , _tgHealthCheckProtocol        :: !(Maybe ProtocolEnum)
  , _tgLoadBalancerARNs           :: !(Maybe [Text])
  , _tgHealthCheckTimeoutSeconds  :: !(Maybe Nat)
  , _tgHealthCheckPort            :: !(Maybe Text)
  , _tgTargetGroupName            :: !(Maybe Text)
  , _tgPort                       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgMatcher' - The HTTP codes to use when checking for a successful response from a target.
--
-- * 'tgHealthCheckPath' - The destination for the health check request.
--
-- * 'tgUnhealthyThresholdCount' - The number of consecutive health check failures required before considering the target unhealthy.
--
-- * 'tgVPCId' - The ID of the VPC for the targets.
--
-- * 'tgTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
--
-- * 'tgProtocol' - The protocol to use for routing traffic to the targets.
--
-- * 'tgHealthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target.
--
-- * 'tgTargetType' - The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (targets are specified by instance ID) or @ip@ (targets are specified by IP address).
--
-- * 'tgHealthyThresholdCount' - The number of consecutive health checks successes required before considering an unhealthy target healthy.
--
-- * 'tgHealthCheckProtocol' - The protocol to use to connect with the target.
--
-- * 'tgLoadBalancerARNs' - The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
--
-- * 'tgHealthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response means a failed health check.
--
-- * 'tgHealthCheckPort' - The port to use to connect with the target.
--
-- * 'tgTargetGroupName' - The name of the target group.
--
-- * 'tgPort' - The port on which the targets are listening.
targetGroup
    :: TargetGroup
targetGroup =
  TargetGroup'
    { _tgMatcher = Nothing
    , _tgHealthCheckPath = Nothing
    , _tgUnhealthyThresholdCount = Nothing
    , _tgVPCId = Nothing
    , _tgTargetGroupARN = Nothing
    , _tgProtocol = Nothing
    , _tgHealthCheckIntervalSeconds = Nothing
    , _tgTargetType = Nothing
    , _tgHealthyThresholdCount = Nothing
    , _tgHealthCheckProtocol = Nothing
    , _tgLoadBalancerARNs = Nothing
    , _tgHealthCheckTimeoutSeconds = Nothing
    , _tgHealthCheckPort = Nothing
    , _tgTargetGroupName = Nothing
    , _tgPort = Nothing
    }


-- | The HTTP codes to use when checking for a successful response from a target.
tgMatcher :: Lens' TargetGroup (Maybe Matcher)
tgMatcher = lens _tgMatcher (\ s a -> s{_tgMatcher = a})

-- | The destination for the health check request.
tgHealthCheckPath :: Lens' TargetGroup (Maybe Text)
tgHealthCheckPath = lens _tgHealthCheckPath (\ s a -> s{_tgHealthCheckPath = a})

-- | The number of consecutive health check failures required before considering the target unhealthy.
tgUnhealthyThresholdCount :: Lens' TargetGroup (Maybe Natural)
tgUnhealthyThresholdCount = lens _tgUnhealthyThresholdCount (\ s a -> s{_tgUnhealthyThresholdCount = a}) . mapping _Nat

-- | The ID of the VPC for the targets.
tgVPCId :: Lens' TargetGroup (Maybe Text)
tgVPCId = lens _tgVPCId (\ s a -> s{_tgVPCId = a})

-- | The Amazon Resource Name (ARN) of the target group.
tgTargetGroupARN :: Lens' TargetGroup (Maybe Text)
tgTargetGroupARN = lens _tgTargetGroupARN (\ s a -> s{_tgTargetGroupARN = a})

-- | The protocol to use for routing traffic to the targets.
tgProtocol :: Lens' TargetGroup (Maybe ProtocolEnum)
tgProtocol = lens _tgProtocol (\ s a -> s{_tgProtocol = a})

-- | The approximate amount of time, in seconds, between health checks of an individual target.
tgHealthCheckIntervalSeconds :: Lens' TargetGroup (Maybe Natural)
tgHealthCheckIntervalSeconds = lens _tgHealthCheckIntervalSeconds (\ s a -> s{_tgHealthCheckIntervalSeconds = a}) . mapping _Nat

-- | The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (targets are specified by instance ID) or @ip@ (targets are specified by IP address).
tgTargetType :: Lens' TargetGroup (Maybe TargetTypeEnum)
tgTargetType = lens _tgTargetType (\ s a -> s{_tgTargetType = a})

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
tgHealthyThresholdCount :: Lens' TargetGroup (Maybe Natural)
tgHealthyThresholdCount = lens _tgHealthyThresholdCount (\ s a -> s{_tgHealthyThresholdCount = a}) . mapping _Nat

-- | The protocol to use to connect with the target.
tgHealthCheckProtocol :: Lens' TargetGroup (Maybe ProtocolEnum)
tgHealthCheckProtocol = lens _tgHealthCheckProtocol (\ s a -> s{_tgHealthCheckProtocol = a})

-- | The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
tgLoadBalancerARNs :: Lens' TargetGroup [Text]
tgLoadBalancerARNs = lens _tgLoadBalancerARNs (\ s a -> s{_tgLoadBalancerARNs = a}) . _Default . _Coerce

-- | The amount of time, in seconds, during which no response means a failed health check.
tgHealthCheckTimeoutSeconds :: Lens' TargetGroup (Maybe Natural)
tgHealthCheckTimeoutSeconds = lens _tgHealthCheckTimeoutSeconds (\ s a -> s{_tgHealthCheckTimeoutSeconds = a}) . mapping _Nat

-- | The port to use to connect with the target.
tgHealthCheckPort :: Lens' TargetGroup (Maybe Text)
tgHealthCheckPort = lens _tgHealthCheckPort (\ s a -> s{_tgHealthCheckPort = a})

-- | The name of the target group.
tgTargetGroupName :: Lens' TargetGroup (Maybe Text)
tgTargetGroupName = lens _tgTargetGroupName (\ s a -> s{_tgTargetGroupName = a})

-- | The port on which the targets are listening.
tgPort :: Lens' TargetGroup (Maybe Natural)
tgPort = lens _tgPort (\ s a -> s{_tgPort = a}) . mapping _Nat

instance FromXML TargetGroup where
        parseXML x
          = TargetGroup' <$>
              (x .@? "Matcher") <*> (x .@? "HealthCheckPath") <*>
                (x .@? "UnhealthyThresholdCount")
                <*> (x .@? "VpcId")
                <*> (x .@? "TargetGroupArn")
                <*> (x .@? "Protocol")
                <*> (x .@? "HealthCheckIntervalSeconds")
                <*> (x .@? "TargetType")
                <*> (x .@? "HealthyThresholdCount")
                <*> (x .@? "HealthCheckProtocol")
                <*>
                (x .@? "LoadBalancerArns" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "HealthCheckTimeoutSeconds")
                <*> (x .@? "HealthCheckPort")
                <*> (x .@? "TargetGroupName")
                <*> (x .@? "Port")

instance Hashable TargetGroup where

instance NFData TargetGroup where

-- | Information about a target group attribute.
--
--
--
-- /See:/ 'targetGroupAttribute' smart constructor.
data TargetGroupAttribute = TargetGroupAttribute'
  { _tgaValue :: !(Maybe Text)
  , _tgaKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetGroupAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgaValue' - The value of the attribute.
--
-- * 'tgaKey' - The name of the attribute.     * @deregistration_delay.timeout_seconds@ - The amount time for Elastic Load Balancing to wait before changing the state of a deregistering target from @draining@ to @unused@ . The range is 0-3600 seconds. The default value is 300 seconds.     * @proxy_protocol_v2.enabled@ - [Network Load Balancers] Indicates whether Proxy Protocol version 2 is enabled.     * @stickiness.enabled@ - [Application Load Balancers] Indicates whether sticky sessions are enabled. The value is @true@ or @false@ .     * @stickiness.type@ - [Application Load Balancers] The type of sticky sessions. The possible value is @lb_cookie@ .     * @stickiness.lb_cookie.duration_seconds@ - [Application Load Balancers] The time period, in seconds, during which requests from a client should be routed to the same target. After this time period expires, the load balancer-generated cookie is considered stale. The range is 1 second to 1 week (604800 seconds). The default value is 1 day (86400 seconds).
targetGroupAttribute
    :: TargetGroupAttribute
targetGroupAttribute =
  TargetGroupAttribute' {_tgaValue = Nothing, _tgaKey = Nothing}


-- | The value of the attribute.
tgaValue :: Lens' TargetGroupAttribute (Maybe Text)
tgaValue = lens _tgaValue (\ s a -> s{_tgaValue = a})

-- | The name of the attribute.     * @deregistration_delay.timeout_seconds@ - The amount time for Elastic Load Balancing to wait before changing the state of a deregistering target from @draining@ to @unused@ . The range is 0-3600 seconds. The default value is 300 seconds.     * @proxy_protocol_v2.enabled@ - [Network Load Balancers] Indicates whether Proxy Protocol version 2 is enabled.     * @stickiness.enabled@ - [Application Load Balancers] Indicates whether sticky sessions are enabled. The value is @true@ or @false@ .     * @stickiness.type@ - [Application Load Balancers] The type of sticky sessions. The possible value is @lb_cookie@ .     * @stickiness.lb_cookie.duration_seconds@ - [Application Load Balancers] The time period, in seconds, during which requests from a client should be routed to the same target. After this time period expires, the load balancer-generated cookie is considered stale. The range is 1 second to 1 week (604800 seconds). The default value is 1 day (86400 seconds).
tgaKey :: Lens' TargetGroupAttribute (Maybe Text)
tgaKey = lens _tgaKey (\ s a -> s{_tgaKey = a})

instance FromXML TargetGroupAttribute where
        parseXML x
          = TargetGroupAttribute' <$>
              (x .@? "Value") <*> (x .@? "Key")

instance Hashable TargetGroupAttribute where

instance NFData TargetGroupAttribute where

instance ToQuery TargetGroupAttribute where
        toQuery TargetGroupAttribute'{..}
          = mconcat ["Value" =: _tgaValue, "Key" =: _tgaKey]

-- | Information about the current health of a target.
--
--
--
-- /See:/ 'targetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { _thState       :: !(Maybe TargetHealthStateEnum)
  , _thReason      :: !(Maybe TargetHealthReasonEnum)
  , _thDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'thState' - The state of the target.
--
-- * 'thReason' - The reason code. If the target state is @healthy@ , a reason code is not provided. If the target state is @initial@ , the reason code can be one of the following values:     * @Elb.RegistrationInProgress@ - The target is in the process of being registered with the load balancer.     * @Elb.InitialHealthChecking@ - The load balancer is still sending the target the minimum number of health checks required to determine its health status. If the target state is @unhealthy@ , the reason code can be one of the following values:     * @Target.ResponseCodeMismatch@ - The health checks did not return an expected HTTP code.     * @Target.Timeout@ - The health check requests timed out.     * @Target.FailedHealthChecks@ - The health checks failed because the connection to the target timed out, the target response was malformed, or the target failed the health check for an unknown reason.     * @Elb.InternalError@ - The health checks failed due to an internal error. If the target state is @unused@ , the reason code can be one of the following values:     * @Target.NotRegistered@ - The target is not registered with the target group.     * @Target.NotInUse@ - The target group is not used by any load balancer or the target is in an Availability Zone that is not enabled for its load balancer.     * @Target.IpUnusable@ - The target IP address is reserved for use by a load balancer.     * @Target.InvalidState@ - The target is in the stopped or terminated state. If the target state is @draining@ , the reason code can be the following value:     * @Target.DeregistrationInProgress@ - The target is in the process of being deregistered and the deregistration delay period has not expired.
--
-- * 'thDescription' - A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
targetHealth
    :: TargetHealth
targetHealth =
  TargetHealth'
    {_thState = Nothing, _thReason = Nothing, _thDescription = Nothing}


-- | The state of the target.
thState :: Lens' TargetHealth (Maybe TargetHealthStateEnum)
thState = lens _thState (\ s a -> s{_thState = a})

-- | The reason code. If the target state is @healthy@ , a reason code is not provided. If the target state is @initial@ , the reason code can be one of the following values:     * @Elb.RegistrationInProgress@ - The target is in the process of being registered with the load balancer.     * @Elb.InitialHealthChecking@ - The load balancer is still sending the target the minimum number of health checks required to determine its health status. If the target state is @unhealthy@ , the reason code can be one of the following values:     * @Target.ResponseCodeMismatch@ - The health checks did not return an expected HTTP code.     * @Target.Timeout@ - The health check requests timed out.     * @Target.FailedHealthChecks@ - The health checks failed because the connection to the target timed out, the target response was malformed, or the target failed the health check for an unknown reason.     * @Elb.InternalError@ - The health checks failed due to an internal error. If the target state is @unused@ , the reason code can be one of the following values:     * @Target.NotRegistered@ - The target is not registered with the target group.     * @Target.NotInUse@ - The target group is not used by any load balancer or the target is in an Availability Zone that is not enabled for its load balancer.     * @Target.IpUnusable@ - The target IP address is reserved for use by a load balancer.     * @Target.InvalidState@ - The target is in the stopped or terminated state. If the target state is @draining@ , the reason code can be the following value:     * @Target.DeregistrationInProgress@ - The target is in the process of being deregistered and the deregistration delay period has not expired.
thReason :: Lens' TargetHealth (Maybe TargetHealthReasonEnum)
thReason = lens _thReason (\ s a -> s{_thReason = a})

-- | A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
thDescription :: Lens' TargetHealth (Maybe Text)
thDescription = lens _thDescription (\ s a -> s{_thDescription = a})

instance FromXML TargetHealth where
        parseXML x
          = TargetHealth' <$>
              (x .@? "State") <*> (x .@? "Reason") <*>
                (x .@? "Description")

instance Hashable TargetHealth where

instance NFData TargetHealth where

-- | Information about the health of a target.
--
--
--
-- /See:/ 'targetHealthDescription' smart constructor.
data TargetHealthDescription = TargetHealthDescription'
  { _thdTargetHealth    :: !(Maybe TargetHealth)
  , _thdHealthCheckPort :: !(Maybe Text)
  , _thdTarget          :: !(Maybe TargetDescription)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetHealthDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'thdTargetHealth' - The health information for the target.
--
-- * 'thdHealthCheckPort' - The port to use to connect with the target.
--
-- * 'thdTarget' - The description of the target.
targetHealthDescription
    :: TargetHealthDescription
targetHealthDescription =
  TargetHealthDescription'
    { _thdTargetHealth = Nothing
    , _thdHealthCheckPort = Nothing
    , _thdTarget = Nothing
    }


-- | The health information for the target.
thdTargetHealth :: Lens' TargetHealthDescription (Maybe TargetHealth)
thdTargetHealth = lens _thdTargetHealth (\ s a -> s{_thdTargetHealth = a})

-- | The port to use to connect with the target.
thdHealthCheckPort :: Lens' TargetHealthDescription (Maybe Text)
thdHealthCheckPort = lens _thdHealthCheckPort (\ s a -> s{_thdHealthCheckPort = a})

-- | The description of the target.
thdTarget :: Lens' TargetHealthDescription (Maybe TargetDescription)
thdTarget = lens _thdTarget (\ s a -> s{_thdTarget = a})

instance FromXML TargetHealthDescription where
        parseXML x
          = TargetHealthDescription' <$>
              (x .@? "TargetHealth") <*> (x .@? "HealthCheckPort")
                <*> (x .@? "Target")

instance Hashable TargetHealthDescription where

instance NFData TargetHealthDescription where
