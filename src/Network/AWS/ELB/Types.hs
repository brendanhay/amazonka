{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ELB.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ELB.Types where

import Data.ByteString      (ByteString)
import Data.Monoid
import Data.Text            (Text)
import Data.Time
import Network.AWS.Internal

-- | Currently supported version of the ELB service.
elb :: Service
elb = Service Regional version4 "elasticloadbalancing" "2012-06-01"

-- | XML namespace to annotate ELB elements with.
elbNS :: ByteString
elbNS = "http://elasticloadbalancing.amazonaws.com/doc/" <> svcVersion elb <> "/"

-- | Helper to define ELB namespaced XML elements.
elbElem :: ByteString -> NName ByteString
elbElem = mkNName elbNS

data ELBError = ELBError
    { elbeCode    :: !Text
    , elbeMessage :: !Text
    , elbeType    :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML ELBError where
    xmlPickler = withNS elbNS

data ErrorResponse = ErrorResponse
    { elberError     :: !ELBError
    , elberRequestId :: !Text
    } deriving (Eq, Show, Generic)

instance ToError ErrorResponse where
    toError = Err . show

instance IsXML ErrorResponse where
    xmlPickler = withNS elbNS

-- | The AppCookieStickinessPolicy data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_AppCookieStickinessPolicy.html>
data AppCookieStickinessPolicy = AppCookieStickinessPolicy
    { acspCookieName :: Maybe Text
      -- ^ The name of the application cookie used for stickiness.
    , acspPolicyName :: Maybe Text
      -- ^ The mnemonic name for the policy being created. The name must be
      -- unique within a set of policies for this load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML AppCookieStickinessPolicy where
    xmlPickler = withNS elbNS

instance IsQuery AppCookieStickinessPolicy

-- | The out for the ApplySecurityGroupsToLoadBalancer action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ApplySecurityGroupsToLoadBalancerResult.html>
data ApplySecurityGroupsToLoadBalancerResult = ApplySecurityGroupsToLoadBalancerResult
    { asgtlbrSecurityGroups :: Maybe Text
      -- ^ A list of security group IDs associated with your load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML ApplySecurityGroupsToLoadBalancerResult where
    xmlPickler = withNS elbNS

instance IsQuery ApplySecurityGroupsToLoadBalancerResult

-- | The output for the AttachLoadBalancerToSubnets action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_AttachLoadBalancerToSubnetsResult.html>
data AttachLoadBalancerToSubnetsResult = AttachLoadBalancerToSubnetsResult
    { albtsrSubnets :: Maybe Text
      -- ^ A list of subnet IDs added for the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML AttachLoadBalancerToSubnetsResult where
    xmlPickler = withNS elbNS

instance IsQuery AttachLoadBalancerToSubnetsResult

-- | This data type is used as a response element in the DescribeLoadBalancers
-- action to describe the configuration of the back-end server.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_BackendServerDescription.html>
data BackendServerDescription = BackendServerDescription
    { bsdInstancePort :: Maybe Integer
      -- ^ Provides the port on which the back-end server is listening.
    , bsdPolicyNames  :: Maybe Text
      -- ^ Provides a list of policy names enabled for the back-end server.
    } deriving (Eq, Show, Generic)

instance IsXML BackendServerDescription where
    xmlPickler = withNS elbNS

instance IsQuery BackendServerDescription

-- | The output for the ConfigureHealthCheck action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ConfigureHealthCheckResult.html>
data ConfigureHealthCheckResult = ConfigureHealthCheckResult
    { chcrHealthCheck :: Maybe HealthCheck
      -- ^ The updated healthcheck for the instances.
    } deriving (Eq, Show, Generic)

instance IsXML ConfigureHealthCheckResult where
    xmlPickler = withNS elbNS

instance IsQuery ConfigureHealthCheckResult

-- | The output for the CreateLoadBalancer action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_CreateLoadBalancerResult.html>
data CreateLoadBalancerResult = CreateLoadBalancerResult
    { clbrDNSName :: Maybe Text
      -- ^ The DNS name for the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML CreateLoadBalancerResult where
    xmlPickler = withNS elbNS

instance IsQuery CreateLoadBalancerResult

-- | The output for the DeregisterInstancesFromLoadBalancer action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DeregisterInstancesFromLoadBalancerResult.html>
data DeregisterInstancesFromLoadBalancerResult = DeregisterInstancesFromLoadBalancerResult
    { diflbrInstances :: Maybe Instance
      -- ^ An updated list of remaining instances registered with the load
      -- balancer.
    } deriving (Eq, Show, Generic)

instance IsXML DeregisterInstancesFromLoadBalancerResult where
    xmlPickler = withNS elbNS

instance IsQuery DeregisterInstancesFromLoadBalancerResult

-- | The output for the DescribeInstanceHealth action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeInstanceHealthResult.html>
data DescribeInstanceHealthResult = DescribeInstanceHealthResult
    { dihrInstanceStates :: Maybe InstanceState
      -- ^ A list containing health information for the specified instances.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeInstanceHealthResult where
    xmlPickler = withNS elbNS

instance IsQuery DescribeInstanceHealthResult

-- | The output for the DescribeLoadBalancerPolicies action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerPoliciesResult.html>
data DescribeLoadBalancerPoliciesResult = DescribeLoadBalancerPoliciesResult
    { dlbprPolicyDescriptions :: Maybe PolicyDescription
      -- ^ A list of policy description structures.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLoadBalancerPoliciesResult where
    xmlPickler = withNS elbNS

instance IsQuery DescribeLoadBalancerPoliciesResult

-- | The output for the DescribeLoadBalancerPolicyTypes action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancerPolicyTypesResult.html>
data DescribeLoadBalancerPolicyTypesResult = DescribeLoadBalancerPolicyTypesResult
    { dlbptrPolicyTypeDescriptions :: Maybe PolicyTypeDescription
      -- ^ List of policy type description structures of the specified
      -- policy type. If no policy type names are specified, returns the
      -- description of all the policy types defined by Elastic Load
      -- Balancing service.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLoadBalancerPolicyTypesResult where
    xmlPickler = withNS elbNS

instance IsQuery DescribeLoadBalancerPolicyTypesResult

-- | The output for the DescribeLoadBalancers action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DescribeLoadBalancersResult.html>
data DescribeLoadBalancersResult = DescribeLoadBalancersResult
    { dlbrLoadBalancerDescriptions :: Members LoadBalancerDescription
      -- ^ A list of load balancer description structures.
    , dlbrNextMarker               :: Maybe Text
      -- ^ An optional parameter reserved for future use.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLoadBalancersResult where
    xmlPickler = withNS elbNS

instance IsQuery DescribeLoadBalancersResult

-- | The output for the DetachLoadBalancerFromSubnets action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DetachLoadBalancerFromSubnetsResult.html>
data DetachLoadBalancerFromSubnetsResult = DetachLoadBalancerFromSubnetsResult
    { dlbfsrSubnets :: Maybe Text
      -- ^ A list of subnet IDs removed from the configured set of subnets
      -- for the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML DetachLoadBalancerFromSubnetsResult where
    xmlPickler = withNS elbNS

instance IsQuery DetachLoadBalancerFromSubnetsResult

-- | The output for the DisableAvailabilityZonesForLoadBalancer action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_DisableAvailabilityZonesForLoadBalancerResult.html>
data DisableAvailabilityZonesForLoadBalancerResult = DisableAvailabilityZonesForLoadBalancerResult
    { dazflbrAvailabilityZones :: Members Text
      -- ^ A list of updated Availability Zones for the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML DisableAvailabilityZonesForLoadBalancerResult where
    xmlPickler = withNS elbNS

instance IsQuery DisableAvailabilityZonesForLoadBalancerResult

-- | The output for the EnableAvailabilityZonesForLoadBalancer action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_EnableAvailabilityZonesForLoadBalancerResult.html>
data EnableAvailabilityZonesForLoadBalancerResult = EnableAvailabilityZonesForLoadBalancerResult
    { eazflbrAvailabilityZones :: Members Text
      -- ^ An updated list of Availability Zones for the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML EnableAvailabilityZonesForLoadBalancerResult where
    xmlPickler = withNS elbNS

instance IsQuery EnableAvailabilityZonesForLoadBalancerResult

-- | The HealthCheck data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_HealthCheck.html>
data HealthCheck = HealthCheck
    { hcHealthyThreshold   :: !Integer
      -- ^ Specifies the number of consecutive health probe successes
      -- required before moving the instance to the Healthy state.
    , hcInterval           :: !Integer
      -- ^ Specifies the approximate interval, in seconds, between health
      -- checks of an individual instance.
    , hcTarget             :: !Text
      -- ^ Specifies the instance being checked. The protocol is either TCP,
      -- HTTP, HTTPS, or SSL. The range of valid ports is one (1) through
      -- 65535.
    , hcTimeout            :: !Integer
      -- ^ Specifies the amount of time, in seconds, during which no
      -- response means a failed health probe.
    , hcUnhealthyThreshold :: !Integer
      -- ^ Specifies the number of consecutive health probe failures
      -- required before moving the instance to the Unhealthy state.
    } deriving (Eq, Show, Generic)

instance IsXML HealthCheck where
    xmlPickler = withNS elbNS

instance IsQuery HealthCheck

-- | The Instance data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_Instance.html>
data Instance = Instance
    { iInstanceId :: Maybe Text
      -- ^ Provides an EC2 instance ID.
    } deriving (Eq, Show, Generic)

instance IsXML Instance where
    xmlPickler = withNS elbNS

instance IsQuery Instance

-- | The InstanceState data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_InstanceState.html>
data InstanceState = InstanceState
    { isDescription :: Maybe Text
      -- ^ Provides a description of the instance state.
    , isInstanceId  :: Maybe Text
      -- ^ Provides an EC2 instance ID.
    , isReasonCode  :: Maybe Text
      -- ^ Provides information about the cause of OutOfService instances.
      -- Specifically, it indicates whether the cause is Elastic Load
      -- Balancing or the instance behind the load balancer.
    , isState       :: Maybe Text
      -- ^ Specifies the current state of the instance. Valid value:
      -- InService|OutOfService
    } deriving (Eq, Show, Generic)

instance IsXML InstanceState where
    xmlPickler = withNS elbNS

instance IsQuery InstanceState

-- | The LBCookieStickinessPolicy data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_LBCookieStickinessPolicy.html>
data LBCookieStickinessPolicy = LBCookieStickinessPolicy
    { lbcspCookieExpirationPeriod :: Maybe Integer
      -- ^ The time period in seconds after which the cookie should be
      -- considered stale. Not specifying this parameter indicates that
      -- the stickiness session will last for the duration of the browser
      -- session.
    , lbcspPolicyName             :: Maybe Text
      -- ^ The name for the policy being created. The name must be unique
      -- within the set of policies for this load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML LBCookieStickinessPolicy where
    xmlPickler = withNS elbNS

instance IsQuery LBCookieStickinessPolicy

-- | The Listener data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_Listener.html>
data Listener = Listener
    { lInstancePort     :: !Integer
      -- ^ Specifies the TCP port on which the instance server is listening.
      -- This property cannot be modified for the life of the load
      -- balancer.
    , lInstanceProtocol :: Maybe Text
      -- ^ Specifies the protocol to use for routing traffic to back-end
      -- instances - HTTP, HTTPS, TCP, or SSL. This property cannot be
      -- modified for the life of the load balancer.
    , lLoadBalancerPort :: !Integer
      -- ^ Specifies the external load balancer port number. This property
      -- cannot be modified for the life of the load balancer.
    , lProtocol         :: !Text
      -- ^ Specifies the load balancer transport protocol to use for routing
      -- - HTTP, HTTPS, TCP or SSL. This property cannot be modified for
      -- the life of the load balancer.
    , lSSLCertificateId :: Maybe Text
      -- ^ The ARN string of the server certificate. To get the ARN of the
      -- server certificate, call the AWS Identity and Access Management
      -- UploadServerCertificate API.
    } deriving (Eq, Show, Generic)

instance IsXML Listener where
    xmlPickler = withNS elbNS

instance IsQuery Listener

-- | The ListenerDescription data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_ListenerDescription.html>
data ListenerDescription = ListenerDescription
    { ldListener    :: Maybe Listener
      -- ^ The Listener data type.
    , ldPolicyNames :: Maybe Text
      -- ^ A list of policies enabled for this listener. An empty list
      -- indicates that no policies are enabled.
    } deriving (Eq, Show, Generic)

instance IsXML ListenerDescription where
    xmlPickler = withNS elbNS

instance IsQuery ListenerDescription

-- | Contains the result of a successful invocation of DescribeLoadBalancers.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_LoadBalancerDescription.html>
data LoadBalancerDescription = LoadBalancerDescription
    { lbdAvailabilityZones         :: [Text]
      -- ^ Specifies a list of Availability Zones.
    , lbdBackendServerDescriptions :: Members BackendServerDescription
      -- ^ Contains a list of back-end server descriptions.
    , lbdCanonicalHostedZoneName   :: Maybe Text
      -- ^ Provides the name of the Amazon Route 53 hosted zone that is
      -- associated with the load balancer. For information on how to
      -- associate your load balancer with a hosted zone, go to Using
      -- Domain Names With Elastic Load Balancing in the Elastic Load
      -- Balancing Developer Guide.
    , lbdCanonicalHostedZoneNameID :: Maybe Text
      -- ^ Provides the ID of the Amazon Route 53 hosted zone name that is
      -- associated with the load balancer. For information on how to
      -- associate or disassociate your load balancer with a hosted zone,
      -- go to Using Domain Names With Elastic Load Balancing in the
      -- Elastic Load Balancing Developer Guide.
    , lbdCreatedTime               :: Maybe UTCTime
      -- ^ Provides the date and time the load balancer was created.
    , lbdDNSName                   :: Maybe Text
      -- ^ Specifies the external DNS name associated with the load
      -- balancer.
    , lbdHealthCheck               :: Maybe HealthCheck
      -- ^ Specifies information regarding the various health probes
      -- conducted on the load balancer.
    , lbdInstances                 :: Members Instance
      -- ^ Provides a list of EC2 instance IDs for the load balancer.
    , lbdListenerDescriptions      :: Members ListenerDescription
      -- ^ LoadBalancerPort, InstancePort, Protocol, InstanceProtocol, and
      -- PolicyNames are returned in a list of tuples in the
      -- ListenerDescriptions element.
    , lbdLoadBalancerName          :: Maybe Text
      -- ^ Specifies the name associated with the load balancer.
    , lbdPolicies                  :: Maybe Policies
      -- ^ Provides a list of policies defined for the load balancer.
    , lbdScheme                    :: Maybe Text
      -- ^ Specifies the type of load balancer.
    , lbdSecurityGroups            :: Members Text
      -- ^ The security groups the load balancer is a member of (VPC only).
    , lbdSourceSecurityGroup       :: Maybe SourceSecurityGroup
      -- ^ The security group that you can use as part of your inbound rules
      -- for your load balancer's back-end Amazon EC2 application
      -- instances. To only allow traffic from load balancers, add a
      -- security group rule to your back end instance that specifies this
      -- source security group as the inbound source.
    , lbdSubnets                   :: Members Text
      -- ^ Provides a list of VPC subnet IDs for the load balancer.
    , lbdVPCId                     :: Maybe Text
      -- ^ Provides the ID of the VPC attached to the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML LoadBalancerDescription where
    xmlPickler = withNS elbNS

instance IsQuery LoadBalancerDescription

-- | The policies data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_Policies.html>
data Policies = Policies
    { pAppCookieStickinessPolicies :: Maybe AppCookieStickinessPolicy
      -- ^ A list of the AppCookieStickinessPolicy objects created with
      -- CreateAppCookieStickinessPolicy.
    , pLBCookieStickinessPolicies  :: Maybe LBCookieStickinessPolicy
      -- ^ A list of LBCookieStickinessPolicy objects created with
      -- CreateAppCookieStickinessPolicy.
    , pOtherPolicies               :: Maybe Text
      -- ^ A list of policy names other than the stickiness policies.
    } deriving (Eq, Show, Generic)

instance IsXML Policies where
    xmlPickler = withNS elbNS

instance IsQuery Policies

-- | The PolicyAttribute data type. This data type contains a key/value pair
-- that defines properties of a specific policy.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_PolicyAttribute.html>
data PolicyAttribute = PolicyAttribute
    { paAttributeName  :: Maybe Text
      -- ^ The name of the attribute associated with the policy.
    , paAttributeValue :: Maybe Text
      -- ^ The value of the attribute associated with the policy.
    } deriving (Eq, Show, Generic)

instance IsXML PolicyAttribute where
    xmlPickler = withNS elbNS

instance IsQuery PolicyAttribute

-- | The PolicyAttributeDescription data type. This data type is used to
-- describe the attributes and values associated with a policy.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_PolicyAttributeDescription.html>
data PolicyAttributeDescription = PolicyAttributeDescription
    { padAttributeName  :: Maybe Text
      -- ^ The name of the attribute associated with the policy.
    , padAttributeValue :: Maybe Text
      -- ^ The value of the attribute associated with the policy.
    } deriving (Eq, Show, Generic)

instance IsXML PolicyAttributeDescription where
    xmlPickler = withNS elbNS

instance IsQuery PolicyAttributeDescription

-- | The PolicyAttributeTypeDescription data type. This data type is used to
-- describe values that are acceptable for the policy attribute.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_PolicyAttributeTypeDescription.html>
data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription
    { patdAttributeName :: Maybe Text
      -- ^ The name of the attribute associated with the policy type.
    , patdAttributeType :: Maybe Text
      -- ^ The type of attribute. For example, Boolean, Integer, etc.
    , patdCardinality   :: Maybe Text
      -- ^ The cardinality of the attribute. Valid Values:
    , patdDefaultValue  :: Maybe Text
      -- ^ The default value of the attribute, if applicable.
    , patdDescription   :: Maybe Text
      -- ^ A human-readable description of the attribute.
    } deriving (Eq, Show, Generic)

instance IsXML PolicyAttributeTypeDescription where
    xmlPickler = withNS elbNS

instance IsQuery PolicyAttributeTypeDescription

-- | The PolicyDescription data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_PolicyDescription.html>
data PolicyDescription = PolicyDescription
    { pdPolicyAttributeDescriptions :: Maybe PolicyAttributeDescription
      -- ^ A list of policy attribute description structures.
    , pdPolicyName                  :: Maybe Text
      -- ^ The name of the policy associated with the load balancer.
    , pdPolicyTypeName              :: Maybe Text
      -- ^ The name of the policy type associated with the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML PolicyDescription where
    xmlPickler = withNS elbNS

instance IsQuery PolicyDescription

-- | The PolicyTypeDescription data type.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_PolicyTypeDescription.html>
data PolicyTypeDescription = PolicyTypeDescription
    { ptdDescription                     :: Maybe Text
      -- ^ A human-readable description of the policy type.
    , ptdPolicyAttributeTypeDescriptions :: Maybe PolicyAttributeTypeDescription
      -- ^ The description of the policy attributes associated with the load
      -- balancer policies defined by the Elastic Load Balancing service.
    , ptdPolicyTypeName                  :: Maybe Text
      -- ^ The name of the policy type.
    } deriving (Eq, Show, Generic)

instance IsXML PolicyTypeDescription where
    xmlPickler = withNS elbNS

instance IsQuery PolicyTypeDescription

-- | The output for the RegisterInstancesWithLoadBalancer action.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_RegisterInstancesWithLoadBalancerResult.html>
data RegisterInstancesWithLoadBalancerResult = RegisterInstancesWithLoadBalancerResult
    { riwlbrInstances :: Maybe Instance
      -- ^ An updated list of instances for the load balancer.
    } deriving (Eq, Show, Generic)

instance IsXML RegisterInstancesWithLoadBalancerResult where
    xmlPickler = withNS elbNS

instance IsQuery RegisterInstancesWithLoadBalancerResult

-- | This data type is used as a response element in the DescribeLoadBalancers
-- action. For information about Elastic Load Balancing security groups, go to
-- Using Security Groups With Elastic Load Balancing in the Elastic Load
-- Balancing Developer Guide.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_SourceSecurityGroup.html>
data SourceSecurityGroup = SourceSecurityGroup
    { ssgGroupName  :: Maybe Text
      -- ^ Name of the source security group. Use this value for the
      -- --source-group parameter of the ec2-authorize command in the
      -- Amazon EC2 command line tool.
    , ssgOwnerAlias :: Maybe Text
      -- ^ Owner of the source security group. Use this value for the
      -- --source-group-user parameter of the ec2-authorize command in the
      -- Amazon EC2 command line tool.
    } deriving (Eq, Show, Generic)

instance IsXML SourceSecurityGroup where
    xmlPickler = withNS elbNS

instance IsQuery SourceSecurityGroup
