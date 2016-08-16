{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBV2
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Elastic Load Balancing
--
-- A load balancer distributes incoming traffic across targets, such as your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered targets and ensures that it routes traffic only to healthy targets. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer. You configure a target group with a protocol and port number for connections from the load balancer to the targets, and with health check settings to be used when checking the health status of the targets.
--
-- Elastic Load Balancing supports two types of load balancers: Classic load balancers and Application load balancers (new). A Classic load balancer makes routing and load balancing decisions either at the transport layer (TCP\/SSL) or the application layer (HTTP\/HTTPS), and supports either EC2-Classic or a VPC. An Application load balancer makes routing and load balancing decisions at the application layer (HTTP\/HTTPS), supports path-based routing, and can route requests to one or more ports on each EC2 instance or container instance in your virtual private cloud (VPC). For more information, see the <http://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/ Elastic Load Balancing User Guide>.
--
-- This reference covers the 2015-12-01 API, which supports Application load balancers. The 2012-06-01 API supports Classic load balancers.
--
-- To get started with an Application load balancer, complete the following tasks:
--
-- 1.  Create a load balancer using < CreateLoadBalancer>.
--
-- 2.  Create a target group using < CreateTargetGroup>.
--
-- 3.  Register targets for the target group using < RegisterTargets>.
--
-- 4.  Create one or more listeners for your load balancer using < CreateListener>.
--
-- 5.  (Optional) Create one or more rules for content routing based on URL using < CreateRule>.
--
-- To delete an Application load balancer and its related resources, complete the following tasks:
--
-- 1.  Delete the load balancer using < DeleteLoadBalancer>.
--
-- 2.  Delete the target group using < DeleteTargetGroup>.
--
-- All Elastic Load Balancing operations are idempotent, which means that they complete at most one time. If you repeat an operation, it succeeds.
module Network.AWS.ELBV2
    (
    -- * Service Configuration
      elbv2

    -- * Errors
    -- $errors

    -- ** InvalidConfigurationRequestException
    , _InvalidConfigurationRequestException

    -- ** SubnetNotFoundException
    , _SubnetNotFoundException

    -- ** TooManyTargetsException
    , _TooManyTargetsException

    -- ** RuleNotFoundException
    , _RuleNotFoundException

    -- ** InvalidSubnetException
    , _InvalidSubnetException

    -- ** TooManyRulesException
    , _TooManyRulesException

    -- ** TooManyTargetGroupsException
    , _TooManyTargetGroupsException

    -- ** DuplicateLoadBalancerNameException
    , _DuplicateLoadBalancerNameException

    -- ** IncompatibleProtocolsException
    , _IncompatibleProtocolsException

    -- ** TooManyCertificatesException
    , _TooManyCertificatesException

    -- ** DuplicateTagKeysException
    , _DuplicateTagKeysException

    -- ** DuplicateListenerException
    , _DuplicateListenerException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** DuplicateTargetGroupNameException
    , _DuplicateTargetGroupNameException

    -- ** HealthUnavailableException
    , _HealthUnavailableException

    -- ** PriorityInUseException
    , _PriorityInUseException

    -- ** TooManyLoadBalancersException
    , _TooManyLoadBalancersException

    -- ** UnsupportedProtocolException
    , _UnsupportedProtocolException

    -- ** InvalidTargetException
    , _InvalidTargetException

    -- ** InvalidSecurityGroupException
    , _InvalidSecurityGroupException

    -- ** TargetGroupNotFoundException
    , _TargetGroupNotFoundException

    -- ** ListenerNotFoundException
    , _ListenerNotFoundException

    -- ** TooManyRegistrationsForTargetIdException
    , _TooManyRegistrationsForTargetIdException

    -- ** TooManyListenersException
    , _TooManyListenersException

    -- ** TargetGroupAssociationLimitException
    , _TargetGroupAssociationLimitException

    -- ** OperationNotPermittedException
    , _OperationNotPermittedException

    -- ** SSLPolicyNotFoundException
    , _SSLPolicyNotFoundException

    -- ** InvalidSchemeException
    , _InvalidSchemeException

    -- ** LoadBalancerNotFoundException
    , _LoadBalancerNotFoundException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- ** CertificateNotFoundException
    , _CertificateNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeLoadBalancers
    , module Network.AWS.ELBV2.DescribeLoadBalancers

    -- ** DescribeTags
    , module Network.AWS.ELBV2.DescribeTags

    -- ** DeleteRule
    , module Network.AWS.ELBV2.DeleteRule

    -- ** RemoveTags
    , module Network.AWS.ELBV2.RemoveTags

    -- ** DeleteTargetGroup
    , module Network.AWS.ELBV2.DeleteTargetGroup

    -- ** SetSubnets
    , module Network.AWS.ELBV2.SetSubnets

    -- ** CreateRule
    , module Network.AWS.ELBV2.CreateRule

    -- ** SetSecurityGroups
    , module Network.AWS.ELBV2.SetSecurityGroups

    -- ** SetRulePriorities
    , module Network.AWS.ELBV2.SetRulePriorities

    -- ** DescribeTargetGroups
    , module Network.AWS.ELBV2.DescribeTargetGroups

    -- ** DescribeRules
    , module Network.AWS.ELBV2.DescribeRules

    -- ** DeleteLoadBalancer
    , module Network.AWS.ELBV2.DeleteLoadBalancer

    -- ** RegisterTargets
    , module Network.AWS.ELBV2.RegisterTargets

    -- ** ModifyListener
    , module Network.AWS.ELBV2.ModifyListener

    -- ** ModifyTargetGroup
    , module Network.AWS.ELBV2.ModifyTargetGroup

    -- ** ModifyTargetGroupAttributes
    , module Network.AWS.ELBV2.ModifyTargetGroupAttributes

    -- ** DescribeTargetGroupAttributes
    , module Network.AWS.ELBV2.DescribeTargetGroupAttributes

    -- ** DeleteListener
    , module Network.AWS.ELBV2.DeleteListener

    -- ** DescribeSSLPolicies
    , module Network.AWS.ELBV2.DescribeSSLPolicies

    -- ** DeregisterTargets
    , module Network.AWS.ELBV2.DeregisterTargets

    -- ** CreateListener
    , module Network.AWS.ELBV2.CreateListener

    -- ** CreateTargetGroup
    , module Network.AWS.ELBV2.CreateTargetGroup

    -- ** ModifyLoadBalancerAttributes
    , module Network.AWS.ELBV2.ModifyLoadBalancerAttributes

    -- ** AddTags
    , module Network.AWS.ELBV2.AddTags

    -- ** DescribeLoadBalancerAttributes
    , module Network.AWS.ELBV2.DescribeLoadBalancerAttributes

    -- ** DescribeListeners
    , module Network.AWS.ELBV2.DescribeListeners

    -- ** DescribeTargetHealth
    , module Network.AWS.ELBV2.DescribeTargetHealth

    -- ** CreateLoadBalancer
    , module Network.AWS.ELBV2.CreateLoadBalancer

    -- ** ModifyRule
    , module Network.AWS.ELBV2.ModifyRule

    -- * Types

    -- ** ActionTypeEnum
    , ActionTypeEnum (..)

    -- ** LoadBalancerSchemeEnum
    , LoadBalancerSchemeEnum (..)

    -- ** LoadBalancerStateEnum
    , LoadBalancerStateEnum (..)

    -- ** LoadBalancerTypeEnum
    , LoadBalancerTypeEnum (..)

    -- ** ProtocolEnum
    , ProtocolEnum (..)

    -- ** TargetHealthReasonEnum
    , TargetHealthReasonEnum (..)

    -- ** TargetHealthStateEnum
    , TargetHealthStateEnum (..)

    -- ** Action
    , Action
    , action
    , aType
    , aTargetGroupARN

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azSubnetId
    , azZoneName

    -- ** Certificate
    , Certificate
    , certificate
    , cCertificateARN

    -- ** Cipher
    , Cipher
    , cipher
    , cPriority
    , cName

    -- ** Listener
    , Listener
    , listener
    , lSSLPolicy
    , lListenerARN
    , lProtocol
    , lDefaultActions
    , lCertificates
    , lLoadBalancerARN
    , lPort

    -- ** LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbState
    , lbSecurityGroups
    , lbLoadBalancerName
    , lbCreatedTime
    , lbVPCId
    , lbCanonicalHostedZoneId
    , lbAvailabilityZones
    , lbLoadBalancerARN
    , lbScheme
    , lbType
    , lbDNSName

    -- ** LoadBalancerAttribute
    , LoadBalancerAttribute
    , loadBalancerAttribute
    , lbaValue
    , lbaKey

    -- ** LoadBalancerState
    , LoadBalancerState
    , loadBalancerState
    , lbsReason
    , lbsCode

    -- ** Matcher
    , Matcher
    , matcher
    , mHTTPCode

    -- ** Rule
    , Rule
    , rule
    , rPriority
    , rActions
    , rConditions
    , rRuleARN
    , rIsDefault

    -- ** RuleCondition
    , RuleCondition
    , ruleCondition
    , rcField
    , rcValues

    -- ** RulePriorityPair
    , RulePriorityPair
    , rulePriorityPair
    , rppPriority
    , rppRuleARN

    -- ** SSLPolicy
    , SSLPolicy
    , sslPolicy
    , spCiphers
    , spName
    , spSSLProtocols

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TagDescription
    , TagDescription
    , tagDescription
    , tdResourceARN
    , tdTags

    -- ** TargetDescription
    , TargetDescription
    , targetDescription
    , tdPort
    , tdId

    -- ** TargetGroup
    , TargetGroup
    , targetGroup
    , tgMatcher
    , tgHealthCheckPath
    , tgUnhealthyThresholdCount
    , tgVPCId
    , tgTargetGroupARN
    , tgProtocol
    , tgHealthCheckIntervalSeconds
    , tgHealthyThresholdCount
    , tgHealthCheckProtocol
    , tgLoadBalancerARNs
    , tgHealthCheckTimeoutSeconds
    , tgHealthCheckPort
    , tgTargetGroupName
    , tgPort

    -- ** TargetGroupAttribute
    , TargetGroupAttribute
    , targetGroupAttribute
    , tgaValue
    , tgaKey

    -- ** TargetHealth
    , TargetHealth
    , targetHealth
    , thState
    , thReason
    , thDescription

    -- ** TargetHealthDescription
    , TargetHealthDescription
    , targetHealthDescription
    , thdTargetHealth
    , thdHealthCheckPort
    , thdTarget
    ) where

import           Network.AWS.ELBV2.AddTags
import           Network.AWS.ELBV2.CreateListener
import           Network.AWS.ELBV2.CreateLoadBalancer
import           Network.AWS.ELBV2.CreateRule
import           Network.AWS.ELBV2.CreateTargetGroup
import           Network.AWS.ELBV2.DeleteListener
import           Network.AWS.ELBV2.DeleteLoadBalancer
import           Network.AWS.ELBV2.DeleteRule
import           Network.AWS.ELBV2.DeleteTargetGroup
import           Network.AWS.ELBV2.DeregisterTargets
import           Network.AWS.ELBV2.DescribeListeners
import           Network.AWS.ELBV2.DescribeLoadBalancerAttributes
import           Network.AWS.ELBV2.DescribeLoadBalancers
import           Network.AWS.ELBV2.DescribeRules
import           Network.AWS.ELBV2.DescribeSSLPolicies
import           Network.AWS.ELBV2.DescribeTags
import           Network.AWS.ELBV2.DescribeTargetGroupAttributes
import           Network.AWS.ELBV2.DescribeTargetGroups
import           Network.AWS.ELBV2.DescribeTargetHealth
import           Network.AWS.ELBV2.ModifyListener
import           Network.AWS.ELBV2.ModifyLoadBalancerAttributes
import           Network.AWS.ELBV2.ModifyRule
import           Network.AWS.ELBV2.ModifyTargetGroup
import           Network.AWS.ELBV2.ModifyTargetGroupAttributes
import           Network.AWS.ELBV2.RegisterTargets
import           Network.AWS.ELBV2.RemoveTags
import           Network.AWS.ELBV2.SetRulePriorities
import           Network.AWS.ELBV2.SetSecurityGroups
import           Network.AWS.ELBV2.SetSubnets
import           Network.AWS.ELBV2.Types
import           Network.AWS.ELBV2.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ELBV2'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
