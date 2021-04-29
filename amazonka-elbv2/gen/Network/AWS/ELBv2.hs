{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Elastic Load Balancing
--
-- A load balancer distributes incoming traffic across targets, such as
-- your EC2 instances. This enables you to increase the availability of
-- your application. The load balancer also monitors the health of its
-- registered targets and ensures that it routes traffic only to healthy
-- targets. You configure your load balancer to accept incoming traffic by
-- specifying one or more listeners, which are configured with a protocol
-- and port number for connections from clients to the load balancer. You
-- configure a target group with a protocol and port number for connections
-- from the load balancer to the targets, and with health check settings to
-- be used when checking the health status of the targets.
--
-- Elastic Load Balancing supports the following types of load balancers:
-- Application Load Balancers, Network Load Balancers, Gateway Load
-- Balancers, and Classic Load Balancers. This reference covers the
-- following load balancer types:
--
-- -   Application Load Balancer - Operates at the application layer (layer
--     7) and supports HTTP and HTTPS.
--
-- -   Network Load Balancer - Operates at the transport layer (layer 4)
--     and supports TCP, TLS, and UDP.
--
-- -   Gateway Load Balancer - Operates at the network layer (layer 3).
--
-- For more information, see the
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/ Elastic Load Balancing User Guide>.
--
-- All Elastic Load Balancing operations are idempotent, which means that
-- they complete at most one time. If you repeat an operation, it succeeds.
module Network.AWS.ELBv2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidSecurityGroupException
    _InvalidSecurityGroupException,

    -- ** ALPNPolicyNotSupportedException
    _ALPNPolicyNotSupportedException,

    -- ** InvalidConfigurationRequestException
    _InvalidConfigurationRequestException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** HealthUnavailableException
    _HealthUnavailableException,

    -- ** PriorityInUseException
    _PriorityInUseException,

    -- ** DuplicateTagKeysException
    _DuplicateTagKeysException,

    -- ** AvailabilityZoneNotSupportedException
    _AvailabilityZoneNotSupportedException,

    -- ** TooManyUniqueTargetGroupsPerLoadBalancerException
    _TooManyUniqueTargetGroupsPerLoadBalancerException,

    -- ** IncompatibleProtocolsException
    _IncompatibleProtocolsException,

    -- ** TooManyRulesException
    _TooManyRulesException,

    -- ** TooManyActionsException
    _TooManyActionsException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** TargetGroupAssociationLimitException
    _TargetGroupAssociationLimitException,

    -- ** TooManyRegistrationsForTargetIdException
    _TooManyRegistrationsForTargetIdException,

    -- ** ListenerNotFoundException
    _ListenerNotFoundException,

    -- ** SubnetNotFoundException
    _SubnetNotFoundException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** UnsupportedProtocolException
    _UnsupportedProtocolException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** AllocationIdNotFoundException
    _AllocationIdNotFoundException,

    -- ** TooManyLoadBalancersException
    _TooManyLoadBalancersException,

    -- ** CertificateNotFoundException
    _CertificateNotFoundException,

    -- ** DuplicateTargetGroupNameException
    _DuplicateTargetGroupNameException,

    -- ** DuplicateListenerException
    _DuplicateListenerException,

    -- ** LoadBalancerNotFoundException
    _LoadBalancerNotFoundException,

    -- ** TooManyCertificatesException
    _TooManyCertificatesException,

    -- ** InvalidSchemeException
    _InvalidSchemeException,

    -- ** SSLPolicyNotFoundException
    _SSLPolicyNotFoundException,

    -- ** InvalidSubnetException
    _InvalidSubnetException,

    -- ** TooManyTargetGroupsException
    _TooManyTargetGroupsException,

    -- ** DuplicateLoadBalancerNameException
    _DuplicateLoadBalancerNameException,

    -- ** TooManyListenersException
    _TooManyListenersException,

    -- ** InvalidLoadBalancerActionException
    _InvalidLoadBalancerActionException,

    -- ** TooManyTargetsException
    _TooManyTargetsException,

    -- ** RuleNotFoundException
    _RuleNotFoundException,

    -- ** TargetGroupNotFoundException
    _TargetGroupNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** LoadBalancersDeleted
    newLoadBalancersDeleted,

    -- ** TargetDeregistered
    newTargetDeregistered,

    -- ** LoadBalancerAvailable
    newLoadBalancerAvailable,

    -- ** TargetInService
    newTargetInService,

    -- ** LoadBalancerExists
    newLoadBalancerExists,

    -- * Operations
    -- $operations

    -- ** DescribeSSLPolicies (Paginated)
    DescribeSSLPolicies (DescribeSSLPolicies'),
    newDescribeSSLPolicies,
    DescribeSSLPoliciesResponse (DescribeSSLPoliciesResponse'),
    newDescribeSSLPoliciesResponse,

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

    -- ** DeleteRule
    DeleteRule (DeleteRule'),
    newDeleteRule,
    DeleteRuleResponse (DeleteRuleResponse'),
    newDeleteRuleResponse,

    -- ** DescribeTags
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DescribeTargetGroupAttributes
    DescribeTargetGroupAttributes (DescribeTargetGroupAttributes'),
    newDescribeTargetGroupAttributes,
    DescribeTargetGroupAttributesResponse (DescribeTargetGroupAttributesResponse'),
    newDescribeTargetGroupAttributesResponse,

    -- ** AddListenerCertificates
    AddListenerCertificates (AddListenerCertificates'),
    newAddListenerCertificates,
    AddListenerCertificatesResponse (AddListenerCertificatesResponse'),
    newAddListenerCertificatesResponse,

    -- ** CreateLoadBalancer
    CreateLoadBalancer (CreateLoadBalancer'),
    newCreateLoadBalancer,
    CreateLoadBalancerResponse (CreateLoadBalancerResponse'),
    newCreateLoadBalancerResponse,

    -- ** RemoveListenerCertificates
    RemoveListenerCertificates (RemoveListenerCertificates'),
    newRemoveListenerCertificates,
    RemoveListenerCertificatesResponse (RemoveListenerCertificatesResponse'),
    newRemoveListenerCertificatesResponse,

    -- ** ModifyRule
    ModifyRule (ModifyRule'),
    newModifyRule,
    ModifyRuleResponse (ModifyRuleResponse'),
    newModifyRuleResponse,

    -- ** ModifyTargetGroup
    ModifyTargetGroup (ModifyTargetGroup'),
    newModifyTargetGroup,
    ModifyTargetGroupResponse (ModifyTargetGroupResponse'),
    newModifyTargetGroupResponse,

    -- ** DeleteLoadBalancer
    DeleteLoadBalancer (DeleteLoadBalancer'),
    newDeleteLoadBalancer,
    DeleteLoadBalancerResponse (DeleteLoadBalancerResponse'),
    newDeleteLoadBalancerResponse,

    -- ** DescribeListeners (Paginated)
    DescribeListeners (DescribeListeners'),
    newDescribeListeners,
    DescribeListenersResponse (DescribeListenersResponse'),
    newDescribeListenersResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** DescribeTargetGroups (Paginated)
    DescribeTargetGroups (DescribeTargetGroups'),
    newDescribeTargetGroups,
    DescribeTargetGroupsResponse (DescribeTargetGroupsResponse'),
    newDescribeTargetGroupsResponse,

    -- ** SetIpAddressType
    SetIpAddressType (SetIpAddressType'),
    newSetIpAddressType,
    SetIpAddressTypeResponse (SetIpAddressTypeResponse'),
    newSetIpAddressTypeResponse,

    -- ** ModifyLoadBalancerAttributes
    ModifyLoadBalancerAttributes (ModifyLoadBalancerAttributes'),
    newModifyLoadBalancerAttributes,
    ModifyLoadBalancerAttributesResponse (ModifyLoadBalancerAttributesResponse'),
    newModifyLoadBalancerAttributesResponse,

    -- ** DescribeAccountLimits (Paginated)
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** CreateRule
    CreateRule (CreateRule'),
    newCreateRule,
    CreateRuleResponse (CreateRuleResponse'),
    newCreateRuleResponse,

    -- ** CreateTargetGroup
    CreateTargetGroup (CreateTargetGroup'),
    newCreateTargetGroup,
    CreateTargetGroupResponse (CreateTargetGroupResponse'),
    newCreateTargetGroupResponse,

    -- ** SetSubnets
    SetSubnets (SetSubnets'),
    newSetSubnets,
    SetSubnetsResponse (SetSubnetsResponse'),
    newSetSubnetsResponse,

    -- ** DeregisterTargets
    DeregisterTargets (DeregisterTargets'),
    newDeregisterTargets,
    DeregisterTargetsResponse (DeregisterTargetsResponse'),
    newDeregisterTargetsResponse,

    -- ** DeleteListener
    DeleteListener (DeleteListener'),
    newDeleteListener,
    DeleteListenerResponse (DeleteListenerResponse'),
    newDeleteListenerResponse,

    -- ** DeleteTargetGroup
    DeleteTargetGroup (DeleteTargetGroup'),
    newDeleteTargetGroup,
    DeleteTargetGroupResponse (DeleteTargetGroupResponse'),
    newDeleteTargetGroupResponse,

    -- ** DescribeLoadBalancers (Paginated)
    DescribeLoadBalancers (DescribeLoadBalancers'),
    newDescribeLoadBalancers,
    DescribeLoadBalancersResponse (DescribeLoadBalancersResponse'),
    newDescribeLoadBalancersResponse,

    -- ** ModifyTargetGroupAttributes
    ModifyTargetGroupAttributes (ModifyTargetGroupAttributes'),
    newModifyTargetGroupAttributes,
    ModifyTargetGroupAttributesResponse (ModifyTargetGroupAttributesResponse'),
    newModifyTargetGroupAttributesResponse,

    -- ** ModifyListener
    ModifyListener (ModifyListener'),
    newModifyListener,
    ModifyListenerResponse (ModifyListenerResponse'),
    newModifyListenerResponse,

    -- ** RegisterTargets
    RegisterTargets (RegisterTargets'),
    newRegisterTargets,
    RegisterTargetsResponse (RegisterTargetsResponse'),
    newRegisterTargetsResponse,

    -- ** DescribeTargetHealth
    DescribeTargetHealth (DescribeTargetHealth'),
    newDescribeTargetHealth,
    DescribeTargetHealthResponse (DescribeTargetHealthResponse'),
    newDescribeTargetHealthResponse,

    -- ** SetRulePriorities
    SetRulePriorities (SetRulePriorities'),
    newSetRulePriorities,
    SetRulePrioritiesResponse (SetRulePrioritiesResponse'),
    newSetRulePrioritiesResponse,

    -- ** DescribeRules (Paginated)
    DescribeRules (DescribeRules'),
    newDescribeRules,
    DescribeRulesResponse (DescribeRulesResponse'),
    newDescribeRulesResponse,

    -- ** SetSecurityGroups
    SetSecurityGroups (SetSecurityGroups'),
    newSetSecurityGroups,
    SetSecurityGroupsResponse (SetSecurityGroupsResponse'),
    newSetSecurityGroupsResponse,

    -- ** DescribeLoadBalancerAttributes
    DescribeLoadBalancerAttributes (DescribeLoadBalancerAttributes'),
    newDescribeLoadBalancerAttributes,
    DescribeLoadBalancerAttributesResponse (DescribeLoadBalancerAttributesResponse'),
    newDescribeLoadBalancerAttributesResponse,

    -- ** DescribeListenerCertificates (Paginated)
    DescribeListenerCertificates (DescribeListenerCertificates'),
    newDescribeListenerCertificates,
    DescribeListenerCertificatesResponse (DescribeListenerCertificatesResponse'),
    newDescribeListenerCertificatesResponse,

    -- ** CreateListener
    CreateListener (CreateListener'),
    newCreateListener,
    CreateListenerResponse (CreateListenerResponse'),
    newCreateListenerResponse,

    -- * Types

    -- ** ActionTypeEnum
    ActionTypeEnum (..),

    -- ** AuthenticateCognitoActionConditionalBehaviorEnum
    AuthenticateCognitoActionConditionalBehaviorEnum (..),

    -- ** AuthenticateOidcActionConditionalBehaviorEnum
    AuthenticateOidcActionConditionalBehaviorEnum (..),

    -- ** IpAddressType
    IpAddressType (..),

    -- ** LoadBalancerSchemeEnum
    LoadBalancerSchemeEnum (..),

    -- ** LoadBalancerStateEnum
    LoadBalancerStateEnum (..),

    -- ** LoadBalancerTypeEnum
    LoadBalancerTypeEnum (..),

    -- ** ProtocolEnum
    ProtocolEnum (..),

    -- ** RedirectActionStatusCodeEnum
    RedirectActionStatusCodeEnum (..),

    -- ** TargetHealthReasonEnum
    TargetHealthReasonEnum (..),

    -- ** TargetHealthStateEnum
    TargetHealthStateEnum (..),

    -- ** TargetTypeEnum
    TargetTypeEnum (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** AuthenticateCognitoActionConfig
    AuthenticateCognitoActionConfig (AuthenticateCognitoActionConfig'),
    newAuthenticateCognitoActionConfig,

    -- ** AuthenticateOidcActionConfig
    AuthenticateOidcActionConfig (AuthenticateOidcActionConfig'),
    newAuthenticateOidcActionConfig,

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** Cipher
    Cipher (Cipher'),
    newCipher,

    -- ** FixedResponseActionConfig
    FixedResponseActionConfig (FixedResponseActionConfig'),
    newFixedResponseActionConfig,

    -- ** ForwardActionConfig
    ForwardActionConfig (ForwardActionConfig'),
    newForwardActionConfig,

    -- ** HostHeaderConditionConfig
    HostHeaderConditionConfig (HostHeaderConditionConfig'),
    newHostHeaderConditionConfig,

    -- ** HttpHeaderConditionConfig
    HttpHeaderConditionConfig (HttpHeaderConditionConfig'),
    newHttpHeaderConditionConfig,

    -- ** HttpRequestMethodConditionConfig
    HttpRequestMethodConditionConfig (HttpRequestMethodConditionConfig'),
    newHttpRequestMethodConditionConfig,

    -- ** Limit
    Limit (Limit'),
    newLimit,

    -- ** Listener
    Listener (Listener'),
    newListener,

    -- ** LoadBalancer
    LoadBalancer (LoadBalancer'),
    newLoadBalancer,

    -- ** LoadBalancerAddress
    LoadBalancerAddress (LoadBalancerAddress'),
    newLoadBalancerAddress,

    -- ** LoadBalancerAttribute
    LoadBalancerAttribute (LoadBalancerAttribute'),
    newLoadBalancerAttribute,

    -- ** LoadBalancerState
    LoadBalancerState (LoadBalancerState'),
    newLoadBalancerState,

    -- ** Matcher
    Matcher (Matcher'),
    newMatcher,

    -- ** PathPatternConditionConfig
    PathPatternConditionConfig (PathPatternConditionConfig'),
    newPathPatternConditionConfig,

    -- ** QueryStringConditionConfig
    QueryStringConditionConfig (QueryStringConditionConfig'),
    newQueryStringConditionConfig,

    -- ** QueryStringKeyValuePair
    QueryStringKeyValuePair (QueryStringKeyValuePair'),
    newQueryStringKeyValuePair,

    -- ** RedirectActionConfig
    RedirectActionConfig (RedirectActionConfig'),
    newRedirectActionConfig,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** RuleCondition
    RuleCondition (RuleCondition'),
    newRuleCondition,

    -- ** RulePriorityPair
    RulePriorityPair (RulePriorityPair'),
    newRulePriorityPair,

    -- ** SourceIpConditionConfig
    SourceIpConditionConfig (SourceIpConditionConfig'),
    newSourceIpConditionConfig,

    -- ** SslPolicy
    SslPolicy (SslPolicy'),
    newSslPolicy,

    -- ** SubnetMapping
    SubnetMapping (SubnetMapping'),
    newSubnetMapping,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagDescription
    TagDescription (TagDescription'),
    newTagDescription,

    -- ** TargetDescription
    TargetDescription (TargetDescription'),
    newTargetDescription,

    -- ** TargetGroup
    TargetGroup (TargetGroup'),
    newTargetGroup,

    -- ** TargetGroupAttribute
    TargetGroupAttribute (TargetGroupAttribute'),
    newTargetGroupAttribute,

    -- ** TargetGroupStickinessConfig
    TargetGroupStickinessConfig (TargetGroupStickinessConfig'),
    newTargetGroupStickinessConfig,

    -- ** TargetGroupTuple
    TargetGroupTuple (TargetGroupTuple'),
    newTargetGroupTuple,

    -- ** TargetHealth
    TargetHealth (TargetHealth'),
    newTargetHealth,

    -- ** TargetHealthDescription
    TargetHealthDescription (TargetHealthDescription'),
    newTargetHealthDescription,
  )
where

import Network.AWS.ELBv2.AddListenerCertificates
import Network.AWS.ELBv2.AddTags
import Network.AWS.ELBv2.CreateListener
import Network.AWS.ELBv2.CreateLoadBalancer
import Network.AWS.ELBv2.CreateRule
import Network.AWS.ELBv2.CreateTargetGroup
import Network.AWS.ELBv2.DeleteListener
import Network.AWS.ELBv2.DeleteLoadBalancer
import Network.AWS.ELBv2.DeleteRule
import Network.AWS.ELBv2.DeleteTargetGroup
import Network.AWS.ELBv2.DeregisterTargets
import Network.AWS.ELBv2.DescribeAccountLimits
import Network.AWS.ELBv2.DescribeListenerCertificates
import Network.AWS.ELBv2.DescribeListeners
import Network.AWS.ELBv2.DescribeLoadBalancerAttributes
import Network.AWS.ELBv2.DescribeLoadBalancers
import Network.AWS.ELBv2.DescribeRules
import Network.AWS.ELBv2.DescribeSSLPolicies
import Network.AWS.ELBv2.DescribeTags
import Network.AWS.ELBv2.DescribeTargetGroupAttributes
import Network.AWS.ELBv2.DescribeTargetGroups
import Network.AWS.ELBv2.DescribeTargetHealth
import Network.AWS.ELBv2.Lens
import Network.AWS.ELBv2.ModifyListener
import Network.AWS.ELBv2.ModifyLoadBalancerAttributes
import Network.AWS.ELBv2.ModifyRule
import Network.AWS.ELBv2.ModifyTargetGroup
import Network.AWS.ELBv2.ModifyTargetGroupAttributes
import Network.AWS.ELBv2.RegisterTargets
import Network.AWS.ELBv2.RemoveListenerCertificates
import Network.AWS.ELBv2.RemoveTags
import Network.AWS.ELBv2.SetIpAddressType
import Network.AWS.ELBv2.SetRulePriorities
import Network.AWS.ELBv2.SetSecurityGroups
import Network.AWS.ELBv2.SetSubnets
import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ELBv2'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
