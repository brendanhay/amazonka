{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ELBV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-12-01@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.ELBV2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ALPNPolicyNotSupportedException
    _ALPNPolicyNotSupportedException,

    -- ** AllocationIdNotFoundException
    _AllocationIdNotFoundException,

    -- ** AvailabilityZoneNotSupportedException
    _AvailabilityZoneNotSupportedException,

    -- ** CertificateNotFoundException
    _CertificateNotFoundException,

    -- ** DuplicateListenerException
    _DuplicateListenerException,

    -- ** DuplicateLoadBalancerNameException
    _DuplicateLoadBalancerNameException,

    -- ** DuplicateTagKeysException
    _DuplicateTagKeysException,

    -- ** DuplicateTargetGroupNameException
    _DuplicateTargetGroupNameException,

    -- ** HealthUnavailableException
    _HealthUnavailableException,

    -- ** IncompatibleProtocolsException
    _IncompatibleProtocolsException,

    -- ** InvalidConfigurationRequestException
    _InvalidConfigurationRequestException,

    -- ** InvalidLoadBalancerActionException
    _InvalidLoadBalancerActionException,

    -- ** InvalidSchemeException
    _InvalidSchemeException,

    -- ** InvalidSecurityGroupException
    _InvalidSecurityGroupException,

    -- ** InvalidSubnetException
    _InvalidSubnetException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** ListenerNotFoundException
    _ListenerNotFoundException,

    -- ** LoadBalancerNotFoundException
    _LoadBalancerNotFoundException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** PriorityInUseException
    _PriorityInUseException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** RuleNotFoundException
    _RuleNotFoundException,

    -- ** SSLPolicyNotFoundException
    _SSLPolicyNotFoundException,

    -- ** SubnetNotFoundException
    _SubnetNotFoundException,

    -- ** TargetGroupAssociationLimitException
    _TargetGroupAssociationLimitException,

    -- ** TargetGroupNotFoundException
    _TargetGroupNotFoundException,

    -- ** TooManyActionsException
    _TooManyActionsException,

    -- ** TooManyCertificatesException
    _TooManyCertificatesException,

    -- ** TooManyListenersException
    _TooManyListenersException,

    -- ** TooManyLoadBalancersException
    _TooManyLoadBalancersException,

    -- ** TooManyRegistrationsForTargetIdException
    _TooManyRegistrationsForTargetIdException,

    -- ** TooManyRulesException
    _TooManyRulesException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** TooManyTargetGroupsException
    _TooManyTargetGroupsException,

    -- ** TooManyTargetsException
    _TooManyTargetsException,

    -- ** TooManyUniqueTargetGroupsPerLoadBalancerException
    _TooManyUniqueTargetGroupsPerLoadBalancerException,

    -- ** UnsupportedProtocolException
    _UnsupportedProtocolException,

    -- * Waiters
    -- $waiters

    -- ** LoadBalancerAvailable
    newLoadBalancerAvailable,

    -- ** LoadBalancerExists
    newLoadBalancerExists,

    -- ** LoadBalancersDeleted
    newLoadBalancersDeleted,

    -- ** TargetDeregistered
    newTargetDeregistered,

    -- ** TargetInService
    newTargetInService,

    -- * Operations
    -- $operations

    -- ** AddListenerCertificates
    AddListenerCertificates (AddListenerCertificates'),
    newAddListenerCertificates,
    AddListenerCertificatesResponse (AddListenerCertificatesResponse'),
    newAddListenerCertificatesResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** CreateListener
    CreateListener (CreateListener'),
    newCreateListener,
    CreateListenerResponse (CreateListenerResponse'),
    newCreateListenerResponse,

    -- ** CreateLoadBalancer
    CreateLoadBalancer (CreateLoadBalancer'),
    newCreateLoadBalancer,
    CreateLoadBalancerResponse (CreateLoadBalancerResponse'),
    newCreateLoadBalancerResponse,

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

    -- ** DeleteListener
    DeleteListener (DeleteListener'),
    newDeleteListener,
    DeleteListenerResponse (DeleteListenerResponse'),
    newDeleteListenerResponse,

    -- ** DeleteLoadBalancer
    DeleteLoadBalancer (DeleteLoadBalancer'),
    newDeleteLoadBalancer,
    DeleteLoadBalancerResponse (DeleteLoadBalancerResponse'),
    newDeleteLoadBalancerResponse,

    -- ** DeleteRule
    DeleteRule (DeleteRule'),
    newDeleteRule,
    DeleteRuleResponse (DeleteRuleResponse'),
    newDeleteRuleResponse,

    -- ** DeleteTargetGroup
    DeleteTargetGroup (DeleteTargetGroup'),
    newDeleteTargetGroup,
    DeleteTargetGroupResponse (DeleteTargetGroupResponse'),
    newDeleteTargetGroupResponse,

    -- ** DeregisterTargets
    DeregisterTargets (DeregisterTargets'),
    newDeregisterTargets,
    DeregisterTargetsResponse (DeregisterTargetsResponse'),
    newDeregisterTargetsResponse,

    -- ** DescribeAccountLimits (Paginated)
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** DescribeListenerCertificates (Paginated)
    DescribeListenerCertificates (DescribeListenerCertificates'),
    newDescribeListenerCertificates,
    DescribeListenerCertificatesResponse (DescribeListenerCertificatesResponse'),
    newDescribeListenerCertificatesResponse,

    -- ** DescribeListeners (Paginated)
    DescribeListeners (DescribeListeners'),
    newDescribeListeners,
    DescribeListenersResponse (DescribeListenersResponse'),
    newDescribeListenersResponse,

    -- ** DescribeLoadBalancerAttributes
    DescribeLoadBalancerAttributes (DescribeLoadBalancerAttributes'),
    newDescribeLoadBalancerAttributes,
    DescribeLoadBalancerAttributesResponse (DescribeLoadBalancerAttributesResponse'),
    newDescribeLoadBalancerAttributesResponse,

    -- ** DescribeLoadBalancers (Paginated)
    DescribeLoadBalancers (DescribeLoadBalancers'),
    newDescribeLoadBalancers,
    DescribeLoadBalancersResponse (DescribeLoadBalancersResponse'),
    newDescribeLoadBalancersResponse,

    -- ** DescribeRules (Paginated)
    DescribeRules (DescribeRules'),
    newDescribeRules,
    DescribeRulesResponse (DescribeRulesResponse'),
    newDescribeRulesResponse,

    -- ** DescribeSSLPolicies (Paginated)
    DescribeSSLPolicies (DescribeSSLPolicies'),
    newDescribeSSLPolicies,
    DescribeSSLPoliciesResponse (DescribeSSLPoliciesResponse'),
    newDescribeSSLPoliciesResponse,

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

    -- ** DescribeTargetGroups (Paginated)
    DescribeTargetGroups (DescribeTargetGroups'),
    newDescribeTargetGroups,
    DescribeTargetGroupsResponse (DescribeTargetGroupsResponse'),
    newDescribeTargetGroupsResponse,

    -- ** DescribeTargetHealth
    DescribeTargetHealth (DescribeTargetHealth'),
    newDescribeTargetHealth,
    DescribeTargetHealthResponse (DescribeTargetHealthResponse'),
    newDescribeTargetHealthResponse,

    -- ** ModifyListener
    ModifyListener (ModifyListener'),
    newModifyListener,
    ModifyListenerResponse (ModifyListenerResponse'),
    newModifyListenerResponse,

    -- ** ModifyLoadBalancerAttributes
    ModifyLoadBalancerAttributes (ModifyLoadBalancerAttributes'),
    newModifyLoadBalancerAttributes,
    ModifyLoadBalancerAttributesResponse (ModifyLoadBalancerAttributesResponse'),
    newModifyLoadBalancerAttributesResponse,

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

    -- ** ModifyTargetGroupAttributes
    ModifyTargetGroupAttributes (ModifyTargetGroupAttributes'),
    newModifyTargetGroupAttributes,
    ModifyTargetGroupAttributesResponse (ModifyTargetGroupAttributesResponse'),
    newModifyTargetGroupAttributesResponse,

    -- ** RegisterTargets
    RegisterTargets (RegisterTargets'),
    newRegisterTargets,
    RegisterTargetsResponse (RegisterTargetsResponse'),
    newRegisterTargetsResponse,

    -- ** RemoveListenerCertificates
    RemoveListenerCertificates (RemoveListenerCertificates'),
    newRemoveListenerCertificates,
    RemoveListenerCertificatesResponse (RemoveListenerCertificatesResponse'),
    newRemoveListenerCertificatesResponse,

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

    -- ** SetIpAddressType
    SetIpAddressType (SetIpAddressType'),
    newSetIpAddressType,
    SetIpAddressTypeResponse (SetIpAddressTypeResponse'),
    newSetIpAddressTypeResponse,

    -- ** SetRulePriorities
    SetRulePriorities (SetRulePriorities'),
    newSetRulePriorities,
    SetRulePrioritiesResponse (SetRulePrioritiesResponse'),
    newSetRulePrioritiesResponse,

    -- ** SetSecurityGroups
    SetSecurityGroups (SetSecurityGroups'),
    newSetSecurityGroups,
    SetSecurityGroupsResponse (SetSecurityGroupsResponse'),
    newSetSecurityGroupsResponse,

    -- ** SetSubnets
    SetSubnets (SetSubnets'),
    newSetSubnets,
    SetSubnetsResponse (SetSubnetsResponse'),
    newSetSubnetsResponse,

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

    -- ** TargetGroupIpAddressTypeEnum
    TargetGroupIpAddressTypeEnum (..),

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

import Amazonka.ELBV2.AddListenerCertificates
import Amazonka.ELBV2.AddTags
import Amazonka.ELBV2.CreateListener
import Amazonka.ELBV2.CreateLoadBalancer
import Amazonka.ELBV2.CreateRule
import Amazonka.ELBV2.CreateTargetGroup
import Amazonka.ELBV2.DeleteListener
import Amazonka.ELBV2.DeleteLoadBalancer
import Amazonka.ELBV2.DeleteRule
import Amazonka.ELBV2.DeleteTargetGroup
import Amazonka.ELBV2.DeregisterTargets
import Amazonka.ELBV2.DescribeAccountLimits
import Amazonka.ELBV2.DescribeListenerCertificates
import Amazonka.ELBV2.DescribeListeners
import Amazonka.ELBV2.DescribeLoadBalancerAttributes
import Amazonka.ELBV2.DescribeLoadBalancers
import Amazonka.ELBV2.DescribeRules
import Amazonka.ELBV2.DescribeSSLPolicies
import Amazonka.ELBV2.DescribeTags
import Amazonka.ELBV2.DescribeTargetGroupAttributes
import Amazonka.ELBV2.DescribeTargetGroups
import Amazonka.ELBV2.DescribeTargetHealth
import Amazonka.ELBV2.Lens
import Amazonka.ELBV2.ModifyListener
import Amazonka.ELBV2.ModifyLoadBalancerAttributes
import Amazonka.ELBV2.ModifyRule
import Amazonka.ELBV2.ModifyTargetGroup
import Amazonka.ELBV2.ModifyTargetGroupAttributes
import Amazonka.ELBV2.RegisterTargets
import Amazonka.ELBV2.RemoveListenerCertificates
import Amazonka.ELBV2.RemoveTags
import Amazonka.ELBV2.SetIpAddressType
import Amazonka.ELBV2.SetRulePriorities
import Amazonka.ELBV2.SetSecurityGroups
import Amazonka.ELBV2.SetSubnets
import Amazonka.ELBV2.Types
import Amazonka.ELBV2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ELBV2'.

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
