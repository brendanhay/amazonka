{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.VPCLattice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-11-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon VPC Lattice is a fully managed application networking service
-- that you use to connect, secure, and monitor all of your services across
-- multiple accounts and virtual private clouds (VPCs). Amazon VPC Lattice
-- interconnects your microservices and legacy services within a logical
-- boundary, so that you can discover and manage them more efficiently. For
-- more information, see the
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/ Amazon VPC Lattice User Guide>
module Amazonka.VPCLattice
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchUpdateRule
    BatchUpdateRule (BatchUpdateRule'),
    newBatchUpdateRule,
    BatchUpdateRuleResponse (BatchUpdateRuleResponse'),
    newBatchUpdateRuleResponse,

    -- ** CreateAccessLogSubscription
    CreateAccessLogSubscription (CreateAccessLogSubscription'),
    newCreateAccessLogSubscription,
    CreateAccessLogSubscriptionResponse (CreateAccessLogSubscriptionResponse'),
    newCreateAccessLogSubscriptionResponse,

    -- ** CreateListener
    CreateListener (CreateListener'),
    newCreateListener,
    CreateListenerResponse (CreateListenerResponse'),
    newCreateListenerResponse,

    -- ** CreateRule
    CreateRule (CreateRule'),
    newCreateRule,
    CreateRuleResponse (CreateRuleResponse'),
    newCreateRuleResponse,

    -- ** CreateService
    CreateService (CreateService'),
    newCreateService,
    CreateServiceResponse (CreateServiceResponse'),
    newCreateServiceResponse,

    -- ** CreateServiceNetwork
    CreateServiceNetwork (CreateServiceNetwork'),
    newCreateServiceNetwork,
    CreateServiceNetworkResponse (CreateServiceNetworkResponse'),
    newCreateServiceNetworkResponse,

    -- ** CreateServiceNetworkServiceAssociation
    CreateServiceNetworkServiceAssociation (CreateServiceNetworkServiceAssociation'),
    newCreateServiceNetworkServiceAssociation,
    CreateServiceNetworkServiceAssociationResponse (CreateServiceNetworkServiceAssociationResponse'),
    newCreateServiceNetworkServiceAssociationResponse,

    -- ** CreateServiceNetworkVpcAssociation
    CreateServiceNetworkVpcAssociation (CreateServiceNetworkVpcAssociation'),
    newCreateServiceNetworkVpcAssociation,
    CreateServiceNetworkVpcAssociationResponse (CreateServiceNetworkVpcAssociationResponse'),
    newCreateServiceNetworkVpcAssociationResponse,

    -- ** CreateTargetGroup
    CreateTargetGroup (CreateTargetGroup'),
    newCreateTargetGroup,
    CreateTargetGroupResponse (CreateTargetGroupResponse'),
    newCreateTargetGroupResponse,

    -- ** DeleteAccessLogSubscription
    DeleteAccessLogSubscription (DeleteAccessLogSubscription'),
    newDeleteAccessLogSubscription,
    DeleteAccessLogSubscriptionResponse (DeleteAccessLogSubscriptionResponse'),
    newDeleteAccessLogSubscriptionResponse,

    -- ** DeleteAuthPolicy
    DeleteAuthPolicy (DeleteAuthPolicy'),
    newDeleteAuthPolicy,
    DeleteAuthPolicyResponse (DeleteAuthPolicyResponse'),
    newDeleteAuthPolicyResponse,

    -- ** DeleteListener
    DeleteListener (DeleteListener'),
    newDeleteListener,
    DeleteListenerResponse (DeleteListenerResponse'),
    newDeleteListenerResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeleteRule
    DeleteRule (DeleteRule'),
    newDeleteRule,
    DeleteRuleResponse (DeleteRuleResponse'),
    newDeleteRuleResponse,

    -- ** DeleteService
    DeleteService (DeleteService'),
    newDeleteService,
    DeleteServiceResponse (DeleteServiceResponse'),
    newDeleteServiceResponse,

    -- ** DeleteServiceNetwork
    DeleteServiceNetwork (DeleteServiceNetwork'),
    newDeleteServiceNetwork,
    DeleteServiceNetworkResponse (DeleteServiceNetworkResponse'),
    newDeleteServiceNetworkResponse,

    -- ** DeleteServiceNetworkServiceAssociation
    DeleteServiceNetworkServiceAssociation (DeleteServiceNetworkServiceAssociation'),
    newDeleteServiceNetworkServiceAssociation,
    DeleteServiceNetworkServiceAssociationResponse (DeleteServiceNetworkServiceAssociationResponse'),
    newDeleteServiceNetworkServiceAssociationResponse,

    -- ** DeleteServiceNetworkVpcAssociation
    DeleteServiceNetworkVpcAssociation (DeleteServiceNetworkVpcAssociation'),
    newDeleteServiceNetworkVpcAssociation,
    DeleteServiceNetworkVpcAssociationResponse (DeleteServiceNetworkVpcAssociationResponse'),
    newDeleteServiceNetworkVpcAssociationResponse,

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

    -- ** GetAccessLogSubscription
    GetAccessLogSubscription (GetAccessLogSubscription'),
    newGetAccessLogSubscription,
    GetAccessLogSubscriptionResponse (GetAccessLogSubscriptionResponse'),
    newGetAccessLogSubscriptionResponse,

    -- ** GetAuthPolicy
    GetAuthPolicy (GetAuthPolicy'),
    newGetAuthPolicy,
    GetAuthPolicyResponse (GetAuthPolicyResponse'),
    newGetAuthPolicyResponse,

    -- ** GetListener
    GetListener (GetListener'),
    newGetListener,
    GetListenerResponse (GetListenerResponse'),
    newGetListenerResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** GetRule
    GetRule (GetRule'),
    newGetRule,
    GetRuleResponse (GetRuleResponse'),
    newGetRuleResponse,

    -- ** GetService
    GetService (GetService'),
    newGetService,
    GetServiceResponse (GetServiceResponse'),
    newGetServiceResponse,

    -- ** GetServiceNetwork
    GetServiceNetwork (GetServiceNetwork'),
    newGetServiceNetwork,
    GetServiceNetworkResponse (GetServiceNetworkResponse'),
    newGetServiceNetworkResponse,

    -- ** GetServiceNetworkServiceAssociation
    GetServiceNetworkServiceAssociation (GetServiceNetworkServiceAssociation'),
    newGetServiceNetworkServiceAssociation,
    GetServiceNetworkServiceAssociationResponse (GetServiceNetworkServiceAssociationResponse'),
    newGetServiceNetworkServiceAssociationResponse,

    -- ** GetServiceNetworkVpcAssociation
    GetServiceNetworkVpcAssociation (GetServiceNetworkVpcAssociation'),
    newGetServiceNetworkVpcAssociation,
    GetServiceNetworkVpcAssociationResponse (GetServiceNetworkVpcAssociationResponse'),
    newGetServiceNetworkVpcAssociationResponse,

    -- ** GetTargetGroup
    GetTargetGroup (GetTargetGroup'),
    newGetTargetGroup,
    GetTargetGroupResponse (GetTargetGroupResponse'),
    newGetTargetGroupResponse,

    -- ** ListAccessLogSubscriptions (Paginated)
    ListAccessLogSubscriptions (ListAccessLogSubscriptions'),
    newListAccessLogSubscriptions,
    ListAccessLogSubscriptionsResponse (ListAccessLogSubscriptionsResponse'),
    newListAccessLogSubscriptionsResponse,

    -- ** ListListeners (Paginated)
    ListListeners (ListListeners'),
    newListListeners,
    ListListenersResponse (ListListenersResponse'),
    newListListenersResponse,

    -- ** ListRules (Paginated)
    ListRules (ListRules'),
    newListRules,
    ListRulesResponse (ListRulesResponse'),
    newListRulesResponse,

    -- ** ListServiceNetworkServiceAssociations (Paginated)
    ListServiceNetworkServiceAssociations (ListServiceNetworkServiceAssociations'),
    newListServiceNetworkServiceAssociations,
    ListServiceNetworkServiceAssociationsResponse (ListServiceNetworkServiceAssociationsResponse'),
    newListServiceNetworkServiceAssociationsResponse,

    -- ** ListServiceNetworkVpcAssociations (Paginated)
    ListServiceNetworkVpcAssociations (ListServiceNetworkVpcAssociations'),
    newListServiceNetworkVpcAssociations,
    ListServiceNetworkVpcAssociationsResponse (ListServiceNetworkVpcAssociationsResponse'),
    newListServiceNetworkVpcAssociationsResponse,

    -- ** ListServiceNetworks (Paginated)
    ListServiceNetworks (ListServiceNetworks'),
    newListServiceNetworks,
    ListServiceNetworksResponse (ListServiceNetworksResponse'),
    newListServiceNetworksResponse,

    -- ** ListServices (Paginated)
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTargetGroups (Paginated)
    ListTargetGroups (ListTargetGroups'),
    newListTargetGroups,
    ListTargetGroupsResponse (ListTargetGroupsResponse'),
    newListTargetGroupsResponse,

    -- ** ListTargets (Paginated)
    ListTargets (ListTargets'),
    newListTargets,
    ListTargetsResponse (ListTargetsResponse'),
    newListTargetsResponse,

    -- ** PutAuthPolicy
    PutAuthPolicy (PutAuthPolicy'),
    newPutAuthPolicy,
    PutAuthPolicyResponse (PutAuthPolicyResponse'),
    newPutAuthPolicyResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** RegisterTargets
    RegisterTargets (RegisterTargets'),
    newRegisterTargets,
    RegisterTargetsResponse (RegisterTargetsResponse'),
    newRegisterTargetsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAccessLogSubscription
    UpdateAccessLogSubscription (UpdateAccessLogSubscription'),
    newUpdateAccessLogSubscription,
    UpdateAccessLogSubscriptionResponse (UpdateAccessLogSubscriptionResponse'),
    newUpdateAccessLogSubscriptionResponse,

    -- ** UpdateListener
    UpdateListener (UpdateListener'),
    newUpdateListener,
    UpdateListenerResponse (UpdateListenerResponse'),
    newUpdateListenerResponse,

    -- ** UpdateRule
    UpdateRule (UpdateRule'),
    newUpdateRule,
    UpdateRuleResponse (UpdateRuleResponse'),
    newUpdateRuleResponse,

    -- ** UpdateService
    UpdateService (UpdateService'),
    newUpdateService,
    UpdateServiceResponse (UpdateServiceResponse'),
    newUpdateServiceResponse,

    -- ** UpdateServiceNetwork
    UpdateServiceNetwork (UpdateServiceNetwork'),
    newUpdateServiceNetwork,
    UpdateServiceNetworkResponse (UpdateServiceNetworkResponse'),
    newUpdateServiceNetworkResponse,

    -- ** UpdateServiceNetworkVpcAssociation
    UpdateServiceNetworkVpcAssociation (UpdateServiceNetworkVpcAssociation'),
    newUpdateServiceNetworkVpcAssociation,
    UpdateServiceNetworkVpcAssociationResponse (UpdateServiceNetworkVpcAssociationResponse'),
    newUpdateServiceNetworkVpcAssociationResponse,

    -- ** UpdateTargetGroup
    UpdateTargetGroup (UpdateTargetGroup'),
    newUpdateTargetGroup,
    UpdateTargetGroupResponse (UpdateTargetGroupResponse'),
    newUpdateTargetGroupResponse,

    -- * Types

    -- ** AuthPolicyState
    AuthPolicyState (..),

    -- ** AuthType
    AuthType (..),

    -- ** HealthCheckProtocolVersion
    HealthCheckProtocolVersion (..),

    -- ** IpAddressType
    IpAddressType (..),

    -- ** ListenerProtocol
    ListenerProtocol (..),

    -- ** ServiceNetworkServiceAssociationStatus
    ServiceNetworkServiceAssociationStatus (..),

    -- ** ServiceNetworkVpcAssociationStatus
    ServiceNetworkVpcAssociationStatus (..),

    -- ** ServiceStatus
    ServiceStatus (..),

    -- ** TargetGroupProtocol
    TargetGroupProtocol (..),

    -- ** TargetGroupProtocolVersion
    TargetGroupProtocolVersion (..),

    -- ** TargetGroupStatus
    TargetGroupStatus (..),

    -- ** TargetGroupType
    TargetGroupType (..),

    -- ** TargetStatus
    TargetStatus (..),

    -- ** AccessLogSubscriptionSummary
    AccessLogSubscriptionSummary (AccessLogSubscriptionSummary'),
    newAccessLogSubscriptionSummary,

    -- ** DnsEntry
    DnsEntry (DnsEntry'),
    newDnsEntry,

    -- ** FixedResponseAction
    FixedResponseAction (FixedResponseAction'),
    newFixedResponseAction,

    -- ** ForwardAction
    ForwardAction (ForwardAction'),
    newForwardAction,

    -- ** HeaderMatch
    HeaderMatch (HeaderMatch'),
    newHeaderMatch,

    -- ** HeaderMatchType
    HeaderMatchType (HeaderMatchType'),
    newHeaderMatchType,

    -- ** HealthCheckConfig
    HealthCheckConfig (HealthCheckConfig'),
    newHealthCheckConfig,

    -- ** HttpMatch
    HttpMatch (HttpMatch'),
    newHttpMatch,

    -- ** ListenerSummary
    ListenerSummary (ListenerSummary'),
    newListenerSummary,

    -- ** Matcher
    Matcher (Matcher'),
    newMatcher,

    -- ** PathMatch
    PathMatch (PathMatch'),
    newPathMatch,

    -- ** PathMatchType
    PathMatchType (PathMatchType'),
    newPathMatchType,

    -- ** RuleAction
    RuleAction (RuleAction'),
    newRuleAction,

    -- ** RuleMatch
    RuleMatch (RuleMatch'),
    newRuleMatch,

    -- ** RuleSummary
    RuleSummary (RuleSummary'),
    newRuleSummary,

    -- ** RuleUpdate
    RuleUpdate (RuleUpdate'),
    newRuleUpdate,

    -- ** RuleUpdateFailure
    RuleUpdateFailure (RuleUpdateFailure'),
    newRuleUpdateFailure,

    -- ** RuleUpdateSuccess
    RuleUpdateSuccess (RuleUpdateSuccess'),
    newRuleUpdateSuccess,

    -- ** ServiceNetworkServiceAssociationSummary
    ServiceNetworkServiceAssociationSummary (ServiceNetworkServiceAssociationSummary'),
    newServiceNetworkServiceAssociationSummary,

    -- ** ServiceNetworkSummary
    ServiceNetworkSummary (ServiceNetworkSummary'),
    newServiceNetworkSummary,

    -- ** ServiceNetworkVpcAssociationSummary
    ServiceNetworkVpcAssociationSummary (ServiceNetworkVpcAssociationSummary'),
    newServiceNetworkVpcAssociationSummary,

    -- ** ServiceSummary
    ServiceSummary (ServiceSummary'),
    newServiceSummary,

    -- ** Target
    Target (Target'),
    newTarget,

    -- ** TargetFailure
    TargetFailure (TargetFailure'),
    newTargetFailure,

    -- ** TargetGroupConfig
    TargetGroupConfig (TargetGroupConfig'),
    newTargetGroupConfig,

    -- ** TargetGroupSummary
    TargetGroupSummary (TargetGroupSummary'),
    newTargetGroupSummary,

    -- ** TargetSummary
    TargetSummary (TargetSummary'),
    newTargetSummary,

    -- ** WeightedTargetGroup
    WeightedTargetGroup (WeightedTargetGroup'),
    newWeightedTargetGroup,
  )
where

import Amazonka.VPCLattice.BatchUpdateRule
import Amazonka.VPCLattice.CreateAccessLogSubscription
import Amazonka.VPCLattice.CreateListener
import Amazonka.VPCLattice.CreateRule
import Amazonka.VPCLattice.CreateService
import Amazonka.VPCLattice.CreateServiceNetwork
import Amazonka.VPCLattice.CreateServiceNetworkServiceAssociation
import Amazonka.VPCLattice.CreateServiceNetworkVpcAssociation
import Amazonka.VPCLattice.CreateTargetGroup
import Amazonka.VPCLattice.DeleteAccessLogSubscription
import Amazonka.VPCLattice.DeleteAuthPolicy
import Amazonka.VPCLattice.DeleteListener
import Amazonka.VPCLattice.DeleteResourcePolicy
import Amazonka.VPCLattice.DeleteRule
import Amazonka.VPCLattice.DeleteService
import Amazonka.VPCLattice.DeleteServiceNetwork
import Amazonka.VPCLattice.DeleteServiceNetworkServiceAssociation
import Amazonka.VPCLattice.DeleteServiceNetworkVpcAssociation
import Amazonka.VPCLattice.DeleteTargetGroup
import Amazonka.VPCLattice.DeregisterTargets
import Amazonka.VPCLattice.GetAccessLogSubscription
import Amazonka.VPCLattice.GetAuthPolicy
import Amazonka.VPCLattice.GetListener
import Amazonka.VPCLattice.GetResourcePolicy
import Amazonka.VPCLattice.GetRule
import Amazonka.VPCLattice.GetService
import Amazonka.VPCLattice.GetServiceNetwork
import Amazonka.VPCLattice.GetServiceNetworkServiceAssociation
import Amazonka.VPCLattice.GetServiceNetworkVpcAssociation
import Amazonka.VPCLattice.GetTargetGroup
import Amazonka.VPCLattice.Lens
import Amazonka.VPCLattice.ListAccessLogSubscriptions
import Amazonka.VPCLattice.ListListeners
import Amazonka.VPCLattice.ListRules
import Amazonka.VPCLattice.ListServiceNetworkServiceAssociations
import Amazonka.VPCLattice.ListServiceNetworkVpcAssociations
import Amazonka.VPCLattice.ListServiceNetworks
import Amazonka.VPCLattice.ListServices
import Amazonka.VPCLattice.ListTagsForResource
import Amazonka.VPCLattice.ListTargetGroups
import Amazonka.VPCLattice.ListTargets
import Amazonka.VPCLattice.PutAuthPolicy
import Amazonka.VPCLattice.PutResourcePolicy
import Amazonka.VPCLattice.RegisterTargets
import Amazonka.VPCLattice.TagResource
import Amazonka.VPCLattice.Types
import Amazonka.VPCLattice.UntagResource
import Amazonka.VPCLattice.UpdateAccessLogSubscription
import Amazonka.VPCLattice.UpdateListener
import Amazonka.VPCLattice.UpdateRule
import Amazonka.VPCLattice.UpdateService
import Amazonka.VPCLattice.UpdateServiceNetwork
import Amazonka.VPCLattice.UpdateServiceNetworkVpcAssociation
import Amazonka.VPCLattice.UpdateTargetGroup
import Amazonka.VPCLattice.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'VPCLattice'.

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
