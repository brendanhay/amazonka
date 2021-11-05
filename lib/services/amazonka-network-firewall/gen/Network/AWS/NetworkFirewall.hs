{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.NetworkFirewall
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-12@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the API Reference for AWS Network Firewall. This guide is for
-- developers who need detailed information about the Network Firewall API
-- actions, data types, and errors.
--
-- -   The REST API requires you to handle connection details, such as
--     calculating signatures, handling request retries, and error
--     handling. For general information about using the AWS REST APIs, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws-apis.html AWS APIs>.
--
--     To access Network Firewall using the REST API endpoint:
--     @https:\/\/network-firewall.\<region>.amazonaws.com @
--
-- -   Alternatively, you can use one of the AWS SDKs to access an API
--     that\'s tailored to the programming language or platform that
--     you\'re using. For more information, see
--     <http://aws.amazon.com/tools/#SDKs AWS SDKs>.
--
-- -   For descriptions of Network Firewall features, including and
--     step-by-step instructions on how to use them through the Network
--     Firewall console, see the
--     <https://docs.aws.amazon.com/network-firewall/latest/developerguide/ Network Firewall Developer Guide>.
--
-- Network Firewall is a stateful, managed, network firewall and intrusion
-- detection and prevention service for Amazon Virtual Private Cloud
-- (Amazon VPC). With Network Firewall, you can filter traffic at the
-- perimeter of your VPC. This includes filtering traffic going to and
-- coming from an internet gateway, NAT gateway, or over VPN or AWS Direct
-- Connect. Network Firewall uses rules that are compatible with Suricata,
-- a free, open source intrusion detection system (IDS) engine. AWS Network
-- Firewall supports Suricata version 5.0.2. For information about
-- Suricata, see the <https://suricata-ids.org/ Suricata website>.
--
-- You can use Network Firewall to monitor and protect your VPC traffic in
-- a number of ways. The following are just a few examples:
--
-- -   Allow domains or IP addresses for known AWS service endpoints, such
--     as Amazon S3, and block all other forms of traffic.
--
-- -   Use custom lists of known bad domains to limit the types of domain
--     names that your applications can access.
--
-- -   Perform deep packet inspection on traffic entering or leaving your
--     VPC.
--
-- -   Use stateful protocol detection to filter protocols like HTTPS,
--     regardless of the port used.
--
-- To enable Network Firewall for your VPCs, you perform steps in both
-- Amazon VPC and in Network Firewall. For information about using Amazon
-- VPC, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon VPC User Guide>.
--
-- To start using Network Firewall, do the following:
--
-- 1.  (Optional) If you don\'t already have a VPC that you want to
--     protect, create it in Amazon VPC.
--
-- 2.  In Amazon VPC, in each Availability Zone where you want to have a
--     firewall endpoint, create a subnet for the sole use of Network
--     Firewall.
--
-- 3.  In Network Firewall, create stateless and stateful rule groups, to
--     define the components of the network traffic filtering behavior that
--     you want your firewall to have.
--
-- 4.  In Network Firewall, create a firewall policy that uses your rule
--     groups and specifies additional default traffic filtering behavior.
--
-- 5.  In Network Firewall, create a firewall and specify your new firewall
--     policy and VPC subnets. Network Firewall creates a firewall endpoint
--     in each subnet that you specify, with the behavior that\'s defined
--     in the firewall policy.
--
-- 6.  In Amazon VPC, use ingress routing enhancements to route traffic
--     through the new firewall endpoints.
module Network.AWS.NetworkFirewall
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** LogDestinationPermissionException
    _LogDestinationPermissionException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** ResourceOwnerCheckException
    _ResourceOwnerCheckException,

    -- ** InvalidResourcePolicyException
    _InvalidResourcePolicyException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidTokenException
    _InvalidTokenException,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** InsufficientCapacityException
    _InsufficientCapacityException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateSubnets
    AssociateSubnets (AssociateSubnets'),
    newAssociateSubnets,
    AssociateSubnetsResponse (AssociateSubnetsResponse'),
    newAssociateSubnetsResponse,

    -- ** UpdateSubnetChangeProtection
    UpdateSubnetChangeProtection (UpdateSubnetChangeProtection'),
    newUpdateSubnetChangeProtection,
    UpdateSubnetChangeProtectionResponse (UpdateSubnetChangeProtectionResponse'),
    newUpdateSubnetChangeProtectionResponse,

    -- ** UpdateFirewallPolicy
    UpdateFirewallPolicy (UpdateFirewallPolicy'),
    newUpdateFirewallPolicy,
    UpdateFirewallPolicyResponse (UpdateFirewallPolicyResponse'),
    newUpdateFirewallPolicyResponse,

    -- ** DeleteFirewallPolicy
    DeleteFirewallPolicy (DeleteFirewallPolicy'),
    newDeleteFirewallPolicy,
    DeleteFirewallPolicyResponse (DeleteFirewallPolicyResponse'),
    newDeleteFirewallPolicyResponse,

    -- ** CreateFirewallPolicy
    CreateFirewallPolicy (CreateFirewallPolicy'),
    newCreateFirewallPolicy,
    CreateFirewallPolicyResponse (CreateFirewallPolicyResponse'),
    newCreateFirewallPolicyResponse,

    -- ** UpdateLoggingConfiguration
    UpdateLoggingConfiguration (UpdateLoggingConfiguration'),
    newUpdateLoggingConfiguration,
    UpdateLoggingConfigurationResponse (UpdateLoggingConfigurationResponse'),
    newUpdateLoggingConfigurationResponse,

    -- ** DisassociateSubnets
    DisassociateSubnets (DisassociateSubnets'),
    newDisassociateSubnets,
    DisassociateSubnetsResponse (DisassociateSubnetsResponse'),
    newDisassociateSubnetsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListFirewallPolicies (Paginated)
    ListFirewallPolicies (ListFirewallPolicies'),
    newListFirewallPolicies,
    ListFirewallPoliciesResponse (ListFirewallPoliciesResponse'),
    newListFirewallPoliciesResponse,

    -- ** UpdateFirewallDeleteProtection
    UpdateFirewallDeleteProtection (UpdateFirewallDeleteProtection'),
    newUpdateFirewallDeleteProtection,
    UpdateFirewallDeleteProtectionResponse (UpdateFirewallDeleteProtectionResponse'),
    newUpdateFirewallDeleteProtectionResponse,

    -- ** CreateRuleGroup
    CreateRuleGroup (CreateRuleGroup'),
    newCreateRuleGroup,
    CreateRuleGroupResponse (CreateRuleGroupResponse'),
    newCreateRuleGroupResponse,

    -- ** DescribeFirewallPolicy
    DescribeFirewallPolicy (DescribeFirewallPolicy'),
    newDescribeFirewallPolicy,
    DescribeFirewallPolicyResponse (DescribeFirewallPolicyResponse'),
    newDescribeFirewallPolicyResponse,

    -- ** UpdateFirewallDescription
    UpdateFirewallDescription (UpdateFirewallDescription'),
    newUpdateFirewallDescription,
    UpdateFirewallDescriptionResponse (UpdateFirewallDescriptionResponse'),
    newUpdateFirewallDescriptionResponse,

    -- ** DescribeRuleGroup
    DescribeRuleGroup (DescribeRuleGroup'),
    newDescribeRuleGroup,
    DescribeRuleGroupResponse (DescribeRuleGroupResponse'),
    newDescribeRuleGroupResponse,

    -- ** DeleteFirewall
    DeleteFirewall (DeleteFirewall'),
    newDeleteFirewall,
    DeleteFirewallResponse (DeleteFirewallResponse'),
    newDeleteFirewallResponse,

    -- ** ListFirewalls (Paginated)
    ListFirewalls (ListFirewalls'),
    newListFirewalls,
    ListFirewallsResponse (ListFirewallsResponse'),
    newListFirewallsResponse,

    -- ** DescribeResourcePolicy
    DescribeResourcePolicy (DescribeResourcePolicy'),
    newDescribeResourcePolicy,
    DescribeResourcePolicyResponse (DescribeResourcePolicyResponse'),
    newDescribeResourcePolicyResponse,

    -- ** AssociateFirewallPolicy
    AssociateFirewallPolicy (AssociateFirewallPolicy'),
    newAssociateFirewallPolicy,
    AssociateFirewallPolicyResponse (AssociateFirewallPolicyResponse'),
    newAssociateFirewallPolicyResponse,

    -- ** UpdateFirewallPolicyChangeProtection
    UpdateFirewallPolicyChangeProtection (UpdateFirewallPolicyChangeProtection'),
    newUpdateFirewallPolicyChangeProtection,
    UpdateFirewallPolicyChangeProtectionResponse (UpdateFirewallPolicyChangeProtectionResponse'),
    newUpdateFirewallPolicyChangeProtectionResponse,

    -- ** CreateFirewall
    CreateFirewall (CreateFirewall'),
    newCreateFirewall,
    CreateFirewallResponse (CreateFirewallResponse'),
    newCreateFirewallResponse,

    -- ** ListRuleGroups (Paginated)
    ListRuleGroups (ListRuleGroups'),
    newListRuleGroups,
    ListRuleGroupsResponse (ListRuleGroupsResponse'),
    newListRuleGroupsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeleteRuleGroup
    DeleteRuleGroup (DeleteRuleGroup'),
    newDeleteRuleGroup,
    DeleteRuleGroupResponse (DeleteRuleGroupResponse'),
    newDeleteRuleGroupResponse,

    -- ** UpdateRuleGroup
    UpdateRuleGroup (UpdateRuleGroup'),
    newUpdateRuleGroup,
    UpdateRuleGroupResponse (UpdateRuleGroupResponse'),
    newUpdateRuleGroupResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** DescribeFirewall
    DescribeFirewall (DescribeFirewall'),
    newDescribeFirewall,
    DescribeFirewallResponse (DescribeFirewallResponse'),
    newDescribeFirewallResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeLoggingConfiguration
    DescribeLoggingConfiguration (DescribeLoggingConfiguration'),
    newDescribeLoggingConfiguration,
    DescribeLoggingConfigurationResponse (DescribeLoggingConfigurationResponse'),
    newDescribeLoggingConfigurationResponse,

    -- * Types

    -- ** AttachmentStatus
    AttachmentStatus (..),

    -- ** ConfigurationSyncState
    ConfigurationSyncState (..),

    -- ** FirewallStatusValue
    FirewallStatusValue (..),

    -- ** GeneratedRulesType
    GeneratedRulesType (..),

    -- ** LogDestinationType
    LogDestinationType (..),

    -- ** LogType
    LogType (..),

    -- ** PerObjectSyncStatus
    PerObjectSyncStatus (..),

    -- ** ResourceStatus
    ResourceStatus (..),

    -- ** RuleGroupType
    RuleGroupType (..),

    -- ** RuleOrder
    RuleOrder (..),

    -- ** StatefulAction
    StatefulAction (..),

    -- ** StatefulRuleDirection
    StatefulRuleDirection (..),

    -- ** StatefulRuleProtocol
    StatefulRuleProtocol (..),

    -- ** TCPFlag
    TCPFlag (..),

    -- ** TargetType
    TargetType (..),

    -- ** ActionDefinition
    ActionDefinition (ActionDefinition'),
    newActionDefinition,

    -- ** Address
    Address (Address'),
    newAddress,

    -- ** Attachment
    Attachment (Attachment'),
    newAttachment,

    -- ** CustomAction
    CustomAction (CustomAction'),
    newCustomAction,

    -- ** Dimension
    Dimension (Dimension'),
    newDimension,

    -- ** Firewall
    Firewall (Firewall'),
    newFirewall,

    -- ** FirewallMetadata
    FirewallMetadata (FirewallMetadata'),
    newFirewallMetadata,

    -- ** FirewallPolicy
    FirewallPolicy (FirewallPolicy'),
    newFirewallPolicy,

    -- ** FirewallPolicyMetadata
    FirewallPolicyMetadata (FirewallPolicyMetadata'),
    newFirewallPolicyMetadata,

    -- ** FirewallPolicyResponse
    FirewallPolicyResponse (FirewallPolicyResponse'),
    newFirewallPolicyResponse,

    -- ** FirewallStatus
    FirewallStatus (FirewallStatus'),
    newFirewallStatus,

    -- ** Header
    Header (Header'),
    newHeader,

    -- ** IPSet
    IPSet (IPSet'),
    newIPSet,

    -- ** LogDestinationConfig
    LogDestinationConfig (LogDestinationConfig'),
    newLogDestinationConfig,

    -- ** LoggingConfiguration
    LoggingConfiguration (LoggingConfiguration'),
    newLoggingConfiguration,

    -- ** MatchAttributes
    MatchAttributes (MatchAttributes'),
    newMatchAttributes,

    -- ** PerObjectStatus
    PerObjectStatus (PerObjectStatus'),
    newPerObjectStatus,

    -- ** PortRange
    PortRange (PortRange'),
    newPortRange,

    -- ** PortSet
    PortSet (PortSet'),
    newPortSet,

    -- ** PublishMetricAction
    PublishMetricAction (PublishMetricAction'),
    newPublishMetricAction,

    -- ** RuleDefinition
    RuleDefinition (RuleDefinition'),
    newRuleDefinition,

    -- ** RuleGroup
    RuleGroup (RuleGroup'),
    newRuleGroup,

    -- ** RuleGroupMetadata
    RuleGroupMetadata (RuleGroupMetadata'),
    newRuleGroupMetadata,

    -- ** RuleGroupResponse
    RuleGroupResponse (RuleGroupResponse'),
    newRuleGroupResponse,

    -- ** RuleOption
    RuleOption (RuleOption'),
    newRuleOption,

    -- ** RuleVariables
    RuleVariables (RuleVariables'),
    newRuleVariables,

    -- ** RulesSource
    RulesSource (RulesSource'),
    newRulesSource,

    -- ** RulesSourceList
    RulesSourceList (RulesSourceList'),
    newRulesSourceList,

    -- ** StatefulEngineOptions
    StatefulEngineOptions (StatefulEngineOptions'),
    newStatefulEngineOptions,

    -- ** StatefulRule
    StatefulRule (StatefulRule'),
    newStatefulRule,

    -- ** StatefulRuleGroupReference
    StatefulRuleGroupReference (StatefulRuleGroupReference'),
    newStatefulRuleGroupReference,

    -- ** StatefulRuleOptions
    StatefulRuleOptions (StatefulRuleOptions'),
    newStatefulRuleOptions,

    -- ** StatelessRule
    StatelessRule (StatelessRule'),
    newStatelessRule,

    -- ** StatelessRuleGroupReference
    StatelessRuleGroupReference (StatelessRuleGroupReference'),
    newStatelessRuleGroupReference,

    -- ** StatelessRulesAndCustomActions
    StatelessRulesAndCustomActions (StatelessRulesAndCustomActions'),
    newStatelessRulesAndCustomActions,

    -- ** SubnetMapping
    SubnetMapping (SubnetMapping'),
    newSubnetMapping,

    -- ** SyncState
    SyncState (SyncState'),
    newSyncState,

    -- ** TCPFlagField
    TCPFlagField (TCPFlagField'),
    newTCPFlagField,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.NetworkFirewall.AssociateFirewallPolicy
import Network.AWS.NetworkFirewall.AssociateSubnets
import Network.AWS.NetworkFirewall.CreateFirewall
import Network.AWS.NetworkFirewall.CreateFirewallPolicy
import Network.AWS.NetworkFirewall.CreateRuleGroup
import Network.AWS.NetworkFirewall.DeleteFirewall
import Network.AWS.NetworkFirewall.DeleteFirewallPolicy
import Network.AWS.NetworkFirewall.DeleteResourcePolicy
import Network.AWS.NetworkFirewall.DeleteRuleGroup
import Network.AWS.NetworkFirewall.DescribeFirewall
import Network.AWS.NetworkFirewall.DescribeFirewallPolicy
import Network.AWS.NetworkFirewall.DescribeLoggingConfiguration
import Network.AWS.NetworkFirewall.DescribeResourcePolicy
import Network.AWS.NetworkFirewall.DescribeRuleGroup
import Network.AWS.NetworkFirewall.DisassociateSubnets
import Network.AWS.NetworkFirewall.Lens
import Network.AWS.NetworkFirewall.ListFirewallPolicies
import Network.AWS.NetworkFirewall.ListFirewalls
import Network.AWS.NetworkFirewall.ListRuleGroups
import Network.AWS.NetworkFirewall.ListTagsForResource
import Network.AWS.NetworkFirewall.PutResourcePolicy
import Network.AWS.NetworkFirewall.TagResource
import Network.AWS.NetworkFirewall.Types
import Network.AWS.NetworkFirewall.UntagResource
import Network.AWS.NetworkFirewall.UpdateFirewallDeleteProtection
import Network.AWS.NetworkFirewall.UpdateFirewallDescription
import Network.AWS.NetworkFirewall.UpdateFirewallPolicy
import Network.AWS.NetworkFirewall.UpdateFirewallPolicyChangeProtection
import Network.AWS.NetworkFirewall.UpdateLoggingConfiguration
import Network.AWS.NetworkFirewall.UpdateRuleGroup
import Network.AWS.NetworkFirewall.UpdateSubnetChangeProtection
import Network.AWS.NetworkFirewall.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'NetworkFirewall'.

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
