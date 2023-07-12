{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.NetworkFirewall
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-12@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the API Reference for Network Firewall. This guide is for
-- developers who need detailed information about the Network Firewall API
-- actions, data types, and errors.
--
-- -   The REST API requires you to handle connection details, such as
--     calculating signatures, handling request retries, and error
--     handling. For general information about using the Amazon Web
--     Services REST APIs, see
--     <https://docs.aws.amazon.com/general/latest/gr/aws-apis.html Amazon Web Services APIs>.
--
--     To access Network Firewall using the REST API endpoint:
--     @https:\/\/network-firewall.\<region>.amazonaws.com @
--
-- -   Alternatively, you can use one of the Amazon Web Services SDKs to
--     access an API that\'s tailored to the programming language or
--     platform that you\'re using. For more information, see
--     <http://aws.amazon.com/tools/#SDKs Amazon Web Services SDKs>.
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
-- coming from an internet gateway, NAT gateway, or over VPN or Direct
-- Connect. Network Firewall uses rules that are compatible with Suricata,
-- a free, open source network analysis and threat detection engine.
-- Network Firewall supports Suricata version 5.0.2. For information about
-- Suricata, see the <https://suricata.io/ Suricata website>.
--
-- You can use Network Firewall to monitor and protect your VPC traffic in
-- a number of ways. The following are just a few examples:
--
-- -   Allow domains or IP addresses for known Amazon Web Services service
--     endpoints, such as Amazon S3, and block all other forms of traffic.
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
module Amazonka.NetworkFirewall
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InsufficientCapacityException
    _InsufficientCapacityException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidOperationException
    _InvalidOperationException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidResourcePolicyException
    _InvalidResourcePolicyException,

    -- ** InvalidTokenException
    _InvalidTokenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** LogDestinationPermissionException
    _LogDestinationPermissionException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceOwnerCheckException
    _ResourceOwnerCheckException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateFirewallPolicy
    AssociateFirewallPolicy (AssociateFirewallPolicy'),
    newAssociateFirewallPolicy,
    AssociateFirewallPolicyResponse (AssociateFirewallPolicyResponse'),
    newAssociateFirewallPolicyResponse,

    -- ** AssociateSubnets
    AssociateSubnets (AssociateSubnets'),
    newAssociateSubnets,
    AssociateSubnetsResponse (AssociateSubnetsResponse'),
    newAssociateSubnetsResponse,

    -- ** CreateFirewall
    CreateFirewall (CreateFirewall'),
    newCreateFirewall,
    CreateFirewallResponse (CreateFirewallResponse'),
    newCreateFirewallResponse,

    -- ** CreateFirewallPolicy
    CreateFirewallPolicy (CreateFirewallPolicy'),
    newCreateFirewallPolicy,
    CreateFirewallPolicyResponse (CreateFirewallPolicyResponse'),
    newCreateFirewallPolicyResponse,

    -- ** CreateRuleGroup
    CreateRuleGroup (CreateRuleGroup'),
    newCreateRuleGroup,
    CreateRuleGroupResponse (CreateRuleGroupResponse'),
    newCreateRuleGroupResponse,

    -- ** DeleteFirewall
    DeleteFirewall (DeleteFirewall'),
    newDeleteFirewall,
    DeleteFirewallResponse (DeleteFirewallResponse'),
    newDeleteFirewallResponse,

    -- ** DeleteFirewallPolicy
    DeleteFirewallPolicy (DeleteFirewallPolicy'),
    newDeleteFirewallPolicy,
    DeleteFirewallPolicyResponse (DeleteFirewallPolicyResponse'),
    newDeleteFirewallPolicyResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeleteRuleGroup
    DeleteRuleGroup (DeleteRuleGroup'),
    newDeleteRuleGroup,
    DeleteRuleGroupResponse (DeleteRuleGroupResponse'),
    newDeleteRuleGroupResponse,

    -- ** DescribeFirewall
    DescribeFirewall (DescribeFirewall'),
    newDescribeFirewall,
    DescribeFirewallResponse (DescribeFirewallResponse'),
    newDescribeFirewallResponse,

    -- ** DescribeFirewallPolicy
    DescribeFirewallPolicy (DescribeFirewallPolicy'),
    newDescribeFirewallPolicy,
    DescribeFirewallPolicyResponse (DescribeFirewallPolicyResponse'),
    newDescribeFirewallPolicyResponse,

    -- ** DescribeLoggingConfiguration
    DescribeLoggingConfiguration (DescribeLoggingConfiguration'),
    newDescribeLoggingConfiguration,
    DescribeLoggingConfigurationResponse (DescribeLoggingConfigurationResponse'),
    newDescribeLoggingConfigurationResponse,

    -- ** DescribeResourcePolicy
    DescribeResourcePolicy (DescribeResourcePolicy'),
    newDescribeResourcePolicy,
    DescribeResourcePolicyResponse (DescribeResourcePolicyResponse'),
    newDescribeResourcePolicyResponse,

    -- ** DescribeRuleGroup
    DescribeRuleGroup (DescribeRuleGroup'),
    newDescribeRuleGroup,
    DescribeRuleGroupResponse (DescribeRuleGroupResponse'),
    newDescribeRuleGroupResponse,

    -- ** DescribeRuleGroupMetadata
    DescribeRuleGroupMetadata (DescribeRuleGroupMetadata'),
    newDescribeRuleGroupMetadata,
    DescribeRuleGroupMetadataResponse (DescribeRuleGroupMetadataResponse'),
    newDescribeRuleGroupMetadataResponse,

    -- ** DisassociateSubnets
    DisassociateSubnets (DisassociateSubnets'),
    newDisassociateSubnets,
    DisassociateSubnetsResponse (DisassociateSubnetsResponse'),
    newDisassociateSubnetsResponse,

    -- ** ListFirewallPolicies (Paginated)
    ListFirewallPolicies (ListFirewallPolicies'),
    newListFirewallPolicies,
    ListFirewallPoliciesResponse (ListFirewallPoliciesResponse'),
    newListFirewallPoliciesResponse,

    -- ** ListFirewalls (Paginated)
    ListFirewalls (ListFirewalls'),
    newListFirewalls,
    ListFirewallsResponse (ListFirewallsResponse'),
    newListFirewallsResponse,

    -- ** ListRuleGroups (Paginated)
    ListRuleGroups (ListRuleGroups'),
    newListRuleGroups,
    ListRuleGroupsResponse (ListRuleGroupsResponse'),
    newListRuleGroupsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

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

    -- ** UpdateFirewallDeleteProtection
    UpdateFirewallDeleteProtection (UpdateFirewallDeleteProtection'),
    newUpdateFirewallDeleteProtection,
    UpdateFirewallDeleteProtectionResponse (UpdateFirewallDeleteProtectionResponse'),
    newUpdateFirewallDeleteProtectionResponse,

    -- ** UpdateFirewallDescription
    UpdateFirewallDescription (UpdateFirewallDescription'),
    newUpdateFirewallDescription,
    UpdateFirewallDescriptionResponse (UpdateFirewallDescriptionResponse'),
    newUpdateFirewallDescriptionResponse,

    -- ** UpdateFirewallEncryptionConfiguration
    UpdateFirewallEncryptionConfiguration (UpdateFirewallEncryptionConfiguration'),
    newUpdateFirewallEncryptionConfiguration,
    UpdateFirewallEncryptionConfigurationResponse (UpdateFirewallEncryptionConfigurationResponse'),
    newUpdateFirewallEncryptionConfigurationResponse,

    -- ** UpdateFirewallPolicy
    UpdateFirewallPolicy (UpdateFirewallPolicy'),
    newUpdateFirewallPolicy,
    UpdateFirewallPolicyResponse (UpdateFirewallPolicyResponse'),
    newUpdateFirewallPolicyResponse,

    -- ** UpdateFirewallPolicyChangeProtection
    UpdateFirewallPolicyChangeProtection (UpdateFirewallPolicyChangeProtection'),
    newUpdateFirewallPolicyChangeProtection,
    UpdateFirewallPolicyChangeProtectionResponse (UpdateFirewallPolicyChangeProtectionResponse'),
    newUpdateFirewallPolicyChangeProtectionResponse,

    -- ** UpdateLoggingConfiguration
    UpdateLoggingConfiguration (UpdateLoggingConfiguration'),
    newUpdateLoggingConfiguration,
    UpdateLoggingConfigurationResponse (UpdateLoggingConfigurationResponse'),
    newUpdateLoggingConfigurationResponse,

    -- ** UpdateRuleGroup
    UpdateRuleGroup (UpdateRuleGroup'),
    newUpdateRuleGroup,
    UpdateRuleGroupResponse (UpdateRuleGroupResponse'),
    newUpdateRuleGroupResponse,

    -- ** UpdateSubnetChangeProtection
    UpdateSubnetChangeProtection (UpdateSubnetChangeProtection'),
    newUpdateSubnetChangeProtection,
    UpdateSubnetChangeProtectionResponse (UpdateSubnetChangeProtectionResponse'),
    newUpdateSubnetChangeProtectionResponse,

    -- * Types

    -- ** AttachmentStatus
    AttachmentStatus (..),

    -- ** ConfigurationSyncState
    ConfigurationSyncState (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** FirewallStatusValue
    FirewallStatusValue (..),

    -- ** GeneratedRulesType
    GeneratedRulesType (..),

    -- ** LogDestinationType
    LogDestinationType (..),

    -- ** LogType
    LogType (..),

    -- ** OverrideAction
    OverrideAction (..),

    -- ** PerObjectSyncStatus
    PerObjectSyncStatus (..),

    -- ** ResourceManagedStatus
    ResourceManagedStatus (..),

    -- ** ResourceManagedType
    ResourceManagedType (..),

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

    -- ** StreamExceptionPolicy
    StreamExceptionPolicy (..),

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

    -- ** CIDRSummary
    CIDRSummary (CIDRSummary'),
    newCIDRSummary,

    -- ** CapacityUsageSummary
    CapacityUsageSummary (CapacityUsageSummary'),
    newCapacityUsageSummary,

    -- ** CustomAction
    CustomAction (CustomAction'),
    newCustomAction,

    -- ** Dimension
    Dimension (Dimension'),
    newDimension,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (EncryptionConfiguration'),
    newEncryptionConfiguration,

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

    -- ** IPSetMetadata
    IPSetMetadata (IPSetMetadata'),
    newIPSetMetadata,

    -- ** IPSetReference
    IPSetReference (IPSetReference'),
    newIPSetReference,

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

    -- ** ReferenceSets
    ReferenceSets (ReferenceSets'),
    newReferenceSets,

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

    -- ** SourceMetadata
    SourceMetadata (SourceMetadata'),
    newSourceMetadata,

    -- ** StatefulEngineOptions
    StatefulEngineOptions (StatefulEngineOptions'),
    newStatefulEngineOptions,

    -- ** StatefulRule
    StatefulRule (StatefulRule'),
    newStatefulRule,

    -- ** StatefulRuleGroupOverride
    StatefulRuleGroupOverride (StatefulRuleGroupOverride'),
    newStatefulRuleGroupOverride,

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

import Amazonka.NetworkFirewall.AssociateFirewallPolicy
import Amazonka.NetworkFirewall.AssociateSubnets
import Amazonka.NetworkFirewall.CreateFirewall
import Amazonka.NetworkFirewall.CreateFirewallPolicy
import Amazonka.NetworkFirewall.CreateRuleGroup
import Amazonka.NetworkFirewall.DeleteFirewall
import Amazonka.NetworkFirewall.DeleteFirewallPolicy
import Amazonka.NetworkFirewall.DeleteResourcePolicy
import Amazonka.NetworkFirewall.DeleteRuleGroup
import Amazonka.NetworkFirewall.DescribeFirewall
import Amazonka.NetworkFirewall.DescribeFirewallPolicy
import Amazonka.NetworkFirewall.DescribeLoggingConfiguration
import Amazonka.NetworkFirewall.DescribeResourcePolicy
import Amazonka.NetworkFirewall.DescribeRuleGroup
import Amazonka.NetworkFirewall.DescribeRuleGroupMetadata
import Amazonka.NetworkFirewall.DisassociateSubnets
import Amazonka.NetworkFirewall.Lens
import Amazonka.NetworkFirewall.ListFirewallPolicies
import Amazonka.NetworkFirewall.ListFirewalls
import Amazonka.NetworkFirewall.ListRuleGroups
import Amazonka.NetworkFirewall.ListTagsForResource
import Amazonka.NetworkFirewall.PutResourcePolicy
import Amazonka.NetworkFirewall.TagResource
import Amazonka.NetworkFirewall.Types
import Amazonka.NetworkFirewall.UntagResource
import Amazonka.NetworkFirewall.UpdateFirewallDeleteProtection
import Amazonka.NetworkFirewall.UpdateFirewallDescription
import Amazonka.NetworkFirewall.UpdateFirewallEncryptionConfiguration
import Amazonka.NetworkFirewall.UpdateFirewallPolicy
import Amazonka.NetworkFirewall.UpdateFirewallPolicyChangeProtection
import Amazonka.NetworkFirewall.UpdateLoggingConfiguration
import Amazonka.NetworkFirewall.UpdateRuleGroup
import Amazonka.NetworkFirewall.UpdateSubnetChangeProtection
import Amazonka.NetworkFirewall.Waiters

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
