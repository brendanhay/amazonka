{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.DAX
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-04-19@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- DAX is a managed caching service engineered for Amazon DynamoDB. DAX
-- dramatically speeds up database reads by caching frequently-accessed
-- data from DynamoDB, so applications can access that data with
-- sub-millisecond latency. You can create a DAX cluster easily, using the
-- AWS Management Console. With a few simple modifications to your code,
-- your application can begin taking advantage of the DAX cluster and
-- realize significant improvements in read performance.
module Network.AWS.DAX
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** SubnetInUse
    _SubnetInUse,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** SubnetQuotaExceededFault
    _SubnetQuotaExceededFault,

    -- ** ParameterGroupQuotaExceededFault
    _ParameterGroupQuotaExceededFault,

    -- ** InsufficientClusterCapacityFault
    _InsufficientClusterCapacityFault,

    -- ** ServiceLinkedRoleNotFoundFault
    _ServiceLinkedRoleNotFoundFault,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** ClusterNotFoundFault
    _ClusterNotFoundFault,

    -- ** SubnetGroupQuotaExceededFault
    _SubnetGroupQuotaExceededFault,

    -- ** TagNotFoundFault
    _TagNotFoundFault,

    -- ** SubnetGroupAlreadyExistsFault
    _SubnetGroupAlreadyExistsFault,

    -- ** NodeQuotaForCustomerExceededFault
    _NodeQuotaForCustomerExceededFault,

    -- ** SubnetGroupNotFoundFault
    _SubnetGroupNotFoundFault,

    -- ** ParameterGroupAlreadyExistsFault
    _ParameterGroupAlreadyExistsFault,

    -- ** ParameterGroupNotFoundFault
    _ParameterGroupNotFoundFault,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** NodeNotFoundFault
    _NodeNotFoundFault,

    -- ** NodeQuotaForClusterExceededFault
    _NodeQuotaForClusterExceededFault,

    -- ** ClusterAlreadyExistsFault
    _ClusterAlreadyExistsFault,

    -- ** InvalidARNFault
    _InvalidARNFault,

    -- ** InvalidClusterStateFault
    _InvalidClusterStateFault,

    -- ** TagQuotaPerResourceExceeded
    _TagQuotaPerResourceExceeded,

    -- ** SubnetGroupInUseFault
    _SubnetGroupInUseFault,

    -- ** InvalidParameterGroupStateFault
    _InvalidParameterGroupStateFault,

    -- ** ClusterQuotaForCustomerExceededFault
    _ClusterQuotaForCustomerExceededFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeParameters (Paginated)
    DescribeParameters (DescribeParameters'),
    newDescribeParameters,
    DescribeParametersResponse (DescribeParametersResponse'),
    newDescribeParametersResponse,

    -- ** DescribeClusters (Paginated)
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** DescribeDefaultParameters (Paginated)
    DescribeDefaultParameters (DescribeDefaultParameters'),
    newDescribeDefaultParameters,
    DescribeDefaultParametersResponse (DescribeDefaultParametersResponse'),
    newDescribeDefaultParametersResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** DecreaseReplicationFactor
    DecreaseReplicationFactor (DecreaseReplicationFactor'),
    newDecreaseReplicationFactor,
    DecreaseReplicationFactorResponse (DecreaseReplicationFactorResponse'),
    newDecreaseReplicationFactorResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeParameterGroups (Paginated)
    DescribeParameterGroups (DescribeParameterGroups'),
    newDescribeParameterGroups,
    DescribeParameterGroupsResponse (DescribeParameterGroupsResponse'),
    newDescribeParameterGroupsResponse,

    -- ** IncreaseReplicationFactor
    IncreaseReplicationFactor (IncreaseReplicationFactor'),
    newIncreaseReplicationFactor,
    IncreaseReplicationFactorResponse (IncreaseReplicationFactorResponse'),
    newIncreaseReplicationFactorResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** UpdateParameterGroup
    UpdateParameterGroup (UpdateParameterGroup'),
    newUpdateParameterGroup,
    UpdateParameterGroupResponse (UpdateParameterGroupResponse'),
    newUpdateParameterGroupResponse,

    -- ** DeleteParameterGroup
    DeleteParameterGroup (DeleteParameterGroup'),
    newDeleteParameterGroup,
    DeleteParameterGroupResponse (DeleteParameterGroupResponse'),
    newDeleteParameterGroupResponse,

    -- ** RebootNode
    RebootNode (RebootNode'),
    newRebootNode,
    RebootNodeResponse (RebootNodeResponse'),
    newRebootNodeResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** UpdateCluster
    UpdateCluster (UpdateCluster'),
    newUpdateCluster,
    UpdateClusterResponse (UpdateClusterResponse'),
    newUpdateClusterResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** CreateSubnetGroup
    CreateSubnetGroup (CreateSubnetGroup'),
    newCreateSubnetGroup,
    CreateSubnetGroupResponse (CreateSubnetGroupResponse'),
    newCreateSubnetGroupResponse,

    -- ** UpdateSubnetGroup
    UpdateSubnetGroup (UpdateSubnetGroup'),
    newUpdateSubnetGroup,
    UpdateSubnetGroupResponse (UpdateSubnetGroupResponse'),
    newUpdateSubnetGroupResponse,

    -- ** DeleteSubnetGroup
    DeleteSubnetGroup (DeleteSubnetGroup'),
    newDeleteSubnetGroup,
    DeleteSubnetGroupResponse (DeleteSubnetGroupResponse'),
    newDeleteSubnetGroupResponse,

    -- ** DescribeSubnetGroups (Paginated)
    DescribeSubnetGroups (DescribeSubnetGroups'),
    newDescribeSubnetGroups,
    DescribeSubnetGroupsResponse (DescribeSubnetGroupsResponse'),
    newDescribeSubnetGroupsResponse,

    -- ** CreateParameterGroup
    CreateParameterGroup (CreateParameterGroup'),
    newCreateParameterGroup,
    CreateParameterGroupResponse (CreateParameterGroupResponse'),
    newCreateParameterGroupResponse,

    -- * Types

    -- ** ChangeType
    ChangeType (..),

    -- ** ClusterEndpointEncryptionType
    ClusterEndpointEncryptionType (..),

    -- ** IsModifiable
    IsModifiable (..),

    -- ** ParameterType
    ParameterType (..),

    -- ** SSEStatus
    SSEStatus (..),

    -- ** SourceType
    SourceType (..),

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** Node
    Node (Node'),
    newNode,

    -- ** NodeTypeSpecificValue
    NodeTypeSpecificValue (NodeTypeSpecificValue'),
    newNodeTypeSpecificValue,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** Parameter
    Parameter (Parameter'),
    newParameter,

    -- ** ParameterGroup
    ParameterGroup (ParameterGroup'),
    newParameterGroup,

    -- ** ParameterGroupStatus
    ParameterGroupStatus (ParameterGroupStatus'),
    newParameterGroupStatus,

    -- ** ParameterNameValue
    ParameterNameValue (ParameterNameValue'),
    newParameterNameValue,

    -- ** SSEDescription
    SSEDescription (SSEDescription'),
    newSSEDescription,

    -- ** SSESpecification
    SSESpecification (SSESpecification'),
    newSSESpecification,

    -- ** SecurityGroupMembership
    SecurityGroupMembership (SecurityGroupMembership'),
    newSecurityGroupMembership,

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** SubnetGroup
    SubnetGroup (SubnetGroup'),
    newSubnetGroup,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.DAX.CreateCluster
import Network.AWS.DAX.CreateParameterGroup
import Network.AWS.DAX.CreateSubnetGroup
import Network.AWS.DAX.DecreaseReplicationFactor
import Network.AWS.DAX.DeleteCluster
import Network.AWS.DAX.DeleteParameterGroup
import Network.AWS.DAX.DeleteSubnetGroup
import Network.AWS.DAX.DescribeClusters
import Network.AWS.DAX.DescribeDefaultParameters
import Network.AWS.DAX.DescribeEvents
import Network.AWS.DAX.DescribeParameterGroups
import Network.AWS.DAX.DescribeParameters
import Network.AWS.DAX.DescribeSubnetGroups
import Network.AWS.DAX.IncreaseReplicationFactor
import Network.AWS.DAX.Lens
import Network.AWS.DAX.ListTags
import Network.AWS.DAX.RebootNode
import Network.AWS.DAX.TagResource
import Network.AWS.DAX.Types
import Network.AWS.DAX.UntagResource
import Network.AWS.DAX.UpdateCluster
import Network.AWS.DAX.UpdateParameterGroup
import Network.AWS.DAX.UpdateSubnetGroup
import Network.AWS.DAX.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DAX'.

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
