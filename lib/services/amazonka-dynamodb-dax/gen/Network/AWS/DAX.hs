{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DAX
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
module Amazonka.DAX
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** SubnetQuotaExceededFault
    _SubnetQuotaExceededFault,

    -- ** ParameterGroupNotFoundFault
    _ParameterGroupNotFoundFault,

    -- ** InvalidParameterGroupStateFault
    _InvalidParameterGroupStateFault,

    -- ** SubnetGroupInUseFault
    _SubnetGroupInUseFault,

    -- ** ParameterGroupAlreadyExistsFault
    _ParameterGroupAlreadyExistsFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** TagQuotaPerResourceExceeded
    _TagQuotaPerResourceExceeded,

    -- ** ClusterNotFoundFault
    _ClusterNotFoundFault,

    -- ** TagNotFoundFault
    _TagNotFoundFault,

    -- ** NodeQuotaForClusterExceededFault
    _NodeQuotaForClusterExceededFault,

    -- ** InvalidClusterStateFault
    _InvalidClusterStateFault,

    -- ** ServiceLinkedRoleNotFoundFault
    _ServiceLinkedRoleNotFoundFault,

    -- ** InsufficientClusterCapacityFault
    _InsufficientClusterCapacityFault,

    -- ** NodeNotFoundFault
    _NodeNotFoundFault,

    -- ** ParameterGroupQuotaExceededFault
    _ParameterGroupQuotaExceededFault,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** SubnetInUse
    _SubnetInUse,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ClusterQuotaForCustomerExceededFault
    _ClusterQuotaForCustomerExceededFault,

    -- ** SubnetGroupNotFoundFault
    _SubnetGroupNotFoundFault,

    -- ** SubnetGroupAlreadyExistsFault
    _SubnetGroupAlreadyExistsFault,

    -- ** NodeQuotaForCustomerExceededFault
    _NodeQuotaForCustomerExceededFault,

    -- ** SubnetGroupQuotaExceededFault
    _SubnetGroupQuotaExceededFault,

    -- ** ClusterAlreadyExistsFault
    _ClusterAlreadyExistsFault,

    -- ** InvalidARNFault
    _InvalidARNFault,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeClusters (Paginated)
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** DescribeParameters (Paginated)
    DescribeParameters (DescribeParameters'),
    newDescribeParameters,
    DescribeParametersResponse (DescribeParametersResponse'),
    newDescribeParametersResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** IncreaseReplicationFactor
    IncreaseReplicationFactor (IncreaseReplicationFactor'),
    newIncreaseReplicationFactor,
    IncreaseReplicationFactorResponse (IncreaseReplicationFactorResponse'),
    newIncreaseReplicationFactorResponse,

    -- ** CreateSubnetGroup
    CreateSubnetGroup (CreateSubnetGroup'),
    newCreateSubnetGroup,
    CreateSubnetGroupResponse (CreateSubnetGroupResponse'),
    newCreateSubnetGroupResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** UpdateCluster
    UpdateCluster (UpdateCluster'),
    newUpdateCluster,
    UpdateClusterResponse (UpdateClusterResponse'),
    newUpdateClusterResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** DescribeDefaultParameters (Paginated)
    DescribeDefaultParameters (DescribeDefaultParameters'),
    newDescribeDefaultParameters,
    DescribeDefaultParametersResponse (DescribeDefaultParametersResponse'),
    newDescribeDefaultParametersResponse,

    -- ** DeleteParameterGroup
    DeleteParameterGroup (DeleteParameterGroup'),
    newDeleteParameterGroup,
    DeleteParameterGroupResponse (DeleteParameterGroupResponse'),
    newDeleteParameterGroupResponse,

    -- ** UpdateParameterGroup
    UpdateParameterGroup (UpdateParameterGroup'),
    newUpdateParameterGroup,
    UpdateParameterGroupResponse (UpdateParameterGroupResponse'),
    newUpdateParameterGroupResponse,

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

    -- ** DescribeParameterGroups (Paginated)
    DescribeParameterGroups (DescribeParameterGroups'),
    newDescribeParameterGroups,
    DescribeParameterGroupsResponse (DescribeParameterGroupsResponse'),
    newDescribeParameterGroupsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

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

    -- ** RebootNode
    RebootNode (RebootNode'),
    newRebootNode,
    RebootNodeResponse (RebootNodeResponse'),
    newRebootNodeResponse,

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

import Amazonka.DAX.CreateCluster
import Amazonka.DAX.CreateParameterGroup
import Amazonka.DAX.CreateSubnetGroup
import Amazonka.DAX.DecreaseReplicationFactor
import Amazonka.DAX.DeleteCluster
import Amazonka.DAX.DeleteParameterGroup
import Amazonka.DAX.DeleteSubnetGroup
import Amazonka.DAX.DescribeClusters
import Amazonka.DAX.DescribeDefaultParameters
import Amazonka.DAX.DescribeEvents
import Amazonka.DAX.DescribeParameterGroups
import Amazonka.DAX.DescribeParameters
import Amazonka.DAX.DescribeSubnetGroups
import Amazonka.DAX.IncreaseReplicationFactor
import Amazonka.DAX.Lens
import Amazonka.DAX.ListTags
import Amazonka.DAX.RebootNode
import Amazonka.DAX.TagResource
import Amazonka.DAX.Types
import Amazonka.DAX.UntagResource
import Amazonka.DAX.UpdateCluster
import Amazonka.DAX.UpdateParameterGroup
import Amazonka.DAX.UpdateSubnetGroup
import Amazonka.DAX.Waiters

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
