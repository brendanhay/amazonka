{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Lens
  ( -- * Operations

    -- ** DescribeParameters
    describeParameters_nextToken,
    describeParameters_maxResults,
    describeParameters_source,
    describeParameters_parameterGroupName,
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,

    -- ** DescribeDefaultParameters
    describeDefaultParameters_nextToken,
    describeDefaultParameters_maxResults,
    describeDefaultParametersResponse_nextToken,
    describeDefaultParametersResponse_parameters,
    describeDefaultParametersResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_nextToken,
    describeClusters_maxResults,
    describeClusters_clusterNames,
    describeClustersResponse_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,

    -- ** CreateCluster
    createCluster_securityGroupIds,
    createCluster_availabilityZones,
    createCluster_sSESpecification,
    createCluster_preferredMaintenanceWindow,
    createCluster_tags,
    createCluster_notificationTopicArn,
    createCluster_parameterGroupName,
    createCluster_description,
    createCluster_subnetGroupName,
    createCluster_clusterName,
    createCluster_nodeType,
    createCluster_replicationFactor,
    createCluster_iamRoleArn,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceName,
    untagResource_tagKeys,
    untagResourceResponse_tags,
    untagResourceResponse_httpStatus,

    -- ** DecreaseReplicationFactor
    decreaseReplicationFactor_availabilityZones,
    decreaseReplicationFactor_nodeIdsToRemove,
    decreaseReplicationFactor_clusterName,
    decreaseReplicationFactor_newReplicationFactor,
    decreaseReplicationFactorResponse_cluster,
    decreaseReplicationFactorResponse_httpStatus,

    -- ** DescribeParameterGroups
    describeParameterGroups_nextToken,
    describeParameterGroups_maxResults,
    describeParameterGroups_parameterGroupNames,
    describeParameterGroupsResponse_nextToken,
    describeParameterGroupsResponse_parameterGroups,
    describeParameterGroupsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceName,
    tagResource_tags,
    tagResourceResponse_tags,
    tagResourceResponse_httpStatus,

    -- ** IncreaseReplicationFactor
    increaseReplicationFactor_availabilityZones,
    increaseReplicationFactor_clusterName,
    increaseReplicationFactor_newReplicationFactor,
    increaseReplicationFactorResponse_cluster,
    increaseReplicationFactorResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_nextToken,
    describeEvents_duration,
    describeEvents_maxResults,
    describeEvents_startTime,
    describeEvents_sourceName,
    describeEvents_endTime,
    describeEvents_sourceType,
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** DeleteParameterGroup
    deleteParameterGroup_parameterGroupName,
    deleteParameterGroupResponse_deletionMessage,
    deleteParameterGroupResponse_httpStatus,

    -- ** UpdateParameterGroup
    updateParameterGroup_parameterGroupName,
    updateParameterGroup_parameterNameValues,
    updateParameterGroupResponse_parameterGroup,
    updateParameterGroupResponse_httpStatus,

    -- ** RebootNode
    rebootNode_clusterName,
    rebootNode_nodeId,
    rebootNodeResponse_cluster,
    rebootNodeResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_clusterName,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_securityGroupIds,
    updateCluster_notificationTopicStatus,
    updateCluster_preferredMaintenanceWindow,
    updateCluster_notificationTopicArn,
    updateCluster_parameterGroupName,
    updateCluster_description,
    updateCluster_clusterName,
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_resourceName,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** CreateSubnetGroup
    createSubnetGroup_description,
    createSubnetGroup_subnetGroupName,
    createSubnetGroup_subnetIds,
    createSubnetGroupResponse_subnetGroup,
    createSubnetGroupResponse_httpStatus,

    -- ** UpdateSubnetGroup
    updateSubnetGroup_subnetIds,
    updateSubnetGroup_description,
    updateSubnetGroup_subnetGroupName,
    updateSubnetGroupResponse_subnetGroup,
    updateSubnetGroupResponse_httpStatus,

    -- ** DeleteSubnetGroup
    deleteSubnetGroup_subnetGroupName,
    deleteSubnetGroupResponse_deletionMessage,
    deleteSubnetGroupResponse_httpStatus,

    -- ** DescribeSubnetGroups
    describeSubnetGroups_nextToken,
    describeSubnetGroups_maxResults,
    describeSubnetGroups_subnetGroupNames,
    describeSubnetGroupsResponse_subnetGroups,
    describeSubnetGroupsResponse_nextToken,
    describeSubnetGroupsResponse_httpStatus,

    -- ** CreateParameterGroup
    createParameterGroup_description,
    createParameterGroup_parameterGroupName,
    createParameterGroupResponse_parameterGroup,
    createParameterGroupResponse_httpStatus,

    -- * Types

    -- ** Cluster
    cluster_clusterArn,
    cluster_subnetGroup,
    cluster_iamRoleArn,
    cluster_status,
    cluster_totalNodes,
    cluster_parameterGroup,
    cluster_nodes,
    cluster_notificationConfiguration,
    cluster_securityGroups,
    cluster_activeNodes,
    cluster_preferredMaintenanceWindow,
    cluster_description,
    cluster_sSEDescription,
    cluster_clusterDiscoveryEndpoint,
    cluster_nodeIdsToRemove,
    cluster_nodeType,
    cluster_clusterName,

    -- ** Endpoint
    endpoint_address,
    endpoint_port,

    -- ** Event
    event_message,
    event_sourceName,
    event_date,
    event_sourceType,

    -- ** Node
    node_nodeStatus,
    node_nodeId,
    node_parameterGroupStatus,
    node_availabilityZone,
    node_nodeCreateTime,
    node_endpoint,

    -- ** NodeTypeSpecificValue
    nodeTypeSpecificValue_value,
    nodeTypeSpecificValue_nodeType,

    -- ** NotificationConfiguration
    notificationConfiguration_topicStatus,
    notificationConfiguration_topicArn,

    -- ** Parameter
    parameter_changeType,
    parameter_allowedValues,
    parameter_source,
    parameter_parameterValue,
    parameter_parameterType,
    parameter_parameterName,
    parameter_description,
    parameter_dataType,
    parameter_isModifiable,
    parameter_nodeTypeSpecificValues,

    -- ** ParameterGroup
    parameterGroup_parameterGroupName,
    parameterGroup_description,

    -- ** ParameterGroupStatus
    parameterGroupStatus_nodeIdsToReboot,
    parameterGroupStatus_parameterGroupName,
    parameterGroupStatus_parameterApplyStatus,

    -- ** ParameterNameValue
    parameterNameValue_parameterValue,
    parameterNameValue_parameterName,

    -- ** SSEDescription
    sSEDescription_status,

    -- ** SSESpecification
    sSESpecification_enabled,

    -- ** SecurityGroupMembership
    securityGroupMembership_status,
    securityGroupMembership_securityGroupIdentifier,

    -- ** Subnet
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- ** SubnetGroup
    subnetGroup_description,
    subnetGroup_subnetGroupName,
    subnetGroup_subnets,
    subnetGroup_vpcId,

    -- ** Tag
    tag_key,
    tag_value,
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
import Network.AWS.DAX.ListTags
import Network.AWS.DAX.RebootNode
import Network.AWS.DAX.TagResource
import Network.AWS.DAX.Types.Cluster
import Network.AWS.DAX.Types.Endpoint
import Network.AWS.DAX.Types.Event
import Network.AWS.DAX.Types.Node
import Network.AWS.DAX.Types.NodeTypeSpecificValue
import Network.AWS.DAX.Types.NotificationConfiguration
import Network.AWS.DAX.Types.Parameter
import Network.AWS.DAX.Types.ParameterGroup
import Network.AWS.DAX.Types.ParameterGroupStatus
import Network.AWS.DAX.Types.ParameterNameValue
import Network.AWS.DAX.Types.SSEDescription
import Network.AWS.DAX.Types.SSESpecification
import Network.AWS.DAX.Types.SecurityGroupMembership
import Network.AWS.DAX.Types.Subnet
import Network.AWS.DAX.Types.SubnetGroup
import Network.AWS.DAX.Types.Tag
import Network.AWS.DAX.UntagResource
import Network.AWS.DAX.UpdateCluster
import Network.AWS.DAX.UpdateParameterGroup
import Network.AWS.DAX.UpdateSubnetGroup
