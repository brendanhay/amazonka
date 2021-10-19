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

    -- ** DescribeClusters
    describeClusters_clusterNames,
    describeClusters_nextToken,
    describeClusters_maxResults,
    describeClustersResponse_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,

    -- ** DescribeParameters
    describeParameters_nextToken,
    describeParameters_source,
    describeParameters_maxResults,
    describeParameters_parameterGroupName,
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_sourceName,
    describeEvents_startTime,
    describeEvents_sourceType,
    describeEvents_nextToken,
    describeEvents_endTime,
    describeEvents_duration,
    describeEvents_maxResults,
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,

    -- ** IncreaseReplicationFactor
    increaseReplicationFactor_availabilityZones,
    increaseReplicationFactor_clusterName,
    increaseReplicationFactor_newReplicationFactor,
    increaseReplicationFactorResponse_cluster,
    increaseReplicationFactorResponse_httpStatus,

    -- ** CreateSubnetGroup
    createSubnetGroup_description,
    createSubnetGroup_subnetGroupName,
    createSubnetGroup_subnetIds,
    createSubnetGroupResponse_subnetGroup,
    createSubnetGroupResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_clusterName,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_securityGroupIds,
    updateCluster_preferredMaintenanceWindow,
    updateCluster_notificationTopicStatus,
    updateCluster_description,
    updateCluster_notificationTopicArn,
    updateCluster_parameterGroupName,
    updateCluster_clusterName,
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,

    -- ** CreateCluster
    createCluster_securityGroupIds,
    createCluster_sSESpecification,
    createCluster_subnetGroupName,
    createCluster_clusterEndpointEncryptionType,
    createCluster_preferredMaintenanceWindow,
    createCluster_availabilityZones,
    createCluster_description,
    createCluster_notificationTopicArn,
    createCluster_tags,
    createCluster_parameterGroupName,
    createCluster_clusterName,
    createCluster_nodeType,
    createCluster_replicationFactor,
    createCluster_iamRoleArn,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** DescribeDefaultParameters
    describeDefaultParameters_nextToken,
    describeDefaultParameters_maxResults,
    describeDefaultParametersResponse_nextToken,
    describeDefaultParametersResponse_parameters,
    describeDefaultParametersResponse_httpStatus,

    -- ** DeleteParameterGroup
    deleteParameterGroup_parameterGroupName,
    deleteParameterGroupResponse_deletionMessage,
    deleteParameterGroupResponse_httpStatus,

    -- ** UpdateParameterGroup
    updateParameterGroup_parameterGroupName,
    updateParameterGroup_parameterNameValues,
    updateParameterGroupResponse_parameterGroup,
    updateParameterGroupResponse_httpStatus,

    -- ** DescribeSubnetGroups
    describeSubnetGroups_subnetGroupNames,
    describeSubnetGroups_nextToken,
    describeSubnetGroups_maxResults,
    describeSubnetGroupsResponse_subnetGroups,
    describeSubnetGroupsResponse_nextToken,
    describeSubnetGroupsResponse_httpStatus,

    -- ** CreateParameterGroup
    createParameterGroup_description,
    createParameterGroup_parameterGroupName,
    createParameterGroupResponse_parameterGroup,
    createParameterGroupResponse_httpStatus,

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

    -- ** DescribeParameterGroups
    describeParameterGroups_nextToken,
    describeParameterGroups_parameterGroupNames,
    describeParameterGroups_maxResults,
    describeParameterGroupsResponse_nextToken,
    describeParameterGroupsResponse_parameterGroups,
    describeParameterGroupsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceName,
    tagResource_tags,
    tagResourceResponse_tags,
    tagResourceResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_resourceName,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DecreaseReplicationFactor
    decreaseReplicationFactor_nodeIdsToRemove,
    decreaseReplicationFactor_availabilityZones,
    decreaseReplicationFactor_clusterName,
    decreaseReplicationFactor_newReplicationFactor,
    decreaseReplicationFactorResponse_cluster,
    decreaseReplicationFactorResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceName,
    untagResource_tagKeys,
    untagResourceResponse_tags,
    untagResourceResponse_httpStatus,

    -- ** RebootNode
    rebootNode_clusterName,
    rebootNode_nodeId,
    rebootNodeResponse_cluster,
    rebootNodeResponse_httpStatus,

    -- * Types

    -- ** Cluster
    cluster_status,
    cluster_iamRoleArn,
    cluster_clusterArn,
    cluster_activeNodes,
    cluster_securityGroups,
    cluster_notificationConfiguration,
    cluster_nodeIdsToRemove,
    cluster_clusterEndpointEncryptionType,
    cluster_totalNodes,
    cluster_preferredMaintenanceWindow,
    cluster_subnetGroup,
    cluster_clusterName,
    cluster_nodeType,
    cluster_nodes,
    cluster_clusterDiscoveryEndpoint,
    cluster_sSEDescription,
    cluster_description,
    cluster_parameterGroup,

    -- ** Endpoint
    endpoint_url,
    endpoint_address,
    endpoint_port,

    -- ** Event
    event_sourceName,
    event_sourceType,
    event_date,
    event_message,

    -- ** Node
    node_nodeStatus,
    node_parameterGroupStatus,
    node_availabilityZone,
    node_nodeId,
    node_endpoint,
    node_nodeCreateTime,

    -- ** NodeTypeSpecificValue
    nodeTypeSpecificValue_value,
    nodeTypeSpecificValue_nodeType,

    -- ** NotificationConfiguration
    notificationConfiguration_topicStatus,
    notificationConfiguration_topicArn,

    -- ** Parameter
    parameter_parameterValue,
    parameter_parameterType,
    parameter_source,
    parameter_isModifiable,
    parameter_dataType,
    parameter_nodeTypeSpecificValues,
    parameter_allowedValues,
    parameter_parameterName,
    parameter_description,
    parameter_changeType,

    -- ** ParameterGroup
    parameterGroup_description,
    parameterGroup_parameterGroupName,

    -- ** ParameterGroupStatus
    parameterGroupStatus_nodeIdsToReboot,
    parameterGroupStatus_parameterApplyStatus,
    parameterGroupStatus_parameterGroupName,

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
    subnetGroup_vpcId,
    subnetGroup_subnets,
    subnetGroup_subnetGroupName,
    subnetGroup_description,

    -- ** Tag
    tag_value,
    tag_key,
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
