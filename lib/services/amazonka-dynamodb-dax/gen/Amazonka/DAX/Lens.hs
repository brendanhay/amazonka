{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DAX.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Lens
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
import Amazonka.DAX.ListTags
import Amazonka.DAX.RebootNode
import Amazonka.DAX.TagResource
import Amazonka.DAX.Types.Cluster
import Amazonka.DAX.Types.Endpoint
import Amazonka.DAX.Types.Event
import Amazonka.DAX.Types.Node
import Amazonka.DAX.Types.NodeTypeSpecificValue
import Amazonka.DAX.Types.NotificationConfiguration
import Amazonka.DAX.Types.Parameter
import Amazonka.DAX.Types.ParameterGroup
import Amazonka.DAX.Types.ParameterGroupStatus
import Amazonka.DAX.Types.ParameterNameValue
import Amazonka.DAX.Types.SSEDescription
import Amazonka.DAX.Types.SSESpecification
import Amazonka.DAX.Types.SecurityGroupMembership
import Amazonka.DAX.Types.Subnet
import Amazonka.DAX.Types.SubnetGroup
import Amazonka.DAX.Types.Tag
import Amazonka.DAX.UntagResource
import Amazonka.DAX.UpdateCluster
import Amazonka.DAX.UpdateParameterGroup
import Amazonka.DAX.UpdateSubnetGroup
